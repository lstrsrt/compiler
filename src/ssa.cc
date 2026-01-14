#include "new-ir.hh"

namespace new_ir::ssa {

static std::unordered_map<BasicBlock *, std::unordered_map<std::string, Inst *>> current_def;

struct IncompletePhi {
    Inst *key;
    PhiInst *phi;
    InstType type;
};

static std::unordered_map<BasicBlock *, std::vector<IncompletePhi>> incomplete_phis;
static InsertionSetMap insertion_sets;

std::vector<BasicBlock *> get_reachable_preds(BasicBlock *block)
{
    std::vector<BasicBlock *> reachable_preds;
    for (auto *pred : block->predecessors) {
        if (pred->reachable) {
            reachable_preds.push_back(pred);
        }
    }
    return reachable_preds;
}

PhiInst *generate_phi(Function *fn, BasicBlock *bb, InstType type)
{
    auto *phi = new PhiInst(type, name_gen[fn].get("phi"));
    insertion_sets[bb].insert_before(bb->code.front(), phi);
    return phi;
}

Inst *try_remove_trivial_phi(Function *fn, Inst *phi)
{
    Inst *same = nullptr;
    for (auto [_, value] : static_cast<PhiInst *>(phi)->incoming) {
        if (value == same || value == phi) {
            continue;
        }
        if (same) {
            ssa_dbgln("***** {} not trivial", phi->name);
            return phi;
        }
        same = value;
    }

    ssa_dbgln("***** {} trivial (always {})", phi->name, same ? same->name : "undef");
    if (!same) {
        same = new Inst(Operation::None, InstKind::Undef, phi->type, name_gen[fn].get("undef"));
    }

    phi->transform_to_identity(same, name_gen[fn].get("id"));
    return same;
}

Inst *read_variable(Function *, BasicBlock *, Inst *, InstType);

Inst *complete_phi(Function *fn, Inst *key, const std::vector<BasicBlock *> &reachable_preds,
    PhiInst *phi, InstType inst_type)
{
    for (auto *pred : reachable_preds) {
        phi->incoming.emplace_back(pred, read_variable(fn, pred, key, inst_type));
    }
    return try_remove_trivial_phi(fn, phi);
}

void write_variable(BasicBlock *bb, const std::string &key, Inst *value)
{
    current_def[bb][key] = value;
}

Inst *read_variable_recursive(Function *fn, BasicBlock *bb, Inst *key, InstType inst_type)
{
    auto reachable_preds = get_reachable_preds(bb);
    Inst *value;

    if (!bb->sealed) {
        // Can't use key->type because it's Ptr (it's (always?) an alloca).
        // inst_type is the actual type.
        auto *phi = generate_phi(fn, bb, inst_type);
        incomplete_phis[bb].emplace_back(key, phi);
        // Operands will be filled in later during block sealing.
        value = phi;
        ssa_dbgln("read_variable inserting phi @ unsealed bb{}", bb->index_in_fn);
    } else if (size(reachable_preds) == 1) {
        value = read_variable(fn, reachable_preds.back(), key, inst_type);
        ssa_dbgln("read_variable found globally @ bb{}", reachable_preds.back()->index_in_fn);
    } else if (size(reachable_preds) > 1) {
        auto *phi = generate_phi(fn, bb, inst_type);
        write_variable(bb, key->name, phi);
        value = complete_phi(fn, key, reachable_preds, phi, inst_type);
        ssa_dbgln("read_variable inserting phi @ bb{}", bb->index_in_fn);
    } else {
        value = new Inst(Operation::None, InstKind::Undef, key->type, name_gen[fn].get("undef"));
        ssa_dbgln("reachable_preds empty for sealed blk {}", bb->index_in_fn);
    }

    write_variable(bb, key->name, value);
    return value;
}

Inst *read_variable(Function *fn, BasicBlock *bb, Inst *key, InstType inst_type)
{
    const auto &bb_it = current_def.find(bb);
    if (bb_it != current_def.end()) {
        const auto &defs = bb_it->second;
        const auto &var_it = defs.find(key->name);
        if (var_it != defs.end()) {
            ssa_dbgln("read_variable found locally @ bb{}", bb->index_in_fn);
            return var_it->second;
        }
    }
    return read_variable_recursive(fn, bb, key, inst_type);
}

void seal_block(Function *fn, BasicBlock *bb)
{
    for (auto [key, phi, type] : incomplete_phis[bb]) {
        complete_phi(fn, key, get_reachable_preds(bb), phi, type);
    }
    bb->sealed = true;
}

void transform_block(Function *fn, BasicBlock *bb, std::vector<BasicBlock *> &unsealed_blocks)
{
    for (auto *inst : bb->code) {
        if (inst->operation == Operation::Alloca) {
            auto *alloca = static_cast<AllocaInst *>(inst);
            if (alloca->force_memory) {
                continue;
            }
            auto *ssa = new SSAInst(alloca->inst_type, name_gen[fn].get("ssa"));
            ssa_dbgln("transforming {} to ssa {}", alloca->name, ssa->name);
            inst->transform_to_identity(ssa, name_gen[fn].get("id"));
            ++stats.vars_to_ssa;
        } else if (inst->operation == Operation::Store) {
            if (inst->args[0]->operation != Operation::Alloca) {
                continue;
            }
            auto *alloca = static_cast<AllocaInst *>(inst->args[0]);
            if (alloca->force_memory) {
                continue;
            }
            ssa_dbgln("writing {} -> {}", inst->args[0]->name, inst->args[1]->name);
            write_variable(bb, inst->args[0]->name, inst->args[1]);
            inst->transform_to_nop();
        } else if (inst->operation == Operation::Load) {
            if (inst->args[0]->operation != Operation::Alloca) {
                continue;
            }
            auto *alloca = static_cast<AllocaInst *>(inst->args[0]);
            if (alloca->force_memory) {
                continue;
            }
            ssa_dbgln("reading {}", inst->args[0]->name);
            auto *def = read_variable(fn, bb, inst->args[0], alloca->inst_type);
            inst->transform_to_identity(def, name_gen[fn].get("id"));
        }
    }

    if (!bb->sealed) {
        unsealed_blocks.push_back(bb);
    }
}

void enter(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        std::vector<BasicBlock *> unsealed_blocks;
        for (auto *bb : fn->blocks) {
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            transform_block(fn, bb, unsealed_blocks);
        }
        for (auto *bb : unsealed_blocks) {
            seal_block(fn, bb);
        }
        incomplete_phis.clear();
        insertion_sets.execute_all();
    }
}

Inst *get_pre_terminator_inst(BasicBlock *bb)
{
    auto *last = bb->code.back();
    if (last->operation == Operation::Return) {
        return last;
    }
    if (last->operation == Operation::Branch) {
        if (!last->args.empty()) {
            // cond followed by branch, insert before cond
            return last->args[0];
        }
        return last;
    }
    die("block should end on return or branch");
}

void leave_block(Function *fn, BasicBlock *bb)
{
    for (auto *insn : bb->code) {
        if (insn->operation == Operation::Phi) {
            auto *phi = static_cast<PhiInst *>(insn);
            auto *ident = new VarInst(phi->type, name_gen[fn].get("fphi"));
            for (auto &[pred, value] : phi->incoming) {
                ssa_dbgln("inserting assign {} = {} for {}", ident->name, value->name, phi->name);
                auto *assign = new Inst(Operation::Assign, InstKind::Binary, value->type, "asn");
                assign->add_arg(ident);
                assign->add_arg(value);
                insertion_sets[pred].insert_before(get_pre_terminator_inst(pred), assign);
            }
            phi->transform_to_identity(ident, name_gen[fn].get("id"));
            continue;
        }
    }
}

void elide_assignments(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        for (auto *bb : fn->blocks) {
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            for (auto *inst : bb->code) {
                if (inst->operation == Operation::Assign) {
                    // Can happen after identities are resolved.
                    if (inst->args[0] == inst->args[1]) {
                        inst->transform_to_nop();
                    }
                }
            }
        }
    }
}

void leave(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        for (auto *bb : fn->blocks) {
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            leave_block(fn, bb);
        }
    }

    insertion_sets.execute_all();

#ifdef SSA_DEBUG
    ssa_dbgln("********************************* pre opt:");
    print(stdout_file(), irb);
    ssa_dbgln("*********************************");
#endif

    replace_identities(irb);
    elide_assignments(irb);
    dce_sweep(irb);
}

} // namespace new_ir::ssa
