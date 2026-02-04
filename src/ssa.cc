#include "compiler.hh"
#include "new-ir.hh"
#include "parser.hh"

namespace new_ir::ssa {

struct IncompletePhi {
    Inst *key;
    PhiInst *phi;
    InstType type;
};

static std::vector<SSAInst *> ssa_transforms;
static std::unordered_map<Inst *, size_t> ssa_reads;
static std::unordered_map<BasicBlock *, std::unordered_map<std::string, Inst *>> current_def;
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

PhiInst *generate_phi(Function *fn, BasicBlock *bb, InstType type, const std::string &name)
{
    auto *phi = new PhiInst(type, name_gen[fn].get(name));
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

Inst *read_variable_recursive(
    Function *fn, BasicBlock *bb, Inst *key, InstType inst_type, const std::string &name)
{
    auto reachable_preds = get_reachable_preds(bb);
    Inst *value;

    if (!bb->sealed) {
        // Can't use key->type because it's Ptr (it's (always?) an alloca).
        // inst_type is the actual type.
        auto *phi = generate_phi(fn, bb, inst_type, name);
        incomplete_phis[bb].emplace_back(key, phi, inst_type);
        // Operands will be filled in later during block sealing.
        value = phi;
        ssa_dbgln("read_variable inserting phi @ unsealed bb{}", bb->index_in_fn);
    } else if (size(reachable_preds) == 1) {
        value = read_variable(fn, reachable_preds.back(), key, inst_type);
        ssa_dbgln("read_variable found globally @ bb{}", reachable_preds.back()->index_in_fn);
    } else if (size(reachable_preds) > 1) {
        auto *phi = generate_phi(fn, bb, inst_type, name);
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
    assert(key->operation == Operation::Alloca);
    const auto &name = static_cast<AllocaInst *>(key)->variable_name;
    ssa_dbgln("read_variable: {}", name);

    const auto &bb_it = current_def.find(bb);
    if (bb_it != current_def.end()) {
        const auto &defs = bb_it->second;
        const auto &var_it = defs.find(key->name);
        if (var_it != defs.end()) {
            ssa_dbgln("read_variable found locally @ bb{}", bb->index_in_fn);
            return var_it->second;
        }
    }
    return read_variable_recursive(fn, bb, key, inst_type, name);
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
            auto *ssa = new SSAInst(alloca->inst_type, name_gen[fn].get(alloca->variable_name));
            ssa->variable_name = alloca->variable_name;
            ssa_dbgln("transforming {} to {}", alloca->name, ssa->name);
            inst->transform_to_identity(ssa, name_gen[fn].get("id"));
            ++stats.vars_to_ssa;
            ssa_transforms.push_back(ssa);
        } else if (inst->operation == Operation::Store) {
            if (inst->args[0]->operation != Operation::Alloca) {
                continue;
            }
            auto *alloca = static_cast<AllocaInst *>(inst->args[0]);
            if (alloca->force_memory) {
                continue;
            }
            ssa_dbgln("writing {} <- {}", inst->real_arg(0)->name, inst->real_arg(1)->name);
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
            ssa_dbgln("reading {}", inst->real_arg(0)->name);
            auto *def = read_variable(fn, bb, inst->args[0], alloca->inst_type);
            ++ssa_reads[inst->real_arg(0)];
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
            if (bb->code.empty()) {
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

Inst *get_pre_terminator_inst(BasicBlock *bb)
{
    auto *last = bb->code.back();
    if (last->operation == Operation::Return) {
        return last;
    }
    if (last->operation == Operation::Branch) {
        if (!last->args.empty()) {
            // cond followed by branch, insert before cond
            // TODO: make sure that this is immediately before `last`?
            return last->args[0];
        }
        return last;
    }
    internal_error("block {} does not end on return or branch", bb->name);
}

struct Copy {
    Inst *src = nullptr;
    Inst *dst = nullptr;
};

static std::unordered_map<BasicBlock *, std::vector<Copy>> copies_map;

Inst *make_assign(Inst *dst, Inst *src, [[maybe_unused]] bool tmp)
{
    auto *assign = new Inst(Operation::Assign, InstKind::Binary, src->type, "asn");
    assign->add_arg(dst);
    assign->add_arg(src);
    ssa_dbgln("inserting assign {} = {}{}", dst->name, src->name, tmp ? " (TEMP)" : "");
    return assign;
}

void resolve_parallel_copies(Function *)
{
    std::vector<Inst *> assigns;

    for (auto &[bb, copies] : copies_map) {
        for (size_t i = 0; i < size(copies); ++i) {
            auto *cur_dst = copies[i].dst;
            bool dst_used_as_src = false;
            for (auto it = copies.begin() + static_cast<ptrdiff_t>(i); it != copies.end(); ++it) {
                if (cur_dst == it->src) {
                    dst_used_as_src = true;
                    break;
                }
            }
            auto [src, dst] = copies[i];
            if (dst_used_as_src) {
                auto *tmp = new VarInst(dst->type, std::format("tmp_{}", dst->name));
                assigns.push_back(make_assign(tmp, dst, true));
                // All future uses of this dst as a src need to use the temp instead.
                for (auto j = i + 1; j < size(copies); ++j) {
                    if (copies[j].src == cur_dst) {
                        copies[j].src = tmp;
                    }
                }
            }
            assigns.push_back(make_assign(dst, src, false));
        }
        for (auto *inst : assigns | std::views::reverse) {
            insertion_sets[bb].insert_before(get_pre_terminator_inst(bb), inst);
        }
        assigns.clear();
    }

    copies_map.clear();
}

BlockInsertionSet critical_edge_splits;

void split_critical_edge(Function *fn, BasicBlock *from, BasicBlock *to)
{
    BasicBlock *split = nullptr;

    // Fix phis in target bb to make assigns be inserted into the split block.
    for (auto *inst : to->code) {
        if (inst->operation == Operation::Phi) {
            if (!split) {
                split = new BasicBlock(fn, "crit_edge");
            }
            auto *phi = static_cast<PhiInst *>(inst);
            for (auto &[pred, _] : phi->incoming) {
                if (pred == from) {
                    pred = split;
                }
            }
        }
    }
    if (!split) {
        dbgln("{0}: skipping for {1} to {2} because {2} has no phi insts", __func__, from->name,
            to->name);
        return;
    }

    if (auto it = std::ranges::find(from->successors, to); it != from->successors.end()) {
        split->reachable = from->reachable;
        *it = split;
        if (auto it2 = std::ranges::find(to->predecessors, from); it2 != to->successors.end()) {
            *it2 = split;
        } else {
            internal_error("{}: {} not found in predecessor list", __func__, from->name);
        }
        auto *jmp
            = new Inst(Operation::Branch, InstKind::Unary, InstType::Void, name_gen[fn].get("j"));
        split->code.push_back(jmp);
        critical_edge_splits.insert_after(from->index_in_fn, split);
        return;
    }
    internal_error("{}: {} not found in successor list", __func__, to->name);
}

using TransformedPhis = std::vector<std::pair<PhiInst *, VarInst *>>;

void leave_block(Function *, BasicBlock *bb, TransformedPhis &phis)
{
    for (auto *insn : bb->code) {
        if (insn->operation != Operation::Phi) {
            continue;
        }
        auto *phi = static_cast<PhiInst *>(insn);
        auto *var = new VarInst(phi->type, phi->name);
        phis.emplace_back(phi, var);
        for (auto &[pred, value] : phi->incoming) {
            copies_map[pred].emplace_back(value, phi);
        }
    }
}

void leave(IRBuilder &irb)
{
    for (auto *ssa : ssa_transforms) {
        if (!ssa_reads.contains(ssa) && !ssa->variable_name.empty() /* ignore temps */) {
            // this doesn't mean the variable is never read, just this value
            dbgln("ssa value {} is never read", ssa->name);
        }
    }

    for (auto *fn : irb.fns) {
        TransformedPhis phis_to_transform;
        for (auto *bb : fn->blocks) {
            if (size(bb->successors) > 1) {
                for (auto *succ : bb->successors) {
                    if (size(succ->predecessors) > 1) {
                        dbgln("critical edge from {} to {}", bb->name, succ->name);
                        split_critical_edge(fn, bb, succ);
                    }
                }
            }
            if (!bb->code.empty()) {
                leave_block(fn, bb, phis_to_transform);
            }
        }
        resolve_parallel_copies(fn);
        for (auto [phi, var] : phis_to_transform) {
            phi->transform_to_identity(var, name_gen[fn].get("id"));
        }
        critical_edge_splits.execute(fn);
    }

    insertion_sets.execute_all();

#ifdef SSA_DEBUG
    ssa_dbgln("********************************* pre opt:");
    print(stdout_file(), irb, SkipUnreachable::No);
    ssa_dbgln("*********************************");
#endif

    replace_identities(irb);
    elide_assignments(irb);
}

} // namespace new_ir::ssa
