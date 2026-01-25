#include "new-ir.hh"
#include "parser.hh"

#include <algorithm>
#include <numeric>
#include <ranges>

// #define INLINE_DEBUG

#ifdef INLINE_DEBUG
#define inl_dbgln(x, ...) std::println(x __VA_OPT__(, __VA_ARGS__))
#else
#define inl_dbgln(...) (void)0
#endif

namespace new_ir {

Inst *generate_alloca(IRBuilder &irb, InstType inst_type, const std::string &name)
{
    auto *inst = new AllocaInst(name);
    inst->inst_type = inst_type;
    irb.alloca_sets.top().insert_before(irb.current_fn->last_alloca, inst);
    ++irb.current_fn->last_alloca;
    return inst;
}

bool should_inline(AstFunction *fn)
{
    switch (fn->should_inline) {
        case InlineDecision::Undecided:
            [[fallthrough]];
        case InlineDecision::TryInline:
            // TODO: decide case-by-case?
            // make up some arbitrary metric for now
            // TODO: don't inline fns containing other fn decls
            if (fn->scope /* null means we're main() */
                && !has_flag(fn->attributes, FunctionAttributes::BUILTIN_PRINT)
                && !has_flag(fn->flags, AstFlags::RECURSIVE)
                && !has_flag(fn->flags, AstFlags::HAS_NESTED_FNS) && size(fn->body->stmts) < 20) {
                fn->should_inline = InlineDecision::Inline;
                return true;
            }
            fn->should_inline = InlineDecision::NoInline;
            return false;
        case InlineDecision::Inline:
            return true;
        case InlineDecision::NoInline:
            return false;
    }
    return false;
}

BlockInsertionSet block_insertions;

void move_insts(std::vector<Inst *> &dst, std::vector<Inst *> &src, ptrdiff_t from)
{
    dst.insert(dst.end(), src.begin() + from, src.end());
    src.erase(src.begin() + from, src.end());
}

std::pair<BasicBlock *, BasicBlock *> split_call(Function *fn, BasicBlock *split, ptrdiff_t where)
{
    auto *start_block = new BasicBlock(fn, "inl.start");
    start_block->index_in_fn = split->index_in_fn + 1;

    auto *merge_block = new BasicBlock(fn, "inl.merge");
    merge_block->terminal = split->terminal;

    // TODO: helper function
    merge_block->successors = split->successors;
    for (auto *succ : merge_block->successors) {
        for (auto *&pred : succ->predecessors) {
            if (pred == split) {
                pred = merge_block;
                break;
            }
        }
    }
    split->successors.clear();
    split->terminal = false;

    // This block might depend on data from later blocks.
    // TODO: this might not be true anymore now that we put it at the end
    merge_block->sealed = false;

    move_insts(merge_block->code, split->code, where);

    // This is the block where we start putting inlined code. Insert it after the split block.
    // We can't insert the merge block yet because there might be more blocks coming and we want the
    // merge block at the end.
    block_insertions.insert_after(split->index_in_fn, start_block);

    std::ranges::iota(merge_block->code | std::views::transform(&Inst::index_in_bb), 0);

    return { start_block, merge_block };
}

void fix_inlined_insts(Function *fn, BasicBlock *new_bb, Inst *result,
    InsertionSetMap &return_stores, BasicBlock *post_bb)
{
    auto *term = new_bb->code.back();
    if (term->operation == Operation::Return) {
        if (result) {
            auto *store = new Inst(
                Operation::Store, InstKind::Binary, result->type, name_gen[fn].get("st"));
            store->add_arg(result);
            store->add_arg(term->args[0]);
            return_stores[new_bb].insert_before(term, store);
        }
        term->transform_to_jump(new_bb, post_bb, name_gen[fn].get("j"));
        // This block does not return anymore.
        new_bb->terminal = false;
    }
}

void inline_call(IRBuilder &irb, Function *fn, BasicBlock *bb, CallInst *call)
{
    // Kill PushArgs but save the operands.
    std::vector<Inst *> args;
    if (call->index_in_bb > 0) {
        for (auto i = call->index_in_bb - 1; i > 0; --i) {
            if (bb->code[i]->operation != Operation::PushArg) {
                break;
            }
            args.push_back(bb->code[i]->args[0]);
            bb->code[i]->transform_to_nop();
        }
    }
    assert(size(args) == size(call->function->params));
    // NOTE: We saved in reverse order, fix it here.
    std::ranges::reverse(args);

    // Split the current block into three:
    // bb (code until call), start_block (for start of inlined code), merge_block (code after call)
    auto [start_block, merge_block] = split_call(fn, bb, static_cast<ptrdiff_t>(call->index_in_bb));
    irb.current_fn = fn;
    irb.current_fn->current_block = bb;
    generate_jump(irb, start_block);
    irb.current_fn->current_block = start_block;

    // Allocate the return value and arguments.
    auto *result = call->function->returns_void()
        ? nullptr
        : generate_alloca(irb, to_inst_type(call->function->return_type), "addr.fret");

    for (auto *decl : call->function->params) {
        auto *alloca = generate_alloca(irb, &decl->var);
        auto *store
            = new Inst(Operation::Store, InstKind::Binary, InstType::Ptr, name_gen[fn].get("st"));
        store->add_arg(alloca);
        store->add_arg(args[decl->var.param_index]);
        irb.add(store);
    }

    // Emit the function.
    // Use the insertion set to force it right after new_bb.
    irb.block_insertion_set = &block_insertions;
    irb.block_insertion_point = bb->index_in_fn + 1;
    generate(irb, call->function->body);
    irb.block_insertion_set = nullptr;

    InsertionSetMap return_stores;

    for (auto [_, new_block] : block_insertions.insertions) {
        inl_dbgln("fixing insts for block {}", new_block->name);
        fix_inlined_insts(fn, new_block, result, return_stores, merge_block);
    }

    // NOTE: we use execute_range() later which inserts the entire set at once.
    // merge_block will be placed at the end.
    block_insertions.insert_after(block_insertions.insertions.back().pos, merge_block);

    if (result) {
        auto *load = make_load(fn, result);
        return_stores[merge_block].insert_before(merge_block->code.front(), load);
        return_stores.execute_all();
        call->transform_to_identity(load, name_gen[fn].get("id"));
    } else {
        call->transform_to_nop();
    }

    irb.current_fn->current_block = merge_block;
}

void inline_pass(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        irb.alloca_sets.emplace();
        for (auto it = fn->blocks.begin(); it != fn->blocks.end(); ++it) {
            auto *bb = *it;
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            for (auto *inst : bb->code) {
                if (inst->kind != InstKind::Identity && inst->operation == Operation::Call) {
                    auto *call = static_cast<CallInst *>(inst);
                    if (should_inline(call->function)) {
                        inl_dbgln(
                            "inlining {} into {}", demangled_name(call->function->name), fn->name);
                        inline_call(irb, fn, bb, call);
                        ++stats.inlined_calls;
                        assert(!block_insertions.insertions.empty());
                        auto skip = std::distance(fn->blocks.begin(), it)
                            + size(block_insertions.insertions) - 1;
                        block_insertions.execute_range(fn);
                        // Fix the iterator and skip over the new blocks so we don't inline too much
                        // or try to inline a function that has now become recursive.
                        it = fn->blocks.begin() + static_cast<ptrdiff_t>(skip);
                        break;
                    }
                }
            }
        }
        irb.alloca_sets.top().execute_range(fn->blocks[0]);
        irb.alloca_sets.pop();
    }
    replace_identities(irb);
}

} // namespace new_ir
