#include "new-ir.hh"
#include "base.hh"
#include "compiler.hh"
#include "debug.hh"
#include "diagnose.hh"
#include "parser.hh"
#include "verify.hh"

#include <algorithm>
#include <ranges>
#include <unordered_set>

// TODO: validation pass

#ifdef alloca
#undef alloca
#endif

namespace new_ir {

static std::unordered_map<Variable *, Inst *> alloca_map;

Inst *get_alloca(Variable *key)
{
    auto it = alloca_map.find(key);
    assert(it != alloca_map.end());
    return it->second;
}

// TODO: lots of shared code

void InsertionSet::insert_before(Inst *at, Inst *new_inst)
{
    insert_before(at->index_in_bb, new_inst);
}

void InsertionSet::insert_before(size_t index_in_bb, Inst *new_inst)
{
    insertions.emplace_back(index_in_bb, new_inst);
}

void InsertionSet::insert_after(Inst *at, Inst *new_inst)
{
    insert_after(at->index_in_bb, new_inst);
}

void InsertionSet::insert_after(size_t index_in_bb, Inst *new_inst)
{
    insertions.emplace_back(index_in_bb + 1, new_inst);
}

void InsertionSet::execute(BasicBlock *bb)
{
    if (insertions.empty()) {
        return;
    }

    // TODO: only sort if we have to.
    // put a check into insert_after/insert_before to that effect.
    std::ranges::stable_sort(insertions, std::ranges::greater{}, &Insertion::pos);

    for (auto &insertion : insertions) {
        bb->code.insert(bb->code.begin() + static_cast<ptrdiff_t>(insertion.pos), insertion.inst);
    }

    for (size_t i = insertions.back().pos; i < size(bb->code); ++i) {
        bb->code[i]->index_in_bb = i;
    }

    insertions.clear();
}

// This assumes that we have a continuous range sorted lowest to highest.
void InsertionSet::execute_range(BasicBlock *bb)
{
    if (insertions.empty()) {
        return;
    }

#ifdef _DEBUG
    size_t last;
    for (const auto &insertion : insertions) {
        last = insertion.pos;
        assert(insertion.pos == last || insertion.pos == last + 1);
    }
#endif

    const auto first = insertions.front().pos;

    bb->code.insert_range(bb->code.begin() + static_cast<ptrdiff_t>(first),
        insertions | std::views::transform(&Insertion::inst));

    for (size_t i = first; i < size(bb->code); ++i) {
        bb->code[i]->index_in_bb = i;
    }

    insertions.clear();
}

void BlockInsertionSet::insert_after(size_t index_in_fn, BasicBlock *new_block)
{
    insertions.emplace_back(index_in_fn + 1, new_block);
}

void BlockInsertionSet::insert_before(size_t index_in_fn, BasicBlock *new_block)
{
    insertions.emplace_back(index_in_fn, new_block);
}

void BlockInsertionSet::execute(Function *fn)
{
    if (insertions.empty()) {
        return;
    }

    std::ranges::stable_sort(insertions, std::ranges::greater{}, &BlockInsertion::pos);

    for (auto &insertion : insertions) {
        fn->blocks.insert(
            fn->blocks.begin() + static_cast<ptrdiff_t>(insertion.pos), insertion.block);
    }

    for (size_t i = insertions.back().pos; i < size(fn->blocks); ++i) {
        fn->blocks[i]->index_in_fn = i;
    }

    insertions.clear();
}

void BlockInsertionSet::execute_range(Function *fn)
{
    if (insertions.empty()) {
        return;
    }

#ifdef _DEBUG
    size_t last;
    for (const auto &insertion : insertions) {
        last = insertion.pos;
        assert(insertion.pos == last || insertion.pos == last + 1);
    }
#endif

    const auto first = insertions.front().pos;

    fn->blocks.insert_range(fn->blocks.begin() + static_cast<ptrdiff_t>(first),
        insertions | std::views::transform(&BlockInsertion::block));

    for (size_t i = first; i < size(fn->blocks); ++i) {
        fn->blocks[i]->index_in_fn = i;
    }

    insertions.clear();
}

void Inst::transform_to_nop()
{
    // FIXME: transforming non-basic inst will leak memory on free
    kind = InstKind::Nop;
    type = InstType::Void;
    operation = Operation::None;
    // don't change the name for easier debugging
    args.clear();
}

void Inst::transform_to_identity(Inst *id, const std::string &new_name)
{
    // FIXME: transforming non-basic inst will leak memory on free
    kind = InstKind::Identity;
    type = id->type;
    name = new_name;
    // TODO: we can't change this->operation because SSA gen checks for operation == Alloca
    // operation = id->operation;
    args.clear();
    add_arg(id);
}

void Inst::transform_to_jump(BasicBlock *cur, BasicBlock *target, const std::string &new_name)
{
    kind = InstKind::Unary;
    operation = Operation::Branch;
    type = InstType::Void;
    name = new_name;
    args.clear();
    cur->add_successor(target);
}

Inst *generate_identifier(IRBuilder &, Variable *);

Inst *IRBuilder::add(Inst *inst) const
{
    auto *bb = current_fn->current_block;
    bb->code.push_back(inst);
    inst->index_in_bb = size(bb->code) - 1;
    return inst;
}

InstType to_inst_type(Type *t)
{
    assert(t);
    t = get_unaliased_type(t);
    if (t == void_type()) {
        return InstType::Void;
    }
    if (t->is_pointer()) {
        return InstType::Ptr;
    }
    if (t->size == 1) {
        return InstType::S1;
    }
    if (t->has_flag(TypeFlags::ENUM)) {
        return to_inst_type(t->real);
    }
    bool is_signed = t->is_signed();
    switch (t->size) {
        case 8:
            return is_signed ? InstType::S8 : InstType::U8;
        case 16:
            return is_signed ? InstType::S16 : InstType::U16;
        case 32:
            return is_signed ? InstType::S32 : InstType::U32;
        case 64:
            return is_signed ? InstType::S64 : InstType::U64;
    }
    TODO();
}

Inst *make_load(Function *fn, Inst *inst)
{
    auto *load
        = new Inst(Operation::Load, InstKind::Unary, InstType::Ptr, name_gen[fn].get("ld.tmp"));
    load->add_arg(inst);
    return load;
}

Inst *make_load(Function *fn, Variable *var, Inst *inst)
{
    auto *load = new Inst(Operation::Load, InstKind::Unary, to_inst_type(var->type),
        name_gen[fn].get(std::format("ld.{}", var->name.substr(0, 5))));
    load->add_arg(inst);
    return load;
}

ConstInst *make_const(AstLiteral *constant, const std::string &name)
{
    auto *c = new ConstInst(to_inst_type(constant), name);
    c->constant = constant->u.any;
    return c;
}

Inst *make_alloca(Variable *var)
{
    auto *inst = new AllocaInst(std::format("addr.{}", var->name.substr(0, 5)));
    inst->inst_type = to_inst_type(var->type);
    inst->variable_name = var->name;
    inst->force_memory = var->force_stack;
    return inst;
}

Inst *generate_alloca(IRBuilder &irb, Variable *var)
{
    auto *alloca = make_alloca(var);
    auto &code = irb.current_fn->blocks[0]->code;
    if (code.empty()) {
        irb.add(alloca);
    } else {
        irb.alloca_sets.top().insert_before(irb.current_fn->last_alloca, alloca);
    }
    ++irb.current_fn->last_alloca;
    alloca_map[var] = alloca;
    return alloca;
}

Inst *generate_undef(IRBuilder &irb, Type *type)
{
    auto *inst = new Inst(Operation::None, InstKind::Undef, to_inst_type(type),
        name_gen[irb.current_fn].get("undef"));
    irb.add(inst);
    return inst;
}

Inst *generate_var_decl(IRBuilder &irb, Ast *ast)
{
    auto *var_decl = static_cast<AstVariableDecl *>(ast);
    auto *alloca = generate_alloca(irb, &var_decl->var);
    auto *expr = var_decl->init_expr ? generate(irb, var_decl->init_expr)
                                     : generate_undef(irb, var_decl->var.type);
    auto *store = new Inst(
        Operation::Store, InstKind::Binary, InstType::Ptr, name_gen[irb.current_fn].get("st"));
    store->add_arg(alloca);
    store->add_arg(expr);
    return irb.add(store);
}

void replace_identities(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        std::vector<Inst *> identities;
        for (auto *bb : fn->blocks) {
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            for (auto *inst : bb->code) {
                if (inst->kind == InstKind::Identity) {
                    identities.push_back(inst);
                }
                for (auto *&arg : inst->args) {
                    // Loop because we can have nested identities
                    while (arg->kind == InstKind::Identity) {
                        identities.push_back(arg);
                        // dbgln("replacing id {} with {}", arg->name, arg->args[0]->name);
                        arg = arg->args[0];
                    }
                }
            }
        }
        for (auto *id : identities) {
            id->transform_to_nop();
        }
    }
}

void dce_sweep(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        for (auto *bb : fn->blocks) {
            bool deleted = false;
            for (auto it = bb->code.begin(); it != bb->code.end();) {
                if ((*it)->kind == InstKind::Nop) {
                    void free(Inst *);
                    free(*it);
                    it = bb->code.erase(it);
                    ++stats.insts_killed;
                    deleted = true;
                } else {
                    if (deleted) {
                        (*it)->index_in_bb = std::distance(bb->code.begin(), it);
                    }
                    ++it;
                }
            }
        }
    }
}

enum class KnownUnique {
    No,
    Yes,
};

BasicBlock *add_block(IRBuilder &irb, const std::string &name, KnownUnique unique = KnownUnique::No)
{
    auto *bb = new BasicBlock;
    if (unique == KnownUnique::No) {
        bb->name = name_gen[irb.current_fn].get(name);
    } else {
        bb->name = name;
    }
    if (irb.block_insertion_set) {
        irb.block_insertion_set->insert_before(irb.block_insertion_point, bb);
    } else {
        bb->index_in_fn = size(irb.current_fn->blocks);
        irb.current_fn->blocks.push_back(bb);
    }
    return bb;
}

void add_existing_block(IRBuilder &irb, Function *fn, BasicBlock *bb)
{
    if (irb.block_insertion_set) {
        irb.block_insertion_set->insert_before(irb.block_insertion_point, bb);
    } else {
        bb->index_in_fn = size(fn->blocks);
        fn->blocks.push_back(bb);
    }
}

void make_function(IRBuilder &irb, AstFunction *fn)
{
    if (!opts.testing) {
        if (has_flag(fn->attributes, FunctionAttributes::DUMP_AST)) {
            std::println("{}============= {}AST for `{}`{} ============={}", colors::Green,
                colors::DefaultBold, fn->name, colors::Green, colors::Default);
            print_ast(stdout_file(), fn);
        }
    }

    irb.current_fn = new Function;
    irb.current_fn->ast = fn;
    irb.current_fn->current_block = add_block(irb, "entry", KnownUnique::Yes);
    irb.current_fn->current_block->reachable = true;
    irb.current_fn->name = fn->name;
    irb.current_fn->location = fn->location;
    irb.alloca_sets.emplace();
    irb.fns.push_back(irb.current_fn);
}

Inst *generate_string(IRBuilder &irb, Ast *ast)
{
    auto *s = new StringInst(name_gen[irb.current_fn].get("str"));
    s->string = &static_cast<AstString *>(ast)->string;
    irb.add(s);
    return s;
}

Inst *generate_const(IRBuilder &irb, Ast *ast)
{
    auto *c = make_const(static_cast<AstLiteral *>(ast), name_gen[irb.current_fn].get("cst"));
    irb.add(c);
    return c;
}

Inst *generate_binary(IRBuilder &irb, Ast *ast)
{
    auto *binary = static_cast<AstBinary *>(ast);

    if (ast->operation == Operation::Assign) {
        auto *store = new Inst(
            Operation::Store, InstKind::Binary, InstType::Ptr, name_gen[irb.current_fn].get("st"));
        store->add_arg(get_alloca(static_cast<AstIdentifier *>(binary->left)->var));
        store->add_arg(generate(irb, binary->right));
        return irb.add(store);
    }

    bool is_store = ast->operation == Operation::Store;
    auto inst_type = to_inst_type(is_store ? binary->left : ast);
    // TODO: strings for other binops
    std::string str = is_store ? "st" : "bin";
    auto *inst
        = new Inst(ast->operation, InstKind::Binary, inst_type, name_gen[irb.current_fn].get(str));
    inst->add_arg(generate(irb, binary->left));
    inst->add_arg(generate(irb, binary->right));
    irb.add(inst);
    return inst;
}

Inst *generate_jump(IRBuilder &irb, BasicBlock *target)
{
    auto *inst = new Inst(
        Operation::Branch, InstKind::Unary, InstType::Void, name_gen[irb.current_fn].get("j"));
    irb.current_fn->current_block->add_successor(target);
    irb.add(inst);
    return inst;
}

Inst *generate_branch(IRBuilder &irb, Ast *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *cond = generate(irb, ast);
    auto *inst = new Inst(
        Operation::Branch, InstKind::Unary, InstType::Void, name_gen[irb.current_fn].get("br"));
    irb.current_fn->current_block->add_successor(true_block);
    irb.current_fn->current_block->add_successor(false_block);
    inst->add_arg(cond);
    irb.add(inst);
    return inst;
}

Inst *generate_return(IRBuilder &irb, Ast *ast)
{
    auto *ret = static_cast<AstReturn *>(ast);
    auto *inst = new Inst(Operation::Return, InstKind::Unary, InstType::Void, "ret");
    if (ret->expr) {
        inst->add_arg(generate(irb, ret->expr));
    }
    irb.add(inst);
    irb.current_fn->current_block->terminal = true;
    return inst;
}

void generate_logical_or(
    IRBuilder &irb, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block);

void generate_logical_and(
    IRBuilder &irb, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *fn = irb.current_fn;
    std::vector<Ast *> flattened;
    flatten_binary(ast, Operation::LogicalAnd, flattened);

    for (size_t i = 0; i < size(flattened); ++i) {
        bool is_final = i == size(flattened) - 1;
        if (flattened[i]->operation == Operation::LogicalOr) {
            auto *true_or = new BasicBlock(irb.current_fn, "then");
            generate_logical_or(irb, static_cast<AstBinary *>(flattened[i]), true_or, false_block);
            add_existing_block(irb, fn, true_or);
            fn->current_block = true_or;
            if (is_final) {
                generate_jump(irb, true_block);
            }
        } else {
            if (is_final) {
                generate_branch(irb, flattened[i], true_block, false_block);
            } else {
                auto *next = add_block(irb, "next");
                generate_branch(irb, flattened[i], next, false_block);
                fn->current_block = next;
            }
        }
    }
}

void generate_logical_and(IRBuilder &irb, AstBinary *ast)
{
    auto *true_block = new BasicBlock(irb.current_fn, "then_and");
    auto *false_block = new BasicBlock(irb.current_fn, "false_and");
    generate_logical_and(irb, ast, true_block, false_block);
    add_existing_block(irb, irb.current_fn, true_block);
    add_existing_block(irb, irb.current_fn, false_block);
}

void generate_logical_or(
    IRBuilder &irb, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *fn = irb.current_fn;
    std::vector<Ast *> flattened;
    flatten_binary(ast, Operation::LogicalOr, flattened);

    for (size_t i = 0; i < flattened.size(); ++i) {
        bool is_final = i == flattened.size() - 1;
        if (flattened[i]->operation == Operation::LogicalAnd) {
            auto *false_and = new BasicBlock(irb.current_fn, "false_and");
            generate_logical_and(
                irb, static_cast<AstBinary *>(flattened[i]), true_block, false_and);
            add_existing_block(irb, fn, false_and);
            fn->current_block = false_and;
            if (is_final) {
                generate_jump(irb, false_block);
            }
        } else {
            auto *next = is_final ? false_block : add_block(irb, "next");
            generate_branch(irb, flattened[i], true_block, next);
            if (!is_final) {
                fn->current_block = next;
            }
        }
    }
}

void generate_logical_or(IRBuilder &irb, AstBinary *ast)
{
    auto *true_block = new BasicBlock(irb.current_fn, "then_or");
    auto *false_block = new BasicBlock(irb.current_fn, "false_or");
    generate_logical_or(irb, ast, true_block, false_block);
    add_existing_block(irb, irb.current_fn, true_block);
    add_existing_block(irb, irb.current_fn, false_block);
}

void generate_condition(IRBuilder &irb, Ast *expr, BasicBlock *true_block, BasicBlock *false_block)
{
    if (expr->operation == Operation::LogicalAnd) {
        generate_logical_and(irb, static_cast<AstBinary *>(expr), true_block, false_block);
    } else if (expr->operation == Operation::LogicalOr) {
        generate_logical_or(irb, static_cast<AstBinary *>(expr), true_block, false_block);
    } else {
        generate_branch(irb, expr, true_block, false_block);
    }
}

BasicBlock *add_compare_block(IRBuilder &irb, BasicBlock *to)
{
    auto *bb = add_block(irb, "cmp");
    generate_jump(irb, bb);
    bb->reachable = to->reachable;
    return bb;
}

void generate_if(IRBuilder &irb, Ast *ast)
{
    auto *if_stmt = static_cast<AstIf *>(ast);
    auto *fn = irb.current_fn;

    auto *true_block = new BasicBlock(irb.current_fn, "then");
    auto *else_block = if_stmt->else_body ? new BasicBlock(irb.current_fn, "else") : nullptr;
    auto *merge_block = new BasicBlock(irb.current_fn, "merge");
    auto *false_block = else_block ? else_block : merge_block;
    generate_condition(irb, if_stmt->expr, true_block, false_block);

    add_existing_block(irb, fn, true_block);
    fn->current_block = true_block;
    generate(irb, if_stmt->body);
    if (!true_block->terminal) {
        generate_jump(irb, merge_block);
    }

    if (else_block) {
        add_existing_block(irb, fn, else_block);
        fn->current_block = else_block;
        generate(irb, if_stmt->else_body);
        if (!else_block->terminal) {
            generate_jump(irb, merge_block);
        }
    }

    if (!true_block->terminal || !else_block || !else_block->terminal) {
        add_existing_block(irb, fn, merge_block);
        fn->current_block = merge_block;
    } else {
        // If this condition is false, we have
        // `if cond { ... return }
        // else     { ... return }`
        // so no merge block is necessary.
        delete merge_block;
    }
}

void generate_for(IRBuilder &irb, Ast *ast)
{
    auto *fn = irb.current_fn;
    auto *for_stmt = static_cast<AstFor *>(ast);

    if (for_stmt->var_decl) {
        generate_var_decl(irb, for_stmt->var_decl);
    }

    auto *cmp_block = add_compare_block(irb, fn->current_block);
    cmp_block->sealed = false;
    auto *true_block = new BasicBlock(irb.current_fn, "true");
    auto *merge_block = new BasicBlock(irb.current_fn, "merge");
    irb.loop_cmp_block = cmp_block;
    irb.loop_merge_block = merge_block;

    fn->current_block = cmp_block;
    if (for_stmt->cmp) {
        generate_branch(irb, for_stmt->cmp, true_block, merge_block);
    } else {
        generate_jump(irb, true_block);
    }

    add_existing_block(irb, fn, true_block);
    fn->current_block = true_block;
    generate(irb, for_stmt->body);
    if (for_stmt->change) {
        generate_binary(irb, for_stmt->change);
    }
    if (cmp_block) {
        generate_jump(irb, cmp_block);
    }

    add_existing_block(irb, fn, merge_block);
    fn->current_block = merge_block;
    irb.loop_cmp_block = nullptr;
    irb.loop_merge_block = nullptr;
}

void generate_while(IRBuilder &irb, Ast *ast)
{
    auto *while_stmt = static_cast<AstWhile *>(ast);
    auto *fn = irb.current_fn;

    auto *cmp_block = add_compare_block(irb, fn->current_block);
    cmp_block->sealed = false;
    irb.loop_cmp_block = cmp_block;
    auto *true_block = new BasicBlock(irb.current_fn, "then");
    auto *merge_block = new BasicBlock(irb.current_fn, "merge");
    irb.loop_merge_block = merge_block;
    fn->current_block = cmp_block;
    generate_condition(irb, while_stmt->expr, true_block, merge_block);

    add_existing_block(irb, fn, true_block);
    fn->current_block = true_block;
    generate(irb, while_stmt->body);
    if (!true_block->terminal) {
        generate_jump(irb, cmp_block);
    }

    add_existing_block(irb, fn, merge_block);
    fn->current_block = merge_block;
    irb.loop_cmp_block = nullptr;
    irb.loop_merge_block = nullptr;
}

void generate_break(IRBuilder &irb, Ast *)
{
    assert(irb.loop_merge_block && "not in loop");
    auto *fn = irb.current_fn;
    generate_jump(irb, irb.loop_merge_block);
    fn->current_block = add_block(irb, "brk");
}

void generate_continue(IRBuilder &irb, Ast *)
{
    assert(irb.loop_cmp_block && "not in loop");
    auto *fn = irb.current_fn;
    generate_jump(irb, irb.loop_cmp_block);
    fn->current_block = add_block(irb, "cont");
}

Inst *generate_identifier(IRBuilder &irb, Variable *var)
{
    auto it = alloca_map.find(var);
    assert(it != alloca_map.end());
    auto *load = make_load(irb.current_fn, var, it->second);
    irb.add(load);
    return load;
}

Inst *generate_unary(IRBuilder &irb, Ast *ast)
{
    if (ast->operation == Operation::Cast) {
        auto *inst = new CastInst(to_inst_type(ast), name_gen[irb.current_fn].get("cast"));
        inst->add_arg(generate(irb, static_cast<AstCast *>(ast)->operand));
        inst->cast = to_inst_type(ast);
        irb.add(inst);
        return inst;
    }

    if (ast->operation == Operation::Call) {
        auto *call = static_cast<AstCall *>(ast);
        auto *call_inst = new CallInst(to_inst_type(call), name_gen[irb.current_fn].get("call"));
        call_inst->function = call->fn;
        std::vector<Inst *> args;
        args.reserve(size(call->args));
        for (auto *arg : call->args) {
            // An argument may consist of another function call, so save
            // it for now and only add them to the current IR once everything
            // has been processed.
            auto *push = new Inst(Operation::PushArg, InstKind::Unary, to_inst_type(call),
                name_gen[irb.current_fn].get("arg"));
            push->add_arg(generate(irb, arg));
            args.push_back(push);
        }
        for (auto *push : args) {
            irb.add(push);
        }
        return irb.add(call_inst);
    }

    auto *unary = static_cast<AstUnary *>(ast);

    if (ast->operation == Operation::AddressOf) {
        // AddressOf only exists on the frontend, it's actually just a pointer Store.
        assert(unary->operand->type == AstType::Identifier);
        auto it = alloca_map.find(static_cast<AstIdentifier *>(unary->operand)->var);
        assert(it != alloca_map.end());
        return it->second;
    }

    if (ast->operation == Operation::Dereference) {
        auto *ptr = generate(irb, unary->operand);
        auto *load = make_load(irb.current_fn, ptr);
        return irb.add(load);
    }

    auto *inst = new Inst(
        ast->operation, InstKind::Unary, to_inst_type(ast), name_gen[irb.current_fn].get("unr"));
    inst->add_arg(generate(irb, unary->operand));
    return irb.add(inst);
}

void mangle_function_name(AstFunction *fn)
{
    auto &name = fn->name;
    name += "_" + std::to_string(size(fn->params));
}

std::string demangled_name(const std::string &s)
{
    if (s == "main") {
        return s;
    }
    auto ret = s;
    while (ret.back() != '_') {
        ret.pop_back();
    }
    ret.pop_back();
    return ret;
}

void finish_function(IRBuilder &irb, Function *last)
{
    irb.alloca_sets.top().execute_range(irb.current_fn->blocks[0]);
    irb.alloca_sets.pop();
    irb.current_fn = last;
}

void generate_function(IRBuilder &irb, AstFunction *fn)
{
    auto *last = irb.current_fn;

    make_function(irb, fn);

    for (auto *decl : fn->params) {
        auto *alloca = generate_alloca(irb, &decl->var);
        auto *store = new Inst(
            Operation::Store, InstKind::Binary, InstType::Ptr, name_gen[irb.current_fn].get("st"));
        auto *arg = new ArgInst(to_inst_type(decl->var.type),
            name_gen[irb.current_fn].get(std::format("arg.{}", decl->var.name)));
        arg->index = decl->var.param_index;
        irb.add(arg);
        store->add_arg(alloca);
        store->add_arg(arg);
        irb.add(store);
    }

    generate(irb, fn->body);
    mangle_function_name(fn);

    finish_function(irb, last);
}

Inst *generate(IRBuilder &irb, Ast *ast)
{
    if (irb.current_fn->current_block->terminal && ast->operation != Operation::FunctionDecl) {
        return nullptr;
    }

    switch (ast->type) {
        case AstType::Integer:
            [[fallthrough]];
        case AstType::Boolean:
            return generate_const(irb, ast);
        case AstType::String:
            return generate_string(irb, ast);
        case AstType::Enum:
            return generate(irb, static_cast<AstEnumMember *>(ast)->expr);
        case AstType::Identifier:
            return generate_identifier(irb, static_cast<AstIdentifier *>(ast)->var);
        case AstType::Unary:
            return generate_unary(irb, ast);
        case AstType::Binary:
            return generate_binary(irb, ast);
        case AstType::Statement:
            if (ast->operation == Operation::VariableDecl) {
                return generate_var_decl(irb, ast);
            } else if (ast->operation == Operation::Return) {
                return generate_return(irb, ast);
            } else if (ast->operation == Operation::FunctionDecl) {
                generate_function(irb, static_cast<AstFunction *>(ast));
            } else if (ast->operation == Operation::If) {
                generate_if(irb, ast);
            } else if (ast->operation == Operation::For) {
                generate_for(irb, ast);
            } else if (ast->operation == Operation::While) {
                generate_while(irb, ast);
            } else if (ast->operation == Operation::Break) {
                generate_break(irb, ast);
            } else if (ast->operation == Operation::Continue) {
                generate_continue(irb, ast);
            }
            break;
        case AstType::Block:
            for (auto *stmt : static_cast<AstBlock *>(ast)->stmts) {
                generate(irb, stmt);
            }
            break;
        default:
            dbgln("type {} op {}", to_string(ast->type), to_string(ast->operation));
            TODO();
    }
    return nullptr;
}

void insert_return(IRBuilder &irb, Function *fn, Type *type, uint64_t return_value)
{
    irb.current_fn = fn;
    fn->current_block->terminal = true;
    // TODO: assert we don't have a terminator already
    // TODO: update successor block reachability?
    auto inst_type = to_inst_type(type);
    auto *ret = new Inst(
        Operation::Return, InstKind::Unary, inst_type, name_gen[irb.current_fn].get("ret"));
    if (inst_type != InstType::Void) {
        auto *expr = new ConstInst(inst_type, name_gen[irb.current_fn].get("cst"));
        expr->constant = Integer(return_value, type->is_signed());
        ret->add_arg(expr);
        irb.add(expr);
    }
    irb.add(ret);
}

void visit_successors(Compiler &cc, Function *fn, BasicBlock *block,
    std::unordered_set<BasicBlock *> &visited, bool is_main)
{
    if (visited.contains(block)) {
        return;
    }

    visited.insert(block);
    for (auto *bb : block->successors) {
        visit_successors(cc, fn, bb, visited, is_main);
    }

    if (block->successors.empty() && !block->terminal) {
        if (is_main || fn->ast->returns_void()) {
            insert_return(cc.new_ir_builder, fn, get_unaliased_type(fn->ast->return_type), 0);
        } else {
            diag::error_at(cc, fn->location, ErrorType::Verification,
                "non-void function does not return a value on all paths");
        }
    }
}

namespace ssa {
void enter(IRBuilder &);
void leave(IRBuilder &);
} // namespace ssa

void generate(Compiler &cc, AstFunction *fn)
{
    generate_function(cc.new_ir_builder, fn);

    for (size_t i = 0; i < size(cc.new_ir_builder.fns); ++i) {
        auto *fn = cc.new_ir_builder.fns[i];
        std::unordered_set<BasicBlock *> visited;
        visit_successors(cc, fn, fn->blocks[0], visited, i == 0);
    }

    if (opts.inliner) {
        void inline_pass(IRBuilder &);
        inline_pass(cc.new_ir_builder);
    }

    if (opts.ssa) {
#ifdef SSA_DEBUG
        ssa_dbgln("********************************* pre ssa:");
        print(stdout_file(), cc.new_ir_builder, SkipUnreachable::No);
        ssa_dbgln("*********************************");
#endif
        ssa::enter(cc.new_ir_builder);
#ifdef SSA_DEBUG
        ssa_dbgln("********************************* ssa:");
        print(stdout_file(), cc.new_ir_builder, SkipUnreachable::No);
        ssa_dbgln("*********************************");
#endif
        ssa::leave(cc.new_ir_builder);
    }

    if (opts.testing) {
        name_gen.clear();
        alloca_map.clear();
    } else {
        for (auto *fn : cc.new_ir_builder.fns) {
            if (has_flag(fn->ast->attributes, FunctionAttributes::DUMP_IR)) {
                std::println("{}============= {}IR for `{}`{} ============={}", colors::Green,
                    colors::DefaultBold, demangled_name(fn->ast->name), colors::Green,
                    colors::Default);
                print(stdout_file(), fn, SkipUnreachable::Yes);
            }
        }
    }

#if defined(_DEBUG) || defined(SSA_DEBUG)
    std::println("\n{}generated ir:{}", colors::Cyan, colors::Default);
    print(stdout_file(), cc.new_ir_builder, SkipUnreachable::No);
#endif
}

void consume_stats(File &file)
{
    file.fwriteln("Insts created:   {:>6}", stats.insts_added);
    file.fwriteln("SSA transforms:  {:>6}", stats.vars_to_ssa);
    file.fwriteln("DCE sweep count: {:>6}", stats.insts_killed);
    file.fwriteln("Inlined calls:   {:>6}", stats.inlined_calls);
    file.commit();
    stats = {};
}

void free(Inst *inst)
{
    switch (inst->kind) {
        case InstKind::Const:
            delete static_cast<ConstInst *>(inst);
            break;
        case InstKind::Arg:
            delete static_cast<ArgInst *>(inst);
            break;
        case InstKind::String:
            delete static_cast<StringInst *>(inst);
            break;
        case InstKind::Var:
            delete static_cast<VarInst *>(inst);
            break;
        case InstKind::SSA:
            delete static_cast<SSAInst *>(inst);
            break;
        case InstKind::Phi:
            delete static_cast<PhiInst *>(inst);
            break;
        case InstKind::Cast:
            delete static_cast<CastInst *>(inst);
            break;
        case InstKind::Call:
            delete static_cast<CallInst *>(inst);
            break;
        case InstKind::Unary:
            if (inst->operation == Operation::Alloca) {
                delete static_cast<AllocaInst *>(inst);
                break;
            }
            [[fallthrough]];
        case InstKind::Binary:
        case InstKind::Identity:
        case InstKind::Nop:
        case InstKind::Undef:
            delete inst;
            break;
        default:
            TODO();
    }
}

void free(IRBuilder &irb)
{
    for (auto *fn : irb.fns) {
        for (auto *bb : fn->blocks) {
            for (auto *inst : bb->code) {
                free(inst);
            }
            delete bb;
        }
        delete fn;
    }
    irb.fns.clear();
}

} // namespace new_ir
