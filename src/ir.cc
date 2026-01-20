#include "ir.hh"
#include "base.hh"
#include "debug.hh"
#include "diagnose.hh"
#include "file.hh"
#include "parser.hh"
#include "testing.hh"
#include "verify.hh"

#include <unordered_set>

// #define SSA_DEBUG

#ifdef SSA_DEBUG
#define ssa_dbgln(x, ...) dbgln(x, __VA_ARGS__)
#else
#define ssa_dbgln(x, ...) (void)0
#endif

IRArg generate_ir_impl(Compiler &, Ast *);

BasicBlock *add_block(IRFunction *ir_fn)
{
    auto *bb = ir_fn->basic_blocks.emplace_back(new BasicBlock);
    bb->index = ir_fn->basic_blocks.size() - 1;
    bb->label_name = std::format(".{}_{}", ir_fn->ast->name, bb->index);
    return bb;
}

void add_block(IRFunction *ir_fn, BasicBlock *bb)
{
    ir_fn->basic_blocks.push_back(bb);
    bb->index = ir_fn->basic_blocks.size() - 1;
    bb->label_name = std::format(".{}_{}", ir_fn->ast->name, bb->index);
}

BasicBlock *new_block()
{
    return new BasicBlock;
}

BasicBlock *get_current_block(IRFunction *ir_fn)
{
    return ir_fn->current_block;
}

void add_ir(IR *ir, BasicBlock *bb)
{
    bb->code.push_back(ir);
}

void new_ir_function(Compiler &cc, AstFunction *ast)
{
    if (!opts.testing) {
        if (has_flag(ast->attributes, FunctionAttributes::DUMP_AST)) {
            std::println("{}============= {}AST for `{}`{} ============={}", colors::Cyan,
                colors::DefaultBold, ast->name, colors::Cyan, colors::Default);
            print_ast(stdout_file(), ast);
        }
    }

    auto *fn = new IRFunction;
    fn->ast = ast;
    auto &irb = cc.ir_builder;
    irb.functions.push_back(fn);
    irb.current_function = fn;
    irb.current_function->current_block = add_block(fn);
    irb.current_function->current_block->reachable = true;
}

void generate_ir(Compiler &cc, IR *ir, Ast *ast)
{
    ir->operands.push_back(generate_ir_impl(cc, ast));
}

IRArg generate_ir_unary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR(ast);

    auto do_generic_unary = [&]() {
        generate_ir(cc, ir, static_cast<AstUnary *>(ast)->operand);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, get_current_block(ir_fn));
    };

    switch (ast->operation) {
        case Operation::Call: {
            auto *call = static_cast<AstCall *>(ast);
            std::vector<IR *> args;
            for (ssize_t i = 0; i < ssize(call->args); ++i) {
                auto *push = new IR;
                push->ast = call->args[i];
                push->operation = Operation::PushArg;
                push->type = AstType::Unary;
                generate_ir(cc, push, call->args[i]);
                push->target = i;
                // An argument may consist of another function call, so save
                // it for now and only add them to the current IR once everything
                // has been processed.
                args.push_back(push);
            }
            auto *fn = get_callee(cc, call);
            ir->operands.push_back(IRArg::make_function(fn));
            if (fn->return_type->get_kind() != TypeFlags::Void) {
                ir->target = ++ir_fn->temp_regs;
            }
            for (auto *push : args) {
                add_ir(push, get_current_block(ir_fn));
            }
            add_ir(ir, get_current_block(ir_fn));
            break;
        }
        case Operation::Negate:
        case Operation::Not:
        case Operation::AddressOf:
        case Operation::Dereference:
            [[fallthrough]];
        case Operation::Load:
            do_generic_unary();
            break;
        case Operation::Cast: {
            delete ir;
            ir = nullptr;
            auto *cast = static_cast<AstCast *>(ast);
            auto *cast_ir = new IRCast(ast, cast->cast_type);
            generate_ir(cc, cast_ir, cast->operand);
            cast_ir->operands.push_back(IRArg{});
            cast_ir->get_right().arg_type = IRArgType::Type;
            cast_ir->get_right().u.type = cast->operand->expr_type;
            cast_ir->target = ++ir_fn->temp_regs;
            add_ir(cast_ir, get_current_block(ir_fn));
            return IRArg::make_vreg(cast_ir->target);
        }
        default:
            TODO();
    }
    return IRArg::make_vreg(ir->target);
}

IRArg generate_ir_binary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR(ast);
    generate_ir(cc, ir, static_cast<AstBinary *>(ast)->left);
    generate_ir(cc, ir, static_cast<AstBinary *>(ast)->right);
    if (ir->operation != Operation::Assign && ir->operation != Operation::Store) {
        ir->target = ++ir_fn->temp_regs;
    }
    add_ir(ir, get_current_block(ir_fn));
    return IRArg::make_vreg(ir->target);
}

IRArg generate_ir_var_decl(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *var_decl = static_cast<AstVariableDecl *>(ast);
    if (var_decl->init_expr) {
        auto *ir = new IR;
        ir->ast = ast;
        // Transform into an assignment; we don't care (yet?)
        ir->operation = Operation::Assign;
        ir->type = AstType::Binary;
        ir->operands.push_back(IRArg::make_variable(&var_decl->var));
        generate_ir(cc, ir, var_decl->init_expr);
        add_ir(ir, get_current_block(ir_fn));
    }
    return IRArg::make_vreg(ir_fn->temp_regs);
}

void generate_ir_return(Compiler &cc, Ast *ast)
{
    auto *ret = static_cast<AstReturn *>(ast);
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    bb->terminal = true;
    auto *ir = new IR(ast);
    if (ret->expr) {
        generate_ir(cc, ir, ret->expr);
    }
    add_ir(ir, bb);
    ir_fn->current_block = add_block(ir_fn);
}

AstLiteral *true_bool()
{
    static AstLiteral ast{ true, {} };
    return &ast;
}

AstLiteral *false_bool()
{
    static AstLiteral ast{ false, {} };
    return &ast;
}

IR *generate_ir_logical(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR(ast);
    assert(ast->type == AstType::Binary);
    generate_ir(cc, ir, static_cast<AstBinary *>(ast)->left);
    generate_ir(cc, ir, static_cast<AstBinary *>(ast)->right);
    ir->target = ++ir_fn->temp_regs;
    add_ir(ir, get_current_block(ir_fn));
    return ir;
}

void mangle_function_name(AstFunction *fn)
{
    auto &name = fn->name;
    name += "_" + std::to_string(fn->params.size());
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

void generate_ir_function(Compiler &cc, Ast *ast)
{
    auto *function = static_cast<AstFunction *>(ast);
    auto *last = cc.ir_builder.current_function;
    new_ir_function(cc, function);
    for (auto *stmt : function->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
    mangle_function_name(function);
    cc.ir_builder.current_function = last;
}

void generate_ir_branch(Compiler &cc, BasicBlock *bb1)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *ir = new IR;
    ir->type = AstType::Unary;
    ir->operation = Operation::Branch;
    // TODO: set AST so we can produce unreachable code warnings
    ir->operands.push_back(IRArg::make_block(bb1));
    if (bb->reachable && !bb->terminal) {
        bb1->reachable = true;
    }
    add_ir(ir, bb);
}

IRCondBranch *new_cond_branch(
    Ast *ast, Operation operation, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *br = new IRCondBranch;
    br->ast = ast;
    br->type = AstType::Binary;
    br->operation = operation;
    br->true_block = true_block;
    br->false_block = false_block;
    return br;
}

void generate_ir_cond_branch(
    Compiler &cc, Ast *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto get_branch_type = [&, func = __func__](Operation operation) {
        assert(ast->expr_type);
        bool is_unsigned = ast->expr_type->is_unsigned();
        using enum Operation;
        switch (operation) {
            case Equals:
                return BranchEq;
            case NotEquals:
                return BranchNe;
            case Greater:
                return is_unsigned ? BranchUGt : BranchSGt;
            case GreaterEquals:
                return is_unsigned ? BranchUGe : BranchSGe;
            case Less:
                return is_unsigned ? BranchULt : BranchSLt;
            case LessEquals:
                return is_unsigned ? BranchULe : BranchSLe;
            default:
                todo(func, __FILE__, __LINE__);
        }
    };
    auto update_bb_state = [&] {
        if (ir_fn->current_block->reachable && !ir_fn->current_block->terminal) {
            true_block->reachable = true;
            false_block->reachable = true;
        }
    };
    if (ast->type == AstType::Binary) {
        auto *bin = static_cast<AstBinary *>(ast);
        auto *br
            = new_cond_branch(bin->left, get_branch_type(bin->operation), true_block, false_block);
        br->operands.push_back(generate_ir_impl(cc, bin->left));
        br->operands.push_back(generate_ir_impl(cc, bin->right));
        update_bb_state();
        add_ir(br, ir_fn->current_block);
    } else if (ast->type == AstType::Identifier) {
        auto *bin = static_cast<AstBinary *>(ast);
        auto *br
            = new_cond_branch(bin->left /* TODO */, Operation::BranchNe, true_block, false_block);
        br->operands.push_back(generate_ir_impl(cc, ast));
        br->operands.push_back(IRArg::make_constant(
            new AstLiteral(static_cast<AstIdentifier *>(ast)->var->type, 0, ast->location)));
        update_bb_state();
        add_ir(br, ir_fn->current_block);
    } else {
        auto *br = new_cond_branch(ast, Operation::BranchNe, true_block, false_block);
        br->operands.push_back(generate_ir_impl(cc, ast));
        br->operands.push_back(IRArg::make_constant(new AstLiteral(s64_type(), 0, ast->location)));
        update_bb_state();
        add_ir(br, ir_fn->current_block);
    }
}

void generate_ir_logical_or(
    Compiler &cc, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block);

void generate_ir_logical_and(
    Compiler &cc, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *ir_fn = cc.ir_builder.current_function;
    std::vector<Ast *> flattened;
    flatten_binary(ast, Operation::LogicalAnd, flattened);

    for (size_t i = 0; i < flattened.size(); ++i) {
        bool is_final = i == flattened.size() - 1;
        if (flattened[i]->operation == Operation::LogicalOr) {
            auto *true_or = new_block();
            generate_ir_logical_or(
                cc, static_cast<AstBinary *>(flattened[i]), true_or, false_block);
            add_block(ir_fn, true_or);
            ir_fn->current_block = true_or;
            if (is_final) {
                generate_ir_branch(cc, true_block);
            }
        } else {
            if (is_final) {
                generate_ir_cond_branch(cc, flattened[i], true_block, false_block);
            } else {
                auto *next = add_block(ir_fn);
                generate_ir_cond_branch(cc, flattened[i], next, false_block);
                ir_fn->current_block = next;
            }
        }
    }
}

void generate_ir_logical_and(Compiler &cc, AstBinary *ast)
{
    auto *true_block = new_block();
    auto *false_block = new_block();
    generate_ir_logical_and(cc, ast, true_block, false_block);
    add_block(cc.ir_builder.current_function, true_block);
    add_block(cc.ir_builder.current_function, false_block);
}

void generate_ir_logical_or(
    Compiler &cc, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *ir_fn = cc.ir_builder.current_function;
    std::vector<Ast *> flattened;
    flatten_binary(ast, Operation::LogicalOr, flattened);

    for (size_t i = 0; i < flattened.size(); ++i) {
        bool is_final = i == flattened.size() - 1;
        if (flattened[i]->operation == Operation::LogicalAnd) {
            auto *false_and = new_block();
            generate_ir_logical_and(
                cc, static_cast<AstBinary *>(flattened[i]), true_block, false_and);
            add_block(ir_fn, false_and);
            ir_fn->current_block = false_and;
            if (is_final) {
                generate_ir_branch(cc, false_block);
            }
        } else {
            auto *next = is_final ? false_block : add_block(ir_fn);
            generate_ir_cond_branch(cc, flattened[i], true_block, next);
            if (!is_final) {
                ir_fn->current_block = next;
            }
        }
    }
}

void generate_ir_logical_or(Compiler &cc, AstBinary *ast)
{
    auto *true_block = new_block();
    auto *false_block = new_block();
    generate_ir_logical_or(cc, ast, true_block, false_block);
    add_block(cc.ir_builder.current_function, true_block);
    add_block(cc.ir_builder.current_function, false_block);
}

void generate_ir_condition(Compiler &cc, Ast *expr, BasicBlock *true_block, BasicBlock *false_block)
{
    if (expr->operation == Operation::LogicalAnd) {
        generate_ir_logical_and(cc, static_cast<AstBinary *>(expr), true_block, false_block);
    } else if (expr->operation == Operation::LogicalOr) {
        generate_ir_logical_or(cc, static_cast<AstBinary *>(expr), true_block, false_block);
    } else {
        generate_ir_cond_branch(cc, expr, true_block, false_block);
    }
}

bool expr_is_const_integral(Ast *ast, IgnoreCasts ignore);

void generate_ir_if(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *if_stmt = static_cast<AstIf *>(ast);

    if (expr_is_const_integral(if_stmt->expr, IgnoreCasts::Yes)) {
        if (get_int_literal(if_stmt->expr)) {
            generate_ir_impl(cc, if_stmt->body);
            auto *after_block = add_block(ir_fn);
            generate_ir_branch(cc, after_block);
            ir_fn->current_block = after_block;
        } else if (if_stmt->else_body) {
            generate_ir_impl(cc, if_stmt->else_body);
        }
        return;
    }

    auto *true_block = new_block();
    auto *else_block = if_stmt->else_body ? new_block() : nullptr;
    auto *after_block = new_block();
    auto *false_block = else_block ? else_block : after_block;
    generate_ir_condition(cc, if_stmt->expr, true_block, false_block);

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    generate_ir_impl(cc, if_stmt->body);
    generate_ir_branch(cc, after_block);

    if (else_block) {
        add_block(ir_fn, else_block);
        ir_fn->current_block = else_block;
        generate_ir_impl(cc, if_stmt->else_body);
        generate_ir_branch(cc, after_block);
    }

    add_block(ir_fn, after_block);
    ir_fn->current_block = after_block;
}

BasicBlock *add_fallthrough_block(IRFunction *ir_fn, BasicBlock *to)
{
    auto *bb = add_block(ir_fn);
    auto *ir = new IR;
    ir->operation = Operation::Branch;
    ir->type = AstType::Unary;
    ir->operands.push_back(IRArg::make_block(bb));
    add_ir(ir, to);
    bb->reachable = to->reachable;
    return bb;
}

void generate_ir_break(Compiler &cc, Ast *)
{
    assert(cc.ir_builder.loop_merge_block && "not in loop");
    auto *ir_fn = cc.ir_builder.current_function;
    generate_ir_branch(cc, cc.ir_builder.loop_merge_block);
    ir_fn->current_block = add_block(ir_fn);
}

void generate_ir_continue(Compiler &cc, Ast *)
{
    assert(cc.ir_builder.loop_cmp_block && "not in loop");
    auto *ir_fn = cc.ir_builder.current_function;
    generate_ir_branch(cc, cc.ir_builder.loop_cmp_block);
    ir_fn->current_block = add_block(ir_fn);
}

namespace ssa {

std::unordered_map<BasicBlock *, std::map<uintptr_t, IRPhi *>> incomplete_phis;

IRPhi *add_phi(IRFunction &ir_fn, BasicBlock *block)
{
    auto *ir = new IRPhi;
    ir->basic_block_index = block->index;
    ir->target = ++ir_fn.temp_regs;
    block->code.push_front(ir);
    return ir;
}

bool args_identical(const IRArg &a, const IRArg &b)
{
    if (a.arg_type != b.arg_type) {
        return false;
    }
    switch (a.arg_type) {
        case IRArgType::BasicBlock:
            return a.u.basic_block == b.u.basic_block;
        case IRArgType::Vreg:
            return a.u.vreg == b.u.vreg;
        case IRArgType::Constant:
            return a.u.constant->expr_type == b.u.constant->expr_type
                && get_int_literal(a.u.constant) == get_int_literal(b.u.constant);
        case IRArgType::Function:
            return a.u.function->name == b.u.function->name;
        case IRArgType::Variable:
            [[fallthrough]];
        case IRArgType::Parameter:
            return a.u.variable->name == b.u.variable->name;
        case IRArgType::SSA:
            return a.u.ssa == b.u.ssa;
        case IRArgType::String:
            return a.u.string->string == b.u.string->string;
        case IRArgType::Empty:
            [[fallthrough]];
        case IRArgType::Undef:
            return true;
        default:
            TODO();
    }
    return false;
}

std::optional<IRArg> try_remove_trivial_phi(BasicBlock *bb, IRPhi *phi)
{
    std::optional<IRArg> same;
    for (const auto &op : phi->phi_operands) {
        if (op.value.arg_type == IRArgType::Vreg && op.value.u.vreg == phi->target) {
            continue;
        }
        // TODO: is same.has_value() correct?
        if (same.has_value() && args_identical(op.value, *same)) {
            continue;
        }
        if (same) {
            ssa_dbgln("***** not trivial");
            return {};
        }
        same = op.value;
    }
    ssa_dbgln("***** trivial");
    if (!same) {
        same = IRArg::make_undef();
    }
    // The phi was inserted at the beginning, so it should be ok to just delete the first insn?
    bb->code.erase(bb->code.begin());
    delete phi;
    // We don't do the recursion from the original algorithm here because there are no use lists to
    // look at.
    return same;
}

IRArg read_variable(IRFunction &, uintptr_t, BasicBlock *);

IRArg complete_phi(IRFunction &ir_fn, BasicBlock *bb, uintptr_t var,
    const std::vector<BasicBlock *> &preds, IRPhi *phi)
{
    for (auto *pred : preds) {
        phi->phi_operands.emplace_back(read_variable(ir_fn, var, pred), pred);
    }
    auto res = try_remove_trivial_phi(bb, phi);
    return res ? *res : IRArg::make_vreg(phi->target);
}

void write_variable(uintptr_t var, BasicBlock *block, IRArg value)
{
    block->current_def[var] = value;
}

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

IRArg read_variable_recursive(IRFunction &ir_fn, uintptr_t var, BasicBlock *block)
{
    auto reachable_preds = get_reachable_preds(block);
    IRArg val;
    if (!block->sealed) {
        auto *phi = add_phi(ir_fn, block);
        incomplete_phis[block][var] = phi;
        // Operands will be filled in later during block sealing.
        val = IRArg::make_vreg(phi->target);
        ssa_dbgln("read_variable inserting phi @ unsealed bb{}", block->index);
    } else if (reachable_preds.size() == 1) {
        // "If the block has a single predecessor, just query it recursively for a definition."
        val = read_variable(ir_fn, var, reachable_preds.back());
        ssa_dbgln("read_variable found globally @ bb{}", reachable_preds.back()->index);
    } else if (reachable_preds.size() > 1) {
        // "Otherwise, we collect the definitions from all predecessors and construct a phi
        // function, which joins them into a single new value."
        auto *phi = add_phi(ir_fn, block);
        write_variable(var, block, IRArg::make_vreg(phi->target));
        val = complete_phi(ir_fn, block, var, reachable_preds, phi);
        ssa_dbgln("read_variable inserting phi @ bb{}", block->index);
    } else {
        ssa_dbgln("reachable_preds empty for sealed blk {}", block->index);
        val = IRArg::make_undef();
    }

    write_variable(var, block, val);
    return val;
}

IRArg read_variable(IRFunction &ir_fn, uintptr_t var, BasicBlock *block)
{
    if (auto it = block->current_def.find(var); it != block->current_def.end()) {
        // Local Value Numbering
        ssa_dbgln("read_variable found locally @ bb{}", block->index);
        return it->second;
    }
    // Global Value Numbering
    return read_variable_recursive(ir_fn, var, block);
}

void seal_block(IRFunction &ir_fn, BasicBlock *block)
{
    auto reachable_preds = get_reachable_preds(block);
    for (auto &[var, phi] : incomplete_phis[block]) {
        // TODO: skip trivial check here?
        complete_phi(ir_fn, block, var, reachable_preds, phi);
    }
    block->sealed = true;
}

IR *make_assign(const IRArg &dst, const IRArg &src)
{
    auto *assign = new IR;
    assign->type = AstType::Binary;
    assign->operation = Operation::Assign;
    assign->operands.push_back(dst);
    assign->operands.push_back(src);
    return assign;
}

std::unordered_map<Variable *, size_t> counter;

size_t new_ssa_id(Variable *var)
{
    return ++counter[var];
}

void transform_params(BasicBlock *first_block, const VariableDecls &decls)
{
    for (auto *decl : decls) {
        auto *var = &decl->var;
        auto lhs_ssa = IRArg::make_ssa(var, new_ssa_id(var));
        auto param = IRArg::make_parameter(var);
        first_block->code.insert(first_block->code.begin(), make_assign(lhs_ssa, param));
        write_variable(param.u.any, first_block, lhs_ssa);
    }
}

void transform_block(IRFunction &ir_fn, BasicBlock *bb, std::vector<BasicBlock *> &unsealed_blocks)
{
    auto replaceable = [](const IRArg &arg) {
        return arg.arg_type == IRArgType::Variable || arg.arg_type == IRArgType::Parameter;
    };
    auto try_replace = [&ir_fn, bb](IRArg &arg, [[maybe_unused]] const char *s) {
        auto tmp = read_variable(ir_fn, arg.u.any, bb);
        ssa_dbgln("{} replace: {} => {}", s, get_ir_arg_value(arg), get_ir_arg_value(tmp));
        arg = tmp;
    };
    for (auto it = bb->code.begin(); it != bb->code.end();) {
        auto *insn = *it;
        if (insn->operation != Operation::Assign) {
            if (!insn->operands.empty() && replaceable(insn->get_left())) {
                try_replace(insn->get_left(), "LVar");
            }
            if (size(insn->operands) > 1 && replaceable(insn->get_right())) {
                try_replace(insn->get_right(), "RVar");
            }
        } else if (!insn->operands.empty() && replaceable(insn->get_left())) {
            if (insn->get_right().arg_type == IRArgType::Constant) {
                ssa_dbgln("write_variable");
                write_variable(insn->get_left().u.any, bb, insn->get_right());
            }

            if (size(insn->operands) > 1 && replaceable(insn->get_right())) {
                insn->get_right() = read_variable(ir_fn, insn->get_right().u.any, bb);
            }
            auto *var = insn->get_left().u.variable;
            auto lhs_ssa = IRArg::make_ssa(var, new_ssa_id(var));
            write_variable(insn->get_left().u.any, bb, lhs_ssa);
            insn->get_left() = lhs_ssa;
        }
        ++it;
    }
    if (!bb->sealed) {
        unsealed_blocks.push_back(bb);
    }
}

void enter(Compiler &cc)
{
    for (auto *ir_fn : cc.ir_builder.functions) {
        transform_params(ir_fn->basic_blocks[0], ir_fn->ast->params);
        std::vector<BasicBlock *> unsealed_blocks;
        for (auto *bb : ir_fn->basic_blocks) {
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            transform_block(*ir_fn, bb, unsealed_blocks);
        }
        for (auto *bb : unsealed_blocks) {
            seal_block(*ir_fn, bb);
        }
        incomplete_phis.clear();
    }
}

void leave_block(IRFunction *, BasicBlock *bb)
{
    for (auto it = bb->code.begin(); it != bb->code.end();) {
        auto *insn = *it;
        if (insn->operation == Operation::Phi) {
            auto *phi = static_cast<IRPhi *>(insn);
            for (auto &[value, pred] : phi->phi_operands) {
                ssa_dbgln("insert assign for phi->target {}, value {}", phi->target,
                    get_ir_arg_value(value));
                auto term = std::prev(pred->code.end());
                pred->code.insert(term, make_assign(IRArg::make_vreg(phi->target), value));
            }
            it = bb->code.erase(it);
            continue;
        }
        ++it;
    }
}

void leave(Compiler &cc)
{
    for (auto *ir_fn : cc.ir_builder.functions) {
        for (auto *bb : ir_fn->basic_blocks) {
            if (bb->code.empty() || !bb->reachable) {
                continue;
            }
            leave_block(ir_fn, bb);
        }
    }
    counter.clear();
}
} // namespace ssa

void generate_ir_for(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *for_stmt = static_cast<AstFor *>(ast);

    if (for_stmt->var_decl) {
        generate_ir_var_decl(cc, for_stmt->var_decl);
    }

    auto *cmp_block = add_fallthrough_block(ir_fn, get_current_block(ir_fn));
    cmp_block->sealed = false;
    auto *true_block = new_block();
    auto *after_block = new_block();
    cc.ir_builder.loop_merge_block = after_block;
    cc.ir_builder.loop_cmp_block = cmp_block;

    ir_fn->current_block = cmp_block;
    if (for_stmt->cmp) {
        generate_ir_cond_branch(cc, for_stmt->cmp, true_block, after_block);
    } else {
        generate_ir_branch(cc, true_block);
    }

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    generate_ir_impl(cc, for_stmt->body);
    if (for_stmt->change) {
        generate_ir_binary(cc, for_stmt->change);
    }
    if (cmp_block) {
        generate_ir_branch(cc, cmp_block);
    }

    add_block(ir_fn, after_block);
    ir_fn->current_block = after_block;
    cc.ir_builder.loop_cmp_block = nullptr;
    cc.ir_builder.loop_merge_block = nullptr;
}

void generate_ir_while(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *while_stmt = static_cast<AstWhile *>(ast);

    auto *cmp_block = add_fallthrough_block(ir_fn, get_current_block(ir_fn));
    cmp_block->sealed = false;
    cc.ir_builder.loop_cmp_block = cmp_block;
    auto *true_block = new_block();
    auto *after_block = new_block();
    cc.ir_builder.loop_merge_block = after_block;
    ir_fn->current_block = cmp_block;
    generate_ir_condition(cc, while_stmt->expr, true_block, after_block);

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    generate_ir_impl(cc, while_stmt->body);
    generate_ir_branch(cc, cmp_block);

    add_block(ir_fn, after_block);
    ir_fn->current_block = after_block;
    cc.ir_builder.loop_cmp_block = nullptr;
    cc.ir_builder.loop_merge_block = nullptr;
}

IR *generate_ir_assign_constant(IRArg temp, AstLiteral *constant)
{
    IR *ir = new IR;
    ir->operation = Operation::Assign;
    ir->type = AstType::Binary;
    ir->operands.push_back(temp);
    ir->operands.push_back(IRArg::make_constant(constant));
    return ir;
}

IRArg generate_ir_cond_result(
    Compiler &cc, IRFunction *ir_fn, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *last = new_block();
    auto temp = IRArg::make_vreg(ir_fn->temp_regs);

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    add_ir(generate_ir_assign_constant(temp, true_bool()), ir_fn->current_block);
    generate_ir_branch(cc, last);

    add_block(ir_fn, false_block);
    ir_fn->current_block = false_block;
    add_ir(generate_ir_assign_constant(temp, false_bool()), ir_fn->current_block);
    generate_ir_branch(cc, last);

    add_block(ir_fn, last);
    ir_fn->current_block = last;
    return temp;
}

// TODO: don't set ir->target when we're in a return stmt
IRArg generate_ir_impl(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    switch (ast->type) {
        case AstType::Integer:
            [[fallthrough]];
        case AstType::Boolean:
            return IRArg::make_constant(static_cast<AstLiteral *>(ast));
        case AstType::String:
            string_map.push_back(static_cast<AstString *>(ast)->string);
            return IRArg::make_string(string_map.size(), static_cast<AstString *>(ast));
        case AstType::Enum:
            return generate_ir_impl(cc, static_cast<AstEnumMember *>(ast)->expr);
        case AstType::Identifier: {
            auto *var = static_cast<AstIdentifier *>(ast)->var;
            if (var->is_parameter()) {
                return IRArg::make_parameter(var);
            }
            return IRArg::make_variable(var);
        }
        case AstType::Unary:
            return generate_ir_unary(cc, ast);
        case AstType::Binary:
            switch (ast->operation) {
                case Operation::LogicalAnd:
                    [[fallthrough]];
                case Operation::LogicalOr: {
                    auto *true_block = new_block();
                    auto *false_block = new_block();
                    if (ast->operation == Operation::LogicalOr) {
                        generate_ir_logical_or(
                            cc, static_cast<AstBinary *>(ast), true_block, false_block);
                    } else {
                        generate_ir_logical_and(
                            cc, static_cast<AstBinary *>(ast), true_block, false_block);
                    }
                    return generate_ir_cond_result(cc, ir_fn, true_block, false_block);
                }
                case Operation::Equals:
                case Operation::NotEquals:
                case Operation::Greater:
                case Operation::GreaterEquals:
                case Operation::Less:
                    [[fallthrough]];
                case Operation::LessEquals:
                    generate_ir_logical(cc, ast);
                    break;
                default:
                    return generate_ir_binary(cc, ast);
            }
            break;
        case AstType::Block:
            for (auto *stmt : static_cast<AstBlock *>(ast)->stmts) {
                generate_ir_impl(cc, stmt);
            }
            break;
        case AstType::Statement: {
            if (ast->operation == Operation::VariableDecl) {
                generate_ir_var_decl(cc, ast);
            } else if (ast->operation == Operation::FunctionDecl) {
                generate_ir_function(cc, ast);
            } else if (ast->operation == Operation::Return) {
                generate_ir_return(cc, ast);
            } else if (ast->operation == Operation::If) {
                generate_ir_if(cc, ast);
            } else if (ast->operation == Operation::For) {
                generate_ir_for(cc, ast);
            } else if (ast->operation == Operation::While) {
                generate_ir_while(cc, ast);
            } else if (ast->operation == Operation::Break) {
                generate_ir_break(cc, ast);
            } else if (ast->operation == Operation::Continue) {
                generate_ir_continue(cc, ast);
            }
        }
    }
    return IRArg::make_vreg(ir_fn->temp_regs);
}

void generate_ir(Compiler &cc, AstFunction *main)
{
    new_ir_function(cc, main);
    if (cc.test_mode.compare_type == CompareType::Ast) {
        cc.test_mode.compare_file
            = write_comparison_file(".ast", main, [](File &f, Ast *a) { print_ast(f, a); });
    }
    main->call_count = 1;
    generate_ir_impl(cc, main->body);
    // TODO: pre or post optimize_ir?
    if (cc.test_mode.compare_type == CompareType::IR) {
        cc.test_mode.compare_file = write_comparison_file(
            ".ir", cc.ir_builder.current_function, [](File &f, IRFunction *i) { print_ir(f, *i); });
    }
}

void free_bb(BasicBlock *bb)
{
    for (auto *ir : bb->code) {
        if (dynamic_cast<IRPhi *>(ir)) {
            delete static_cast<IRPhi *>(ir);
        } else if (dynamic_cast<IRCondBranch *>(ir)) {
            delete static_cast<IRCondBranch *>(ir);
        } else if (dynamic_cast<IRCast *>(ir)) {
            delete static_cast<IRCast *>(ir);
        } else {
            delete ir;
        }
    }
    delete bb;
}

AstLiteral *s32_literal(int value)
{
    return new AstLiteral{ s32_type(), static_cast<uint64_t>(value), {} };
}

void insert_return(IRFunction *ir_fn, int32_t return_value)
{
    auto *bb = get_current_block(ir_fn);
    bb->terminal = true;
    auto *ir = new IR;
    ir->type = AstType::Statement;
    ir->operation = Operation::Return;
    ir->operands.push_back(IRArg::make_constant(s32_literal(return_value)));
    add_ir(ir, bb);
}

void visit_successors(Compiler &cc, IRFunction *fn, BasicBlock *block,
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
            insert_return(fn, 0);
        } else {
            diag::error_at(cc, fn->ast->location, ErrorType::Verification,
                "non-void function does not return a value on all paths");
        }
    }
}

void build_successor_lists(Compiler &cc)
{
    for (size_t i = 0; i < cc.ir_builder.functions.size(); ++i) {
        auto *fn = cc.ir_builder.functions[i];
        // Would be faster to do this during IR gen, but then dealing with invalidated blocks
        // becomes annoying.
        for (auto *bb : fn->basic_blocks) {
            if (bb->code.empty()) {
                continue;
            }
            auto *code = bb->code.back();
            if (auto *br = dynamic_cast<IRCondBranch *>(code)) {
                bb->successors.push_back(br->true_block);
                bb->successors.push_back(br->false_block);
            } else if (code->operation == Operation::Branch) {
                bb->successors.push_back(code->get_left().u.basic_block);
            }
            for (auto *succ : bb->successors) {
                succ->predecessors.push_back(bb);
            }
        }
        std::unordered_set<BasicBlock *> visited;
        visit_successors(cc, fn, fn->basic_blocks[0], visited, i == 0);
    }
}

void optimize_ir(Compiler &cc)
{
    std::erase_if(cc.ir_builder.functions, [&cc](IRFunction *fn) {
        if (!fn->ast->call_count) {
            diag::ast_warning(cc, fn->ast, "unused function");
            free_ir_function(fn);
            return true;
        }
        return false;
    });

    // TODO: Remove this.
    // We can set the indices on construction.
    for (auto *ir_fn : cc.ir_builder.functions) {
        for (size_t i = 0; auto *bb : ir_fn->basic_blocks) {
            if (!bb->reachable) {
                if (!bb->code.empty() && bb->code.back()->ast) {
                    diag::ast_warning(cc, bb->code.back()->ast, "unreachable code");
                }
            } else {
                bb->index = i;
                for (auto *ir : bb->code) {
                    ir->basic_block_index = i;
                }
            }
            ++i;
        }
    }

    build_successor_lists(cc);
    if (opts.ssa) {
        ssa::enter(cc);
        ssa_dbgln("********* SSA form:");
#ifdef SSA_DEBUG
        for (const auto *fn : cc.ir_builder.functions) {
            print_ir(stdout_file(), *fn);
        }
#endif
        ssa_dbgln("********* End SSA form");
        ssa::leave(cc);
    }

    if (!opts.testing) {
        for (auto *ir_fn : cc.ir_builder.functions) {
            if (has_flag(ir_fn->ast->attributes, FunctionAttributes::DUMP_IR)) {
                std::println("{}============= {}IR for `{}`{} ============={}", colors::Green,
                    colors::Cyan, demangled_name(ir_fn->ast->name), colors::Green, colors::Default);
                print_ir(stdout_file(), *ir_fn);
            }
        }
    }
}

void print_bb(File &file, BasicBlock *bb)
{
    for (auto *ir : bb->code) {
        print_ir(file, ir);
    }
}

void free_ir_function(IRFunction *fn)
{
    for (auto *bb : fn->basic_blocks) {
        free_bb(bb);
    }
    delete fn;
}

void free_ir(IRBuilder &irb)
{
    for (auto *fn : irb.functions) {
        free_ir_function(fn);
    }
    irb.functions.clear();
    irb.current_function = nullptr;
    irb.loop_merge_block = nullptr;
    irb.loop_cmp_block = nullptr;
}
