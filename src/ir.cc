#include "ir.hh"
#include "debug.hh"
#include "diagnose.hh"
#include "verify.hh"

#include <algorithm> // std::ranges::remove
#include <unordered_set>

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

void new_ir_function(IRBuilder &irb, AstFunction *ast)
{
    auto *fn = new IRFunction;
    fn->ast = ast;
    irb.functions.push_back(fn);
    irb.current_function = fn;
    irb.current_function->current_block = add_block(fn);
    irb.current_function->current_block->reachable = true;
}

void generate_ir(Compiler &cc, [[maybe_unused]] IR *ir, IRArg &arg, Ast *ast)
{
    arg = generate_ir_impl(cc, ast);
    // FIXME wtf
    /*
    if (arg.type == IRArgType::Vreg && arg.vreg > 0) {
        cc.ir_builder.current_function->basic_blocks[arg.vreg - 1]->target_used_by.push_back(ir);
    }*/
}

IRArg generate_ir_unary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *ir = new IR(ast);
    if (ast->operation == Operation::Call) {
        auto *call = static_cast<AstCall *>(ast);
        std::vector<IR *> args;
        for (ssize_t i = 0; i < ssize(call->args); ++i) {
            auto *push = new IR;
            push->ast = call->args[i];
            push->operation = Operation::PushArg;
            push->type = AstType::Unary;
            generate_ir(cc, ir, push->left, call->args[i]);
            push->target = i;
            // An argument may consist of another function call, so save
            // it for now and only add them to the current IR once everything
            // has been processed.
            args.push_back(push);
        }
        auto *fn = get_callee(cc, call);
        ir->left = IRArg::make_function(fn);
        if (fn->return_type->get_kind() != TypeFlags::Void) {
            ir->target = ++ir_fn->temp_regs;
        }
        for (auto *push : args) {
            add_ir(push, bb);
        }
        add_ir(ir, bb);
    } else if (ast->operation == Operation::Negate) {
        generate_ir(cc, ir, ir->left, static_cast<AstNegate *>(ast)->operand);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, bb);
    } else if (ast->operation == Operation::LogicalNot) {
        generate_ir(cc, ir, ir->left, static_cast<AstLogicalNot *>(ast)->operand);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, bb);
    } else if (ast->operation == Operation::Cast) {
        auto *cast = static_cast<AstCast *>(ast);
        ir->left.arg_type = IRArgType::Type;
        ir->left.u.type = cast->cast_type;
        generate_ir(cc, ir, ir->right, cast->expr);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, bb);
    }
    return IRArg::make_vreg(ir->target);
}

IRArg generate_ir_binary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR(ast);
    generate_ir(cc, ir, ir->left, static_cast<AstBinary *>(ast)->left);
    generate_ir(cc, ir, ir->right, static_cast<AstBinary *>(ast)->right);
    if (ir->operation != Operation::Assign) {
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
        ir->left = IRArg::make_variable(&var_decl->var);
        generate_ir(cc, ir, ir->right, var_decl->init_expr);
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
        // TODO: void call needs special handling
        generate_ir(cc, ir, ir->left, ret->expr);
    }
    add_ir(ir, bb);
}

enum class ComparisonKind {
    ConstantFalse,
    ConstantTrue,
    Runtime,
};

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

IR *generate_ir_logical(Compiler &cc, Ast *ast, ComparisonKind *kind)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR(ast);
    *kind = ComparisonKind::Runtime;
    switch (ast->type) {
        case AstType::Unary:
            generate_ir(cc, ir, ir->left, ast);
            ir->target = ir_fn->temp_regs;
            return ir;
        case AstType::Binary:
            generate_ir(cc, ir, ir->left, static_cast<AstBinary *>(ast)->left);
            generate_ir(cc, ir, ir->right, static_cast<AstBinary *>(ast)->right);
            break;
        case AstType::Integer: {
            auto *constant = static_cast<AstLiteral *>(ast);
            if (constant->u.u64) {
                *kind = ComparisonKind::ConstantTrue;
            } else {
                *kind = ComparisonKind::ConstantFalse;
            }
            delete ir;
            return nullptr;
        }
        case AstType::Boolean: {
            auto *constant = static_cast<AstLiteral *>(ast);
            if (constant->u.boolean) {
                *kind = ComparisonKind::ConstantTrue;
            } else {
                *kind = ComparisonKind::ConstantFalse;
            }
            delete ir;
            return nullptr;
        }
        case AstType::Identifier: {
            delete ir;
            // TODO: xx_literal(1) for integers
            auto *eq = new AstBinary(Operation::Equals, ast, true_bool(), {});
            return generate_ir_logical(cc, eq, kind);
        }
        case AstType::Statement:
            /*if (ast->operation != Operation::VariableDecl) {
                cc.diag_ast_error(ast, "illegal expression in if statement");
            }*/
            TODO();
            break;
        default:
            TODO();
            break;
    }

    ir->target = ++ir_fn->temp_regs;
    add_ir(ir, get_current_block(ir_fn));
    return ir;
}

void mangle_function_name(AstFunction *fn)
{
    auto &name = fn->name;
    name += std::to_string(fn->params.size());
}

void generate_ir_function(Compiler &cc, Ast *ast)
{
    auto *function = static_cast<AstFunction *>(ast);
    auto *last = cc.ir_builder.current_function;
    mangle_function_name(function);
    new_ir_function(cc.ir_builder, function);
    for (auto *stmt : function->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
    cc.ir_builder.current_function = last;
}

IRBranch *new_ir_branch(Ast *ast, AstType type, Operation operation)
{
    auto *ir = new IRBranch;
    ir->ast = ast;
    ir->operation = operation;
    ir->type = type;
    return ir;
}

IRBranch *new_ir_branch(Ast *ast, AstType type, Operation operation, IRArg cond)
{
    auto *ir = new_ir_branch(ast, type, operation);
    ir->cond = cond;
    return ir;
}

void generate_ir_cond_branch(Compiler &cc, IRArg cond, BasicBlock *bb1, BasicBlock *bb2)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *ir = new_ir_branch(nullptr, AstType::Binary, Operation::CondBranch, cond);
    ir->left = IRArg::make_block(bb1);
    ir->right = IRArg::make_block(bb2);
    if (bb->reachable && !bb->terminal) {
        bb1->reachable = true;
        bb2->reachable = true;
    }
    add_ir(ir, bb);
}

void generate_ir_branch(Compiler &cc, BasicBlock *bb1)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *ir = new_ir_branch(nullptr, AstType::Unary, Operation::Branch);
    ir->left = IRArg::make_block(bb1);
    if (bb->reachable && !bb->terminal) {
        bb1->reachable = true;
    }
    add_ir(ir, bb);
}

AstLiteral *null_s32()
{
    static AstLiteral ast{ s32_type(), 0, {} };
    return &ast;
}

ComparisonKind get_if_comparison_kind(Compiler &, AstIf *if_stmt)
{
    auto *expr = if_stmt->expr;
    while (expr->operation == Operation::Cast) {
        expr = static_cast<AstCast *>(expr)->expr;
    }
    if (expr->type == AstType::Integer || expr->type == AstType::Boolean) {
        return get_int_literal(expr) ? ComparisonKind::ConstantTrue : ComparisonKind::ConstantFalse;
    }
    return ComparisonKind::Runtime;
}

void generate_ir_if(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *if_stmt = static_cast<AstIf *>(ast);

    auto cmp_kind = get_if_comparison_kind(cc, if_stmt);

    if (cmp_kind == ComparisonKind::ConstantTrue) {
        generate_ir_impl(cc, if_stmt->body);
        auto *after_block = add_block(ir_fn);
        generate_ir_branch(cc, after_block);
        ir_fn->current_block = after_block;
        return;
    }

    if (cmp_kind == ComparisonKind::ConstantFalse) {
        if (if_stmt->else_body) {
            generate_ir_impl(cc, if_stmt->else_body);
        }
        auto *after_block = add_block(ir_fn);
        generate_ir_branch(cc, after_block);
        ir_fn->current_block = after_block;
        return;
    }

    auto cond = generate_ir_impl(cc, if_stmt->expr);

    auto *true_block = add_block(ir_fn);
    auto *else_block = if_stmt->else_body ? add_block(ir_fn) : nullptr;

    auto *after_block = add_block(ir_fn);
    auto *false_block = else_block ? else_block : after_block;
    generate_ir_cond_branch(cc, cond, true_block, false_block);

    ir_fn->current_block = true_block;
    generate_ir_impl(cc, if_stmt->body);
    generate_ir_branch(cc, after_block);

    if (else_block) {
        ir_fn->current_block = else_block;
        generate_ir_impl(cc, if_stmt->else_body);
        generate_ir_branch(cc, after_block);
    }

    ir_fn->current_block = after_block;
}

BasicBlock *add_fallthrough_block(IRFunction *ir_fn, BasicBlock *to)
{
    auto *bb = add_block(ir_fn);
    auto *ir = new IR;
    ir->operation = Operation::Fallthrough;
    ir->type = AstType::Unary;
    ir->left = IRArg::make_block(bb);
    add_ir(ir, to);
    bb->reachable = to->reachable;
    return bb;
}

void generate_ir_while(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *while_stmt = static_cast<AstWhile *>(ast);

    auto *cmp_block = add_fallthrough_block(ir_fn, get_current_block(ir_fn));
    auto *true_block = new_block();
    auto *after_block = new_block();
    ir_fn->current_block = cmp_block;
    auto cond = generate_ir_impl(cc, while_stmt->expr);
    generate_ir_cond_branch(cc, cond, true_block, after_block);

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    for (auto *ast : while_stmt->body->stmts) {
        generate_ir_impl(cc, ast);
    }
    generate_ir_branch(cc, cmp_block);

    add_block(ir_fn, after_block);
    ir_fn->current_block = after_block;
}

IR *generate_ir_assign_constant(IRArg temp, AstLiteral *constant)
{
    IR *ir = new IR;
    ir->operation = Operation::Assign;
    ir->type = AstType::Binary;
    ir->left = temp;
    ir->right = IRArg::make_constant(constant);
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

void generate_ir_logical_or(Compiler &cc, AstBinary *ast, BasicBlock *, BasicBlock *);

void generate_ir_logical_and(
    Compiler &cc, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *ir_fn = cc.ir_builder.current_function;
    std::vector<Ast *> flattened;
    flatten_binary(ast, Operation::LogicalAnd, flattened);

    for (size_t i = 0; i < flattened.size(); ++i) {
        if (flattened[i]->operation == Operation::LogicalOr) {
            auto *true_or = new_block();
            generate_ir_logical_or(
                cc, static_cast<AstBinary *>(flattened[i]), true_or, false_block);
            add_block(ir_fn, true_or);
            ir_fn->current_block = true_or;
            if (i == flattened.size() - 1) {
                generate_ir_branch(cc, true_block);
            }
        } else {
            ComparisonKind kind;
            auto *cmp = generate_ir_logical(cc, flattened[i], &kind);
            if (i == flattened.size() - 1) {
                if (cmp) {
                    generate_ir_cond_branch(
                        cc, IRArg::make_vreg(cmp->target), true_block, false_block);
                } else if (kind == ComparisonKind::ConstantFalse) {
                    generate_ir_branch(cc, false_block);
                } else {
                    generate_ir_branch(cc, true_block);
                }
            } else {
                if (cmp) {
                    auto *next = add_block(ir_fn);
                    generate_ir_cond_branch(cc, IRArg::make_vreg(cmp->target), next, false_block);
                    ir_fn->current_block = next;
                } else if (kind == ComparisonKind::ConstantFalse) {
                    generate_ir_branch(cc, false_block);
                } else {
                    auto *next = add_block(ir_fn);
                    generate_ir_branch(cc, next);
                    ir_fn->current_block = next;
                }
            }
        }
    }
}

IRArg generate_ir_logical_and(Compiler &cc, AstBinary *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *true_block = new_block();
    auto *false_block = new_block();
    generate_ir_logical_and(cc, ast, true_block, false_block);
    return generate_ir_cond_result(cc, ir_fn, true_block, false_block);
}

void generate_ir_logical_or(
    Compiler &cc, AstBinary *ast, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *ir_fn = cc.ir_builder.current_function;
    std::vector<Ast *> flattened;
    flatten_binary(ast, Operation::LogicalOr, flattened);

    for (size_t i = 0; i < flattened.size(); ++i) {
        if (flattened[i]->operation == Operation::LogicalAnd) {
            auto *false_and = new_block();
            generate_ir_logical_and(
                cc, static_cast<AstBinary *>(flattened[i]), true_block, false_and);
            add_block(ir_fn, false_and);
            ir_fn->current_block = false_and;
            if (i == flattened.size() - 1) {
                generate_ir_branch(cc, false_block);
            }
        } else {
            ComparisonKind kind;
            auto *cmp = generate_ir_logical(cc, flattened[i], &kind);
            auto is_final = i == flattened.size() - 1;
            auto *next = is_final ? false_block : add_block(ir_fn);
            if (cmp) {
                generate_ir_cond_branch(cc, IRArg::make_vreg(cmp->target), true_block, next);
            } else if (kind == ComparisonKind::ConstantFalse) {
                generate_ir_branch(cc, false_block);
            } else {
                generate_ir_branch(cc, true_block);
            }
            if (!is_final) {
                ir_fn->current_block = next;
            }
        }
    }
}

IRArg generate_ir_logical_or(Compiler &cc, AstBinary *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *true_block = new_block();
    auto *false_block = new_block();
    generate_ir_logical_or(cc, ast, true_block, false_block);
    return generate_ir_cond_result(cc, ir_fn, true_block, false_block);
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
                    return generate_ir_logical_and(cc, static_cast<AstBinary *>(ast));
                case Operation::LogicalOr:
                    return generate_ir_logical_or(cc, static_cast<AstBinary *>(ast));
                case Operation::Equals:
                case Operation::NotEquals:
                case Operation::Greater:
                case Operation::GreaterEquals:
                case Operation::Less:
                    [[fallthrough]];
                case Operation::LessEquals: {
                    ComparisonKind kind;
                    generate_ir_logical(cc, ast, &kind);
                    break;
                }
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
            } else if (ast->operation == Operation::While) {
                generate_ir_while(cc, ast);
            }
        }
    }
    return IRArg::make_vreg(ir_fn->temp_regs);
}

void generate_ir(Compiler &cc, AstFunction *main)
{
    new_ir_function(cc.ir_builder, main);
    main->call_count = 1;
    generate_ir_impl(cc, main->body);
}

void free_bb(BasicBlock *bb)
{
    for (auto *code : bb->code) {
        delete code;
    }
    delete bb;
}

void optimize_ir(Compiler &cc, IRFunction &ir_fn)
{
    auto &bbs = ir_fn.basic_blocks;

    const auto killable = [&](BasicBlock *bb) {
        if (bb->code.empty()) {
            return true;
        }
        if (!bb->reachable) {
            if (bb->code.back()->ast) {
                diag_ast_warning(cc, bb->code.back()->ast, "unreachable code");
            }
            return true;
        }
        return false;
    };

    for (size_t i = 0; i < ir_fn.basic_blocks.size();) {
        auto *bb = bbs[i];
        if (killable(bb)) {
            free_bb(bb);
            auto [beg, end] = std::ranges::remove(bbs, bb);
            bbs.erase(beg, end);
        } else {
            ++i;
        }
    }

    for (size_t i = 0; auto *bb : ir_fn.basic_blocks) {
        bb->index = i;
        for (auto *ir : bb->code) {
            ir->basic_block_index = i;
        }
        ++i;
    }
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
    ir->left = IRArg::make_constant(s32_literal(return_value));
    add_ir(ir, bb);
}

void visit_successors(Compiler &cc, IRFunction *fn, BasicBlock *block,
    std::unordered_set<BasicBlock *> &visited, bool is_main)
{
    if (visited.find(block) != visited.end()) {
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
            // TODO: print demangled name
            diag_error_at(cc, fn->ast->location, ErrorType::Verification,
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
            switch (code->operation) {
                case Operation::CondBranch:
                    bb->successors.push_back(code->left.u.basic_block);
                    bb->successors.push_back(code->right.u.basic_block);
                    break;
                case Operation::Branch:
                    [[fallthrough]];
                case Operation::Fallthrough:
                    bb->successors.push_back(code->left.u.basic_block);
                    break;
                default:
                    break;
            }
        }
        std::unordered_set<BasicBlock *> visited;
        visit_successors(cc, fn, fn->basic_blocks[0], visited, i == 0);
    }
}

void optimize_ir(Compiler &cc)
{
    for (auto *ir_fn : cc.ir_builder.functions) {
        for (size_t i = 0; auto *bb : ir_fn->basic_blocks) {
            if (!bb->reachable && !bb->code.empty()) {
                if (bb->code.back()->ast) {
                    diag_ast_warning(cc, bb->code.back()->ast, "unreachable code");
                }
            }
            bb->index = i;
            for (auto *ir : bb->code) {
                ir->basic_block_index = i;
            }
            ++i;
        }
    }

    // TODO: use this to insert ret for void functions with no explicit return
    // also need to always ret from main
    build_successor_lists(cc);

    for (size_t i = 0; i < cc.ir_builder.functions.size();) {
        auto *fn = cc.ir_builder.functions[i];
        if (!fn->ast->call_count) {
            diag_ast_warning(cc, fn->ast, "unused function");
            free_ir_function(fn);
            // Why
            auto [beg, end] = std::ranges::remove(cc.ir_builder.functions, fn);
            cc.ir_builder.functions.erase(beg, end);
        } else {
            ++i;
        }
    }
}

void print_bb(BasicBlock *bb)
{
    for (auto *ir : bb->code) {
        print_ir(ir);
    }
}

void free_ir_function(IRFunction *fn)
{
    for (auto *bb : fn->basic_blocks) {
        free_bb(bb);
    }
    delete fn;
}
