#include "compiler.hh"

#include <algorithm> // std::ranges::remove

IRArg generate_ir_impl(Compiler &cc, Ast *ast);

BasicBlock *add_block(IRFunction *ir_fn)
{
    // TODO - add constructor
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
    ir->basic_block_index = bb->index;
    bb->code.push_back(ir);
}

void new_ir_function(IRBuilder &irb, AstFunctionDecl *ast)
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
    auto ir = new IR;
    ir->ast = ast;
    ir->operation = ast->operation;
    ir->type = ast->type;
    if (ast->operation == Operation::Call) {
        auto call = static_cast<AstCall *>(ast);
        std::vector<IR *> args;
        for (ssize_t i = 0; i < ssize(call->args); ++i) {
            auto push = new IR;
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
        ir->left = IRArg{ .arg_type = IRArgType::Function, .function = fn };
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
    } else if (ast->operation == Operation::Cast) {
        auto *cast = static_cast<AstCast *>(ast);
        ir->left.arg_type = IRArgType::Type;
        ir->left.type = cast->cast_type;
        generate_ir(cc, ir, ir->right, cast->expr);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, bb);
    }
    return IRArg{ .arg_type = IRArgType::Vreg, .vreg = ir->target };
}

void print_bb(BasicBlock *bb)
{
    for (auto *ir : bb->code) {
        print_ir(ir);
    }
}

IRArg generate_ir_binary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto ir = new IR;
    ir->ast = ast;
    generate_ir(cc, ir, ir->left, static_cast<AstBinary *>(ast)->left);
    generate_ir(cc, ir, ir->right, static_cast<AstBinary *>(ast)->right);
    ir->operation = ast->operation;
    ir->type = ast->type;
    if (ir->operation != Operation::Assign) {
        ir->target = ++ir_fn->temp_regs;
    }
    add_ir(ir, get_current_block(ir_fn));
    return IRArg{ .arg_type = IRArgType::Vreg, .vreg = ir->target };
}

IRArg generate_ir_var_decl(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *var_decl = static_cast<AstVariableDecl *>(ast);
    if (var_decl->init_expr) {
        auto ir = new IR;
        ir->ast = ast;
        ir->left = IRArg{ .arg_type = IRArgType::Variable, .variable = &var_decl->var };
        generate_ir(cc, ir, ir->right, var_decl->init_expr);
        // Transform into an assignment; we don't care (yet?)
        ir->operation = Operation::Assign;
        ir->type = AstType::Binary;
        add_ir(ir, get_current_block(ir_fn));
    }
    return IRArg{ .arg_type = IRArgType::Vreg, .vreg = ir_fn->temp_regs };
}

void generate_ir_return(Compiler &cc, Ast *ast)
{
    auto *ret = static_cast<AstReturn *>(ast);
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto ir = new IR;
    ir->ast = ast;
    ir->type = AstType::Statement;
    ir->operation = Operation::Return;
    if (ret->expr) {
        // TODO void call needs special handling
        generate_ir(cc, ir, ir->left, ret->expr);
    }
    add_ir(ir, bb);
    ir_fn->current_block = add_block(ir_fn);
}

IR *generate_ir_comparison(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR;
    ir->ast = ast;
    ir->operation = ast->operation;
    ir->type = ast->type;
    switch (ast->type) {
        case AstType::Unary:
            generate_ir(cc, ir, ir->left, static_cast<AstNegate *>(ast)->operand);
            break;
        case AstType::Binary:
            generate_ir(cc, ir, ir->left, static_cast<AstBinary *>(ast)->left);
            generate_ir(cc, ir, ir->right, static_cast<AstBinary *>(ast)->right);
            break;
        case AstType::Integer:
            [[fallthrough]];
        case AstType::Boolean:
            TODO();
            // TODO - if 0, kill body, else kill condition instead
            break;
        case AstType::Identifier:
            TODO();
            break;
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

void mangle_function_name(AstFunctionDecl *fn)
{
    auto &name = fn->name;
    name += std::to_string(fn->params.size());
}

void generate_ir_fn_decl(Compiler &cc, Ast *ast)
{
    auto *fn_decl = static_cast<AstFunctionDecl *>(ast);
    auto *last = cc.ir_builder.current_function;
    mangle_function_name(fn_decl);
    new_ir_function(cc.ir_builder, fn_decl);
    for (auto *stmt : fn_decl->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
    cc.ir_builder.current_function = last;
}

IRBranch *new_ir_branch(Ast *ast, AstType type, Operation operation, ssize_t cond_vreg = -1)
{
    auto *ir = new IRBranch;
    ir->ast = ast;
    ir->operation = operation;
    ir->type = type;
    ir->cond_vreg = cond_vreg;
    return ir;
}

void generate_ir_cond_branch(Compiler &cc, ssize_t cond, BasicBlock *bb1, BasicBlock *bb2)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new_ir_branch(nullptr, AstType::Binary, Operation::CondBranch, cond);
    auto *bb = get_current_block(ir_fn);
    ir->left = IRArg{ .arg_type = IRArgType::BasicBlock, .basic_block = bb1 };
    ir->right = IRArg{ .arg_type = IRArgType::BasicBlock, .basic_block = bb2 };
    if (bb->reachable) {
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
    ir->left = IRArg{ .arg_type = IRArgType::BasicBlock, .basic_block = bb1 };
    if (bb->reachable) {
        bb1->reachable = true;
    }
    add_ir(ir, bb);
}

void generate_ir_if(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *if_stmt = static_cast<AstIf *>(ast);

    auto cond = generate_ir_impl(cc, if_stmt->expr);

    auto *true_block = add_block(ir_fn);
    BasicBlock *else_block = nullptr;
    if (if_stmt->else_body) {
        else_block = add_block(ir_fn);
    }

    auto *after_block = add_block(ir_fn);
    auto *false_block = else_block ? else_block : after_block;
    generate_ir_cond_branch(cc, cond.vreg, true_block, false_block);

    ir_fn->current_block = true_block;
    for (auto *ast : if_stmt->body->stmts) {
        generate_ir_impl(cc, ast);
    }
    generate_ir_branch(cc, after_block);

    if (else_block) {
        ir_fn->current_block = else_block;
        for (auto *ast : if_stmt->else_body->stmts) {
            generate_ir_impl(cc, ast);
        }
        generate_ir_branch(cc, after_block);
    }

    ir_fn->current_block = after_block;
}

void generate_ir_while(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *while_stmt = static_cast<AstWhile *>(ast);

    auto *cmp_block = add_block(ir_fn);
    auto *true_block = new_block();
    auto *after_block = new_block();
    cmp_block->reachable = get_current_block(ir_fn)->reachable;
    ir_fn->current_block = cmp_block;
    auto cond = generate_ir_impl(cc, while_stmt->expr);
    generate_ir_cond_branch(cc, cond.vreg, true_block, after_block);

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    for (auto *ast : while_stmt->body->stmts) {
        generate_ir_impl(cc, ast);
    }
    generate_ir_branch(cc, cmp_block);

    add_block(ir_fn, after_block);
    ir_fn->current_block = after_block;
}

AstLiteral *true_bool()
{
    static AstLiteral ast{ bool_type(), AstType::Boolean, true, {} };
    return &ast;
}

AstLiteral *false_bool()
{
    static AstLiteral ast{ bool_type(), AstType::Boolean, false, {} };
    return &ast;
}

IR *constant_ir(IRArg temp, AstLiteral *constant)
{
    IR *ir = new IR;
    ir->operation = Operation::Assign;
    ir->type = AstType::Binary;
    ir->left = temp;
    ir->right = IRArg{ .arg_type = IRArgType::Constant, .constant = constant };
    return ir;
}

IRArg generate_ir_cond_result(
    Compiler &cc, IRFunction *ir_fn, BasicBlock *true_block, BasicBlock *false_block)
{
    auto *last = new_block();
    IRArg temp{ .arg_type = IRArgType::Vreg, .vreg = ir_fn->temp_regs };

    add_block(ir_fn, true_block);
    ir_fn->current_block = true_block;
    add_ir(constant_ir(temp, true_bool()), ir_fn->current_block);
    generate_ir_branch(cc, last);

    add_block(ir_fn, false_block);
    ir_fn->current_block = false_block;
    add_ir(constant_ir(temp, false_bool()), ir_fn->current_block);
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
            auto cmp = generate_ir_comparison(cc, flattened[i]);
            if (i == flattened.size() - 1) {
                generate_ir_cond_branch(cc, cmp->target, true_block, false_block);
            } else {
                auto *next = add_block(ir_fn);
                generate_ir_cond_branch(cc, cmp->target, next, false_block);
                ir_fn->current_block = next;
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
            auto cmp = generate_ir_comparison(cc, flattened[i]);
            if (i == flattened.size() - 1) {
                generate_ir_cond_branch(cc, cmp->target, true_block, false_block);
            } else {
                auto *next = add_block(ir_fn);
                generate_ir_cond_branch(cc, cmp->target, true_block, next);
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

void flatten_logical(Ast *ast, std::vector<Ast *> &flattened)
{
    if (ast->operation == Operation::LogicalAnd) {
        flatten_binary(ast, Operation::LogicalAnd, flattened);
    } else if (ast->operation == Operation::LogicalOr) {
        flatten_binary(ast, Operation::LogicalOr, flattened);
    }
}

// TODO - don't set ir->target when we're in a return stmt
IRArg generate_ir_impl(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    switch (ast->type) {
        case AstType::Integer:
            [[fallthrough]];
        case AstType::Boolean:
            return IRArg{ .arg_type = IRArgType::Constant,
                .constant = static_cast<AstLiteral *>(ast) };
        case AstType::String:
            string_map.push_back(static_cast<AstString *>(ast)->string);
            return IRArg{ .arg_type = IRArgType::String,
                .string_index = string_map.size(),
                .string = static_cast<AstString *>(ast) };
        case AstType::Identifier: {
            auto *var = static_cast<AstIdentifier *>(ast)->var;
            auto src_type = var->is_parameter() ? IRArgType::Parameter : IRArgType::Variable;
            return IRArg{ .arg_type = src_type, .variable = var };
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
                case Operation::LessEquals:
                    generate_ir_comparison(cc, ast);
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
                generate_ir_fn_decl(cc, ast);
            } else if (ast->operation == Operation::Return) {
                generate_ir_return(cc, ast);
            } else if (ast->operation == Operation::If) {
                generate_ir_if(cc, ast);
            } else if (ast->operation == Operation::While) {
                generate_ir_while(cc, ast);
            }
        }
    }
    return IRArg{ .arg_type = IRArgType::Vreg, .vreg = ir_fn->temp_regs };
}

void generate_ir(Compiler &cc, AstFunctionDecl *main)
{
    new_ir_function(cc.ir_builder, main);
    main->call_count = 1;
    for (auto *stmt : main->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
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
            if (bb->code.front()->ast) {
                diag_ast_warning(cc, bb->code.front()->ast, "unreachable code");
            }
            return true;
        }
        return false;
    };

    for (size_t i = 0; i < ir_fn.basic_blocks.size();) {
        auto *bb = bbs[i];
        if (killable(bb)) {
            free_bb(bb);
            bbs.erase(bbs.begin() + i);
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

void optimize_ir(Compiler &cc)
{
    for (auto *fn : cc.ir_builder.functions) {
        optimize_ir(cc, *fn);
    }
}

void free_ir_function(IRFunction *fn)
{
    for (auto *bb : fn->basic_blocks) {
        free_bb(bb);
    }
    delete fn;
}
