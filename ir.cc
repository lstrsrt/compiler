#include "compiler.hh"

#include <algorithm> // std::ranges::remove

IRArg generate_ir_impl(Compiler &cc, Ast *ast);

BasicBlock *add_block(IRFunction *ir_fn)
{
    // TODO - add constructor
    auto *bb = ir_fn->basic_blocks.emplace_back(new BasicBlock);
    bb->index = ir_fn->basic_blocks.size() - 1;
    bb->label_name = std::format(".{}_{}", ir_fn->ast->name, bb->index);
    ir_fn->current_block = bb;
    return bb;
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
    add_block(fn);
}

void generate_ir(Compiler &cc, IR *ir, IRArg &arg, Ast *ast)
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
        ir->left = IRArg{ .type = IRArgType::Function, .function = fn };
        if (fn->return_type->get_kind() != TypeFlags::Void) {
            ir->target = ++ir_fn->temp_regs;
        }
        for (auto *push : args) {
            add_ir(push, bb);
        }
        add_ir(ir, bb);
    } else if (ast->operation == Operation::Negate) {
        generate_ir(cc, ir, ir->left, static_cast<AstUnary *>(ast)->operand);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, bb);
    }
    return IRArg{ .type = IRArgType::Vreg, .vreg = ir->target };
}

IRArg generate_ir_binary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto ir = new IR;
    ir->ast = ast;
    generate_ir(cc, ir, ir->left, static_cast<AstBinary *>(ast)->left);
    generate_ir(cc, ir, ir->right, static_cast<AstBinary *>(ast)->right);
    ir->operation = ast->operation;
    ir->type = ast->type;
    ir->target = ++ir_fn->temp_regs;
    add_ir(ir, bb);
    return IRArg{ .type = IRArgType::Vreg, .vreg = ir->target };
}

IRArg generate_ir_var_decl(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *var_decl = static_cast<AstVariableDecl *>(ast);
    if (var_decl->init_expr) {
        auto ir = new IR;
        ir->ast = ast;
        ir->left = IRArg{ .type = IRArgType::Variable, .variable = &var_decl->var };
        generate_ir(cc, ir, ir->right, var_decl->init_expr);
        // Transform into an assignment; we don't care (yet?)
        ir->operation = Operation::Assign;
        ir->type = AstType::Binary;
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, bb);
    }
    return IRArg{ .type = IRArgType::Vreg, .vreg = ir_fn->temp_regs };
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
}

IR *generate_ir_cmp(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *ir = new IR;
    ir->ast = ast;
    ir->operation = ast->operation;
    ir->type = ast->type;
    switch (ast->type) {
        case AstType::Unary:
            generate_ir(cc, ir, ir->left, static_cast<AstUnary *>(ast)->operand);
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
            cc.diag_ast_error(ast, "illegal expression in if statement");
    }
    ir->target = ++ir_fn->temp_regs;
    add_ir(ir, bb);
    return ir;
}

void generate_ir_fn_decl(Compiler &cc, Ast *ast)
{
    auto *fn_decl = static_cast<AstFunctionDecl *>(ast);
    auto *last = cc.ir_builder.current_function;
    new_ir_function(cc.ir_builder, fn_decl);
    for (auto *stmt : fn_decl->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
    cc.ir_builder.current_function = last;
}

void generate_ir_cond_branch(Compiler &cc, IR *cond, BasicBlock *bb1, BasicBlock *bb2)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *bb = get_current_block(ir_fn);
    auto *ir = new IRBranch;
    ir->ast = cond->ast;
    ir->operation = Operation::CondBranch;
    ir->type = AstType::Binary;
    ir->cond_vreg = cond->target;
    ir->left = IRArg{ .type = IRArgType::BasicBlock, .basic_block = bb1 };
    ir->right = IRArg{ .type = IRArgType::BasicBlock, .basic_block = bb2 };
    add_ir(ir, bb);
}

void generate_ir_if(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *if_stmt = static_cast<AstIf *>(ast);
    // Generate the compare instruction.
    auto *cmp = generate_ir_cmp(cc, if_stmt->expr);
    cmp->ast = ast;
    // TODO add unary excl mark operator
    auto current_block = ir_fn->current_block;
    auto true_block = add_block(ir_fn);
    auto false_block = add_block(ir_fn);
    // The if body has to be in true_block.
    ir_fn->current_block = true_block;
    // verify_ast(cc, if_stmt->body, current_function);
    for (auto *stmt : if_stmt->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
    // Add the conditional branch to the current block.
    ir_fn->current_block = current_block;
    if (cmp) {
        generate_ir_cond_branch(cc, cmp, true_block, false_block);
    }
    ir_fn->current_block = false_block;
}

// TODO - don't set ir->target when we're in a return stmt
IRArg generate_ir_impl(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    switch (ast->type) {
        case AstType::Integer:
            return IRArg{ .type = IRArgType::Constant,
                .constant = { static_cast<AstInteger *>(ast)->number } };
        case AstType::Boolean:
            return IRArg{ .type = IRArgType::Constant,
                .constant = { static_cast<AstBoolean *>(ast)->boolean } };
        case AstType::Identifier: {
            auto *var = static_cast<AstIdentifier *>(ast)->var;
            auto src_type = var->is_parameter() ? IRArgType::Parameter : IRArgType::Variable;
            return IRArg{ .type = src_type, .variable = var };
        }
        case AstType::Unary:
            return generate_ir_unary(cc, ast);
        case AstType::Binary:
            return generate_ir_binary(cc, ast);
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
            }
        }
    }
    return IRArg{ .type = IRArgType::Vreg, .vreg = ir_fn->temp_regs };
}

void free_ir(IRFunction *);

void generate_ir(Compiler &cc, AstFunctionDecl *main)
{
    new_ir_function(cc.ir_builder, main);
    main->call_count = 1;
    for (auto *stmt : main->body->stmts) {
        generate_ir_impl(cc, stmt);
    }
    for (size_t i = 0; i < cc.ir_builder.functions.size(); ++i) {
        auto *fn = cc.ir_builder.functions[i];
        if (!fn->ast->call_count) {
            cc.diag_ast_warning(fn->ast, "unused function");
            free_ir(fn);
            // Why
            auto [beg, end] = std::ranges::remove(cc.ir_builder.functions, fn);
            cc.ir_builder.functions.erase(beg, end);
        }
    }
}

void optimize_ir(IRFunction &fn) { }

void optimize_ir(IRBuilder &irb)
{
    for (auto *fn : irb.functions) {
        optimize_ir(*fn);
    }
}

void free_ir(IRFunction *fn)
{
    for (auto *bb : fn->basic_blocks) {
        for (auto *code : bb->code) {
            delete code;
        }
        delete bb;
    }
    delete fn;
}

void Compiler::free_ir()
{
    for (auto *fn : ir_builder.functions) {
        ::free_ir(fn);
    }
    ir_builder.functions.clear();
}
