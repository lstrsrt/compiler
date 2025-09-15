#include "ir.hh"
#include "base.hh"
#include "debug.hh"
#include "diagnose.hh"
#include "file.hh"
#include "parser.hh"
#include "testing.hh"
#include "verify.hh"

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

void new_ir_function(Compiler &cc, AstFunction *ast)
{
    if (has_flag(ast->attributes, FunctionAttributes::DumpAst)) {
        if (!opts.testing) {
            std::println("{}============= {}AST for `{}`{} ============={}", colors::Cyan,
                colors::DefaultBold, ast->name, colors::Cyan, colors::Default);
            print_ast(cc.stdout_file, ast);
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

void generate_ir(Compiler &cc, [[maybe_unused]] IR *ir, IRArg &arg, Ast *ast)
{
    arg = generate_ir_impl(cc, ast);
}

IRArg generate_ir_unary(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
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
            add_ir(push, get_current_block(ir_fn));
        }
        add_ir(ir, get_current_block(ir_fn));
    } else if (ast->operation == Operation::Negate) {
        generate_ir(cc, ir, ir->left, static_cast<AstNegate *>(ast)->operand);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, get_current_block(ir_fn));
    } else if (ast->operation == Operation::Cast) {
        auto *cast = static_cast<AstCast *>(ast);
        ir->left.arg_type = IRArgType::Type;
        ir->left.u.type = cast->cast_type;
        generate_ir(cc, ir, ir->right, cast->expr);
        ir->target = ++ir_fn->temp_regs;
        add_ir(ir, get_current_block(ir_fn));
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
        generate_ir(cc, ir, ir->left, ret->expr);
    }
    add_ir(ir, bb);
    ir_fn->current_block = add_block(ir_fn);
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

IR *generate_ir_logical(Compiler &cc, Ast *ast)
{
    auto *ir_fn = cc.ir_builder.current_function;
    auto *ir = new IR(ast);
    assert(ast->type == AstType::Binary);
    generate_ir(cc, ir, ir->left, static_cast<AstBinary *>(ast)->left);
    generate_ir(cc, ir, ir->right, static_cast<AstBinary *>(ast)->right);
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
        switch (operation) {
            case Operation::Equals:
                return Operation::BranchEq;
            case Operation::NotEquals:
                return Operation::BranchNe;
            case Operation::Greater:
                return Operation::BranchGt;
            case Operation::GreaterEquals:
                return Operation::BranchGe;
            case Operation::Less:
                return Operation::BranchLt;
            case Operation::LessEquals:
                return Operation::BranchLe;
            default:
                todo(func);
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
        auto *b
            = new_cond_branch(bin->left, get_branch_type(bin->operation), true_block, false_block);
        b->left = generate_ir_impl(cc, bin->left);
        b->right = generate_ir_impl(cc, bin->right);
        update_bb_state();
        add_ir(b, ir_fn->current_block);
    } else if (ast->type == AstType::Identifier) {
        auto *bin = static_cast<AstBinary *>(ast);
        auto *br
            = new_cond_branch(bin->left /* TODO */, Operation::BranchNe, true_block, false_block);
        br->left = generate_ir_impl(cc, ast);
        br->right = IRArg::make_constant(
            new AstLiteral(static_cast<AstIdentifier *>(ast)->var->type, 0, ast->location));
        update_bb_state();
        add_ir(br, ir_fn->current_block);
    } else {
        auto arg = generate_ir_impl(cc, ast);
        auto *br = new_cond_branch(ast, Operation::BranchNe, true_block, false_block);
        br->left = arg;
        br->right = IRArg::make_constant(new AstLiteral(s64_type(), 0, ast->location));
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

    auto *true_block = new_block();
    auto *else_block = if_stmt->else_body ? new_block() : nullptr;
    auto *after_block = new_block();
    auto *false_block = else_block ? else_block : after_block;
    generate_ir_condition(cc, if_stmt->expr, true_block, false_block);

    add_block(ir_fn, true_block);
    if (else_block) {
        add_block(ir_fn, else_block);
    }
    add_block(ir_fn, after_block);

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
    generate_ir_condition(cc, while_stmt->expr, true_block, after_block);

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
            } else if (ast->operation == Operation::While) {
                generate_ir_while(cc, ast);
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
    for (auto *code : bb->code) {
        delete code;
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
    ir->left = IRArg::make_constant(s32_literal(return_value));
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
            if (auto *br = dynamic_cast<IRCondBranch *>(code)) {
                bb->successors.push_back(br->true_block);
                bb->successors.push_back(br->false_block);
            } else if (code->operation == Operation::Branch
                || code->operation == Operation::Fallthrough) {
                bb->successors.push_back(code->left.u.basic_block);
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
            diag_ast_warning(cc, fn->ast, "unused function");
            free_ir_function(fn);
            return true;
        }
        return false;
    });

    for (auto *ir_fn : cc.ir_builder.functions) {
        for (size_t i = 0; auto *bb : ir_fn->basic_blocks) {
            if (!bb->reachable) {
                if (!bb->code.empty() && bb->code.back()->ast) {
                    diag_ast_warning(cc, bb->code.back()->ast, "unreachable code");
                }
            } else {
                bb->index = i;
                for (auto *ir : bb->code) {
                    ir->basic_block_index = i;
                }
                ++i;
            }
        }
    }

    build_successor_lists(cc);

    for (auto *ir_fn : cc.ir_builder.functions) {
        if (has_flag(ir_fn->ast->attributes, FunctionAttributes::DumpIR)) {
            if (!opts.testing) {
                std::println("{}============= {}IR for `{}`{} ============={}", colors::Cyan,
                    colors::DefaultBold, ir_fn->ast->name, colors::Cyan, colors::Default);
                print_ir(cc.stdout_file, *cc.ir_builder.current_function);
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
