#include "compiler.hh"

Type *get_unaliased_type(Type *type)
{
    auto *tmp = type;
    while (tmp->has_flag(TypeFlags::ALIAS)) {
        tmp = tmp->real;
    }
    return tmp;
}

AstFunctionDecl *get_callee(Compiler &cc, AstCall *call)
{
    if (!call->fn) {
        // Cache it so the next lookup takes the fast path
        call->fn = find_function(call->scope, call->name);
        if (!call->fn) {
            cc.diag_ast_error(call, "function `{}` is not declared", call->name);
        }
    }
    return call->fn;
}

Type *get_expression_type(Compiler &cc, Ast *ast)
{
    if (!ast) {
        return nullptr;
    }

    switch (ast->type) {
        // TODO - change this when we support strings, chars, floats etc
        case AstType::Integer:
            return s32_type();
        case AstType::Boolean:
            return bool_type();
        case AstType::Identifier: {
            auto *var_decl = find_variable(ast->scope, static_cast<AstIdentifier *>(ast)->string);
            if (!var_decl) {
                return nullptr;
            }
            return var_decl->var.type;
        }
        // TODO - pick more fitting type instead
        // i.e. x := y + z where y is s32 and z is s64 should cast y to s64 first
        case AstType::Binary:
            return get_expression_type(cc, static_cast<AstBinary *>(ast)->left);
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                auto *call = static_cast<AstCall *>(ast);
                auto *fn = get_callee(cc, call);
                if (!fn) {
                    cc.diag_ast_error(call, "function `{}` is not declared", call->name);
                }
                return fn->return_type;
            } else if (ast->operation == Operation::Negate) {
                return get_expression_type(cc, static_cast<AstUnary *>(ast)->operand);
            }
            break;
        case AstType::Block:
            return nullptr;
        case AstType::Statement:
            return nullptr;
    }
    assert(!"get_expression_type unhandled ast type");
    return nullptr;
}

void resolve_type(Compiler &cc, Scope *scope, Type *&type, Ast *init_expr = nullptr)
{
    // If a type is unresolved, it's a) invalid, b) auto inferred, or c) declared later
    // (possibly as an alias)
    if (type->has_flag(TypeFlags::UNRESOLVED)) {
        // If no type string was provided, this is an auto inferred type.
        if (type->name.empty()) {
            // The placeholder for these types is not a heap variable, so don't delete it.
            type = get_expression_type(cc, init_expr);
            return;
        }
        auto *resolved = find_type(scope, type->name);
        if (resolved) {
            delete type;
            type = resolved;
        } else {
            cc.diag_type_error(type, "type `{}` is not declared in this scope", type->name);
        }
    } else if (type->has_flag(TypeFlags::ALIAS) && type->real->has_flag(TypeFlags::UNRESOLVED)) {
        auto *resolved = find_type(scope, type->real->name);
        if (resolved) {
            delete type->real;
            type->real = resolved;
        } else {
            cc.diag_type_error(type,
                "invalid alias `{}`: underlying type `{}` is not declared in this scope",
                type->name, type->real->name);
        }
    }

    // Walk the alias chain and see if we end up with a circular definition
    // TODO - can we do this faster/more efficiently?
    if (type->has_flag(TypeFlags::ALIAS)) {
        auto *slow = type;
        auto *fast = type->real;
        while (fast && fast->real) {
            if (slow == fast) {
                cc.diag_type_error(type->real, "circular alias: `{}`", type->real->name);
            }
            slow = slow->real;
            fast = fast->real->real;
        }
    }
}

bool has_top_level_return(AstBlock *block)
{
    for (auto *stmt : block->stmts) {
        if (stmt->operation == Operation::Return) {
            return true;
        }
        if (stmt->type == AstType::Block) {
            return has_top_level_return(static_cast<AstBlock *>(stmt));
        }
    }
    return false;
}

// Collects all operands (recursively) for binary operations matching `matching_operation`.
void flatten_binary(Ast *ast, Operation matching_operation, std::vector<Ast *> &flattened)
{
    if (ast->type == AstType::Binary && ast->operation == matching_operation) {
        flatten_binary(static_cast<AstBinary *>(ast)->left, matching_operation, flattened);
        flatten_binary(static_cast<AstBinary *>(ast)->right, matching_operation, flattened);
    } else {
        flattened.push_back(ast);
    }
}

Ast *partial_fold_associative(const std::vector<Ast *> &operands, Operation operation)
{
    std::vector<Ast *> non_constants;
    int64_t accumulator = operation == Operation::Multiply ? 1 : 0;

    for (auto *ast : operands) {
        if (ast->type == AstType::Integer) {
            if (operation == Operation::Add) {
                accumulator += static_cast<AstInteger *>(ast)->number;
            } else if (operation == Operation::Multiply) {
                accumulator *= static_cast<AstInteger *>(ast)->number;
            }
            delete static_cast<AstInteger *>(ast);
        } else {
            non_constants.push_back(ast);
        }
    }

    auto *ret = non_constants[0];
    for (size_t i = 1; i < non_constants.size(); ++i) {
        ret = new AstBinary(operation, ret, non_constants[i], {});
    }
    return new AstBinary(operation, ret, new AstInteger(accumulator, {}), {});
}

Ast *try_fold_identities(AstBinary *binary, Ast *constant_ast, Ast *variable_ast, int64_t constant)
{
    if (binary->operation == Operation::Add) {
        if (constant == 0) {
            // x+0=x and 0+x=x
            delete static_cast<AstInteger *>(constant_ast);
            delete static_cast<AstBinary *>(binary);
            return variable_ast;
        }
    } else /* multiply */ {
        if (constant == 0) {
            // x*0=0 and 0*x=0
            delete static_cast<AstIdentifier *>(variable_ast);
            delete static_cast<AstBinary *>(binary);
            return constant_ast;
        }
        if (constant == 1) {
            // x*1=x and 1*x=x
            delete static_cast<AstInteger *>(constant_ast);
            delete static_cast<AstBinary *>(binary);
            return variable_ast;
        }
    }
    return nullptr;
}

Ast *try_partial_fold_associative(
    AstBinary *binary, Ast *constant_ast, Ast *variable_ast, int64_t constant)
{
    if (auto *id = try_fold_identities(binary, constant_ast, variable_ast, constant)) {
        return id;
    }
    if (binary->left->type != AstType::Binary && binary->right->type != AstType::Binary) {
        // If this is a simple expression with no chance of further constants and
        // try_fold_identities also failed, give up here.
        return binary;
    }
    std::vector<Ast *> flattened;
    flatten_binary(binary, binary->operation, flattened);
    return partial_fold_associative(flattened, binary->operation);
}

Ast *try_constant_fold(Compiler &cc, Ast *ast);

Ast *try_fold_unary(Compiler &cc, AstUnary *unary)
{
    if (unary->operation != Operation::Negate) {
        return unary;
    }

    auto *leaf = try_constant_fold(cc, unary->operand);
    if (leaf && leaf->type == AstType::Integer) {
        auto lhs = static_cast<AstInteger *>(leaf)->number;
        auto result = -lhs;
        delete static_cast<AstInteger *>(leaf);
        delete static_cast<AstUnary *>(unary);
        return new AstInteger(result, {});
    }

    return unary;
}

Ast *try_fold_constants(Compiler &cc, AstBinary *binary, int64_t left_const, int64_t right_const)
{
    int64_t result;
    switch (binary->operation) {
        case Operation::Add:
            result = left_const + right_const;
            break;
        case Operation::Subtract:
            result = left_const - right_const;
            break;
        case Operation::Multiply:
            result = left_const * right_const;
            break;
        case Operation::Divide:
            if (right_const == 0) {
                cc.diag_ast_warning(binary, "division by 0");
                return binary;
            }
            result = left_const / right_const;
            break;
        case Operation::Modulo:
            if (right_const == 0) {
                cc.diag_ast_warning(binary, "modulo 0");
                return binary;
            }
            result = left_const % right_const;
            break;
        default:
            return binary; // Do nothing
    }
    auto loc = binary->location;
    delete static_cast<AstInteger *>(binary->left);
    delete static_cast<AstInteger *>(binary->right);
    delete static_cast<AstBinary *>(binary);
    return new AstInteger(result, loc);
}

Ast *try_fold_binary(Compiler &cc, AstBinary *binary)
{
    // Figure out which parts of the expression are constant.
    auto *left = try_constant_fold(cc, binary->left);
    auto *right = try_constant_fold(cc, binary->right);
    bool left_is_const = left->type == AstType::Integer;
    bool right_is_const = right->type == AstType::Integer;
    if (!left_is_const && !right_is_const) {
        // Give up...
        return binary;
    }

    int64_t left_const, right_const;
    if (left_is_const) {
        left_const = static_cast<AstInteger *>(left)->number;
    }
    if (right_is_const) {
        right_const = static_cast<AstInteger *>(right)->number;
    }

    if (left_is_const && right_is_const) {
        // Easy case: if both sides are constants, fold unless the operation is illegal.
        return try_fold_constants(cc, binary, left_const, right_const);
    }

    // Only one side is a constant, and the operation is associative.
    if (binary->operation == Operation::Multiply || binary->operation == Operation::Add) {
        auto *constant_ast = left_is_const ? left : right;
        auto *variable_ast = left_is_const ? right : left;
        auto constant = left_is_const ? left_const : right_const;
        return try_partial_fold_associative(binary, constant_ast, variable_ast, constant);
    }

    // TODO - non-associative folding

    if (binary->operation == Operation::Divide || binary->operation == Operation::Modulo) {
        if (right_is_const && right_const == 0) {
            const char *type = binary->operation == Operation::Divide ? "division by" : "modulo";
            cc.diag_ast_warning(binary, "{} 0", type);
        }
    }

    return binary;
}

Ast *try_constant_fold(Compiler &cc, Ast *ast)
{
    if (!ast) {
        return nullptr;
    }
    if (ast->type == AstType::Unary) {
        return try_fold_unary(cc, static_cast<AstUnary *>(ast));
    }
    if (ast->type == AstType::Binary) {
        return try_fold_binary(cc, static_cast<AstBinary *>(ast));
    }
    return ast;
}

enum class WarnDiscardedReturn { No, Yes };

void verify_expr(Compiler &cc, Ast *&, WarnDiscardedReturn);

void verify_call(Compiler &cc, AstCall *call, WarnDiscardedReturn warn_discarded)
{
    auto *fn = get_callee(cc, call);
    if (call->args.size() != fn->params.size()) {
        cc.diag_ast_error(call, "function `{}` takes {} arguments but was called with {}", fn->name,
            fn->params.size(), call->args.size());
    }
    for (auto *arg : call->args) {
        verify_expr(cc, arg, warn_discarded);
    }
    // TODO - check that arg types match
    if (warn_discarded == WarnDiscardedReturn::Yes && !fn->returns_void()) {
        cc.diag_ast_warning(
            call, "discarded return value for non-void function call `{}`", fn->name);
    }
    // TODO recursive calls should maybe not be counted
    ++fn->call_count;
}

void verify_expr(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    if (ast->type == AstType::Integer || ast->type == AstType::Boolean) {
        ; // ?
    } else if (ast->type == AstType::Unary) {
        if (ast->operation == Operation::Call) {
            verify_call(cc, static_cast<AstCall *>(ast), warn_discarded);
        } else {
            verify_expr(cc, static_cast<AstUnary *>(ast)->operand, warn_discarded);
        }
        ast = try_constant_fold(cc, ast);
    } else if (ast->type == AstType::Binary) {
        auto binary = static_cast<AstBinary *>(ast);
        if (ast->operation == Operation::Assign) {
            if (binary->left->type != AstType::Identifier) {
                // TODO - arrow in wrong location
                cc.diag_ast_error(binary->left, "assignment to non-identifier");
            }
        }
        verify_expr(cc, binary->left, warn_discarded);
        verify_expr(cc, binary->right, warn_discarded);
        ast = try_constant_fold(cc, binary);
    }
}

void resolve_var_decl(Compiler &cc, AstVariableDecl *var_decl)
{
    auto &var = var_decl->var;
    // TODO - look up by type hash instead of name?
    resolve_type(cc, var_decl->scope, var.type, var_decl->init_expr);
    if (var_decl->init_expr) {
        verify_expr(cc, var_decl->init_expr, WarnDiscardedReturn::No);
        auto *expr_type = get_expression_type(cc, var_decl->init_expr);
        // TODO - relax this for integer types (insert cast)?
        if (expr_type != var.type) {
            cc.diag_ast_error(var_decl->init_expr,
                "variable `{}` was explicitly declared as `{}`, but initializer expression "
                "resolved to type `{}`",
                var.name, var.type->name, expr_type->name);
        }
    }
    auto *type = get_unaliased_type(var.type);
    if (!type || type->has_flag(TypeFlags::UNRESOLVED)) {
        cc.diag_ast_error(var_decl, "unable to resolve type `{}`", var.type->name);
    }
    if (type->get_kind() == TypeFlags::Void) {
        std::string alias_str = var.type->has_flag(TypeFlags::ALIAS)
            ? std::format(" (through alias {})", var.type->name)
            : "";
        cc.diag_ast_error(var_decl, "variable `{}` declared void{}", var.name, alias_str);
    }
    // Update this scope's variables table.
    // Why? Because if this variable is assigned to another auto inferred variable,
    // it's going to get its type from the table via find_variable().
    var_decl->scope->variables[var.index_in_scope]->var.type = var.type;
}

void verify_ast(Compiler &, Ast *, AstFunctionDecl *);

void verify_return(Compiler &cc, AstReturn *return_stmt, AstFunctionDecl *current_function)
{
    if (current_function->returns_void()) {
        if (return_stmt->expr) {
            if (return_stmt->expr->operation == Operation::Call) {
                auto *call = static_cast<AstCall *>(return_stmt->expr);
                verify_call(cc, call, WarnDiscardedReturn::No);
                if (get_callee(cc, call)->returns_void()) {
                    // Returning f() (where f returns void) is allowed
                    return;
                }
            }
            cc.diag_ast_error(return_stmt->expr,
                "void function `{}` must not return a value (got type `{}`)",
                current_function->name, get_expression_type(cc, return_stmt->expr)->name);
        }
        return;
    }

    if (!return_stmt->expr) {
        cc.diag_ast_error(return_stmt, "cannot return void in function `{}` with return type `{}`",
            current_function->name, current_function->return_type->name);
    }
    verify_expr(cc, return_stmt->expr, WarnDiscardedReturn::No);
    auto *type = get_expression_type(cc, return_stmt->expr);
    assert(type);
    if (type != current_function->return_type) {
        // Includes aliases
        cc.diag_ast_error(return_stmt,
            "function `{}` must return a value of type `{}` (got type `{}`)",
            current_function->name, current_function->return_type->name, type->name);
    }
}

void verify_ast(Compiler &cc, Ast *ast, AstFunctionDecl *current_function)
{
    if (ast->type == AstType::Statement) {
        if (ast->operation == Operation::VariableDecl) {
            resolve_var_decl(cc, static_cast<AstVariableDecl *>(ast));
        } else if (ast->operation == Operation::FunctionDecl) {
            auto *fn = static_cast<AstFunctionDecl *>(ast);
            resolve_type(cc, fn->scope, fn->return_type);
            for (auto &param : fn->params) {
                resolve_type(cc, fn->scope, param->var.type);
            }
            verify_ast(cc, fn->body, fn);
            // TODO - main function (top level scope) also needs to be checked for some of this
            // stuff (e.g. consistent return types)
            // TODO - can we just check if at least one return stmt has the same scope as
            // `current_function`? probably not quite because it could be in an unconditional
            // block, but that could be handled...
            if (!fn->returns_void()
                && (fn->return_stmts.empty() || !has_top_level_return(fn->body))) {
                cc.diag_ast_error(
                    fn, "function `{}` is missing a top level return statement", fn->name);
            }
        } else if (ast->operation == Operation::If) {
            auto *if_stmt = static_cast<AstIf *>(ast);
            if (if_stmt->expr) {
                if (if_stmt->expr->operation == Operation::VariableDecl) {
                    resolve_var_decl(cc, static_cast<AstVariableDecl *>(if_stmt->expr));
                } else {
                    verify_expr(cc, if_stmt->expr, WarnDiscardedReturn::No);
                }
            }
        } else if (ast->operation == Operation::Return) {
            verify_return(cc, static_cast<AstReturn *>(ast), current_function);
        }
    } else if (ast->type == AstType::Block) {
        for (auto *stmt : static_cast<AstBlock *>(ast)->stmts) {
            verify_ast(cc, stmt, current_function);
        }
    }
    verify_expr(cc, ast, WarnDiscardedReturn::Yes);
}

void resolve_types(Compiler &cc, Scope *scope)
{
    for (auto &[name, ptr] : scope->types) {
        resolve_type(cc, scope, ptr);
    }
}

void verify_main(Compiler &cc, AstFunctionDecl *main)
{
    for (auto *scope : g_scopes) {
        resolve_types(cc, scope);
    }
    for (auto *ast : main->body->stmts) {
        verify_ast(cc, ast, main);
    }
}
