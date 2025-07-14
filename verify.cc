#include "compiler.hh"

#define verification_error(ast, msg, ...) \
    diag_error_at(cc, ast->location, ErrorType::Verification, msg __VA_OPT__(, __VA_ARGS__))

#define verification_type_error(location, msg, ...) \
    diag_error_at(cc, location, ErrorType::TypeCheck, msg __VA_OPT__(, __VA_ARGS__))

Type *void_type()
{
    static Type s_type{ .name = "void", .flags = TypeFlags::Void | TypeFlags::BUILTIN, .size = 0 };
    return &s_type;
}

Type *u32_type()
{
    static Type s_type{ .name = "u32",
        .flags = TypeFlags::Integer | TypeFlags::UNSIGNED | TypeFlags::BUILTIN,
        .size = 4 };
    return &s_type;
}

Type *u64_type()
{
    static Type s_type{ .name = "u64",
        .flags = TypeFlags::Integer | TypeFlags::UNSIGNED | TypeFlags::BUILTIN,
        .size = 8 };
    return &s_type;
}

Type *s32_type()
{
    static Type s_type{
        .name = "s32", .flags = TypeFlags::Integer | TypeFlags::BUILTIN, .size = 4
    };
    return &s_type;
}

Type *s64_type()
{
    static Type s_type{
        .name = "s64", .flags = TypeFlags::Integer | TypeFlags::BUILTIN, .size = 8
    };
    return &s_type;
}

Type *bool_type()
{
    static Type s_type{
        .name = "bool", .flags = TypeFlags::Boolean | TypeFlags::BUILTIN, .size = 1
    };
    return &s_type;
}

Type *string_type()
{
    // TODO - this is null terminated for now.
    // in the future, use an explicit length member instead.
    static Type s_type{
        .name = "string", .flags = TypeFlags::String | TypeFlags::BUILTIN, .size = 8
    };
    return &s_type;
}

Type *unresolved_type()
{
    static Type s_type{ .flags = TypeFlags::UNRESOLVED };
    return &s_type;
}

Type *get_unaliased_type(Type *type)
{
    auto *tmp = type;
    while (tmp->has_flag(TypeFlags::ALIAS)) {
        tmp = tmp->real;
    }
    return tmp;
}

AstFunction *get_callee(Compiler &cc, AstCall *call)
{
    if (!call->fn) {
        // Cache it so the next lookup takes the fast path
        call->fn = find_function(call->scope, call->name);
        if (!call->fn) {
            verification_error(call, "function `{}` is not declared", call->name);
        }
    }
    return call->fn;
}

void flatten_binary(Ast *ast, std::vector<Ast *> &flattened)
{
    if (ast->type == AstType::Binary) {
        flatten_binary(static_cast<AstBinary *>(ast)->left, flattened);
        flatten_binary(static_cast<AstBinary *>(ast)->right, flattened);
    } else {
        flattened.push_back(ast);
    }
}

void insert_cast(Ast *&expr, Type *to)
{
    expr = new AstCast(expr, to, expr->location);
}

enum class TypeError {
    None,
    NotAnInteger,
    SignednessMismatch,
    SizeMismatch,
    Unspecified,
};

void type_error(Compiler &cc, Ast *ast, Type *lhs_type, Type *rhs_type, TypeError err)
{
    switch (err) {
        case TypeError::None:
            return;
        case TypeError::SizeMismatch: {
            verification_type_error(ast->location,
                "incompatible sizes for types `{}` ({} bytes) and `{}` ({} bytes)", rhs_type->name,
                rhs_type->size, lhs_type->name, lhs_type->size);
            break;
        }
        case TypeError::SignednessMismatch: {
            const char *lhs_str = lhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            const char *rhs_str = rhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            verification_type_error(ast->location,
                "incompatible {} expression applied to {} variable of type `{}`", rhs_str, lhs_str,
                lhs_type->name);
            break;
        }
        default:
            verification_type_error(
                ast->location, "incompatible types `{}` and `{}`", rhs_type->name, lhs_type->name);
            break;
    }
}

enum class ExprConstness {
    SawConstant = 1 << 0,
    SawNonConstant = 1 << 1
};

constexpr ExprConstness &operator|=(ExprConstness &lhs, const ExprConstness rhs)
{
    return lhs = static_cast<ExprConstness>(to_underlying(lhs) | to_underlying(rhs));
}

constexpr ExprConstness operator|(const ExprConstness lhs, const ExprConstness rhs)
{
    return static_cast<ExprConstness>(to_underlying(lhs) | to_underlying(rhs));
}

bool expr_has_no_constants(ExprConstness e)
{
    return e == ExprConstness::SawNonConstant;
}

bool expr_is_fully_constant(ExprConstness e)
{
    return e == ExprConstness::SawConstant;
}

bool expr_is_partly_constant(ExprConstness e)
{
    return e == (ExprConstness::SawConstant | ExprConstness::SawNonConstant);
}

bool expr_is_constexpr_int(Type *t, ExprConstness e)
{
    return expr_is_fully_constant(e) && t->get_kind() == TypeFlags::Integer;
}

Type *get_expression_type(Compiler &, Ast *&, ExprConstness *, TypeOverridable);

Type *get_common_integer_type(Type *t1, Type *t2)
{
    assert(t1);
    if (!t2) {
        return t1;
    }
    if (t1->size != t2->size) {
        return (t1->size > t2->size) ? t1 : t2;
    }
    if (t1->has_flag(TypeFlags::UNSIGNED)) {
        return t1;
    }
    return t2;
}

void traverse_postorder(Ast *&ast, auto &&callback)
{
    if (!ast) {
        return;
    }

    if (ast->operation == Operation::Cast) {
        traverse_postorder(static_cast<AstCast *>(ast)->expr, callback);
    }

    if (ast->type == AstType::Binary) {
        traverse_postorder(static_cast<AstBinary *>(ast)->left, callback);
        traverse_postorder(static_cast<AstBinary *>(ast)->right, callback);
    }

    callback(ast);
}

Type *get_binary_expression_type(
    Compiler &cc, Ast *&ast, ExprConstness *constness, TypeOverridable overridable)
{
    auto *binary = static_cast<AstBinary *>(ast);

    switch (binary->operation) {
        case Operation::Assign:
            return void_type();
        case Operation::Equals:
        case Operation::NotEquals:
        case Operation::Greater:
        case Operation::GreaterEquals:
        case Operation::Less:
        case Operation::LessEquals:
        case Operation::LogicalAnd:
            [[fallthrough]];
        case Operation::LogicalOr:
            return bool_type();
        default:
            break;
    }

    bool seen_non_const = false;
    Type *current = nullptr;
    Type *ret = nullptr;
    std::vector<Ast *> operands;
    flatten_binary(binary, operands);

    for (auto *ast : operands) {
        ExprConstness expr_constness{};
        current = get_expression_type(cc, ast, &expr_constness, overridable);
        if (!ret) {
            // First time, just set it to whatever we got.
            ret = current;
        } else if (!seen_non_const && expr_has_no_constants(expr_constness)) {
            // If we got a non-constant, override the type unless it has already been overwritten.
            seen_non_const = true;
            ret = current;
        } else if (expr_is_constexpr_int(current, expr_constness)) {
            ret = get_common_integer_type(current, ret);
        } else if (!types_match(ret, current)) {
            verification_type_error(ast->location,
                "invalid types `{}` and `{}` in binary operation", ret->name, current->name);
        }
        if (constness) {
            *constness |= expr_constness;
        }
    }

    if (!expr_has_no_constants(*constness) && !(ast->flags & AstFlags::FOLDED)) {
        // Fold here to detect if a constant expr overflows the detected type, and warn/promote if
        // needed. This allows us to resolve `x := 0xffffffff+1` to an s64 instead of a u32 with an
        // overflowing add.
        traverse_postorder(
            ast, [&](Ast *&ast) { ast = try_constant_fold(cc, ast, ret, overridable); });
        ast->flags |= AstFlags::FOLDED;
    }

    return ret;
}

Type *get_expression_type(
    Compiler &cc, Ast *&ast, ExprConstness *constness, TypeOverridable overridable)
{
    if (!ast) {
        return nullptr;
    }

    switch (ast->type) {
        case AstType::Integer: {
            if (constness) {
                *constness |= ExprConstness::SawConstant;
            }
            return static_cast<AstLiteral *>(ast)->literal_type;
        }
        case AstType::Boolean:
            if (constness) {
                *constness |= ExprConstness::SawConstant;
            }
            return bool_type();
        case AstType::String:
            if (constness) {
                *constness |= ExprConstness::SawConstant;
            }
            return string_type();
        case AstType::Identifier:
            if (constness) {
                *constness |= ExprConstness::SawNonConstant;
            }
            return static_cast<AstIdentifier *>(ast)->var->type;
        case AstType::Binary: {
            return get_binary_expression_type(cc, ast, constness, overridable);
        }
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                if (constness) {
                    *constness |= ExprConstness::SawNonConstant;
                }
                auto *call = static_cast<AstCall *>(ast);
                return get_callee(cc, call)->return_type;
            }
            if (ast->operation == Operation::Negate) {
                return get_expression_type(
                    cc, static_cast<AstNegate *>(ast)->operand, constness, overridable);
            }
            if (ast->operation == Operation::Cast) {
                return static_cast<AstCast *>(ast)->cast_type;
            }
            break;
        default:
            break;
    }
    assert(!"get_expression_type unhandled ast type");
    return nullptr;
}

// Only valid before the type has been inferred.
bool is_auto_inferred(Type *type)
{
    return type->has_flag(TypeFlags::UNRESOLVED) && type->name.empty();
}

void resolve_type(Compiler &cc, Scope *scope, Type *&type)
{
    // If a type is unresolved, it's a) invalid, b) auto inferred, or c) declared later
    // (possibly as an alias). b) is dealt with in verify_var_decl.
    if (type->has_flag(TypeFlags::UNRESOLVED)) {
        auto *resolved = find_type(scope, type->name);
        if (resolved) {
            // c)
            delete type;
            type = resolved;
        } else {
            // a)
            verification_type_error(
                type->location, "type `{}` is not declared in this scope", type->name);
        }
    } else if (type->has_flag(TypeFlags::ALIAS) && type->real->has_flag(TypeFlags::UNRESOLVED)) {
        // c), alias
        auto *resolved = find_type(scope, type->real->name);
        if (resolved) {
            delete type->real;
            type->real = resolved;
        } else {
            verification_type_error(type->location,
                "invalid alias `{}`: underlying type `{}` is not declared in this scope",
                type->name, type->real->name);
        }
    }

    if (type->has_flag(TypeFlags::ALIAS)) {
        // Walk the alias chain and see if we end up with a circular definition
        // TODO: can we do this faster/more efficiently?
        auto *slow = type;
        auto *fast = type->real;
        while (fast && fast->real) {
            if (slow == fast) {
                verification_type_error(
                    type->real->location, "circular alias: `{}`", type->real->name);
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

enum class WarnDiscardedReturn {
    No,
    Yes
};

void verify_expr(
    Compiler &cc, Ast *&expr, WarnDiscardedReturn warn_discarded, Type *expected = nullptr);

TypeError maybe_cast_integer(Type *wanted, Type *type, Ast *&expr, ExprConstness constness)
{
    Type *wanted_type = get_unaliased_type(wanted);
    if (wanted_type == type) {
        return TypeError::None;
    }
    if (!type->has_flag(TypeFlags::Integer) || !wanted_type->has_flag(TypeFlags::Integer)) {
        return TypeError::NotAnInteger;
    }

    if (type->size > wanted_type->size) {
        return TypeError::SizeMismatch;
    }
    // The constness check makes something like x: u64 = 0 possible, but not x: u64 = y
    // where y is a s64.
    // TODO: This also means U32_MAX can be casted to an s32. Add size check for constant literals?
    if (expr_has_no_constants(constness)
        && (type->has_flag(TypeFlags::UNSIGNED) ^ wanted_type->has_flag(TypeFlags::UNSIGNED))) {
        return TypeError::SignednessMismatch;
    }
    if (expr_is_fully_constant(constness) && expr->operation == Operation::Negate) {
        return TypeError::SignednessMismatch;
    }
    insert_cast(expr, wanted_type);
    return TypeError::None;
}

void verify_call(Compiler &cc, AstCall *call, WarnDiscardedReturn warn_discarded)
{
    auto *fn = get_callee(cc, call);
    if (call->args.size() != fn->params.size()) {
        const char *plural = fn->params.size() == 1 ? "" : "s";
        verification_error(call, "function `{}` takes {} argument{} but was called with {}",
            fn->name, fn->params.size(), plural, call->args.size());
    }
    for (size_t i = 0; i < call->args.size(); ++i) {
        auto *&arg = call->args[i];
        auto *wanted_type = fn->params[i]->var.type;
        verify_expr(cc, arg, WarnDiscardedReturn::No, wanted_type);
    }
    if (warn_discarded == WarnDiscardedReturn::Yes && !fn->returns_void()) {
        diag_ast_warning(
            cc, call, "discarded return value for non-void function call `{}`", fn->name);
    }
    // FIXME: not entirely accurate (e.g. recursive calls)
    ++fn->call_count;
}

void verify_negate(
    Compiler &cc, AstNegate *unary, WarnDiscardedReturn warn_discarded, Type *expected)
{
    verify_expr(cc, unary->operand, warn_discarded, expected);
    ExprConstness constness{};
    auto *type = get_expression_type(cc, unary->operand, &constness, TypeOverridable::No);
    // Must be a signed integer
    if (!type->has_flag(TypeFlags::Integer)
        || (!expr_is_constexpr_int(type, constness) && type->has_flag(TypeFlags::UNSIGNED))) {
        verification_type_error(unary->location, "negate of unsupported type `{}`", type->name);
    }
}

void verify_binary(
    Compiler &cc, AstBinary *binary, Type *expected, WarnDiscardedReturn warn_discarded)
{
    verify_expr(cc, binary->left, warn_discarded, expected);
    verify_expr(cc, binary->right, warn_discarded, expected);
}

Type *resolve_binary_type(Compiler &cc, AstBinary *ast)
{
    ExprConstness lhs_constness{}, rhs_constness{};
    auto *lhs = get_expression_type(cc, ast->left, &lhs_constness, TypeOverridable::No);
    auto *rhs = get_expression_type(cc, ast->right, &rhs_constness, TypeOverridable::No);
    Type *exp = rhs;
    if (expr_is_constexpr_int(lhs, lhs_constness) && expr_is_constexpr_int(rhs, rhs_constness)) {
        exp = get_common_integer_type(lhs, rhs);
    } else if (expr_is_fully_constant(rhs_constness)) {
        exp = lhs;
    }
    return exp;
}

bool types_match(Type *t1, Type *t2)
{
    if (t1->has_flag(TypeFlags::ALIAS)) {
        if (t2->has_flag(TypeFlags::ALIAS)) {
            return get_unaliased_type(t1) == get_unaliased_type(t2);
        }
        return get_unaliased_type(t1) == t2;
    } else if (t2->has_flag(TypeFlags::ALIAS)) {
        return t1 == get_unaliased_type(t2);
    }
    return t1 == t2;
}

void verify_comparison(Compiler &cc, AstBinary *cmp, WarnDiscardedReturn warn_discarded)
{
    auto *exp = resolve_binary_type(cc, cmp);
    if (exp->get_kind() == TypeFlags::Integer || exp->get_kind() == TypeFlags::Boolean) {
        verify_expr(cc, cmp->left, warn_discarded, exp);
        verify_expr(cc, cmp->right, warn_discarded, exp);
    } else {
        verification_type_error(cmp->location, "comparison operator does not apply to this type");
    }
}

void verify_int(Compiler &cc, Ast *&ast, Type *expected)
{
    if (!expected) {
        return;
    }

    ExprConstness constness{};
    if (auto *type = get_expression_type(cc, ast, &constness, TypeOverridable::No);
        !types_match(type, expected)) {
        if (auto err = maybe_cast_integer(expected, type, ast, constness); err != TypeError::None) {
            type_error(cc, ast, expected, type, err);
        }
    }
}

void verify_expr(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded, Type *expected)
{
    Type *type;
    switch (ast->type) {
        case AstType::Integer:
            return verify_int(cc, ast, expected);
        case AstType::Boolean:
        case AstType::Identifier:
            [[fallthrough]];
        case AstType::String:
            break;
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                verify_call(cc, static_cast<AstCall *>(ast), warn_discarded);
            } else if (ast->operation == Operation::Negate) {
                verify_negate(cc, static_cast<AstNegate *>(ast), warn_discarded, expected);
            }
            break;
        case AstType::Binary: {
            auto binary = static_cast<AstBinary *>(ast);
            switch (binary->operation) {
                case Operation::Equals:
                case Operation::NotEquals:
                case Operation::Greater:
                case Operation::GreaterEquals:
                case Operation::Less:
                    [[fallthrough]];
                case Operation::LessEquals:
                    verify_comparison(cc, binary, warn_discarded);
                    break;
                default:
                    verify_binary(cc, binary, expected, warn_discarded);
            }
            ast = try_constant_fold(cc, ast, expected, TypeOverridable::No);
            break;
        }
        default:
            TODO();
            return;
    }
    if (expected) {
        ExprConstness constness{};
        if (type = get_expression_type(cc, ast, &constness, TypeOverridable::No);
            !expr_is_constexpr_int(type, constness) && !types_match(type, expected)) {
            type_error(cc, ast, expected, type, TypeError::Unspecified);
        }
    }
}

void verify_var_decl(Compiler &cc, AstVariableDecl *var_decl)
{
    auto &var = var_decl->var;
    if (is_auto_inferred(var.type)) {
        // x := y
        // var_decl->init_expr must not be null (checked by the parser).
        // The placeholder type for inferred types is not a heap variable, so var.type is not
        // deleted before reassigning.
        ExprConstness constness{};
        var.type = get_expression_type(cc, var_decl->init_expr, &constness, TypeOverridable::Yes);
        verify_expr(cc, var_decl->init_expr, WarnDiscardedReturn::No, var.type);
    } else if (var_decl->init_expr) {
        // x: T = y
        resolve_type(cc, var_decl->scope, var.type);
        verify_expr(cc, var_decl->init_expr, WarnDiscardedReturn::No, var.type);
    } else {
        // x: T
        // var.type is already set and there is nothing to verify.
        resolve_type(cc, var_decl->scope, var.type);
    }
    auto *type = get_unaliased_type(var.type);
    if (!type || type->has_flag(TypeFlags::UNRESOLVED)) {
        verification_error(var_decl, "unable to resolve type `{}`", var.type->name);
    }
    if (type->get_kind() == TypeFlags::Void) {
        std::string alias_str = var.type->has_flag(TypeFlags::ALIAS)
            ? std::format(" (through alias {})", var.type->name)
            : "";
        verification_error(var_decl, "variable `{}` declared void{}", var.name, alias_str);
    }
    // Update this scope's variables table.
    // Why? Because if this variable is assigned to another auto inferred variable,
    // it's going to get its type from the table via find_variable().
    var_decl->scope->variables[var.index_in_scope]->var.type = var.type;
}

void verify_ast(Compiler &, Ast *, AstFunction *);

void verify_return(
    Compiler &cc, AstReturn *return_stmt, AstFunction *current_function, Type *expected)
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
            ExprConstness constness{};
            verification_error(return_stmt->expr,
                "void function `{}` must not return a value (got type `{}`)",
                current_function->name,
                get_expression_type(cc, return_stmt->expr, &constness, TypeOverridable::No)->name);
        }
        return;
    }

    if (!return_stmt->expr) {
        verification_error(return_stmt, "function `{}` must return value of type `{}`",
            current_function->name, current_function->return_type->name);
    }

    verify_expr(cc, return_stmt->expr, WarnDiscardedReturn::No, expected);
}

void verify_if(Compiler &cc, AstIf *if_stmt, AstFunction *current_function)
{
    if (if_stmt->expr->operation == Operation::VariableDecl) {
        verify_var_decl(cc, static_cast<AstVariableDecl *>(if_stmt->expr));
    } else {
        verify_expr(cc, if_stmt->expr, WarnDiscardedReturn::No, bool_type());
    }
    verify_ast(cc, if_stmt->body, current_function);
    if (if_stmt->else_body) {
        verify_ast(cc, if_stmt->else_body, current_function);
    }
}

void verify_while(Compiler &cc, Ast *ast, AstFunction *current_function)
{
    auto *while_stmt = static_cast<AstWhile *>(ast);
    verify_expr(cc, while_stmt->expr, WarnDiscardedReturn::No, bool_type());
    verify_ast(cc, while_stmt->body, current_function);
}

void verify_assign(Compiler &cc, Ast *ast)
{
    auto *binary = static_cast<AstBinary *>(ast);
    if (binary->left->type != AstType::Identifier) {
        verification_error(ast, "assignment to invalid value");
    }
    auto *expected = static_cast<AstIdentifier *>(binary->left)->var->type;
    verify_expr(cc, binary->right, WarnDiscardedReturn::No, expected);
}

void verify_function_decl(Compiler &cc, Ast *ast)
{
    auto *fn = static_cast<AstFunction *>(ast);
    resolve_type(cc, fn->scope, fn->return_type);
    for (auto &param : fn->params) {
        resolve_type(cc, fn->scope, param->var.type);
    }
    verify_ast(cc, fn->body, fn);
    if (!fn->returns_void() && (fn->return_stmts.empty() || !has_top_level_return(fn->body))) {
        verification_error(fn, "function `{}` is missing a top level return statement", fn->name);
    }
}

void verify_ast(Compiler &cc, Ast *ast, AstFunction *current_function)
{
    if (ast->type == AstType::Statement) {
        switch (ast->operation) {
            case Operation::VariableDecl:
                verify_var_decl(cc, static_cast<AstVariableDecl *>(ast));
                break;
            case Operation::FunctionDecl:
                verify_function_decl(cc, ast);
                break;
            case Operation::If:
                verify_if(cc, static_cast<AstIf *>(ast), current_function);
                break;
            case Operation::Return:
                verify_return(cc, static_cast<AstReturn *>(ast), current_function,
                    current_function->return_type);
                break;
            case Operation::While:
                verify_while(cc, ast, current_function);
                break;
            default:
                TODO();
        }
    } else if (ast->type == AstType::Block) {
        for (auto *stmt : static_cast<AstBlock *>(ast)->stmts) {
            verify_ast(cc, stmt, current_function);
        }
    } else {
        if (ast->operation == Operation::Assign) {
            verify_assign(cc, ast);
        } else if (ast->operation == Operation::Call) {
            verify_call(cc, static_cast<AstCall *>(ast), WarnDiscardedReturn::Yes);
        } else {
            ExprConstness constness{};
            verify_expr(cc, ast, WarnDiscardedReturn::Yes,
                get_expression_type(cc, ast, &constness, TypeOverridable::No));
        }
    }
}

void verify_main(Compiler &cc, AstFunction *main)
{
    for (auto *ast : main->body->stmts) {
        verify_ast(cc, ast, main);
    }
}
