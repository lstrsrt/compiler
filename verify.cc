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

void flatten_binary(Ast *ast, std::vector<Ast *> &flattened)
{
    if (ast->type == AstType::Binary) {
        flatten_binary(static_cast<AstBinary *>(ast)->left, flattened);
        flatten_binary(static_cast<AstBinary *>(ast)->right, flattened);
    } else {
        flattened.push_back(ast);
    }
}

void insert_implicit_cast(Ast *&expr, Type *to)
{
    expr = new AstCast(expr, to, expr->location);
}

enum class TypeError {
    None,
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
            cc.diag_ast_error(ast,
                "incompatible sizes for types `{}` ({} bytes) and `{}` ({} bytes)", rhs_type->name,
                rhs_type->size, lhs_type->name, lhs_type->size);
            break;
        }
        case TypeError::SignednessMismatch: {
            const char *lhs_str = lhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            const char *rhs_str = rhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            cc.diag_ast_error(ast, "incompatible {} expression applied to {} variable of type `{}`",
                rhs_str, lhs_str, lhs_type->name);
            break;
        }
        default:
            cc.diag_ast_error(
                ast, "incompatible types `{}` and `{}`", rhs_type->name, lhs_type->name);
            break;
    }
}

enum class ExprConstness { SeenConstant = 1 << 0, SeenNonConstant = 1 << 1 };

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
    return e == ExprConstness::SeenNonConstant;
}

bool expr_is_fully_constant(ExprConstness e)
{
    return e == ExprConstness::SeenConstant;
}

bool expr_is_partly_constant(ExprConstness e)
{
    return e == (ExprConstness::SeenConstant | ExprConstness::SeenNonConstant);
}

bool expr_is_constexpr_int(Type *t, ExprConstness e)
{
    return expr_is_fully_constant(e) && t->get_kind() == TypeFlags::Integer;
}

enum class ForceSigned { No, Yes };

Type *get_expression_type(Compiler &cc, Ast *ast, ExprConstness *constant = nullptr,
    ForceSigned force_signed = ForceSigned::No);

TypeError maybe_cast_integer(Type *wanted_type, Type *type, Ast *&expr, ExprConstness constness);

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

Type *get_integer_expression_type(uint64_t u64, ForceSigned force_signed)
{
    // TODO - warn on overflow (or warn in string_to_number/parser)
    // TODO - just default to 64 bit numbers?
    if (force_signed == ForceSigned::Yes) {
        if (u64 > std::numeric_limits<int64_t>::max()) {
            // TODO - test me
            // cc.diag_ast_error(literal, "overflow");
        }
        if (u64 > std::numeric_limits<int32_t>::max()) {
            return s64_type();
        }
        return s32_type();
    }
    if (u64 > std::numeric_limits<int64_t>::max()) {
        return u64_type();
    }
    if (u64 > std::numeric_limits<uint32_t>::max()) {
        return s64_type();
    }
    if (u64 > std::numeric_limits<int32_t>::max()) {
        return u32_type();
    }
    return s32_type();
}

Type *get_binary_expression_type(
    Compiler &cc, AstBinary *binary, ExprConstness *constness = nullptr)
{
    switch (binary->operation) {
        case Operation::Assign:
            return void_type();
        case Operation::Equals:
        case Operation::NotEquals:
        case Operation::Greater:
        case Operation::GreaterEquals:
        case Operation::Less:
            [[fallthrough]];
        case Operation::LessEquals:
            return bool_type();
        default:
            break;
    }

    Type *current = nullptr;
    Type *ret = nullptr;
    std::vector<Ast *> operands;
    flatten_binary(binary, operands);

    for (auto *ast : operands) {
        ExprConstness expr_constness{};
        current = get_expression_type(cc, ast, &expr_constness);
        if (expr_is_constexpr_int(current, expr_constness)) {
            ret = get_common_integer_type(current, ret);
        } else {
            ret = current;
        }
        if (constness) {
            *constness |= expr_constness;
        }
    }

    return ret;
}

Type *get_expression_type(
    Compiler &cc, Ast *ast, ExprConstness *constness, ForceSigned force_signed)
{
    if (!ast) {
        return nullptr;
    }

    switch (ast->type) {
        case AstType::Integer: {
            if (constness) {
                *constness |= ExprConstness::SeenConstant;
            }
            return get_integer_expression_type(static_cast<AstLiteral *>(ast)->u.u64, force_signed);
        }
        case AstType::Boolean:
            if (constness) {
                *constness |= ExprConstness::SeenConstant;
            }
            return bool_type();
        case AstType::Identifier:
            if (constness) {
                *constness |= ExprConstness::SeenNonConstant;
            }
            return static_cast<AstIdentifier *>(ast)->var->type;
        case AstType::Binary: {
            return get_binary_expression_type(cc, static_cast<AstBinary *>(ast), constness);
        }
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                if (constness) {
                    *constness |= ExprConstness::SeenNonConstant;
                }
                auto *call = static_cast<AstCall *>(ast);
                return get_callee(cc, call)->return_type;
            }
            if (ast->operation == Operation::Negate) {
                return get_expression_type(
                    cc, static_cast<AstUnary *>(ast)->operand, constness, ForceSigned::Yes);
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
    // (possibly as an alias). b) is dealt with in resolve_var_decl.
    if (type->has_flag(TypeFlags::UNRESOLVED)) {
        auto *resolved = find_type(scope, type->name);
        if (resolved) {
            // c)
            delete type;
            type = resolved;
        } else {
            // a)
            cc.diag_type_error(type, "type `{}` is not declared in this scope", type->name);
        }
    } else if (type->has_flag(TypeFlags::ALIAS) && type->real->has_flag(TypeFlags::UNRESOLVED)) {
        // c), alias
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

    if (type->has_flag(TypeFlags::ALIAS)) {
        // Walk the alias chain and see if we end up with a circular definition
        // TODO - can we do this faster/more efficiently?
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

Type *get_integer_expression_type(uint64_t u64, ForceSigned force_signed);

Ast *partial_fold_associative(const std::vector<Ast *> &operands, Operation operation)
{
    std::vector<Ast *> non_constants;
    uint64_t accumulator = operation == Operation::Multiply ? 1 : 0;

    // FIXME - s64 hardcoded
    // we should figure out the expr type beforehand and pass it in
    for (auto *ast : operands) {
        if (ast->type == AstType::Integer) {
            if (operation == Operation::Add) {
                accumulator += static_cast<AstLiteral *>(ast)->u.u64;
            } else if (operation == Operation::Multiply) {
                accumulator *= static_cast<AstLiteral *>(ast)->u.u64;
            }
            delete static_cast<AstLiteral *>(ast);
        } else {
            non_constants.push_back(ast);
        }
    }

    auto *ret = non_constants[0];
    for (size_t i = 1; i < non_constants.size(); ++i) {
        ret = new AstBinary(operation, ret, non_constants[i], {});
    }
    return new AstBinary(operation, ret,
        new AstLiteral(get_integer_expression_type(accumulator, ForceSigned::No), AstType::Integer,
            accumulator, {}),
        {});
}

Ast *try_fold_identities(AstBinary *binary, Ast *constant_ast, Ast *variable_ast, int64_t constant)
{
    if (binary->operation == Operation::Add) {
        if (constant == 0) {
            // x+0=x and 0+x=x
            delete static_cast<AstLiteral *>(constant_ast);
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
            delete static_cast<AstLiteral *>(constant_ast);
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
        auto lhs = static_cast<AstLiteral *>(leaf)->u.s64;
        auto result = -lhs;
        delete static_cast<AstLiteral *>(leaf);
        delete static_cast<AstUnary *>(unary);
        return new AstLiteral(s64_type(), AstType::Integer, result, {});
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
    delete static_cast<AstLiteral *>(binary->left);
    delete static_cast<AstLiteral *>(binary->right);
    delete static_cast<AstBinary *>(binary);
    return new AstLiteral(
        get_integer_expression_type(result, ForceSigned::No), AstType::Integer, result, loc);
}

Ast *try_fold_binary(Compiler &cc, AstBinary *binary)
{
    // Figure out which parts of the expression are constant.
    auto *left = try_constant_fold(cc, binary->left);
    auto *right = try_constant_fold(cc, binary->right);
    bool left_is_const = left->type == AstType::Integer;
    bool right_is_const = right->type == AstType::Integer;
    if (!left_is_const && !right_is_const) {
        return binary;
    }

    int64_t left_const, right_const;
    if (left_is_const) {
        left_const = static_cast<AstLiteral *>(left)->u.s64;
    }
    if (right_is_const) {
        right_const = static_cast<AstLiteral *>(right)->u.s64;
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

void verify_expr(Compiler &cc, Ast *&expr, WarnDiscardedReturn warn_discarded,
    Type *expected = nullptr, ForceSigned force_signed = ForceSigned::No);

TypeError maybe_cast_integer(Type *wanted_type, Type *type, Ast *&expr, ExprConstness constness)
{
    if (type->size > wanted_type->size) {
        return TypeError::SizeMismatch;
    }
    // The constness check makes something like x: u64 = 0 possible, but not x: u64 = y
    // where y is a s64.
    // TODO - This also means U32_MAX can be casted to an s32. Add size check for constant literals?
    if (expr_has_no_constants(constness)
        && (type->has_flag(TypeFlags::UNSIGNED) ^ wanted_type->has_flag(TypeFlags::UNSIGNED))) {
        return TypeError::SignednessMismatch;
    }
    if (expr_is_fully_constant(constness) && expr->operation == Operation::Negate) {
        return TypeError::SignednessMismatch;
    }
    dbgln("casting {} to {}", type->name, wanted_type->name);
    insert_implicit_cast(expr, wanted_type);
    return TypeError::None;
}

void verify_call(Compiler &cc, AstCall *call, WarnDiscardedReturn warn_discarded)
{
    auto *fn = get_callee(cc, call);
    if (call->args.size() != fn->params.size()) {
        cc.diag_ast_error(call, "function `{}` takes {} arguments but was called with {}", fn->name,
            fn->params.size(), call->args.size());
    }
    for (size_t i = 0; i < call->args.size(); ++i) {
        auto *&arg = call->args[i];
        auto *wanted_type = fn->params[i]->var.type;
        verify_expr(cc, arg, WarnDiscardedReturn::No, wanted_type);
    }
    if (warn_discarded == WarnDiscardedReturn::Yes && !fn->returns_void()) {
        cc.diag_ast_warning(
            call, "discarded return value for non-void function call `{}`", fn->name);
    }
    // FIXME - not entirely accurate (e.g. recursive calls)
    ++fn->call_count;
}

void verify_negate(
    Compiler &cc, AstUnary *unary, WarnDiscardedReturn warn_discarded, Type *expected)
{
    verify_expr(cc, unary->operand, warn_discarded, expected, ForceSigned::Yes);
    auto *type = get_expression_type(cc, unary->operand, nullptr, ForceSigned::Yes);
    // Must be a signed integer
    if (!type->has_flag(TypeFlags::Integer) || type->has_flag(TypeFlags::UNSIGNED)) {
        cc.diag_ast_error(unary, "negate of unsupported type `{}`", type->name);
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
    auto *lhs = get_expression_type(cc, ast->left, &lhs_constness);
    auto *rhs = get_expression_type(cc, ast->right, &rhs_constness);
    dbgln("lhs: {} rhs: {}", lhs->name, rhs->name);
    Type *exp = rhs;
    if (expr_is_fully_constant(lhs_constness) && expr_is_fully_constant(rhs_constness)) {
        exp = get_common_integer_type(lhs, rhs);
    } else if (expr_is_fully_constant(rhs_constness)) {
        exp = lhs;
    }
    return exp;
}

void verify_comparison(Compiler &cc, AstBinary *cmp, WarnDiscardedReturn warn_discarded)
{
    auto *exp = resolve_binary_type(cc, cmp);
    verify_expr(cc, cmp->left, warn_discarded, exp);
    verify_expr(cc, cmp->right, warn_discarded, exp);
}

void verify_expr(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded, Type *expected,
    ForceSigned force_signed)
{
    Type *type;
    switch (ast->type) {
        case AstType::Integer:
            if (expected) {
                ExprConstness constness{};
                if (type = get_expression_type(cc, ast, &constness, force_signed);
                    type != expected) {
                    if (auto err = maybe_cast_integer(expected, type, ast, constness);
                        err != TypeError::None) {
                        type_error(cc, ast, expected, type, err);
                    }
                }
            }
            return;
        case AstType::Boolean:
            break;
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                verify_call(cc, static_cast<AstCall *>(ast), warn_discarded);
            } else if (ast->operation == Operation::Negate) {
                verify_negate(cc, static_cast<AstUnary *>(ast), warn_discarded, expected);
            } else {
                dbgln("encountered cast");
            }
            break;
        // ast = try_constant_fold(cc, ast);
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
            // ast = try_constant_fold(cc, binary);
            break;
        }
        case AstType::Identifier:
            break;
        default:
            return;
    }
    if (expected) {
        ExprConstness constness{};
        if (type = get_expression_type(cc, ast, &constness, force_signed); type != expected) {
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
        var.type = get_expression_type(cc, var_decl->init_expr);
        verify_expr(cc, var_decl->init_expr, WarnDiscardedReturn::No, var.type);
    } else if (var_decl->init_expr) {
        // x: T = y
        verify_expr(cc, var_decl->init_expr, WarnDiscardedReturn::No, var.type);
    } else {
        // x: T
        // var.type is already set and there is nothing to verify.
        ;
    }
    resolve_type(cc, var_decl->scope, var.type);
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

void verify_return(
    Compiler &cc, AstReturn *return_stmt, AstFunctionDecl *current_function, Type *expected)
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
        cc.diag_ast_error(return_stmt, "function `{}` must return value of type `{}`",
            current_function->name, current_function->return_type->name);
    }

    verify_expr(cc, return_stmt->expr, WarnDiscardedReturn::No, expected);
}

void verify_if(Compiler &cc, AstIf *if_stmt)
{
    if (if_stmt->expr->operation == Operation::VariableDecl) {
        verify_var_decl(cc, static_cast<AstVariableDecl *>(if_stmt->expr));
    } else {
        verify_expr(cc, if_stmt->expr, WarnDiscardedReturn::No, bool_type());
    }
}

void verify_ast(Compiler &cc, Ast *ast, AstFunctionDecl *current_function)
{
    if (ast->type == AstType::Statement) {
        if (ast->operation == Operation::VariableDecl) {
            verify_var_decl(cc, static_cast<AstVariableDecl *>(ast));
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
            verify_if(cc, static_cast<AstIf *>(ast));
        } else if (ast->operation == Operation::Return) {
            verify_return(
                cc, static_cast<AstReturn *>(ast), current_function, current_function->return_type);
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
