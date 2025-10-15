#include "verify.hh"
#include "diagnose.hh"
#include "lexer.hh"
#include "optimizer.hh"
#include "parser.hh"

#define verification_error(ast, msg, ...) \
    diag::error_at(cc, ast->location, ErrorType::Verification, msg __VA_OPT__(, __VA_ARGS__))

#define verification_type_error(location, msg, ...) \
    diag::error_at(cc, location, ErrorType::TypeCheck, msg __VA_OPT__(, __VA_ARGS__))

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

Type *null_type()
{
    static Type s_null{ .name = "null",
        .flags = TypeFlags::Integer | TypeFlags::BUILTIN,
        .size = 8,
        .pointer = 1,
        .real = nullptr,
        .location = {} };
    return &s_null;
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

Variable *unresolved_var(std::string_view name)
{
    return new Variable(unresolved_type(), name);
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

void type_error(Compiler &cc, Ast *ast, Type *lhs_type, Type *rhs_type, TypeError err)
{
    assert(err != TypeError::None);
    switch (err) {
        case TypeError::SizeMismatch: {
            const char *lhs_str = lhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            const char *rhs_str = rhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            verification_type_error(ast->location,
                "incompatible sizes for types `{}` ({} {} bytes) and `{}` ({} {} bytes)",
                rhs_type->get_name(), rhs_str, rhs_type->size, lhs_type->get_name(), lhs_str,
                lhs_type->size);
            break;
        }
        case TypeError::SignednessMismatch: {
            const char *lhs_str = lhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            const char *rhs_str = rhs_type->has_flag(TypeFlags::UNSIGNED) ? "unsigned" : "signed";
            verification_type_error(ast->location,
                "incompatible {} expression applied to left-hand {} variable of type `{}`", rhs_str,
                lhs_str, lhs_type->get_name());
            break;
        }
        case TypeError::PointerMismatch: {
            const char *lhs_str = lhs_type->is_pointer() ? "pointer" : "non-pointer";
            const char *rhs_str = rhs_type->is_pointer() ? "pointer" : "non-pointer";
            verification_type_error(ast->location,
                "incompatible expression applied to left-hand {} type and right-hand {} type",
                lhs_str, rhs_str);
            break;
        }
        default:
            verification_type_error(ast->location, "incompatible types `{}` and `{}`",
                rhs_type->get_name(), lhs_type->get_name());
            break;
    }
}

enum_flags(ExprConstness, uint8_t){
    SawConstant = 1 << 0,
    SawNonConstant = 1 << 1,
};

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

bool expr_is_const_int(Type *t, ExprConstness e)
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
        traverse_postorder(static_cast<AstCast *>(ast)->operand, callback);
    }

    if (ast->type == AstType::Binary) {
        traverse_postorder(static_cast<AstBinary *>(ast)->left, callback);
        traverse_postorder(static_cast<AstBinary *>(ast)->right, callback);
    }

    callback(ast);
}

Type *get_binary_expression_type(
    Compiler &cc, Ast *&ast, ExprConstness &constness, TypeOverridable overridable)
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
        ExprConstness expr_constness;
        current = get_expression_type(cc, ast, &expr_constness, overridable);
        if (!ret) {
            // First time, just set it to whatever we got.
            ret = current;
            seen_non_const = expr_has_no_constants(expr_constness);
        } else if (!seen_non_const && expr_has_no_constants(expr_constness)) {
            // If we got a non-constant, override the type unless it has already been overwritten.
            seen_non_const = true;
            ret = current;
        } else if (expr_is_const_int(current, expr_constness)) {
            ret = get_common_integer_type(current, ret);
        } else if (types_match(ret, current) != TypeError::None) {
            verification_type_error(ast->location,
                "incompatible types `{}` and `{}` in binary operation", ret->get_name(),
                current->get_name());
        }
        constness |= expr_constness;
    }

    if (!expr_has_no_constants(constness) && !(ast->flags & AstFlags::FOLDED)) {
        // Fold here to detect if a constant expr overflows the detected type, and warn/promote if
        // needed. This allows us to resolve `x := 0xffffffff+1` to an s64 instead of a u32 with an
        // overflowing add.
        traverse_postorder(
            ast, [&](Ast *&ast) { ast = try_constant_fold(cc, ast, ret, overridable); });
    }

    return ret;
}

Type *get_expression_type(
    Compiler &cc, Ast *&ast, ExprConstness *constness, TypeOverridable overridable)
{
    if (!ast) {
        return nullptr;
    }
    assert(constness);
    *constness = {};

    ast->expr_type = [&]() -> Type * {
        switch (ast->type) {
            case AstType::Integer: {
                *constness |= ExprConstness::SawConstant;
                return static_cast<AstLiteral *>(ast)->expr_type;
            }
            case AstType::Boolean:
                *constness |= ExprConstness::SawConstant;
                return bool_type();
            case AstType::String:
                *constness |= ExprConstness::SawConstant;
                return string_type();
            case AstType::Identifier:
                *constness |= ExprConstness::SawNonConstant;
                return static_cast<AstIdentifier *>(ast)->var->type;
            case AstType::Binary: {
                return get_binary_expression_type(cc, ast, *constness, overridable);
            }
            case AstType::Unary:
                if (ast->operation == Operation::Call) {
                    *constness |= ExprConstness::SawNonConstant;
                    auto *call = static_cast<AstCall *>(ast);
                    return get_callee(cc, call)->return_type;
                }
                if (ast->operation == Operation::AddressOf) {
                    *constness |= ExprConstness::SawNonConstant;
                    auto *operand = static_cast<AstAddressOf *>(ast)->operand;
                    auto *type = get_expression_type(cc, operand, constness, overridable);
                    auto *ptr = new Type;
                    *ptr = *type;
                    ptr->size = 8;
                    ptr->pointer = type->pointer + 1;
                    ptr->real = type;
                    return ptr;
                }
                if (ast->operation == Operation::Dereference) {
                    auto *type = get_expression_type(
                        cc, static_cast<AstDereference *>(ast)->operand, constness, overridable);

                    if (!type->is_pointer()) {
                        verification_error(static_cast<AstDereference *>(ast)->operand,
                            "cannot dereference non-pointer of type `{}`", type->get_name());
                    }

                    return type->real;
                }
                // TODO: set constness?
                if (ast->operation == Operation::Negate) {
                    return get_expression_type(
                        cc, static_cast<AstUnary *>(ast)->operand, constness, overridable);
                }
                if (ast->operation == Operation::LogicalNot) {
                    return bool_type();
                }
                if (ast->operation == Operation::Cast) {
                    return static_cast<AstCast *>(ast)->cast_type;
                }
                [[fallthrough]];
            default:
                TODO();
                return nullptr;
        }
    }();
    return ast->expr_type;
}

// Only valid before the type has been inferred.
bool is_auto_inferred(Type *type)
{
    return type->has_flag(TypeFlags::UNRESOLVED) && type->get_name().empty();
}

void resolve_type(Compiler &cc, Scope *scope, Type *&type)
{
    // If a type is unresolved, it's a) invalid, b) auto inferred, or c) declared later
    // (possibly as an alias). b) is dealt with in verify_var_decl.
    if (type->has_flag(TypeFlags::UNRESOLVED)) {
        auto *resolved = find_type(scope, type->get_name());
        if (resolved) {
            // c)
            delete type;
            type = resolved;
        } else {
            // a)
            verification_type_error(
                type->location, "type `{}` is not declared in this scope", type->get_name());
        }
    } else if (type->has_flag(TypeFlags::ALIAS) && type->real->has_flag(TypeFlags::UNRESOLVED)) {
        // c), alias
        auto *resolved = find_type(scope, type->real->get_name());
        if (resolved) {
            delete type->real;
            type->real = resolved;
        } else {
            verification_type_error(type->location,
                "invalid alias `{}`: underlying type `{}` is not declared in this scope",
                type->get_name(), type->real->get_name());
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
                    type->real->location, "circular alias: `{}`", type->real->get_name());
            }
            slow = slow->real;
            fast = fast->real->real;
        }
    }
}

enum class WarnDiscardedReturn {
    No,
    Yes
};

enum class InConditional {
    No,
    Yes
};

void verify_expr(Compiler &, Ast *&, WarnDiscardedReturn, Type *expected = nullptr,
    InConditional = InConditional::No);

uint64_t max_for_type(Type *t)
{
    bool is_unsigned = t->has_flag(TypeFlags::UNSIGNED);
    switch (t->size) {
        case 1:
            return 1;
        case 4:
            return is_unsigned ? std::numeric_limits<uint32_t>::max()
                               : std::numeric_limits<int32_t>::max();
        case 8:
            return is_unsigned ? std::numeric_limits<uint64_t>::max()
                               : std::numeric_limits<int64_t>::max();
    }
    TODO();
}

uint64_t get_int_literal(Ast *ast)
{
    return static_cast<AstLiteral *>(ast)->u.u64;
}

TypeError maybe_cast_int(Type *wanted, Type *type, Ast *&expr, ExprConstness constness)
{
    Type *wanted_type = get_unaliased_type(wanted);
    if (wanted_type == type) {
        return TypeError::None;
    }
    if (!type->has_flag(TypeFlags::Integer) || !wanted_type->has_flag(TypeFlags::Integer)) {
        return TypeError::Default;
    }
    if (type->pointer != wanted_type->pointer) {
        return TypeError::PointerMismatch;
    }

    if (type->size > wanted_type->size) {
        return TypeError::SizeMismatch;
    }
    // The constness check makes something like x: u64 = 0 possible, but not x: u64 = y
    // where y is a s64.
    if (expr_has_no_constants(constness)
        && (type->has_flag(TypeFlags::UNSIGNED) ^ wanted_type->has_flag(TypeFlags::UNSIGNED))) {
        return TypeError::SignednessMismatch;
    }
    if (expr_is_fully_constant(constness) && get_int_literal(expr) > max_for_type(wanted_type)) {
        return TypeError::SizeMismatch;
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
    std::vector<bool> filled(call->args.size());
    auto is_named_param = [](Ast *ast) {
        return ast->operation == Operation::Assign;
    };
    bool named_call = !call->args.empty() && is_named_param(call->args[0]);
    for (size_t i = 0; i < call->args.size(); ++i) {
        auto *&arg = call->args[i];
        bool named_param = is_named_param(arg);
        if (named_param ^ named_call) {
            verification_error(arg, "either no or all parameters must be named");
        }
        if (named_param) {
            // Named parameter
            auto *named_param = static_cast<AstBinary *>(arg);
            if (named_param->left->type != AstType::Identifier) {
                verification_error(
                    named_param, "named parameter needs a valid parameter name on the left");
            }
            // At this point, the identifier's type is still unresolved, but it will be set and
            // checked by verify_expr. Here we only check that the call expr itself is valid.
            auto *ident = static_cast<AstIdentifier *>(named_param->left);
            int j = -1;
            for (auto *param : fn->params) {
                if (param->var.name == ident->var->name) {
                    j = param->var.param_index;
                    if (std::cmp_not_equal(i, j) && filled[j]) {
                        verification_error(ident, "repeated named parameter");
                    }
                    filled[j] = true;
                    break;
                }
            }
            if (j == -1) {
                diag::prepare_error(cc, ident->location,
                    "named parameter doesn't match any function parameter name");
                diag::print_line(cc.lexer.string, ident->location);
                verification_error(fn, "function definition:");
            }
            if (std::cmp_not_equal(i, j)) {
                std::swap(call->args[i], call->args[j]);
                --i;
            }
            auto *expected = fn->params[j]->var.type;
            verify_expr(cc, named_param->right, WarnDiscardedReturn::No, expected);
        } else {
            auto *wanted_type = fn->params[i]->var.type;
            verify_expr(cc, arg, WarnDiscardedReturn::No, wanted_type);
        }
    }
    if (named_call) {
        // Replace "assignments" with right-hand expression
        for (auto *&arg : call->args) {
            arg = static_cast<AstBinary *>(arg)->right;
        }
    }
    if (warn_discarded == WarnDiscardedReturn::Yes && !fn->returns_void()) {
        diag::ast_warning(
            cc, call, "discarded return value for non-void function call `{}`", fn->name);
    }
    // FIXME: not entirely accurate (e.g. recursive calls)
    ++fn->call_count;
}

void verify_addressof(Compiler &cc, AstAddressOf *unary, Type *)
{
    if (unary->operand->type != AstType::Identifier) {
        verification_error(unary->operand, "cannot take address of rvalue");
    }
    ExprConstness constness;
    auto *type = get_expression_type(cc, unary->operand, &constness, TypeOverridable::No);
    if (type->pointer == std::numeric_limits<decltype(Type::pointer)>::max()) {
        verification_error(unary, "exceeded indirection limit");
    }
}

void verify_dereference(Compiler &cc, AstDereference *unary, Type *)
{
    if (unary->operand->type != AstType::Identifier
        && unary->operand->operation != Operation::Dereference
        && unary->operand->operation != Operation::AddressOf) {
        verification_error(unary->operand, "cannot dereference rvalue");
    }
    ExprConstness constness;
    auto *type = get_expression_type(cc, unary->operand, &constness, TypeOverridable::No);
    if (!type->is_pointer()) {
        verification_error(unary, "cannot dereference non-pointer of type `{}`", type->get_name());
    }
}

void verify_negate(
    Compiler &cc, AstUnary *unary, Type *expected, WarnDiscardedReturn warn_discarded)
{
    if (!expected->has_flag(TypeFlags::Integer) || expected->has_flag(TypeFlags::UNSIGNED)
        || expected->is_pointer()) {
        verification_type_error(unary->location,
            "negation of unsupported type `{}`.\n"
            "only signed integers may be negated.",
            expected->get_name());
    }

    verify_expr(cc, unary->operand, warn_discarded);

    ExprConstness constness;
    auto *type = get_expression_type(cc, unary->operand, &constness, TypeOverridable::No);
    if (!type->has_flag(TypeFlags::Integer) || type->has_flag(TypeFlags::UNSIGNED)) {
        verification_type_error(unary->location,
            "negation of unsupported type `{}`.\n"
            "only signed integers may be negated.",
            type->get_name());
    }
}

bool is_arithmetic_operation(Operation op)
{
    switch (op) {
        case Operation::Add:
        case Operation::Subtract:
        case Operation::Multiply:
        case Operation::Divide:
            [[fallthrough]];
        case Operation::Modulo:
            return true;
        default:
            return false;
    }
}

void verify_binary_operation(Compiler &cc, AstBinary *binary)
{
    ExprConstness constness;
    auto *lhs_type = get_expression_type(cc, binary->left, &constness, TypeOverridable::No);
    auto *rhs_type = get_expression_type(cc, binary->right, &constness, TypeOverridable::No);
    if (is_arithmetic_operation(binary->operation)) {
        if (lhs_type->get_kind() != TypeFlags::Integer
            || rhs_type->get_kind() != TypeFlags::Integer) {
            verification_type_error(binary->location,
                "invalid operator applied to types `{}` and `{}`", lhs_type->get_name(),
                rhs_type->get_name());
        }
    }
}

void verify_binary(Compiler &cc, AstBinary *binary, Type *expected,
    WarnDiscardedReturn warn_discarded, InConditional in_conditional)
{
    // TODO: operator overloading
    verify_binary_operation(cc, binary);
    verify_expr(cc, binary->left, warn_discarded, expected, in_conditional);
    verify_expr(cc, binary->right, warn_discarded, expected, in_conditional);
}

TypeError types_match(Type *t1, Type *t2)
{
    auto *t1_u = get_unaliased_type(t1);
    auto *t2_u = get_unaliased_type(t2);

    if (t1_u == t2_u) {
        return TypeError::None;
    }

    if (t1_u->pointer != t2_u->pointer) {
        return TypeError::PointerMismatch;
    }

    if (t1_u->get_kind() == TypeFlags::Integer) {
        if (t2_u->get_kind() != TypeFlags::Integer) {
            return TypeError::Default;
        }
        if (t1_u->size != t2_u->size) {
            return TypeError::SizeMismatch;
        }
        if (t1_u->has_flag(TypeFlags::UNSIGNED) ^ t2_u->has_flag(TypeFlags::UNSIGNED)) {
            return TypeError::SignednessMismatch;
        }
        return TypeError::None;
    }

    if (t1_u->get_kind() == TypeFlags::Boolean) {
        if (t2_u->get_kind() != TypeFlags::Boolean) {
            return TypeError::Default;
        }
        return TypeError::None;
    }

    return TypeError::Default;
}

void verify_comparison(Compiler &cc, AstBinary *cmp, WarnDiscardedReturn warn_discarded)
{
    ExprConstness lhs_constness;
    ExprConstness rhs_constness;
    auto *lhs_type = get_expression_type(cc, cmp->left, &lhs_constness, TypeOverridable::No);
    auto *rhs_type = get_expression_type(cc, cmp->right, &rhs_constness, TypeOverridable::No);

    auto *exp = rhs_type;
    bool one_side_const
        = expr_is_fully_constant(lhs_constness) || expr_is_fully_constant(rhs_constness);
    if (one_side_const && lhs_type->has_flag(TypeFlags::Integer)
        && rhs_type->has_flag(TypeFlags::Integer)) {
        exp = get_common_integer_type(lhs_type, rhs_type);
    } else if (auto err = types_match(lhs_type, rhs_type); err != TypeError::None) {
        type_error(cc, cmp, lhs_type, rhs_type, err);
    }

    cmp->expr_type = exp;
    auto kind = cmp->expr_type->get_kind();
    if (kind == TypeFlags::Integer || kind == TypeFlags::Boolean) {
        verify_expr(cc, cmp->left, warn_discarded, cmp->expr_type);
        verify_expr(cc, cmp->right, warn_discarded, cmp->expr_type);
    } else {
        verification_type_error(cmp->location, "comparison operator does not apply to this type");
    }
}

void verify_logical_not(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    // Canonicalize into a binary Equals
    ExprConstness constness;
    auto *type = get_expression_type(
        cc, static_cast<AstLogicalNot *>(ast)->operand, &constness, TypeOverridable::No);
    ast = new AstBinary(Operation::Equals, static_cast<AstLogicalNot *>(ast)->operand,
        new AstLiteral(type, 0, ast->location), ast->location);
    ast->expr_type = type;
    verify_comparison(cc, static_cast<AstBinary *>(ast), warn_discarded);
}

void verify_int(Compiler &cc, Ast *&ast, Type *expected)
{
    if (!expected) {
        return;
    }

    if (get_unaliased_type(expected) == bool_type()) {
        static_cast<AstLiteral *>(ast)->u.u64 = std::clamp<uint64_t>(get_int_literal(ast), 0, 1);
        return;
    }

    ExprConstness constness;
    auto *type = get_unaliased_type(get_expression_type(cc, ast, &constness, TypeOverridable::No));
    if (types_match(type, expected) == TypeError::None) {
        return;
    }

    if (auto err = maybe_cast_int(expected, type, ast, constness); err != TypeError::None) {
        type_error(cc, ast, expected, type, err);
    }
}

void convert_expr_to_boolean(Compiler &, Ast *&);

void convert_expr_to_boolean(Compiler &cc, Ast *&expr, Type *type)
{
    if (expr->operation == Operation::LogicalAnd || expr->operation == Operation::LogicalOr) {
        convert_expr_to_boolean(cc, static_cast<AstBinary *>(expr)->left);
        convert_expr_to_boolean(cc, static_cast<AstBinary *>(expr)->right);
    } else if (expr->operation == Operation::LogicalNot) {
        convert_expr_to_boolean(cc, static_cast<AstLogicalNot *>(expr)->operand);
    } else if (type->get_kind() == TypeFlags::Integer) {
        expr = new AstBinary(
            Operation::NotEquals, expr, new AstLiteral(type, 0, expr->location), expr->location);
        expr->expr_type = type;
    } else if (auto err = types_match(type, bool_type()); err != TypeError::None) {
        type_error(cc, expr, bool_type(), type, err);
    }
}

void convert_expr_to_boolean(Compiler &cc, Ast *&expr)
{
    ExprConstness constness;
    auto *type = get_unaliased_type(get_expression_type(cc, expr, &constness, TypeOverridable::No));
    convert_expr_to_boolean(cc, expr, type);
}

void verify_expr(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded, Type *expected,
    InConditional in_conditional)
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
            } else if (ast->operation == Operation::AddressOf) {
                verify_addressof(cc, static_cast<AstAddressOf *>(ast), expected);
            } else if (ast->operation == Operation::Dereference) {
                verify_dereference(cc, static_cast<AstDereference *>(ast), expected);
            } else if (ast->operation == Operation::Negate) {
                verify_negate(cc, static_cast<AstUnary *>(ast), expected, warn_discarded);
            } else if (ast->operation == Operation::LogicalNot) {
                verify_logical_not(cc, ast, warn_discarded);
                ast = try_constant_fold(cc, ast, expected, TypeOverridable::No);
                ast = apply_de_morgan_laws(ast);
            }
            break;
        case AstType::Binary: {
            auto *binary = static_cast<AstBinary *>(ast);
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
                    verify_binary(cc, binary, expected, warn_discarded, in_conditional);
                    break;
            }
            ast = try_constant_fold(cc, ast, expected, TypeOverridable::No);
            if (ast->operation == Operation::LogicalOr || ast->operation == Operation::LogicalAnd) {
                ast = try_fold_logical_chain(cc, binary);
            } else if (ast->operation == Operation::Equals
                || ast->operation == Operation::NotEquals) {
                ast = apply_de_morgan_laws(static_cast<AstBinary *>(ast));
            }
            break;
        }
        default:
            TODO();
            return;
    }
    if (expected) {
        ExprConstness constness;
        type = get_unaliased_type(get_expression_type(cc, ast, &constness, TypeOverridable::No));
        if (get_unaliased_type(expected) == bool_type()) {
            if (type->get_kind() == TypeFlags::Boolean) {
                return;
            }
            convert_expr_to_boolean(cc, ast, type);
            return;
        }
        if (auto err = types_match(type, expected); err != TypeError::None) {
            type_error(cc, ast, expected, type, err);
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
        ExprConstness constness;
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
        verification_error(var_decl, "unable to resolve type `{}`", var.type->get_name());
    }
    if (type->get_kind() == TypeFlags::Void) {
        std::string alias_str = var.type->has_flag(TypeFlags::ALIAS)
            ? std::format(" (through alias {})", var.type->get_name())
            : "";
        verification_error(var_decl, "variable `{}` declared void{}", var.name, alias_str);
    }
    // Update this scope's variables table.
    // Why? Because if this variable is assigned to another auto inferred variable,
    // it's going to get its type from the table via find_variable().
    var_decl->scope->variables[var.name]->var.type = var.type;
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
            ExprConstness constness;
            verification_error(return_stmt->expr,
                "void function `{}` must not return a value (got type `{}`)",
                current_function->name,
                get_expression_type(cc, return_stmt->expr, &constness, TypeOverridable::No)
                    ->get_name());
        }
        return;
    }

    if (!return_stmt->expr) {
        verification_error(return_stmt, "function `{}` must return value of type `{}`",
            current_function->name, current_function->return_type->get_name());
    }

    verify_expr(cc, return_stmt->expr, WarnDiscardedReturn::No, expected);
}

void verify_if(Compiler &cc, AstIf *if_stmt, AstFunction *current_function)
{
    if (if_stmt->expr->operation == Operation::VariableDecl) {
        verify_var_decl(cc, static_cast<AstVariableDecl *>(if_stmt->expr));
    } else {
        // If verify_expr was called immediately, "x + 123" would be converted to "(x != 0) + 123"
        // The conversion has to happen on the uppermost level instead: "(x + 123) != 0".
        convert_expr_to_boolean(cc, if_stmt->expr);
        verify_expr(cc, if_stmt->expr, WarnDiscardedReturn::No, bool_type(), InConditional::Yes);
    }
    verify_ast(cc, if_stmt->body, current_function);
    if (if_stmt->else_body) {
        verify_ast(cc, if_stmt->else_body, current_function);
    }
}

void verify_while(Compiler &cc, Ast *ast, AstFunction *current_function)
{
    auto *while_stmt = static_cast<AstWhile *>(ast);
    // See verify_if for explanation
    convert_expr_to_boolean(cc, while_stmt->expr);
    verify_expr(cc, while_stmt->expr, WarnDiscardedReturn::No, bool_type());
    verify_ast(cc, while_stmt->body, current_function);
}

void verify_assign(Compiler &cc, Ast *ast)
{
    auto *binary = static_cast<AstBinary *>(ast);
    if (binary->left->type != AstType::Identifier) {
        verification_error(ast, "assignment to invalid value");
    }
    verify_expr(cc, binary->left, WarnDiscardedReturn::No);
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
            case Operation::Break:
            case Operation::Continue:
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
            ExprConstness constness;
            verify_expr(cc, ast, WarnDiscardedReturn::Yes,
                get_expression_type(cc, ast, &constness, TypeOverridable::No));
        }
    }
}

void verify_main(Compiler &cc, AstFunction *main)
{
    verify_ast(cc, main, main);
}
