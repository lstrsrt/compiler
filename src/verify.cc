#include "verify.hh"
#include "diagnose.hh"
#include "lexer.hh"
#include "optimizer.hh"
#include "parser.hh"

#include <algorithm>
#include <limits>
#include <map>

#define verification_error(ast, msg, ...) \
    diag::error_at(cc, ast->location, ErrorType::Verification, msg __VA_OPT__(, __VA_ARGS__))

#define verification_type_error(location, msg, ...) \
    diag::error_at(cc, location, ErrorType::TypeCheck, msg __VA_OPT__(, __VA_ARGS__))

Type *void_type()
{
    static Type s_type{ .name = "void", .flags = TypeFlags::Void | TypeFlags::BUILTIN, .size = 0 };
    return &s_type;
}

Type *u8_type()
{
    static Type s_type{ .name = "u8",
        .flags = TypeFlags::Integer | TypeFlags::UNSIGNED | TypeFlags::BUILTIN,
        .size = 8 };
    return &s_type;
}

Type *u16_type()
{
    static Type s_type{ .name = "u16",
        .flags = TypeFlags::Integer | TypeFlags::UNSIGNED | TypeFlags::BUILTIN,
        .size = 16 };
    return &s_type;
}

Type *u32_type()
{
    static Type s_type{ .name = "u32",
        .flags = TypeFlags::Integer | TypeFlags::UNSIGNED | TypeFlags::BUILTIN,
        .size = 32 };
    return &s_type;
}

Type *u64_type()
{
    static Type s_type{ .name = "u64",
        .flags = TypeFlags::Integer | TypeFlags::UNSIGNED | TypeFlags::BUILTIN,
        .size = 64 };
    return &s_type;
}

Type *s8_type()
{
    static Type s_type{ .name = "s8", .flags = TypeFlags::Integer | TypeFlags::BUILTIN, .size = 8 };
    return &s_type;
}

Type *s16_type()
{
    static Type s_type{
        .name = "s16", .flags = TypeFlags::Integer | TypeFlags::BUILTIN, .size = 16
    };
    return &s_type;
}

Type *s32_type()
{
    static Type s_type{
        .name = "s32", .flags = TypeFlags::Integer | TypeFlags::BUILTIN, .size = 32
    };
    return &s_type;
}

Type *s64_type()
{
    static Type s_type{
        .name = "s64", .flags = TypeFlags::Integer | TypeFlags::BUILTIN, .size = 64
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
        .name = "string", .flags = TypeFlags::String | TypeFlags::BUILTIN, .size = 64
    };
    return &s_type;
}

Type *null_type()
{
    static Type s_null{ .name = "null",
        .flags = TypeFlags::Integer | TypeFlags::BUILTIN,
        .size = 64,
        .pointer = 1,
        .real = s64_type(),
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
    return new Variable(unresolved_type(), name, true);
}

AstFunction *print_builtin()
{
    static AstFunction *print_fn = nullptr;
    static bool once = false;
    if (!once) {
#ifdef AST_USE_ARENA
        StmtVec stmts(ast_vec_allocator());
        VariableDecls params(ast_vec_allocator());
#else
        StmtVec stmts;
        VariableDecls params;
#endif
        params.push_back(new AstVariableDecl(string_type(), "__fmt", nullptr, {}));
        print_fn = new AstFunction("print", void_type(), params, new AstBlock(stmts), {});
        print_fn->attributes |= FunctionAttributes::BUILTIN_PRINT;
        once = true;
    }
    return print_fn;
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
            verification_type_error(ast->location, "incompatible sizes for types {} and {}",
                to_string(lhs_type), to_string(rhs_type));
            break;
        }
        case TypeError::SignednessMismatch: {
            const char *lhs_str = lhs_type->is_unsigned() ? "unsigned" : "signed";
            const char *rhs_str = rhs_type->is_unsigned() ? "unsigned" : "signed";
            verification_type_error(ast->location,
                "incompatible {} expression applied to left-hand {} variable of type {}", rhs_str,
                lhs_str, to_string(lhs_type));
            break;
        }
        case TypeError::PointerMismatch: {
            const char *lhs_str = lhs_type->is_pointer() ? "pointer" : "non-pointer";
            const char *rhs_str = rhs_type->is_pointer() ? "pointer" : "non-pointer";
            verification_type_error(ast->location,
                "incompatible expression applied to left-hand {} type {} and right-hand {} type {}",
                lhs_str, to_string(lhs_type), rhs_str, to_string(rhs_type));
            break;
        }
        default:
            verification_type_error(ast->location, "incompatible types {} and {}",
                to_string(rhs_type), to_string(lhs_type));
            break;
    }
}

enum_flags(ExprConstness, uint8_t){
    SAW_CONSTANT = 1 << 0,
    SAW_NON_CONSTANT = 1 << 1,
};

bool expr_has_no_constants(ExprConstness e)
{
    return e == ExprConstness::SAW_NON_CONSTANT;
}

bool expr_is_fully_constant(ExprConstness e)
{
    return e == ExprConstness::SAW_CONSTANT;
}

bool expr_is_partly_constant(ExprConstness e)
{
    return e == (ExprConstness::SAW_CONSTANT | ExprConstness::SAW_NON_CONSTANT);
}

bool type_is_const_int(Type *t, ExprConstness e)
{
    return expr_is_fully_constant(e) && t->is_int();
}

Type *get_expression_type(
    Compiler &, Ast *, ExprConstness *, TypeOverridable = TypeOverridable::No);

Type *get_common_integer_type(Type *t1, Type *t2)
{
    assert(t1);
    if (!t2) {
        return t1;
    }
    if (t1->size != t2->size) {
        return (t1->size > t2->size) ? t1 : t2;
    }
    if (t1->is_unsigned() ^ t2->is_unsigned()) {
        auto *signed_type = t1->is_unsigned() ? t2 : t1;
        switch (signed_type->size) {
            case 8:
                return s16_type();
            case 16:
                return s32_type();
            case 32:
                [[fallthrough]];
            case 64:
                return s64_type();
            default:
                TODO();
        }
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
    Compiler &cc, Ast *ast, ExprConstness &constness, TypeOverridable overridable)
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
            seen_non_const = expr_has_no_constants(expr_constness);
        } else if (!seen_non_const && expr_has_no_constants(expr_constness)) {
            // If we got a non-constant, override the type unless it has already been overwritten.
            seen_non_const = true;
            ret = current;
        } else if (type_is_const_int(current, expr_constness)) {
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
        //
        // The AST is not actually overwritten because we want to verify the individual operands
        // later.
        traverse_postorder(ast, [&](Ast *ast) { try_constant_fold(cc, ast, ret, overridable); });
    }

    return ret;
}

Type *make_unsigned(Type *type)
{
    assert(type->is_int());
    if (type->is_unsigned()) {
        return type;
    }
    switch (type->size) {
        case 8:
            return u8_type();
        case 16:
            return u16_type();
        case 32:
            return u32_type();
        case 64:
            return u64_type();
    }
    TODO();
}

Type *get_expression_type(
    Compiler &cc, Ast *ast, ExprConstness *constness, TypeOverridable overridable)
{
    if (!ast) {
        return nullptr;
    }
    assert(constness);

    ast->expr_type = [&, func = __func__]() -> Type * {
        switch (ast->type) {
            case AstType::Integer: {
                *constness |= ExprConstness::SAW_CONSTANT;
                return static_cast<AstLiteral *>(ast)->expr_type;
            }
            case AstType::Boolean:
                *constness |= ExprConstness::SAW_CONSTANT;
                return bool_type();
            case AstType::String:
                *constness |= ExprConstness::SAW_CONSTANT;
                return string_type();
            case AstType::Identifier:
                *constness |= ExprConstness::SAW_NON_CONSTANT;
                return static_cast<AstIdentifier *>(ast)->var->type;
            case AstType::Binary: {
                return get_binary_expression_type(cc, ast, *constness, overridable);
            }
            case AstType::Unary:
                if (ast->operation == Operation::Call) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    auto *call = static_cast<AstCall *>(ast);
                    return get_callee(cc, call)->return_type;
                }
                if (ast->operation == Operation::AddressOf) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    auto *operand = static_cast<AstAddressOf *>(ast)->operand;
                    auto *type = get_expression_type(cc, operand, constness, overridable);
                    auto *ptr = new Type;
                    *ptr = *type;
                    ptr->size = 64;
                    ptr->pointer = type->pointer + 1;
                    ptr->real = type;
                    return ptr;
                }
                if (ast->operation == Operation::Dereference || ast->operation == Operation::Load) {
                    auto *type = get_expression_type(
                        cc, static_cast<AstUnary *>(ast)->operand, constness, overridable);

                    if (!type->is_pointer()) {
                        verification_error(static_cast<AstUnary *>(ast)->operand,
                            "cannot dereference non-pointer of type {}", to_string(type));
                    }

                    return type->real;
                }
                if (ast->operation == Operation::Negate) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    return get_expression_type(
                        cc, static_cast<AstNegate *>(ast)->operand, constness, overridable);
                }
                if (ast->operation == Operation::LogicalNot) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    return bool_type();
                }
                if (ast->operation == Operation::Not) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    auto *type = get_expression_type(
                        cc, static_cast<AstNot *>(ast)->operand, constness, overridable);

                    if (!type->is_int()) {
                        verification_error(static_cast<AstUnary *>(ast)->operand,
                            "bitwise not can only be applied to only unsigned integers");
                    }

                    return make_unsigned(type);
                }
                if (ast->operation == Operation::Cast) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    return static_cast<AstCast *>(ast)->cast_type;
                }
                [[fallthrough]];
            default:
                todo(func, __FILE__, __LINE__);
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
    // (possibly as an alias). b) only applies to variable declarations, so it is dealt with in
    // verify_var_decl().
    // This function also handles underlying enum types.

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

void verify_expr(Compiler &, Ast *&, WarnDiscardedReturn, Type *expected = nullptr);

uint64_t max_for_type(Type *t)
{
    assert(t->is_int());
    switch (t->size) {
        case 1:
            return 1;
        case 8:
            return t->is_unsigned() ? std::numeric_limits<uint8_t>::max()
                                    : std::numeric_limits<int8_t>::max();
        case 16:
            return t->is_unsigned() ? std::numeric_limits<uint16_t>::max()
                                    : std::numeric_limits<int16_t>::max();
        case 32:
            return t->is_unsigned() ? std::numeric_limits<uint32_t>::max()
                                    : std::numeric_limits<int32_t>::max();
        case 64:
            return t->is_unsigned() ? std::numeric_limits<uint64_t>::max()
                                    : std::numeric_limits<int64_t>::max();
    }
    TODO();
}

Integer min_for_type(Type *t)
{
    assert(t->is_int());
    if (t->is_unsigned()) {
        return Integer(0, false);
    }
    switch (t->size) {
        case 64:
            return Integer(std::numeric_limits<int64_t>::min(), true);
        case 32:
            return Integer(std::numeric_limits<int32_t>::min(), true);
        case 16:
            return Integer(std::numeric_limits<int16_t>::min(), true);
        case 8:
            return Integer(std::numeric_limits<int8_t>::min(), true);
        default:
            TODO();
    }
}

uint64_t get_int_literal(Ast *ast)
{
    return static_cast<AstLiteral *>(ast)->u.u64;
}

TypeError maybe_cast_int(Type *wanted, Type *type, Ast *&expr, ExprConstness constness)
{
    wanted = get_unaliased_type(wanted);
    type = get_unaliased_type(type);

    if (wanted == type) {
        return TypeError::None;
    }
    if (!type->is_int() || !wanted->is_int()) {
        return TypeError::Default;
    }
    if (type->pointer != wanted->pointer) {
        return TypeError::PointerMismatch;
    }

    bool needs_cast = false;
    if (type->size != wanted->size) {
        if (!expr_is_fully_constant(constness)) {
            // Needs an explicit cast.
            return TypeError::SizeMismatch;
        }
        // This is a literal that can be casted without precision loss.
        if (get_int_literal(expr) <= max_for_type(wanted)) {
            needs_cast = true;
        }
    }
    if (type->is_unsigned() ^ wanted->is_unsigned()) {
        // The constness check makes something like x: u64 = 0 possible, but not x: u64 = y
        // where y is a s64.
        if (expr_has_no_constants(constness)) {
            return TypeError::SignednessMismatch;
        }
        needs_cast = true;
    }
    // TODO: duplicate logic
    if (expr_is_fully_constant(constness) && get_int_literal(expr) > max_for_type(wanted)) {
        return TypeError::SizeMismatch;
    }
    if (needs_cast) {
        insert_cast(expr, wanted);
    }
    return TypeError::None;
}

void verify_arg(Compiler &cc, Ast *ast, Type *expected)
{
    if (ast->type == AstType::Identifier) {
        auto *ident = static_cast<AstIdentifier *>(ast);
        if (ident->var->is_unresolved) {
            verification_error(
                ident, "variable `{}` is not declared in this scope", ident->var->name);
        }
    }
    verify_expr(cc, ast, WarnDiscardedReturn::No, expected);
}

void verify_print(Compiler &cc, Ast *ast)
{
    auto *print = static_cast<AstCall *>(ast);
    if (print->args.size() < 1) {
        verification_error(print, "print needs at least one argument");
    }
    auto *fmt = print->args[0];
    verify_expr(cc, fmt, WarnDiscardedReturn::No, string_type());

    // Handle fmt string
    auto &str = static_cast<AstString *>(fmt)->string;
    std::vector<size_t> to_replace;
    for (size_t i = 0; i < str.size(); ++i) {
        char c = str[i];
        if (c == '{') {
            if (i == str.size() || str[i + 1] != '}') {
                verification_error(fmt, "unterminated format argument");
            }
            to_replace.push_back(i);
            ++i;
        }
    }

    if (to_replace.size() != print->args.size() - 1) {
        verification_error(ast,
            "format specifier amount ({}) does not match format argument amount ({})",
            to_replace.size(), print->args.size() - 1);
    }

    // Loop in reverse so the format specifier replacement indices stay correct
    for (size_t i = print->args.size() - 1; i >= 1; --i) {
        ExprConstness constness;
        auto *type = get_unaliased_type(
            get_expression_type(cc, print->args[i], &constness, TypeOverridable::Yes));
        verify_arg(cc, print->args[i], type);
        if (type->is_int()) {
            std::string fmt_s = [&]() {
                switch (type->size) {
                    case 64:
                        return type->is_unsigned() ? "%lu" : "%l";
                    case 32:
                        return type->is_unsigned() ? "%u" : "%d";
                    case 16:
                        return type->is_unsigned() ? "%hu" : "%hd";
                    case 8:
                        return type->is_unsigned() ? "%hhu" : "%hhd";
                }
                return "%d";
            }();
            str.erase(to_replace[i - 1], __builtin_strlen("{}"));
            str.insert(to_replace[i - 1], fmt_s);
        } else if (type->get_kind() == TypeFlags::String) {
            str.replace(to_replace[i - 1], __builtin_strlen("{}"), "%s");
        } else {
            TODO();
        }
    }
}

void verify_call(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    auto *call = static_cast<AstCall *>(ast);
    auto *fn = get_callee(cc, call);
    if (has_flag(fn->attributes, FunctionAttributes::BUILTIN_PRINT)) {
        verify_print(cc, ast);
        return;
    }

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
    for (size_t arg_idx = 0; arg_idx < call->args.size(); ++arg_idx) {
        auto *&arg = call->args[arg_idx];
        bool named_param = is_named_param(arg);
        if (named_param ^ named_call) {
            auto *p = named_param ? static_cast<AstBinary *>(arg)->left : arg;
            verification_error(p, "either no or all parameters must be named");
        }
        if (named_param) {
            auto *named_param = static_cast<AstBinary *>(arg);
            if (named_param->left->type != AstType::Identifier) {
                verification_error(
                    named_param, "named parameter needs a valid parameter name on the left");
            }
            // At this point, the identifier's type is still unresolved, but it will be set and
            // checked by verify_expr. Here we only check that the call expr itself is valid.
            auto *ident = static_cast<AstIdentifier *>(named_param->left);
            int param_idx = -1;
            for (auto *param : fn->params) {
                if (param->var.name == ident->var->name) {
                    param_idx = param->var.param_index;
                    if (std::cmp_not_equal(arg_idx, param_idx) && filled[param_idx]) {
                        verification_error(ident, "repeated named parameter `{}`", param->var.name);
                    }
                    filled[param_idx] = true;
                    break;
                }
            }
            if (param_idx == -1) {
                diag::prepare_error(cc, ident->location,
                    "named parameter doesn't match any function parameter name");
                diag::print_line(cc.lexer.string, ident->location);
                verification_error(fn, "function definition:");
            }
            if (std::cmp_not_equal(arg_idx, param_idx)) {
                std::swap(call->args[arg_idx], call->args[param_idx]);
                --arg_idx;
            }
            auto *expected = fn->params[param_idx]->var.type;
            verify_arg(cc, named_param->right, expected);
        } else {
            auto *wanted_type = fn->params[arg_idx]->var.type;
            verify_arg(cc, arg, wanted_type);
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

void verify_addressof(Compiler &cc, Ast *&ast, Type *, WarnDiscardedReturn warn_discarded)
{
    auto *unary = static_cast<AstAddressOf *>(ast);
    if (unary->operand->type != AstType::Identifier) {
        verification_error(unary->operand, "cannot take address of rvalue");
    }

    ExprConstness constness{};
    auto *type = get_expression_type(cc, unary->operand, &constness);
    if (type->pointer == std::numeric_limits<decltype(Type::pointer)>::max()) {
        verification_error(unary, "exceeded indirection limit");
    }

    verify_expr(cc, unary->operand, warn_discarded);
}

void verify_dereference(Compiler &cc, Ast *&ast, Type *, WarnDiscardedReturn warn_discarded)
{
    auto *unary = static_cast<AstDereference *>(ast);
    // TODO: this is not ideal
    if (unary->operand->type != AstType::Identifier
        && unary->operand->operation != Operation::Dereference
        && unary->operand->operation != Operation::AddressOf) {
        verification_error(unary->operand, "cannot dereference rvalue");
    }

    ExprConstness constness{};
    auto *type = get_expression_type(cc, unary->operand, &constness);
    if (!type->is_pointer()) {
        verification_error(unary, "cannot dereference non-pointer of type {}", to_string(type));
    }

    verify_expr(cc, unary->operand, warn_discarded);
}

void verify_negate(Compiler &cc, Ast *&ast, Type *expected, WarnDiscardedReturn warn_discarded)
{
    auto *unary = static_cast<AstNegate *>(ast);

    auto negation_error = [&]() {
        verification_type_error(unary->location,
            "negation of unsupported type {}.\n"
            "only signed integers may be negated",
            to_string(expected));
    };

    if (!expected->is_int() || expected->is_unsigned() || expected->is_pointer()) {
        negation_error();
    }

    verify_expr(cc, unary->operand, warn_discarded, expected);

    ExprConstness constness{};
    auto *type = get_expression_type(cc, unary->operand, &constness);
    if (!type->is_int() || type->is_unsigned()) {
        negation_error();
    }
}

bool is_unsigned_or_convertible(Type *type, ExprConstness constness)
{
    return type->is_int() && (expr_is_fully_constant(constness) || type->is_unsigned());
}

void verify_not(Compiler &cc, Ast *&ast, Type *expected, WarnDiscardedReturn warn_discarded)
{
    auto *unary = static_cast<AstNot *>(ast);
    if (!expected->is_int()) {
        verification_type_error(
            unary->location, "bitwise not can only be applied to unsigned integers");
    }

    ExprConstness constness{};
    auto *type = get_expression_type(cc, unary->operand, &constness);
    verify_expr(cc, unary->operand, warn_discarded, expected);

    if (!type->is_int() || !is_unsigned_or_convertible(type, constness)) {
        verification_type_error(
            unary->location, "bitwise not can only be applied to only unsigned integers");
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

bool is_bitwise_operation(Operation op)
{
    switch (op) {
        case Operation::And:
        case Operation::Or:
        case Operation::Xor:
        case Operation::Not:
        case Operation::LeftShift:
        case Operation::LeftRotate:
        case Operation::RightShift:
            [[fallthrough]];
        case Operation::RightRotate:
            return true;
        default:
            return false;
    }
}

bool is_logical_operation(Operation op)
{
    switch (op) {
        case Operation::Equals:
        case Operation::NotEquals:
        case Operation::Greater:
        case Operation::GreaterEquals:
        case Operation::Less:
        case Operation::LessEquals:
        case Operation::LogicalAnd:
        case Operation::LogicalOr:
            [[fallthrough]];
        case Operation::LogicalNot:
            return true;
        default:
            return false;
    }
}

bool has_side_effects(Ast *);

enum class CheckSideEffects {
    No,
    Yes
};

bool exprs_identical(Ast *left, Ast *right, CheckSideEffects check_side_effects)
{
    if (left->operation != right->operation) {
        return false;
    }
    if (check_side_effects == CheckSideEffects::Yes
        && (has_side_effects(left) || has_side_effects(right))) {
        return false;
    }

    switch (left->type) {
        case AstType::Integer:
            [[fallthrough]];
        case AstType::Boolean:
            return get_int_literal(left) == get_int_literal(right);
        case AstType::String:
            return hash(static_cast<AstString *>(left)->string)
                == hash(static_cast<AstString *>(right)->string);
        case AstType::Identifier:
            return static_cast<AstIdentifier *>(left)->var
                == static_cast<AstIdentifier *>(right)->var;
        case AstType::Binary:
            return exprs_identical(static_cast<AstBinary *>(left)->left,
                       static_cast<AstBinary *>(right)->left, CheckSideEffects::Yes)
                && exprs_identical(static_cast<AstBinary *>(left)->right,
                    static_cast<AstBinary *>(right)->right, CheckSideEffects::Yes);
        case AstType::Unary:
            return exprs_identical(static_cast<AstUnary *>(left)->operand,
                static_cast<AstUnary *>(right)->operand, CheckSideEffects::Yes);
        default:
            TODO();
    }
}

bool expr_is_integral(Ast *ast)
{
    return ast->type == AstType::Integer || ast->type == AstType::Boolean;
}

void verify_logical_chain(Compiler &cc, AstBinary *cmp)
{
    if (has_side_effects(cmp->left) || has_side_effects(cmp->right)) {
        return;
    }

    if (exprs_identical(cmp->left, cmp->right, CheckSideEffects::No /* already checked */)) {
        diag::ast_warning(cc, cmp, "redundant logical chain");
        return;
    }

    if (cmp->operation == Operation::LogicalOr) {
        return;
    }

    auto get_parts = [](AstBinary *ast) -> std::pair<AstIdentifier *, AstLiteral *> {
        if (ast->left->type == AstType::Identifier) {
            if (expr_is_integral(ast->right)) {
                return { static_cast<AstIdentifier *>(ast->left),
                    static_cast<AstLiteral *>(ast->right) };
            }
        } else if (ast->right->type == AstType::Identifier) {
            if (expr_is_integral(ast->left)) {
                return { static_cast<AstIdentifier *>(ast->right),
                    static_cast<AstLiteral *>(ast->left) };
            }
        }
        return { nullptr, nullptr };
    };

    enum class LogicalType {
        Equals,
        NotEquals,
        Range
    };

    struct LogicalRange {
        Integer first{};
        Integer last{};
        Ast *ast = nullptr;
        LogicalType logical_type{};

        explicit LogicalRange(Ast *operand, AstLiteral *constant, Type *type)
            : ast(operand)
            , logical_type(LogicalType::Range)
        {
            const auto literal = get_int_literal(constant);
            first.is_signed = last.is_signed = type->is_signed();
            // TODO: canonicalize L/LE into G/GE earlier?
            switch (operand->operation) {
                case Operation::Equals:
                    first.value = literal;
                    logical_type = LogicalType::Equals;
                    break;
                case Operation::GreaterEquals:
                    first.value = literal;
                    last.value = max_for_type(type);
                    break;
                case Operation::Greater:
                    first.value = literal + 1; // TODO: account for overflow
                    last.value = max_for_type(type);
                    break;
                case Operation::Less:
                    first = min_for_type(type);
                    last.value = literal - 1; // TODO: account for overflow
                    break;
                case Operation::LessEquals:
                    first = min_for_type(type);
                    last.value = literal;
                    break;
                case Operation::NotEquals:
                    first.value = get_int_literal(constant);
                    logical_type = LogicalType::NotEquals;
                    break;
                default:
                    break;
            }
        }
    };

    std::vector<Ast *> flattened;
    flatten_binary(cmp, cmp->operation, flattened);
    std::map<Variable *, std::vector<LogicalRange>> ranges_map;

    for (auto *ast : flattened) {
        assert(ast->type == AstType::Binary);
        auto *operand = static_cast<AstBinary *>(ast);
        auto [ident, constant] = get_parts(operand);
        if (!ident || !constant) {
            continue;
        }
        ranges_map[ident->var].emplace_back(ast, constant, ident->var->type);
    }

    auto overlap_warning = [&](Ast *first, Ast *second, bool result) {
        diag::prepare_warning(cc, first->location, "this comparison:");
        diag::print_line(cc.lexer.string, first->location);
        diag::ast_warning(cc, second, "{} with this comparison, so the result will always be {}:",
            result ? "fully overlaps" : "does not overlap", result ? "true" : "false");
    };

    for (auto &[var, ranges] : ranges_map) {
        auto is_signed = var->type->is_signed();
        std::ranges::sort(ranges, [is_signed](const LogicalRange &a, const LogicalRange &b) {
            if (is_signed) {
                // TODO: add sle/ule helpers
                return a.first.as_signed() < b.first.as_signed();
            }
            return a.first < b.first;
        });

        for (size_t i = 0; i < size(ranges) - 1; ++i) {
            auto &range = ranges[i];
            auto &next = ranges[i + 1];

            if (range.logical_type == LogicalType::Equals
                && next.logical_type == LogicalType::Equals) {
                overlap_warning(range.ast, next.ast, range.first == next.first);
                continue;
            }
            if (range.logical_type == LogicalType::Range
                || next.logical_type == LogicalType::Range) {
                if (range.last < next.first) {
                    overlap_warning(range.ast, next.ast, false);
                }
            }
        }
    }
}

void verify_binary_operation(Compiler &cc, AstBinary *binary, Type *expected)
{
    ExprConstness lhs_constness{};
    ExprConstness rhs_constness{};
    auto *lhs_type = get_unaliased_type(get_expression_type(cc, binary->left, &lhs_constness));
    auto *rhs_type = get_unaliased_type(get_expression_type(cc, binary->right, &rhs_constness));
    auto op = binary->operation;

    if (is_arithmetic_operation(op) || is_bitwise_operation(op)) {
        if (is_arithmetic_operation(op) && (!lhs_type->is_int() || !rhs_type->is_int())) {
            verification_type_error(binary->location,
                "invalid operator applied to types `{}` and `{}`", lhs_type->get_name(),
                rhs_type->get_name());
        }

        if (op == Operation::LeftRotate || op == Operation::RightRotate) {
            if (!is_unsigned_or_convertible(lhs_type, lhs_constness)) {
                verification_error(binary, "rotate must be performed on an unsigned expression");
            }
            if (binary->left->operation == Operation::Negate) {
                diag::ast_warning(cc, binary->left, "rotating negative value");
            }
            if (!type_is_const_int(rhs_type, rhs_constness)) {
                return;
            }

            if (get_int_literal(binary->right) > std::numeric_limits<uint8_t>::max()) {
                verification_error(binary->right, "rotate count exceeds maximum of {}",
                    std::numeric_limits<uint8_t>::max());
            }
            return;
        }

        if (op == Operation::LeftShift || op == Operation::RightShift) {
            if (binary->right->operation == Operation::Negate) {
                verification_error(binary->right, "negative shift counts are not allowed");
            }
            if (op == Operation::LeftShift && binary->left->operation == Operation::Negate) {
                diag::ast_warning(cc, binary->left, "left shifting negative value");
            }
            if (!type_is_const_int(rhs_type, rhs_constness)) {
                return;
            }

            if (get_int_literal(binary->right) > std::numeric_limits<uint8_t>::max()) {
                verification_error(binary->right, "shift count exceeds maximum of {}",
                    std::numeric_limits<uint8_t>::max());
            }
            // The lhs check is here so we don't warn twice (here and again in the optimizer)
            if (!type_is_const_int(lhs_type, lhs_constness)) {
                if (get_int_literal(binary->right) >= expected->size) {
                    const char *shift_type = op == Operation::LeftShift ? "left" : "right";
                    diag::ast_warning(cc, binary, "{} shift overflows expected type {}", shift_type,
                        to_string(expected));
                }
            }
            return;
        }
    }
}

void verify_binary(
    Compiler &cc, AstBinary *binary, Type *expected, WarnDiscardedReturn warn_discarded)
{
    // TODO: operator overloading
    verify_binary_operation(cc, binary, expected);
    verify_expr(cc, binary->left, warn_discarded, expected);
    verify_expr(cc, binary->right, warn_discarded, expected);
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

    if (t1_u->is_int()) {
        if (!t2_u->is_int()) {
            return TypeError::Default;
        }
        if (t1_u->size != t2_u->size) {
            return TypeError::SizeMismatch;
        }
        if (t1_u->is_unsigned() ^ t2_u->is_unsigned()) {
            return TypeError::SignednessMismatch;
        }
        return TypeError::None;
    }

    if (t1_u->is_bool()) {
        if (!t2_u->is_bool()) {
            return TypeError::Default;
        }
        return TypeError::None;
    }

    return TypeError::Default;
}

enum class ComparisonLogicError {
    None,
    AlwaysFalse,
    AlwaysTrue,
};

ComparisonLogicError is_illogical_comparison(Type *type, Operation cmp, Integer constant)
{
    assert(is_logical_operation(cmp));

    if (cmp == Operation::Greater) {
        if (constant.is_signed
            && constant.as_signed() >= static_cast<int64_t>(max_for_type(type))) {
            return ComparisonLogicError::AlwaysFalse;
        }
        if (!constant.is_signed && constant > max_for_type(type)) {
            return ComparisonLogicError::AlwaysFalse;
        }
    } else if (cmp == Operation::LessEquals) {
        if (constant == max_for_type(type)) {
            return ComparisonLogicError::AlwaysTrue;
        }
    } else if (type->is_unsigned() || type->is_bool()) {
        if (constant == 0) {
            if (cmp == Operation::GreaterEquals) {
                return ComparisonLogicError::AlwaysTrue;
            }
            if (cmp == Operation::Less) {
                return ComparisonLogicError::AlwaysFalse;
            }
        }
    }

    return ComparisonLogicError::None;
}

void verify_comparison(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    auto *cmp = static_cast<AstBinary *>(ast);
    ExprConstness lhs_constness{};
    ExprConstness rhs_constness{};
    auto *lhs_type = get_expression_type(cc, cmp->left, &lhs_constness, TypeOverridable::Yes);
    auto *rhs_type = get_expression_type(cc, cmp->right, &rhs_constness, TypeOverridable::Yes);

    bool is_left_const = expr_is_fully_constant(lhs_constness);
    bool is_right_const = expr_is_fully_constant(rhs_constness);
    cmp->expr_type = is_left_const ? rhs_type : lhs_type;

    auto kind = cmp->expr_type->get_kind();
    if (kind != TypeFlags::Integer && kind != TypeFlags::Boolean) {
        verification_type_error(cmp->location, "comparison operator does not apply to this type");
    }

    verify_expr(cc, cmp->left, warn_discarded, cmp->expr_type);
    verify_expr(cc, cmp->right, warn_discarded, cmp->expr_type);

    if (is_left_const || is_right_const) {
        auto constant
            = Integer(is_left_const ? get_int_literal(cmp->left) : get_int_literal(cmp->right),
                cmp->expr_type->is_signed());
        if (auto err = is_illogical_comparison(cmp->expr_type, cmp->operation, constant);
            err != ComparisonLogicError::None) {
            const char *s = err == ComparisonLogicError::AlwaysFalse ? "false" : "true";
            diag::ast_warning(cc, cmp, "comparison is always {}", s);
        } else if (is_left_const && is_right_const) {
            ast = try_constant_fold(cc, cmp, cmp->expr_type, TypeOverridable::Yes);
            if (expr_is_integral(cmp)) {
                diag::ast_warning(
                    cc, cmp, "comparison is always {}", get_int_literal(cmp) ? "true" : "false");
            }
        }
    } else if (exprs_identical(cmp->left, cmp->right, CheckSideEffects::Yes)) {
        const char *s = cmp->operation == Operation::Equals ? "true" : "false";
        diag::ast_warning(cc, cmp, "comparison is always {}", s);
    }
}

void verify_logical_not(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    // Canonicalize into a binary Equals
    ExprConstness constness{};
    auto *type = get_expression_type(cc, static_cast<AstLogicalNot *>(ast)->operand, &constness);
    ast = new AstBinary(Operation::Equals, static_cast<AstLogicalNot *>(ast)->operand,
        new AstLiteral(type, 0, ast->location), ast->location);
    ast->expr_type = type;
    verify_comparison(cc, ast, warn_discarded);
}

void convert_expr_to_boolean(Compiler &cc, Ast *&expr, Type *type)
{
    void convert_expr_to_boolean(Compiler &, Ast *&);

    if (expr->operation == Operation::LogicalAnd || expr->operation == Operation::LogicalOr) {
        convert_expr_to_boolean(cc, static_cast<AstBinary *>(expr)->left);
        convert_expr_to_boolean(cc, static_cast<AstBinary *>(expr)->right);
    } else if (expr->operation == Operation::LogicalNot) {
        convert_expr_to_boolean(cc, static_cast<AstLogicalNot *>(expr)->operand);
    } else if (!type->is_int()) {
        if (auto err = types_match(type, bool_type()); err != TypeError::None) {
            type_error(cc, expr, bool_type(), type, err);
        }
    }

    if (!is_logical_operation(expr->operation)) {
        expr = new AstBinary(
            Operation::NotEquals, expr, new AstLiteral(type, 0, expr->location), expr->location);
        expr->expr_type = type;
    }
}

void convert_expr_to_boolean(Compiler &cc, Ast *&expr)
{
    ExprConstness constness{};
    auto *type = get_unaliased_type(get_expression_type(cc, expr, &constness));
    convert_expr_to_boolean(cc, expr, type);
}

void match_type_or_cast(
    Compiler &cc, Ast *&ast, ExprConstness constness, Type *type, Type *expected)
{
    auto *t = type;
    auto *e = expected;

    expected = get_unaliased_type(expected);
    type = get_unaliased_type(type);

    auto err = types_match(type, expected);
    if (err == TypeError::None) {
        return;
    }

    if (expected->is_bool()) {
        convert_expr_to_boolean(cc, ast, type);
        return;
    }

    if (expected->is_int()) {
        if (err = maybe_cast_int(expected, type, ast, constness); err == TypeError::None) {
            return;
        }
    }

    type_error(cc, ast, expected, type, err);
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

    ExprConstness constness{};
    auto *type = get_unaliased_type(get_expression_type(cc, ast, &constness));
    match_type_or_cast(cc, ast, constness, type, expected);
}

void remove_cast(Ast *&ast)
{
    auto *operand = static_cast<AstCast *>(ast)->operand;
    delete static_cast<AstCast *>(ast);
    ast = operand;
}

void verify_explicit_cast(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    // Current behavior notes
    //
    // pointer casts: always allowed
    // integer casts: always allowed // TODO: add overflow checks
    // -> bool: not allowed
    // self -> self: removed without warning

    auto *cast = static_cast<AstCast *>(ast);
    auto *cast_type = cast->cast_type;

    ExprConstness constness{};
    auto *type = get_unaliased_type(get_expression_type(cc, cast->operand, &constness));

    if (cast_type->is_bool() && !type->is_bool()) {
        verification_type_error(ast->location, "cannot cast `{}` to `bool`", type->get_name());
    }

    if (cast_type->pointer != type->pointer) {
        verification_type_error(ast->location,
            "cannot cast `{}` to `{}` due to different indirection levels", type->get_name(),
            cast_type->get_name());
    }

    if (types_match(cast_type, type) == TypeError::None) {
        // TODO: there are more instances where a cast can be deleted
        remove_cast(ast);
        verify_expr(cc, ast /* this is the operand now */, warn_discarded);
    } else {
        // We don't pass anything for `expected` because verify_expr uses get_expression_type
        // so passing `type` would always succeed, and passing the type we're casting to would
        // always fail.
        verify_expr(cc, cast->operand, warn_discarded);
    }
}

void verify_unary(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded, Type *expected)
{
    switch (ast->operation) {
        case Operation::Call:
            verify_call(cc, ast, warn_discarded);
            break;
        case Operation::AddressOf:
            verify_addressof(cc, ast, expected, warn_discarded);
            break;
        case Operation::Dereference:
            verify_dereference(cc, ast, expected, warn_discarded);
            break;
        case Operation::Negate:
            verify_negate(cc, ast, expected, warn_discarded);
            break;
        case Operation::Not:
            verify_not(cc, ast, expected, warn_discarded);
            break;
        case Operation::LogicalNot:
            verify_logical_not(cc, ast, warn_discarded);
            ast = try_constant_fold(cc, ast, expected, TypeOverridable::No);
            ast = apply_de_morgan_laws(ast);
            break;
        case Operation::Cast:
            verify_explicit_cast(cc, ast, warn_discarded);
            break;
        case Operation::Load:
            // synthetic operation
            return;
        default:
            TODO();
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
            verify_unary(cc, ast, warn_discarded, expected);
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
                    verify_comparison(cc, ast, warn_discarded);
                    break;
                default:
                    verify_binary(cc, binary, expected, warn_discarded);
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
        ExprConstness constness{};
        type = get_expression_type(cc, ast, &constness);
        match_type_or_cast(cc, ast, constness, type, expected);
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
                verify_call(cc, return_stmt->expr, WarnDiscardedReturn::No);
                if (get_callee(cc, call)->returns_void()) {
                    // Returning f() (where f returns void) is allowed
                    return;
                }
            }
            ExprConstness constness{};
            verification_error(return_stmt->expr,
                "void function `{}` must not return a value (got type `{}`)",
                current_function->name,
                get_expression_type(cc, return_stmt->expr, &constness)->get_name());
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
        // If verify_expr was called immediately, "x + 123" would be converted to "(x != 0) + 123".
        // The conversion has to happen on the uppermost level instead: "(x + 123) != 0".
        convert_expr_to_boolean(cc, if_stmt->expr);
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
    // See verify_if for explanation
    convert_expr_to_boolean(cc, while_stmt->expr);
    verify_expr(cc, while_stmt->expr, WarnDiscardedReturn::No, bool_type());
    verify_ast(cc, while_stmt->body, current_function);
}

void verify_assign(Compiler &cc, Ast *ast)
{
    auto *binary = static_cast<AstBinary *>(ast);
    if (binary->left->type != AstType::Identifier
        && binary->left->operation != Operation::Dereference) {
        verification_error(ast, "assignment to invalid value");
    }
    if (binary->left->operation == Operation::Dereference) {
        binary->operation = Operation::Store;
        binary->left->operation = Operation::Load;
    }
    verify_expr(cc, binary->left, WarnDiscardedReturn::No);
    ExprConstness constness{};
    auto *expected = get_expression_type(cc, binary->left, &constness);
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

TypeError const_int_compatible_with_underlying(Type *type, Type *underlying, Ast *expr)
{
    underlying = get_unaliased_type(underlying);
    type = get_unaliased_type(type);

    if (underlying == type) {
        return TypeError::None;
    }
    if (!type->is_int() || !underlying->is_int()) {
        return TypeError::Default;
    }
    if (type->pointer != underlying->pointer) {
        return TypeError::PointerMismatch;
    }
    // FIXME: enum(s64) with member 0xffff_ffff?
    if (type->is_unsigned() ^ underlying->is_unsigned()) {
        return TypeError::SignednessMismatch;
    }
    // FIXME: incorrect when type is signed and folded into negative?
    if (get_int_literal(expr) > max_for_type(underlying)) {
        return TypeError::SizeMismatch;
    }

    return TypeError::None;
}

void verify_enum_member(Compiler &cc, AstEnumMember *&ast)
{
    auto *enum_decl = ast->enum_decl;
    auto *underlying = enum_decl->enum_type->real;
    auto *real = get_unaliased_type(underlying);

    if (!real->is_int() && !real->is_string()) {
        verification_type_error(underlying->location,
            "cannot declare type `{}` as the underlying enum type.\n"
            "the underlying type must be an integer or string type.",
            underlying->get_name());
    }

    ExprConstness constness{};
    auto *type = get_unaliased_type(get_expression_type(cc, ast, &constness));

    if (real->is_int()) {
        // We don't do casting here so match_type_or_cast() is not suitable.
        if (auto err = const_int_compatible_with_underlying(type, underlying, ast);
            err == TypeError::None) {
            // HACK: usage of this enum member relies on this being correct
            ast->expr_type = real;
        } else {
            type_error(cc, ast, type, underlying, err);
        }
    }
}

void verify_enum_decl(Compiler &cc, Ast *ast)
{
    auto *enum_decl = static_cast<AstEnumDecl *>(ast);
    // `enum_type` is guaranteed to exist.
    resolve_type(cc, enum_decl->scope, enum_decl->enum_type->real);
    for (auto &[name, member] : enum_decl->members) {
        verify_enum_member(cc, member);
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
            case Operation::EnumDecl:
                verify_enum_decl(cc, ast);
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
            verify_call(cc, ast, WarnDiscardedReturn::Yes);
        } else {
            ExprConstness constness{};
            verify_expr(
                cc, ast, WarnDiscardedReturn::Yes, get_expression_type(cc, ast, &constness));
        }
    }
}

void verify_main(Compiler &cc, AstFunction *main)
{
    verify_ast(cc, main, main);
}
