#include "verify.hh"
#include "ast-fold.hh"
#include "compiler.hh"
#include "diagnose.hh"
#include "lexer.hh"
#include "parser.hh"
#include "range.hh"

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
        .real = u8_type(),
        .location = {} };
    return &s_null;
}

Type *unresolved_type()
{
    static Type s_type{ .name = {}, .flags = TypeFlags::UNRESOLVED };
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
    return print_fn;
}

AstFunction *get_callee(Compiler &cc, AstCall *call)
{
    if (!call->fn) {
        // Cache it so the next lookup takes the fast path
        call->fn = find_function(call->scope, call->name);
        if (!call->fn) {
            verification_error(call, "function `{}` is not declared in this scope", call->name);
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
    expr->expr_type = to;
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
        case TypeError::EnumMismatch: {
            const char *lhs_str = lhs_type->has_flag(TypeFlags::ENUM) ? "enum" : "non-enum";
            const char *rhs_str = rhs_type->has_flag(TypeFlags::ENUM) ? "enum" : "non-enum";
            verification_type_error(ast->location,
                "incompatible expression applied to left-hand {} type {} and right-hand {} "
                "type {}",
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

Type *get_expression_type(Compiler &, Ast *, ExprConstness *);

Type *get_common_integer_type(Type *t1, Type *t2)
{
    assert(t1);
    if (!t2) {
        return t1;
    }
    if (t1->size != t2->size) {
        return (t1->size > t2->size) ? t1 : t2;
    }
    if (t1->is_unsigned() != t2->is_unsigned()) {
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

Type *get_binary_expression_type(Compiler &cc, Ast *ast, ExprConstness &constness)
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
        current = get_expression_type(cc, ast, &expr_constness);
        constness |= expr_constness;
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
            if (current == s64_type() && ret->is_pointer()) {
                // Pointer arithmetic.
                continue;
            }
            verification_type_error(ast->location,
                "incompatible types {} and {} in binary operation", to_string(ret),
                to_string(current));
        }
    }

    if (!expr_has_no_constants(constness) && !(ast->flags & AstFlags::FOLDED)) {
        // Fold here to detect if a constant expr overflows the detected type, and warn/promote if
        // needed. This allows us to resolve `x := 0xffffffff+1` to an s64 instead of a u32 with an
        // overflowing add.
        //
        // The AST is not actually overwritten because we want to verify the individual operands
        // later. `overridable` needs to always be Yes so the optimizer can properly determine
        // `ret`.
        traverse_postorder(
            ast, [&](Ast *ast) { try_constant_fold(cc, ast, ret, TypeOverridable::Yes); });
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

Type *make_pointer(Compiler &, Type *real);

Type *get_expression_type(Compiler &cc, Ast *ast, ExprConstness *constness)
{
    if (!ast) {
        return nullptr;
    }
    assert(constness);

    ast->expr_type = [&, func = __func__]() -> Type * {
        switch (ast->type) {
            case AstType::Integer:
                *constness |= ExprConstness::SAW_CONSTANT;
                return static_cast<AstLiteral *>(ast)->expr_type;
            case AstType::Boolean:
                *constness |= ExprConstness::SAW_CONSTANT;
                return bool_type();
            case AstType::String:
                *constness |= ExprConstness::SAW_CONSTANT;
                return string_type();
            case AstType::Enum: {
                auto *member = static_cast<AstEnumMember *>(ast);
                (void)get_expression_type(cc, member->expr, constness);
                if (!member->enum_decl) {
                    member->enum_decl = find_enum(member->scope, member->enum_name);
                }
                return member->enum_decl->enum_type;
            }
            case AstType::Identifier:
                *constness |= ExprConstness::SAW_NON_CONSTANT;
                return static_cast<AstIdentifier *>(ast)->var->type;
            case AstType::Binary:
                return get_binary_expression_type(cc, ast, *constness);
            case AstType::Unary:
                if (ast->operation == Operation::Call) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    auto *call = static_cast<AstCall *>(ast);
                    return get_callee(cc, call)->return_type;
                }
                if (ast->operation == Operation::AddressOf) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    auto *operand = static_cast<AstAddressOf *>(ast)->operand;
                    auto *type = get_expression_type(cc, operand, constness);
                    return make_pointer(cc, type);
                }
                if (ast->operation == Operation::Dereference || ast->operation == Operation::Load) {
                    auto *type
                        = get_expression_type(cc, static_cast<AstUnary *>(ast)->operand, constness);

                    if (!type->is_pointer()) {
                        verification_error(static_cast<AstUnary *>(ast)->operand,
                            "cannot dereference non-pointer of type {}", to_string(type));
                    }

                    return type->real;
                }
                if (ast->operation == Operation::Negate) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    return get_expression_type(
                        cc, static_cast<AstNegate *>(ast)->operand, constness);
                }
                if (ast->operation == Operation::LogicalNot) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    return bool_type();
                }
                if (ast->operation == Operation::Not) {
                    *constness |= ExprConstness::SAW_NON_CONSTANT;
                    auto *type
                        = get_expression_type(cc, static_cast<AstNot *>(ast)->operand, constness);

                    if (!type->is_int()) {
                        verification_error(static_cast<AstUnary *>(ast)->operand,
                            "bitwise not can only be applied to only unsigned integers");
                    }

                    return make_unsigned(type);
                }
                if (ast->operation == Operation::Cast) {
                    // Call only to get the constness.
                    (void)get_expression_type(cc, static_cast<AstCast *>(ast)->operand, constness);
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
    // (possibly as an enum or alias). b) only applies to variable declarations, so it is dealt with
    // in verify_var_decl().

    if (type->has_flag(TypeFlags::UNRESOLVED)) {
        // Don't use get_name() because it includes the pointer stars etc.
        auto *resolved = find_type(scope, type->name);
        if (!resolved) {
            if (auto *enum_decl = find_enum(scope, type->name)) {
                resolve_type(cc, enum_decl->scope, enum_decl->enum_type->real);
                resolved = enum_decl->enum_type;
            }
        }
        if (resolved) {
            // c)
            for (int i = 0; i < type->pointer; ++i) {
                resolved = make_pointer(cc, resolved);
            }
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

    // TODO: what if pointer was an unresolved alias?
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
    assert(t->is_integral() || t->is_pointer());
    switch (t->size) {
        case 1:
            return 1;
        case 8:
            return t->is_unsigned() ? U8Max : S8Max;
        case 16:
            return t->is_unsigned() ? U16Max : S16Max;
        case 32:
            return t->is_unsigned() ? U32Max : S32Max;
        case 64:
            return t->is_unsigned() ? U64Max : S64Max;
    }
    TODO();
}

Integer min_for_type(Type *t)
{
    assert(t->is_integral() || t->is_pointer());
    if (t->is_unsigned()) {
        return Integer(0, false);
    }
    switch (t->size) {
        case 64:
            return Integer(S64Min, true);
        case 32:
            return Integer(S32Min, true);
        case 16:
            return Integer(S16Min, true);
        case 8:
            return Integer(S8Min, true);
        default:
            TODO();
    }
}

bool expr_is_const_integral(Ast *ast, IgnoreCasts ignore)
{
    if (ast->operation == Operation::Cast && ignore == IgnoreCasts::Yes) {
        return expr_is_const_integral(static_cast<AstCast *>(ast)->operand, ignore);
    }
    return ast->type == AstType::Integer || ast->type == AstType::Boolean;
}

uint64_t get_int_literal(Ast *ast)
{
    if (ast->operation == Operation::Cast) {
        return get_int_literal(static_cast<AstCast *>(ast)->operand);
    }
    assert(expr_is_const_integral(ast, IgnoreCasts::No));
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
    if (expr_is_fully_constant(constness) && get_int_literal(expr) > max_for_type(wanted)) {
        return TypeError::SizeMismatch;
    }

    bool needs_cast = false;
    if (type->is_unsigned() != wanted->is_unsigned()) {
        // The constness check makes something like x: u64 = 0 possible, but not x: u64 = y
        // where y is a s64.
        if (!expr_is_fully_constant(constness)) {
            if (wanted->is_unsigned() || wanted->size <= type->size) {
                return TypeError::SignednessMismatch;
            }
        }
        needs_cast = true;
    } else if (type->size != wanted->size) {
        if (!expr_is_fully_constant(constness)) {
            if (wanted->size <= type->size) {
                // Needs an explicit cast.
                return TypeError::SizeMismatch;
            }
        }
        // This expr can be casted without precision loss.
        needs_cast = true;
    }

    if (needs_cast) {
        insert_cast(expr, wanted);
    }
    return TypeError::None;
}

void verify_arg(Compiler &cc, Ast *&ast, Type *expected)
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
        } else if (c == '%') {
            str.insert(i, "%");
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
        auto *type = get_unaliased_type(get_expression_type(cc, print->args[i], &constness));
        verify_arg(cc, print->args[i], type);
        if (type->is_pointer()) {
            str.replace(to_replace[i - 1], __builtin_strlen("{}"), "%p");
        } else if (type->is_integral()) {
            std::string fmt_s = [&]() {
                switch (type->size) {
                    case 64:
                        return type->is_unsigned() ? "%lu" : "%ld";
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
            verification_error(print->args[i], "don't know how to print type {}", to_string(type));
        }
    }
}

void verify_call(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    auto *call = static_cast<AstCall *>(ast);
    auto *fn = get_callee(cc, call);
    call->expr_type = fn->return_type;
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
        if (named_param != named_call) {
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
        if (has_flag(fn->attributes, FunctionAttributes::FORCE_USE)) {
            verification_error(call,
                "discarded return value for non-void function call `{}` (function marked with "
                "force_use attribute)",
                fn->name);
        } else {
            diag::ast_warning(
                cc, call, "discarded return value for non-void function call `{}`", fn->name);
        }
    }
    if (fn == cc.current_function) {
        // call_count is for calls from other places, don't increment it here.
        fn->flags |= AstFlags::RECURSIVE;
    } else {
        ++fn->call_count;
    }
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
    static_cast<AstIdentifier *>(unary->operand)->var->force_stack = true;

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
    expected = get_unaliased_type(expected);
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

bool has_side_effects(Ast *ast)
{
    if (!ast) {
        return false;
    }

    switch (ast->type) {
        case AstType::Integer:
        case AstType::Boolean:
        case AstType::String:
        case AstType::Enum:
            [[fallthrough]];
        case AstType::Identifier:
            return false;
        case AstType::Binary:
            return has_side_effects(static_cast<AstBinary *>(ast)->left)
                || has_side_effects(static_cast<AstBinary *>(ast)->right);
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                // We don't see through calls yet.
                return true;
            }
            if (ast->operation == Operation::AddressOf || ast->operation == Operation::Dereference
                || ast->operation == Operation::Load || ast->operation == Operation::Store) {
                // Same for memory operations.
                return true;
            }
            return has_side_effects(static_cast<AstUnary *>(ast)->operand);
        case AstType::Block:
            return std::ranges::any_of(static_cast<AstBlock *>(ast)->stmts, has_side_effects);
        case AstType::Statement:
            return true;
    }
}

bool exprs_identical(Ast *left, Ast *right, CheckSideEffects check_side_effects)
{
    if (left->operation != right->operation || left->type != right->type) {
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
                       static_cast<AstBinary *>(right)->left, check_side_effects)
                && exprs_identical(static_cast<AstBinary *>(left)->right,
                    static_cast<AstBinary *>(right)->right, check_side_effects);
        case AstType::Unary:
            return exprs_identical(static_cast<AstUnary *>(left)->operand,
                static_cast<AstUnary *>(right)->operand, check_side_effects);
        default:
            TODO();
    }
}

void diagnose_tautological_comparisons(
    Compiler &cc, Ast *&cmp, Variable *var, const LogicalRange &ranges)
{
    auto min = min_for_type(ranges.type);
    // HACK: make max_for_type() return Integer as well
    Integer max{};
    max.is_signed = ranges.type->is_signed();
    max.value = max_for_type(ranges.type);

    Ranges feasible;
    if (cmp->operation == Operation::LogicalAnd) {
        feasible = feasible_ranges_and(ranges, min, max);
    } else {
        assert(cmp->operation == Operation::LogicalOr);
        feasible = feasible_ranges_or(ranges, min, max);
    }
    std::optional<bool> result;
    if (feasible.empty()) {
        result = false;
    } else if (size(feasible) == 1) {
        const auto &r = feasible.back();
        if (r.lo == min && r.lo_inc && r.hi == max && r.hi_inc) {
            result = true;
        }
    }
    if (!result) {
        return;
    }
    diag::ast_warning(cc, ranges.comp.back().comparison,
        "comparison range for `{}` covers {} possible values, so the expression will always be {}",
        var->name, *result ? "all" : "no", *result ? "true" : "false");
    // TODO: if optimizations are on:
    // cmp = new AstLiteral(s32_type(), *result ? 1 : 0, cmp->location);
}

Ast *uncasted_expr(Ast *ast)
{
    if (ast->operation == Operation::Cast) {
        return uncasted_expr(static_cast<AstCast *>(ast)->operand);
    }
    return ast;
}

void verify_logical_chain_components(Compiler &cc, Ast *cmp)
{
    std::vector<Ast *> flattened;
    flatten_binary(cmp, cmp->operation, flattened, CheckSideEffects::Yes);

    for (size_t i = 0; i < size(flattened); ++i) {
        auto *ast = flattened[i];
        for (size_t j = 0; j < i; ++j) {
            auto *ast2 = flattened[j];
            if (exprs_identical(ast, ast2, CheckSideEffects::Yes)) {
                diag::ast_warning(cc, ast, "redundant logical chain");
            }
        }
    }
}

// TODO: less terrible name
std::pair<AstIdentifier *, AstLiteral *> split_binary_to_ident_and_const(AstBinary *ast)
{
    if (ast->left->type == AstType::Identifier) {
        if (expr_is_const_integral(ast->right, IgnoreCasts::Yes)) {
            return { static_cast<AstIdentifier *>(ast->left),
                static_cast<AstLiteral *>(uncasted_expr(ast->right)) };
        }
    } else if (ast->right->type == AstType::Identifier) {
        if (expr_is_const_integral(ast->left, IgnoreCasts::Yes)) {
            return { static_cast<AstIdentifier *>(ast->right),
                static_cast<AstLiteral *>(uncasted_expr(ast->left)) };
        }
    }
    return { nullptr, nullptr };
}

void verify_logical_chain(Compiler &cc, Ast *&_cmp)
{
    auto *cmp = static_cast<AstBinary *>(_cmp);

    verify_logical_chain_components(cc, _cmp);

    if (has_side_effects(cmp->left) || has_side_effects(cmp->right)) {
        return;
    }

    std::vector<Ast *> flattened;
    flatten_binary(cmp, cmp->operation, flattened);
    std::map<Variable *, LogicalRange> ranges_map;

    for (auto *ast : flattened) {
        // If this is not a binary, we may be dealing with something like
        // "x and 0" which always evaluates to false but is not folded since 0 is the last part of
        // the expr. So just skip it.
        // TODO: we already know that there are no side effects...
        // so can we just fold and exit at this point?
        if (ast->type != AstType::Binary) {
            continue;
        }
        if (ast->operation == Operation::LogicalOr || ast->operation == Operation::LogicalAnd) {
            verify_logical_chain(cc, ast);
            continue;
        }
        auto *operand = static_cast<AstBinary *>(ast);
        auto [ident, constant] = split_binary_to_ident_and_const(operand);
        if (!ident || !constant) {
            continue;
        }
        ranges_map[ident->var].type = ident->var->type;
        ranges_map[ident->var].comp.emplace_back(operand, constant);
    }

    for (auto &[var, ranges] : ranges_map) {
        diagnose_tautological_comparisons(cc, _cmp, var, ranges);
    }
}

bool check_pointer_arithmetic(Compiler &cc, AstBinary *binary, Type *lhs_type,
    ExprConstness lhs_constness, Type *rhs_type, ExprConstness rhs_constness)
{
    auto create_offset = [&cc, binary](Ast *&offset, Type *ptr_type, Type *offset_type,
                             ExprConstness offset_constness) {
        assert(ptr_type->is_pointer());
        if (binary->operation != Operation::Add && binary->operation != Operation::Subtract) {
            verification_error(binary, "pointers only support addition and subtraction");
        }
        auto size = ptr_type->real->byte_size();
        bool is_literal = expr_is_const_integral(offset, IgnoreCasts::No);
        if (is_literal) {
            // Multiply immediately instead of letting the optimizer do it.
            // We don't want warnings here.
            static_cast<AstLiteral *>(offset)->u.u64 *= size;
        } else {
            offset = new AstBinary(Operation::Multiply, offset,
                new AstLiteral(s64_type(), size, offset->location), binary->location);
            static_cast<AstBinary *>(offset)->left->flags |= AstFlags::PTR_ARITH;
            static_cast<AstBinary *>(offset)->right->flags |= AstFlags::PTR_ARITH;
        }

        // Cast to s64. Removed again later if possible.
        if (auto err = maybe_cast_int(s64_type(), offset_type, offset, offset_constness);
            err != TypeError::None) {
            type_error(cc, binary, ptr_type, offset_type, err);
        }
    };

    if (lhs_type->is_pointer()) {
        if (rhs_type->is_pointer()) {
            verification_type_error(binary->location, "cannot add two pointers");
        }
        if (!rhs_type->is_int()) {
            return false;
        }
        create_offset(binary->right, lhs_type, rhs_type, rhs_constness);
    } else if (rhs_type->is_pointer()) {
        if (!lhs_type->is_int()) {
            return false;
        }
        create_offset(binary->left, rhs_type, lhs_type, lhs_constness);
    } else {
        return false;
    }

    return true;
}

void verify_binary_operation(Compiler &cc, AstBinary *binary, Type *expected)
{
    ExprConstness lhs_constness{};
    ExprConstness rhs_constness{};
    auto *lhs_type = get_expression_type(cc, binary->left, &lhs_constness);
    auto *rhs_type = get_expression_type(cc, binary->right, &rhs_constness);

    // TODO: figure out a diag api that does not need this stuff
    auto *lt = lhs_type;
    auto *rt = rhs_type;
    lhs_type = get_unaliased_type(lhs_type);
    rhs_type = get_unaliased_type(rhs_type);

    auto op = binary->operation;

    if (is_arithmetic_operation(op) || is_bitwise_operation(op)) {
        if (is_arithmetic_operation(op)) {
            if (check_pointer_arithmetic(
                    cc, binary, lhs_type, lhs_constness, rhs_type, rhs_constness)) {
                binary->flags |= AstFlags::PTR_ARITH;
                binary->left->flags |= AstFlags::PTR_ARITH;
                binary->right->flags |= AstFlags::PTR_ARITH;
                return;
            }
            if (!lhs_type->is_int() || !rhs_type->is_int()) {
                verification_type_error(binary->location,
                    "invalid operator applied to types {} and {}", to_string(lt), to_string(rt));
            }
            return;
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

            if (get_int_literal(binary->right) > U8Max) {
                verification_error(binary->right, "rotate count exceeds maximum of {}", U8Max);
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

            if (get_int_literal(binary->right) > U8Max) {
                verification_error(binary->right, "shift count exceeds maximum of {}", U8Max);
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
    if (!expected && binary->operation == Operation::Assign) {
        ExprConstness c{};
        expected = get_expression_type(cc, binary->left, &c);
    }
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

    if (t1_u->is_pointer() && t2_u->is_pointer()) {
        if (t1_u == null_type() || t2_u == null_type()) {
            // Any pointer can be set to null.
            return TypeError::None;
        }
    }

    if (t1_u->pointer != t2_u->pointer) {
        if (t1_u != null_type() || !t2_u->is_pointer()) {
            return TypeError::PointerMismatch;
        }
    } else if (t1_u->is_pointer()) {
        // Both are pointers with equal depth.
        return types_match(t1_u->real, t2_u->real);
    }

    if (t1_u->is_int()) {
        if (!t2_u->is_int()) {
            return TypeError::Default;
        }
        if (t1_u->is_unsigned() != t2_u->is_unsigned()) {
            return TypeError::SignednessMismatch;
        }
        if (t1_u->size != t2_u->size) {
            return TypeError::SizeMismatch;
        }
    }

    if (t1_u->has_flag(TypeFlags::ENUM) != t2_u->has_flag(TypeFlags::ENUM)) {
        return TypeError::EnumMismatch;
    }

    return TypeError::Default;
}

enum class ComparisonLogicError {
    None,
    AlwaysFalse,
    AlwaysTrue,
};

int64_t get_signed(int size, Integer integer)
{
    switch (size) {
        case 64:
            return integer.as_signed();
        case 32:
            return static_cast<int64_t>(static_cast<int32_t>(integer.as_signed()));
        case 16:
            return static_cast<int64_t>(static_cast<int16_t>(integer.as_signed()));
        case 8:
            return static_cast<int64_t>(static_cast<int8_t>(integer.as_signed()));
    }
    TODO();
}

ComparisonLogicError is_illogical_comparison(Type *type, Operation cmp, Integer constant)
{
    assert(is_logical_operation(cmp));

    if (cmp == Operation::Greater) {
        if (constant.is_signed
            && get_signed(type->size, constant) >= static_cast<int64_t>(max_for_type(type))) {
            return ComparisonLogicError::AlwaysFalse;
        }
        if (!constant.is_signed && constant.value >= max_for_type(type)) {
            return ComparisonLogicError::AlwaysFalse;
        }
    } else if (cmp == Operation::GreaterEquals) {
        if (constant.value == min_for_type(type).value) {
            return ComparisonLogicError::AlwaysTrue;
        }
    } else if (cmp == Operation::LessEquals) {
        if (constant.value == max_for_type(type)) {
            return ComparisonLogicError::AlwaysTrue;
        }
    } else if (type->is_unsigned() || type->is_bool()) {
        if (constant.value == 0) {
            if (cmp == Operation::Less) {
                return ComparisonLogicError::AlwaysFalse;
            }
        }
    }

    return ComparisonLogicError::None;
}

struct ComparisonTypes {
    Type *lhs_type = nullptr;
    Type *rhs_type = nullptr;
    Type *type = nullptr;
    ExprConstness lhs_constness{};
    ExprConstness rhs_constness{};
};

void get_comparison_types(Compiler &cc, AstBinary *&cmp, ComparisonTypes &c)
{
    c.lhs_type = get_expression_type(cc, cmp->left, &c.lhs_constness);
    c.rhs_type = get_expression_type(cc, cmp->right, &c.rhs_constness);
    c.type = expr_is_fully_constant(c.lhs_constness) ? c.rhs_type : c.lhs_type;
    cmp->expr_type = c.type;
}

void verify_comparison(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    auto *cmp = static_cast<AstBinary *>(ast);
    ComparisonTypes c;
    get_comparison_types(cc, cmp, c);
    auto *expected = get_unaliased_type(c.type);

    if (!expected->is_integral()) {
        verification_type_error(cmp->location, "comparison operator does not apply to this type");
    }

    verify_expr(cc, cmp->left, warn_discarded, expected);
    verify_expr(cc, cmp->right, warn_discarded, expected);

    // verify_expr can insert casts etc so get type again
    get_comparison_types(cc, cmp, c);
    expected = get_unaliased_type(c.type);
    bool is_left_const = expr_is_const_integral(cmp->left, IgnoreCasts::Yes);
    bool is_right_const = expr_is_const_integral(cmp->right, IgnoreCasts::Yes);

    if (is_left_const || is_right_const) {
        auto constant
            = Integer(is_left_const ? get_int_literal(cmp->left) : get_int_literal(cmp->right),
                expected->is_signed());
        if (auto err = is_illogical_comparison(expected, cmp->operation, constant);
            err != ComparisonLogicError::None) {
            const char *s = err == ComparisonLogicError::AlwaysFalse ? "false" : "true";
            diag::ast_warning(cc, cmp, "comparison is always {}", s);
        } else {
            ast = try_constant_fold(cc, cmp, expected, TypeOverridable::Yes);
            if (expr_is_const_integral(ast, IgnoreCasts::Yes)) {
                diag::ast_warning(
                    cc, ast, "comparison is always {}", get_int_literal(ast) ? "true" : "false");
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

    if (has_flag(ast->flags, AstFlags::PTR_ARITH)) {
        if (expected->is_pointer() && type == s64_type()) {
            return;
        }
    }

    type_error(cc, ast, e, t, err);
}

void remove_cast(Ast *&ast)
{
    auto *operand = static_cast<AstCast *>(ast)->operand;
    delete static_cast<AstCast *>(ast);
    ast = operand;
}

void verify_cast(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded)
{
    // Current behavior notes
    //
    // pointer casts: always allowed
    // integer casts: always allowed // TODO: add overflow checks
    // -> bool: not allowed
    // <-> string: not allowed
    // self -> self: removed without warning

    auto *cast = static_cast<AstCast *>(ast);
    auto *cast_type = cast->cast_type;

    ExprConstness constness{};
    auto *t = get_expression_type(cc, cast->operand, &constness);
    auto *type = get_unaliased_type(t);

    auto err = types_match(cast_type, type);
    if (err == TypeError::None) {
        remove_cast(ast);
        verify_expr(cc, ast /* this is the operand now */, warn_discarded, type);
        return;
    }

    // TODO: can remove_cast when casting enum to underlying

    {
        auto *type2 = type;
        auto *t2 = t;
        if (type->has_flag(TypeFlags::ENUM)) {
            t = t->real;
            type = get_unaliased_type(t);
        }

        if (cast_type->is_string() != type->is_string()) {
            verification_type_error(ast->location, "cannot cast to or from string");
        }

        if (cast_type->is_bool() && !type->is_bool()) {
            verification_type_error(ast->location, "cannot cast {} to `bool`", to_string(t));
        }

        if (cast_type->pointer != type->pointer) {
            if (!cast_type->pointer && cast_type->size < 64) {
                verification_type_error(ast->location, "cannot cast {} to {} due to precision loss",
                    to_string(t), to_string(cast_type));
            }
        }

        type = type2;
        t = t2;
    }

    // We don't pass anything for `expected` because verify_expr uses get_expression_type
    // so passing `type` would always succeed, and passing the type we're casting to would
    // always fail.
    // ^ ^ ^ ^ ^
    // This is false now that pointer arithmetic checking inserts a cast to s64.
    // Pass `type` so we don't crash in constant folding.
    verify_expr(cc, cast->operand, warn_discarded, type);
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
            verify_cast(cc, ast, warn_discarded);
            break;
        case Operation::Load:
            // synthetic operation
            return;
        default:
            TODO();
    }
}

AstEnumMember *get_enum_member(Compiler &cc, AstEnumDecl *enum_decl, AstEnumMember *unresolved)
{
    if (auto it = enum_decl->members->map.find(unresolved->name);
        it != enum_decl->members->map.end()) {
        return it->second;
    }
    verification_error(unresolved, "enum `{}` does not have a member named `{}`", enum_decl->name,
        unresolved->name);
}

// NOTE: copy-pasted from maybe_cast_int()
TypeError could_implicitly_cast_int(Type *wanted, Type *type, Ast *expr, ExprConstness constness)
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
    if (expr_is_fully_constant(constness) && get_int_literal(expr) > max_for_type(wanted)) {
        return TypeError::SizeMismatch;
    }

    if (type->is_unsigned() != wanted->is_unsigned()) {
        // The constness check makes something like x: u64 = 0 possible, but not x: u64 = y
        // where y is a s64.
        if (!expr_is_fully_constant(constness)) {
            if (wanted->is_unsigned() || wanted->size <= type->size) {
                return TypeError::SignednessMismatch;
            }
        }
    } else if (type->size != wanted->size) {
        if (!expr_is_fully_constant(constness)) {
            if (wanted->size <= type->size) {
                // Needs an explicit cast.
                return TypeError::SizeMismatch;
            }
        }
    }

    return TypeError::None;
}

void verify_enum_member(Compiler &cc, Ast *&ast)
{
    // TODO: some of this work can be done only once.

    auto *member = static_cast<AstEnumMember *>(ast);
    if (!member->enum_decl) {
        member->enum_decl = find_enum(member->scope, member->enum_name);
    }
    auto *enum_decl = member->enum_decl;

    assert(enum_decl->enum_type);
    // `t` needs to be a reference so we do the following work only once.
    auto *&t = enum_decl->enum_type->real;   // This is our underlying.
    resolve_type(cc, enum_decl->scope, t);   // Maybe it is unresolved.
    auto *unaliased = get_unaliased_type(t); // Maybe it is also an alias.

    auto *&expr = member->expr;
    auto *member_decl = get_enum_member(cc, enum_decl, member);
    if (!member_decl->expr) {
        if (!unaliased->is_int()) {
            verification_type_error(enum_decl->location,
                "only integer based enums can have enumerators with implicit values, but"
                " enum `{}` is based on type {}",
                enum_decl->name, to_string(t));
        }
        member_decl->expr = new AstLiteral(unaliased, enum_decl->next_int, ast->location);
    } else {
        if (unaliased->is_int()) {
            // HACK: make constant folding work by setting the enum nodes to their underlying
            // exprs... real fix is to make try_constant_fold() support enums.
            traverse_postorder(member_decl->expr, [](Ast *&node) {
                if (node->operation == Operation::EnumMember) {
                    auto *tmp = static_cast<AstEnumMember *>(node)->expr; // decl
                    node = static_cast<AstEnumMember *>(tmp)->expr;       // decl's expr
                }
            });
            member_decl->expr
                = try_constant_fold(cc, member_decl->expr, unaliased, TypeOverridable::No);
        }
    }
    expr = member_decl->expr;

    ExprConstness constness{};
    auto *expr_type = get_expression_type(cc, member->expr, &constness);
    if (!expr_is_fully_constant(constness)) {
        verification_error(member->expr, "enum initializer needs to be fully constant");
    }

    if (auto err = types_match(expr_type, t); err != TypeError::None) {
        // If it's not the underlying type, we might be assigning from another member.
        err = types_match(expr_type, enum_decl->enum_type);
        if (err != TypeError::None && unaliased->is_int()) {
            // Underlying might be of a different int type than our expr.
            // Figure it out here.
            err = could_implicitly_cast_int(unaliased, expr_type, expr, constness);
        }
        if (err != TypeError::None) {
            verification_type_error(member->expr->location,
                "enum initializer resolves to type {} instead of underlying type {}",
                to_string(expr_type), to_string(t));
        }
    }
    if (unaliased->is_int()) {
        assert(expr_is_const_integral(expr, IgnoreCasts::Yes));
        enum_decl->next_int = get_int_literal(expr) + 1;
    }

    // Force override the type to what the user specified.
    // We know this is ok because the earlier checks would have failed otherwise.
    expr->expr_type = unaliased;
}

void verify_expr(Compiler &cc, Ast *&ast, WarnDiscardedReturn warn_discarded, Type *expected)
{
    Type *type;
    switch (ast->type) {
        case AstType::Integer:
        case AstType::Boolean:
        case AstType::String:
            [[fallthrough]];
        case AstType::Identifier:
            break;
        case AstType::Enum:
            verify_enum_member(cc, ast);
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
        var.type = get_expression_type(cc, var_decl->init_expr, &constness);
        if (var_decl->init_expr->operation == Operation::LogicalOr
            || var_decl->init_expr->operation == Operation::LogicalAnd) {
            verify_logical_chain(cc, var_decl->init_expr);
        }
        verify_expr(cc, var_decl->init_expr, WarnDiscardedReturn::No, var.type);
    } else if (var_decl->init_expr) {
        // x: T = y
        resolve_type(cc, var_decl->scope, var.type);
        if (get_unaliased_type(var.type)->is_bool()) {
            convert_expr_to_boolean(cc, var_decl->init_expr);
        }
        if (var_decl->init_expr->operation == Operation::LogicalOr
            || var_decl->init_expr->operation == Operation::LogicalAnd) {
            verify_logical_chain(cc, var_decl->init_expr);
        }
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
    auto *expr = return_stmt->expr;

    if (current_function->returns_void()) {
        if (expr) {
            if (expr->operation == Operation::Call) {
                auto *call = static_cast<AstCall *>(expr);
                verify_call(cc, expr, WarnDiscardedReturn::No);
                if (get_callee(cc, call)->returns_void()) {
                    // Returning f() (where f returns void) is allowed
                    return;
                }
            }
            ExprConstness constness{};
            verification_error(expr, "void function `{}` must not return a value (got type `{}`)",
                current_function->name, get_expression_type(cc, expr, &constness)->get_name());
        }
        return;
    }

    if (!expr) {
        verification_error(return_stmt, "function `{}` must return value of type `{}`",
            current_function->name, current_function->return_type->get_name());
    }

    ExprConstness constness{};
    get_expression_type(cc, expr, &constness);
    verify_expr(cc, expr, WarnDiscardedReturn::No, expected);
}

void verify_if(Compiler &cc, AstIf *if_stmt, AstFunction *current_function)
{
    if (if_stmt->expr->operation == Operation::VariableDecl) {
        verify_var_decl(cc, static_cast<AstVariableDecl *>(if_stmt->expr));
    } else {
        // If verify_expr was called immediately, "x + 123" would be converted to "(x != 0) + 123".
        // The conversion has to happen on the uppermost level instead: "(x + 123) != 0".
        auto *&ast = if_stmt->expr;
        convert_expr_to_boolean(cc, ast);
        verify_expr(cc, ast, WarnDiscardedReturn::No, bool_type());
        if (ast->operation == Operation::LogicalOr || ast->operation == Operation::LogicalAnd) {
            verify_logical_chain(cc, ast);
        }
    }
    verify_ast(cc, if_stmt->body, current_function);
    if (if_stmt->else_body) {
        verify_ast(cc, if_stmt->else_body, current_function);
    }
}

void verify_for_in(Compiler &cc, Ast *ast, AstFunction *current_function)
{
    auto *for_stmt = static_cast<AstFor *>(ast);
    auto *var = &for_stmt->var_decl->var;

    verify_expr(cc, for_stmt->cmp, WarnDiscardedReturn::No, bool_type());
    if (for_stmt->end) {
        verify_expr(cc, for_stmt->end, WarnDiscardedReturn::No, var->type);
    }

    auto *start = for_stmt->var_decl->init_expr;
    auto *end = static_cast<AstBinary *>(for_stmt->cmp)->right;
    if (expr_is_const_integral(start, IgnoreCasts::Yes)
        && expr_is_const_integral(end, IgnoreCasts::Yes)) {
        auto eq = get_int_literal(start) <=> get_int_literal(end);
        if (eq == std::strong_ordering::greater
            || (eq == std::strong_ordering::equal && for_stmt->cmp->operation == Operation::Less)) {
            verification_error(for_stmt->cmp, "invalid range");
        }
    }

    verify_expr(cc, for_stmt->change, WarnDiscardedReturn::No);

    verify_ast(cc, for_stmt->body, current_function);
}

void verify_for(Compiler &cc, Ast *ast, AstFunction *current_function)
{
    auto *for_stmt = static_cast<AstFor *>(ast);

    if (for_stmt->var_decl) {
        verify_var_decl(cc, for_stmt->var_decl);
        auto *expected = get_unaliased_type(for_stmt->var_decl->var.type);
        if (!expected->is_int()) {
            verification_type_error(
                for_stmt->var_decl->location, "range variable must be of an integer-like type");
        }
    }

    if (for_stmt->end) {
        // This is a for-in loop.
        verify_for_in(cc, ast, current_function);
        return;
    }

    if (for_stmt->cmp) {
        verify_expr(cc, for_stmt->cmp, WarnDiscardedReturn::No, bool_type());
    }

    if (for_stmt->change) {
        verify_expr(cc, for_stmt->change, WarnDiscardedReturn::No);
    }

    verify_ast(cc, for_stmt->body, current_function);
}

void verify_while(Compiler &cc, Ast *ast, AstFunction *current_function)
{
    auto *while_stmt = static_cast<AstWhile *>(ast);
    // See verify_if for explanation
    convert_expr_to_boolean(cc, while_stmt->cmp);
    if (while_stmt->cmp->operation == Operation::LogicalOr
        || while_stmt->cmp->operation == Operation::LogicalAnd) {
        verify_logical_chain(cc, while_stmt->cmp);
    }
    verify_expr(cc, while_stmt->cmp, WarnDiscardedReturn::No, bool_type());
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
    ExprConstness constness{};
    auto *expected = get_expression_type(cc, binary->left, &constness);
    if (get_unaliased_type(expected)->is_bool()) {
        convert_expr_to_boolean(cc, binary->right);
    }
    if (binary->right->operation == Operation::LogicalOr
        || binary->right->operation == Operation::LogicalAnd) {
        verify_logical_chain(cc, binary->right);
    }
    verify_expr(cc, binary->left, WarnDiscardedReturn::No);
    verify_expr(cc, binary->right, WarnDiscardedReturn::No, expected);
    if (exprs_identical(binary->left, binary->right, CheckSideEffects::Yes)) {
        diag::ast_warning(cc, binary, "self-assignment has no effect");
    }
}

void verify_function_decl(Compiler &cc, Ast *ast)
{
    auto *fn = static_cast<AstFunction *>(ast);
    resolve_type(cc, fn->scope, fn->return_type);
    for (auto &param : fn->params) {
        resolve_type(cc, fn->scope, param->var.type);
    }
    auto *last = cc.current_function;
    if (last && last->scope) {
        last->flags |= AstFlags::HAS_NESTED_FNS;
    }
    cc.current_function = fn;
    verify_ast(cc, fn->body, fn);
    cc.current_function = last;
}

void verify_enum_decl(Compiler &cc, Ast *ast)
{
    auto *enum_decl = static_cast<AstEnumDecl *>(ast);

    resolve_type(cc, enum_decl->scope, enum_decl->enum_type->real);
    auto *real = get_unaliased_type(enum_decl->enum_type->real);

    if (!real->is_int() && !real->is_string()) {
        verification_type_error(real->location,
            "cannot use type `{}` as the underlying type for enum `{}`.\n"
            "the underlying type must be an integer or string type.",
            real->get_name(), enum_decl->name);
    }

    enum_decl->enum_type->size = real->size;
    // NOTE: Technically this would be correct but we don't do it because it would make us be
    // treated as a literal in some places.
    // enum_decl->enum_type->flags |= (real->flags & TypeFlags::kind_mask);

    for (auto *&member : enum_decl->members->vector) {
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
            case Operation::For:
                verify_for(cc, ast, current_function);
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
