#include "compiler.hh"

#include <limits>

void flatten_binary(Ast *ast, Operation matching_operation, std::vector<Ast *> &flattened)
{
    if (ast->type == AstType::Binary && ast->operation == matching_operation) {
        flatten_binary(static_cast<AstBinary *>(ast)->left, matching_operation, flattened);
        flatten_binary(static_cast<AstBinary *>(ast)->right, matching_operation, flattened);
    } else {
        flattened.push_back(ast);
    }
}

Ast *partial_fold_associative(
    const std::vector<Ast *> &operands, Operation operation, Type *expected)
{
    std::vector<Ast *> non_constants;
    uint64_t accumulator = operation == Operation::Multiply ? 1 : 0;

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

    if (accumulator != 0 || non_constants.empty()) {
        non_constants.push_back(new AstLiteral(expected, accumulator, {}));
    }

    auto *ret = non_constants[0];
    for (size_t i = 1; i < non_constants.size(); ++i) {
        ret = new AstBinary(operation, ret, non_constants[i], {});
    }
    return ret;
}

Ast *try_fold_identities(
    AstBinary *binary, Ast *constant_ast, Ast *variable_ast, uint64_t constant, Type *expected)
{
    if (binary->operation == Operation::Add) {
        if (constant == 0) {
            // x+0=x and 0+x=x
            delete static_cast<AstLiteral *>(constant_ast);
            delete static_cast<AstBinary *>(binary);
            // TODO: delete var->type if not builtin
            static_cast<AstIdentifier *>(variable_ast)->var->type = expected;
            return variable_ast;
        }
    } else /* multiply */ {
        if (constant == 0) {
            // x*0=0 and 0*x=0
            delete static_cast<AstIdentifier *>(variable_ast);
            delete static_cast<AstBinary *>(binary);
            static_cast<AstLiteral *>(constant_ast)->literal_type = expected;
            return constant_ast;
        }
        if (constant == 1) {
            // x*1=x and 1*x=x
            delete static_cast<AstLiteral *>(constant_ast);
            delete static_cast<AstBinary *>(binary);
            // TODO: delete var->type if not builtin
            static_cast<AstIdentifier *>(variable_ast)->var->type = expected;
            return variable_ast;
        }
    }
    return nullptr;
}

Ast *try_partial_fold_associative(
    AstBinary *binary, Ast *constant_ast, Ast *variable_ast, uint64_t constant, Type *expected)
{
    if (auto *id = try_fold_identities(binary, constant_ast, variable_ast, constant, expected)) {
        return id;
    }
    if (binary->left->type != AstType::Binary && binary->right->type != AstType::Binary) {
        // If this is a simple expression with no chance of further constants and
        // try_fold_identities also failed, give up here.
        return binary;
    }
    std::vector<Ast *> flattened;
    flatten_binary(binary, binary->operation, flattened);
    return partial_fold_associative(flattened, binary->operation, expected);
}

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

Type *get_integer_type(uint64_t x)
{
    if (x > max_for_type(s64_type())) {
        return u64_type();
    }
    if (x > max_for_type(u32_type())) {
        return s64_type();
    }
    if (x > max_for_type(s32_type())) {
        return u32_type();
    }
    return s32_type();
}

#define fold_and_warn_overflow(left_const, right_const, result, expected, fn)                    \
    {                                                                                            \
        bool overflows_u64 = fn(left_const, right_const, &result);                               \
        bool overflows_type = result > max_for_type(get_unaliased_type(expected));               \
        bool warn = false;                                                                       \
        if (overflows_u64 || overflows_type) {                                                   \
            if (overridable == TypeOverridable::Yes) {                                           \
                if (overflows_u64) {                                                             \
                    warn = true;                                                                 \
                    expected = u64_type();                                                       \
                } else if (overflows_type) {                                                     \
                    expected = get_integer_type(result);                                         \
                }                                                                                \
            }                                                                                    \
            if (overridable == TypeOverridable::No || warn) {                                    \
                diag_warning_at(cc, binary->location, "constant expression overflows type `{}`", \
                    expected->name);                                                             \
            }                                                                                    \
        }                                                                                        \
    }

uint64_t fold_sub_and_warn_overflow(Compiler &cc, AstBinary *binary, uint64_t left_const,
    uint64_t right_const, Type *&expected, TypeOverridable overridable)
{
    if (expected->has_flag(TypeFlags::UNSIGNED)) {
        // `0 - 0xffff_ffff` resolves to `1` (of type unsigned int) in C/C++. In this
        // language, we either warn or cast up to s64.
        if (left_const < right_const) {
            if (expected->size < 8 && overridable == TypeOverridable::Yes) {
                expected = s64_type();
            } else {
                diag_warning_at(cc, binary->location, "constant expression overflows type `{}`",
                    expected->name);
            }
        }
        return left_const - right_const;
    }

    int64_t i64;
    bool overflows_i64 = __builtin_sub_overflow(
        static_cast<int64_t>(left_const), static_cast<int64_t>(right_const), &i64);
    if ((expected->size < 8 && i64 < std::numeric_limits<int32_t>::min()) || overflows_i64) {
        if (overridable == TypeOverridable::Yes) {
            expected = s64_type();
        }
        if (overridable == TypeOverridable::No || overflows_i64) {
            diag_warning_at(
                cc, binary->location, "constant expression overflows type `{}`", expected->name);
        }
    }
    return static_cast<uint64_t>(i64);
}

Ast *try_fold_constants(Compiler &cc, AstBinary *binary, uint64_t left_const, uint64_t right_const,
    Type *&expected, TypeOverridable overridable)
{
    uint64_t result;
    switch (binary->operation) {
        case Operation::Add:
            fold_and_warn_overflow(
                left_const, right_const, result, expected, __builtin_add_overflow);
            break;
        case Operation::Subtract: {
            result = fold_sub_and_warn_overflow(
                cc, binary, left_const, right_const, expected, overridable);
            break;
        }
        case Operation::Multiply:
            fold_and_warn_overflow(
                left_const, right_const, result, expected, __builtin_mul_overflow);
            break;
        case Operation::Divide:
            result = left_const / right_const;
            break;
        case Operation::Modulo:
            result = left_const % right_const;
            break;
        default:
            return binary; // Do nothing
    }

    if (get_unaliased_type(expected) == bool_type()) {
        result = std::clamp<uint64_t>(result, 0, 1);
    }

    auto loc = binary->location;
    // TODO: fix double free (only happens when identity folding + constant folding)
    delete static_cast<AstLiteral *>(binary->left);
    delete static_cast<AstLiteral *>(binary->right);
    delete static_cast<AstBinary *>(binary);
    return new AstLiteral(expected, result, loc);
}

Ast *try_fold_binary(Compiler &cc, AstBinary *binary, Type *&expected, TypeOverridable overridable)
{
    // Figure out which parts of the expression are constant.
    auto *left = try_constant_fold(cc, binary->left, expected, overridable);
    auto *right = try_constant_fold(cc, binary->right, expected, overridable);
    bool left_is_const = left->type == AstType::Integer || left->type == AstType::Boolean;
    bool right_is_const = right->type == AstType::Integer || right->type == AstType::Boolean;
    if (!left_is_const && !right_is_const) {
        return binary;
    }

    uint64_t left_const, right_const;
    if (left_is_const) {
        left_const = static_cast<AstLiteral *>(left)->u.u64;
    }
    if (right_is_const) {
        right_const = static_cast<AstLiteral *>(right)->u.u64;
    }

    if (binary->operation == Operation::Divide || binary->operation == Operation::Modulo) {
        if (right_is_const && right_const == 0) {
            diag_ast_warning(cc, binary, "trying to divide by 0");
            return binary;
        }
    }

    if (left_is_const && right_is_const) {
        // Easy case: if both sides are constants, fold unless the operation is illegal.
        return try_fold_constants(cc, binary, left_const, right_const, expected, overridable);
    }

    // Only one side is a constant, and the operation is associative.
    if (binary->operation == Operation::Multiply || binary->operation == Operation::Add) {
        auto *constant_ast = left_is_const ? left : right;
        auto *variable_ast = left_is_const ? right : left;
        auto constant = left_is_const ? left_const : right_const;
        return try_partial_fold_associative(binary, constant_ast, variable_ast, constant, expected);
    }

    // TODO: non-associative folding

    return binary;
}

Ast *try_constant_fold(Compiler &cc, Ast *ast, Type *&expected, TypeOverridable overridable)
{
    if (!ast) {
        return nullptr;
    }
    if (ast->type == AstType::Binary) {
        if (!(ast->flags & AstFlags::FOLDED)) {
            ast->flags |= AstFlags::FOLDED;
            return try_fold_binary(cc, static_cast<AstBinary *>(ast), expected, overridable);
        }
    }
    if (ast->type == AstType::Unary) {
        if (ast->operation == Operation::Cast) {
            return static_cast<AstCast *>(ast)->expr;
        }
    }
    return ast;
}
