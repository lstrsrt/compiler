#include "optimizer.hh"
#include "diagnose.hh"
#include "parser.hh"
#include "verify.hh"

#include <algorithm>
#include <limits>

void flatten_binary(Ast *ast, Operation operation, std::vector<Ast *> &flattened)
{
    if (ast->type == AstType::Binary && ast->operation == operation) {
        flatten_binary(static_cast<AstBinary *>(ast)->left, operation, flattened);
        flatten_binary(static_cast<AstBinary *>(ast)->right, operation, flattened);
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
                accumulator += get_int_literal(ast);
            } else if (operation == Operation::Multiply) {
                accumulator *= get_int_literal(ast);
            }
            delete static_cast<AstLiteral *>(ast);
        } else {
            non_constants.push_back(ast);
        }
    }

    if (accumulator != 0 || non_constants.empty()) {
        non_constants.push_back(new AstLiteral(expected, accumulator, operands[0]->location));
    }

    auto *ret = non_constants[0];
    for (size_t i = 1; i < non_constants.size(); ++i) {
        ret = new AstBinary(operation, ret, non_constants[i], ret->location);
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
            static_cast<AstLiteral *>(constant_ast)->expr_type = expected;
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

Ast *try_fold_logical_chain(
    AstBinary *binary, const std::vector<Ast *> &operands, Operation operation)
{
    std::vector<Ast *> chain;
    std::optional<uint64_t> constant{};

    for (size_t i = 0; i < operands.size(); ++i) {
        auto *ast = operands[i];
        if (ast->type == AstType::Integer || ast->type == AstType::Boolean) {
            constant.emplace(get_int_literal(ast));
            // The break condition is "not 0" for LogicalOr and 0 for LogicalAnd.
            if ((constant == 0) ^ (operation == Operation::LogicalOr)) {
                if (i != 0) {
                    // If this is not the first operand, override the previous expression result by
                    // inserting the constant.
                    chain.push_back(ast);
                }
                // Ignore the rest
                break;
            }
        } else {
            chain.push_back(ast);
        }
    }

    if (chain.empty()) {
        // This happens if we broke on the first operand
        return new AstLiteral(*constant != 0, binary->location);
    }

    if (chain.size() == operands.size() && !constant.has_value()) {
        // No rebuild necessary
        return binary;
    }

    auto *ret = chain[0];
    for (size_t i = 1; i < chain.size(); ++i) {
        ret = new AstBinary(operation, ret, chain[i], ret->location);
    }
    return ret;
}

void get_binary_operations(Ast *ast, Operation op, std::vector<Ast *> &asts)
{
    if (ast->operation == op) {
        asts.push_back(ast);
        get_binary_operations(static_cast<AstBinary *>(ast)->left, op, asts);
        get_binary_operations(static_cast<AstBinary *>(ast)->right, op, asts);
    }
}

bool is_equals_x(Ast *ast, bool value)
{
    uint64_t cmp1 = value ? 1 : 0;
    uint64_t cmp2 = value ? 0 : 1;
    // Return true for "Equals x, 1" and "NotEquals x, 0"
    if (ast->operation == Operation::Equals || ast->operation == Operation::NotEquals) {
        auto *cmp = static_cast<AstBinary *>(ast)->right;
        return (cmp->type == AstType::Boolean || cmp->type == AstType::Integer)
            && get_int_literal(cmp) == (ast->operation == Operation::Equals ? cmp1 : cmp2);
    }
    return false;
}

bool is_logical(Operation operation)
{
    return operation == Operation::LogicalAnd || operation == Operation::LogicalOr
        || (operation >= Operation::Equals && operation <= Operation::GreaterEquals);
}

void invert_logical(Ast *ast)
{
    using enum Operation;
    static const std::unordered_map<Operation, Operation> opposites{
        { Equals, NotEquals },
        { NotEquals, Equals },
        { Greater, LessEquals },
        { LessEquals, Greater },
        { GreaterEquals, Less },
        { Less, GreaterEquals },
    };
    auto it = opposites.find(ast->operation);
    assert(it != opposites.end());
    ast->operation = it->second;
}

Ast *apply_de_morgan_laws(Ast *ast)
{
    auto *outer = static_cast<AstBinary *>(ast);
    auto *operand = outer->left;
    if (!is_logical(operand->operation)) {
        return ast;
    }

    if (operand->operation == Operation::LogicalAnd || operand->operation == Operation::LogicalOr) {
        // Multiple operands.
        bool is_and = operand->operation == Operation::LogicalAnd;
        std::vector<Ast *> operands;
        flatten_binary(operand, operand->operation, operands);
        // If another inner operand is of a different chain type than this one, give up.
        if (std::ranges::any_of(operands, [is_and](Ast *a) {
                return a->operation == (is_and ? Operation::LogicalOr : Operation::LogicalAnd);
            })) {
            return ast;
        }
        std::vector<Ast *> operations;
        get_binary_operations(operand, operand->operation, operations);
        if (is_equals_x(ast, false)) {
            for (auto *op : operands) {
                invert_logical(op);
            }
            for (auto *op : operations) {
                if (is_and) {
                    op->operation = Operation::LogicalOr;
                } else {
                    op->operation = Operation::LogicalAnd;
                }
            }
            delete ast;
            return operand;
        }
        if (is_equals_x(ast, true)) {
            delete ast;
            return operand;
        }
        return ast;
    }

    // Single operand.
    if (is_equals_x(ast, false)) {
        // Transform !(x != 0) i.e. (x != 0) == 0 into just x == 0.
        invert_logical(operand);
        delete ast;
        return operand;
    }
    if (is_equals_x(ast, true)) {
        // Transform (x != 0) == true i.e. (x != 0) == 1 into just x != 0.
        delete ast;
        return operand;
    }
    return ast;
}

Ast *try_fold_logical_chain(Compiler &, AstBinary *binary)
{
    assert(binary->operation == Operation::LogicalAnd || binary->operation == Operation::LogicalOr);
    std::vector<Ast *> flattened;
    flatten_binary(binary, binary->operation, flattened);
    auto *res = try_fold_logical_chain(binary, flattened, binary->operation);
    res->flags |= AstFlags::FOLDED;
    return res;
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

#define fold_and_warn_overflow(left_const, right_const, result, expected, fn)                     \
    {                                                                                             \
        bool overflows_u64 = fn(left_const, right_const, &result);                                \
        bool overflows_type = get_unaliased_type(expected) != bool_type()                         \
            && result > max_for_type(get_unaliased_type(expected));                               \
        bool warn = false;                                                                        \
        if (overflows_u64 || overflows_type) {                                                    \
            if (overridable == TypeOverridable::Yes) {                                            \
                if (overflows_u64) {                                                              \
                    warn = true;                                                                  \
                    expected = u64_type();                                                        \
                } else if (overflows_type) {                                                      \
                    expected = get_integer_type(result);                                          \
                }                                                                                 \
            }                                                                                     \
            if (overridable == TypeOverridable::No || warn) {                                     \
                diag::warning_at(cc, binary->location, "constant expression overflows type `{}`", \
                    expected->name);                                                              \
            }                                                                                     \
        }                                                                                         \
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
                diag::warning_at(cc, binary->location, "constant expression overflows type `{}`",
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
            diag::warning_at(
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
        case Operation::Equals:
            result = left_const == right_const;
            break;
        case Operation::NotEquals:
            result = left_const != right_const;
            break;
        case Operation::Less:
            result = left_const < right_const;
            break;
        case Operation::LessEquals:
            result = left_const <= right_const;
            break;
        case Operation::Greater:
            result = left_const > right_const;
            break;
        case Operation::GreaterEquals:
            result = left_const >= right_const;
            break;
        case Operation::LogicalAnd:
            result = left_const && right_const;
            break;
        case Operation::LogicalOr:
            result = left_const || right_const;
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
        left_const = get_int_literal(left);
    }
    if (right_is_const) {
        right_const = get_int_literal(right);
    }

    if (binary->operation == Operation::Divide || binary->operation == Operation::Modulo) {
        if (right_is_const && right_const == 0) {
            diag::ast_warning(cc, binary, "trying to divide by 0");
            return binary;
        }
    }

    if (left_is_const && right_is_const) {
        // Easy case: if both sides are constants, fold unless the operation is illegal.
        return try_fold_constants(cc, binary, left_const, right_const, expected, overridable);
    }

    if (binary->operation == Operation::Multiply || binary->operation == Operation::Add) {
        // Only one side is a constant, and the operation is associative.
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
