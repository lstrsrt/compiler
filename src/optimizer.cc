#include "optimizer.hh"
#include "diagnose.hh"
#include "parser.hh"
#include "verify.hh"

#include <algorithm>
#include <bit>
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
    AstBinary *binary, const std::vector<Ast *> &operands, Operation operation, Type *expected)
{
    std::vector<Ast *> non_constants;
    std::vector<Ast *> constants;
    uint64_t accumulator = operation == Operation::Multiply ? 1 : 0;

    for (auto *ast : operands) {
        if (ast->type == AstType::Integer) {
            if (operation == Operation::Add) {
                accumulator += get_int_literal(ast);
                constants.push_back(ast);
            } else if (operation == Operation::Multiply) {
                accumulator *= get_int_literal(ast);
                constants.push_back(ast);
            }
        } else {
            non_constants.push_back(ast);
        }
    }

    if (constants.size() < 2) {
        return binary;
    }

    for (auto *constant : constants) {
        delete static_cast<AstLiteral *>(constant);
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
    return partial_fold_associative(binary, flattened, binary->operation, expected);
}

Ast *try_fold_logical_chain(
    AstBinary *binary, const std::vector<Ast *> &operands, Operation operation)
{
    std::vector<Ast *> chain;
    std::optional<uint64_t> constant{};

    for (size_t i = 0; i < operands.size(); ++i) {
        auto *ast = operands[i];
        if (ast->type == AstType::Integer) {
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
        return cmp->type == AstType::Integer
            && get_int_literal(cmp) == (ast->operation == Operation::Equals ? cmp1 : cmp2);
    }
    return false;
}

bool is_logical(Operation operation)
{
    switch (operation) {
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
    if (ast->type != AstType::Binary) {
        return ast;
    }

    auto *outer = static_cast<AstBinary *>(ast);
    auto *operand = outer->left;
    if (!is_logical(operand->operation)) {
        return ast;
    }

    // TODO: only do this if operands don't have side effects

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

Type *get_fitting_int_type(uint64_t x)
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

uint64_t truncate(Type *type, uint64_t v)
{
    bool is_unsigned = type->has_flag(TypeFlags::UNSIGNED);
    switch (type->size) {
        case 1:
            return static_cast<uint64_t>(static_cast<bool>(v));
        case 8:
            return static_cast<uint64_t>(
                is_unsigned ? static_cast<uint8_t>(v) : static_cast<int8_t>(v));
        case 16:
            return static_cast<uint64_t>(
                is_unsigned ? static_cast<uint16_t>(v) : static_cast<int16_t>(v));
        case 32:
            return static_cast<uint64_t>(
                is_unsigned ? static_cast<uint32_t>(v) : static_cast<int32_t>(v));
        case 64:
            return is_unsigned ? v : static_cast<int64_t>(v);
    }
    TODO();
}

void handle_overflow(
    Compiler &cc, AstBinary *binary, Integer result, Type *&expected, TypeOverridable overridable)
{
    if (overridable == TypeOverridable::Yes) {
        expected = get_fitting_int_type(result);
    } else {
        result.value = truncate(expected, result.value);
        diag::ast_warning(cc, binary, "constant expression overflows type `{}`", expected->name);
    }
}

#define fold_and_warn_overflow(left_const, right_const, result, expected, fn) \
    {                                                                         \
        bool overflows_u64 = fn(left_const, right_const, &result);            \
        bool overflows_type = get_unaliased_type(expected) != bool_type()     \
            && result > max_for_type(get_unaliased_type(expected));           \
        bool warn = false;                                                    \
        if (overflows_u64 || overflows_type) {                                \
            handle_overflow(cc, ast, result, expected, overridable)           \
        }                                                                     \
    }

enum class Overflow {
    No,
    SignBitOnly,
    Yes,
};

Overflow check_overflow(Integer left_const, Integer right_const, Integer result, Type *&expected)
{
    if (expected->is_unsigned()) {
        if (result.value < right_const || result.value > max_for_type(expected)) {
            return Overflow::Yes;
        }
    } else {
        result.is_signed = true;
        if ((left_const.is_nonnegative() == right_const.is_nonnegative()
                && result.is_nonnegative() != left_const.is_nonnegative())
            || result.as_signed() > static_cast<int64_t>(max_for_type(expected))) {
            return Overflow::Yes;
        }
    }
    return Overflow::No;
}

uint64_t fold_sub_and_warn_overflow(Compiler &cc, AstBinary *binary, Integer left_const,
    Integer right_const, Type *&expected, TypeOverridable overridable)
{
    if (expected->has_flag(TypeFlags::UNSIGNED)) {
        // `0 - 0xffff_ffff` resolves to `1` (of type unsigned int) in C/C++. In this
        // language, we either warn or cast up to s64.
        if (left_const < right_const) {
            if (expected->size < 8 && overridable == TypeOverridable::Yes) {
                expected = s64_type();
            } else {
                handle_overflow(
                    cc, binary, Integer(left_const - right_const, false), expected, overridable);
            }
        }
        return left_const - right_const;
    }

    int64_t i64;
    bool overflows_i64 = __builtin_sub_overflow(
        static_cast<int64_t>(left_const), static_cast<int64_t>(right_const), &i64);
    if ((expected->size < 8 && i64 < std::numeric_limits<int32_t>::min()) || overflows_i64) {
        handle_overflow(
            cc, binary, Integer(static_cast<uint64_t>(i64), true), expected, overridable);
    }
    return static_cast<uint64_t>(i64);
}

template<std::integral T>
Overflow check_shift_overflow(T left, int right)
{
    using UnsignedT = std::make_unsigned_t<T>;
    constexpr int bit_width = sizeof(T) * 8;

    if (left == 0) {
        return Overflow::No;
    }
    if (left < 0 || right >= bit_width) {
        return Overflow::Yes;
    }

    auto leading_zeroes = std::countl_zero(static_cast<UnsignedT>(left));
    if (right == leading_zeroes) {
        return Overflow::SignBitOnly;
    }
    if (right > leading_zeroes) {
        return Overflow::Yes;
    }
    return Overflow::No;
}

void diagnose_shift_overflow(Compiler &cc, AstBinary *binary, Integer left_const,
    Integer right_const, Integer result, Type *&expected, TypeOverridable overridable)
{
    bool is_signed = !expected->has_flag(TypeFlags::UNSIGNED);
    const char *shift_type = binary->operation == Operation::LeftShift ? "left" : "right";
    auto overflow = [&, func = __func__]() {
        switch (expected->size) {
            case 8:
                return check_shift_overflow(
                    static_cast<int8_t>(left_const), static_cast<int>(right_const));
            case 16:
                return check_shift_overflow(
                    static_cast<int16_t>(left_const), static_cast<int>(right_const));
            case 32:
                return check_shift_overflow(
                    static_cast<int32_t>(left_const), static_cast<int>(right_const));
            case 64:
                return check_shift_overflow(
                    static_cast<int64_t>(left_const), static_cast<int>(right_const));
            default:
                todo(func, __FILE__, __LINE__);
        }
    }();
    if (is_signed && overflow == Overflow::SignBitOnly) {
        // NOTE: This is special cased because "x := 1 << 31" should == INT32_MIN instead of
        // casting to u32.
        diag::ast_warning(cc, binary,
            "{} shift on signed type `{}` changes its sign bit to negative", shift_type,
            expected->get_name());
        return;
    }

    bool overflows_u64 = right_const >= u64_type()->size;
    if (overflow != Overflow::Yes && !overflows_u64) {
        return;
    }

    handle_overflow(cc, binary, result, expected, overridable);
}

Ast *try_fold_constants(Compiler &cc, AstBinary *binary, Integer left_const, Integer right_const,
    Type *&expected, TypeOverridable overridable)
{
    Integer result{};
    switch (binary->operation) {
        case Operation::Add: {
            result.value = left_const + right_const;
            if (check_overflow(left_const, right_const, result, expected) == Overflow::Yes) {
                handle_overflow(cc, binary, result, expected, overridable);
            }
            break;
        }
        case Operation::Subtract: {
            result.value = fold_sub_and_warn_overflow(
                cc, binary, left_const, right_const, expected, overridable);
            break;
        }
        case Operation::Multiply: {
            result.value = left_const * right_const;
            if (check_overflow(left_const, right_const, result, expected) == Overflow::Yes) {
                handle_overflow(cc, binary, result, expected, overridable);
            }
            break;
        }
        case Operation::Divide:
            result.value = left_const / right_const;
            break;
        case Operation::Modulo:
            result.value = left_const % right_const;
            break;
        case Operation::Equals:
            result.value = left_const == right_const;
            break;
        case Operation::NotEquals:
            result.value = left_const != right_const;
            break;
        case Operation::Less:
            result.value = left_const < right_const;
            break;
        case Operation::LessEquals:
            result.value = left_const <= right_const;
            break;
        case Operation::Greater:
            result.value = left_const > right_const;
            break;
        case Operation::GreaterEquals:
            result.value = left_const >= right_const;
            break;
        case Operation::LogicalAnd:
            result.value = left_const && right_const;
            break;
        case Operation::LogicalOr:
            result.value = left_const || right_const;
            break;
        case Operation::And:
            result.value = left_const & right_const;
            break;
        case Operation::Or:
            result.value = left_const | right_const;
            break;
        case Operation::Xor:
            result.value = left_const ^ right_const;
            break;
        case Operation::LeftShift: {
            result.value = left_const << right_const;
            diagnose_shift_overflow(
                cc, binary, left_const, right_const, result, expected, overridable);
            break;
        }
        case Operation::RightShift:
            if (!expected->has_flag(TypeFlags::UNSIGNED)) {
                result.value
                    = static_cast<int64_t>(left_const) >> static_cast<int64_t>(right_const);
            } else {
                result.value = left_const >> right_const;
            }
            diagnose_shift_overflow(
                cc, binary, left_const, right_const, result, expected, overridable);
            break;
        case Operation::LeftRotate:
            switch (expected->size) {
                case 8:
                    result.value = std::rotl(
                        static_cast<uint8_t>(left_const), static_cast<int>(right_const));
                    break;
                case 16:
                    result.value = std::rotl(
                        static_cast<uint16_t>(left_const), static_cast<int>(right_const));
                    break;
                case 32:
                    result.value = std::rotl(
                        static_cast<uint32_t>(left_const), static_cast<int>(right_const));
                    break;
                case 64:
                    result.value = std::rotl(left_const.value, static_cast<int>(right_const));
                    break;
                default:
                    TODO();
            }
            break;
        case Operation::RightRotate:
            switch (expected->size) {
                case 8:
                    result.value = std::rotr(
                        static_cast<uint8_t>(left_const), static_cast<int>(right_const));
                    break;
                case 16:
                    result.value = std::rotr(
                        static_cast<uint16_t>(left_const), static_cast<int>(right_const));
                    break;
                case 32:
                    result.value = std::rotr(
                        static_cast<uint32_t>(left_const), static_cast<int>(right_const));
                    break;
                case 64:
                    result.value = std::rotr(left_const.value, static_cast<int>(right_const));
                    break;
                default:
                    TODO();
            }
            break;
        default:
            return binary; // Do nothing
    }

    if (get_unaliased_type(expected) == bool_type()) {
        result.value = std::clamp<uint64_t>(result, 0, 1);
    }

    auto loc = binary->location;
    // TODO: fix double free (only happens when identity folding + constant folding)
    free_ast(binary);
    return new AstLiteral(expected, result, loc);
}

Ast *try_fold_binary(Compiler &cc, AstBinary *binary, Type *&expected, TypeOverridable overridable)
{
    // Figure out which parts of the expression are constant.
    auto *left = try_constant_fold(cc, binary->left, expected, overridable);
    auto *right = try_constant_fold(cc, binary->right, expected, overridable);
    bool left_is_const = left->type == AstType::Integer;
    bool right_is_const = right->type == AstType::Integer;
    if (!left_is_const && !right_is_const) {
        return binary;
    }

    Integer left_const{};
    Integer right_const{};
    if (left_is_const) {
        left_const = static_cast<AstLiteral *>(left)->u.any;
    }
    if (right_is_const) {
        right_const = static_cast<AstLiteral *>(right)->u.any;
    }

    if (binary->operation == Operation::Divide || binary->operation == Operation::Modulo) {
        if (right_is_const && right_const == 0) {
            diag::ast_warning(cc, binary->right, "trying to divide by 0");
            return binary;
        }
    }

    // TODO: also do this for rotate, maybe other bit ops
    // TODO: free nodes?
    if (binary->operation == Operation::LeftShift || binary->operation == Operation::RightShift) {
        if (right_is_const && right_const == 0) {
            diag::ast_warning(cc, binary->right, "shift by 0 has no effect");
            return binary->left;
        }
        if (left_is_const && left_const == 0) {
            diag::ast_warning(cc, binary->left, "shifting 0 has no effect");
            return binary->left;
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
    if (has_flag(ast->flags, AstFlags::FOLDED)) {
        return ast;
    }

    if (ast->type == AstType::Binary) {
        auto *ret = try_fold_binary(cc, static_cast<AstBinary *>(ast), expected, overridable);
        ret->flags |= AstFlags::FOLDED;
        return ret;
    }

    if (ast->type == AstType::Unary) {
        if (ast->operation == Operation::Cast) {
            // TODO: actually cast?
            return static_cast<AstCast *>(ast)->operand;
        }
    }
    return ast;
}
