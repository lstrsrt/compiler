#include "ast-fold.hh"
#include "diagnose.hh"
#include "parser.hh"
#include "verify.hh"

#include <algorithm>
#include <bit>

void flatten_binary(Ast *ast, Operation operation, std::vector<Ast *> &flattened,
    CheckSideEffects check_side_effects)
{
    if (ast->type == AstType::Binary && ast->operation == operation) {
        flatten_binary(
            static_cast<AstBinary *>(ast)->left, operation, flattened, check_side_effects);
        flatten_binary(
            static_cast<AstBinary *>(ast)->right, operation, flattened, check_side_effects);
    } else {
        if (check_side_effects == CheckSideEffects::No || !has_side_effects(ast)) {
            flattened.push_back(ast);
        }
    }
}

Ast *partial_fold_commutative(
    AstBinary *binary, const std::vector<Ast *> &operands, Operation operation, Type *expected)
{
    std::vector<Ast *> non_constants;
    std::vector<Ast *> constants;
    uint64_t accumulator = operation == Operation::Multiply ? 1 : 0;

    for (auto *ast : operands) {
        if (ast->type == AstType::Integer) {
            switch (operation) {
                case Operation::Add:
                    accumulator += get_int_literal(ast);
                    constants.push_back(ast);
                    break;
                case Operation::Multiply:
                    accumulator *= get_int_literal(ast);
                    constants.push_back(ast);
                    break;
                case Operation::And:
                    accumulator &= get_int_literal(ast);
                    constants.push_back(ast);
                    break;
                case Operation::Or:
                    accumulator |= get_int_literal(ast);
                    constants.push_back(ast);
                    break;
                case Operation::Xor:
                    accumulator ^= get_int_literal(ast);
                    constants.push_back(ast);
                    break;
                default:
                    break;
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

Ast *try_fold_identities(AstBinary *binary, Ast *constant_ast, Ast *variable_ast, uint64_t constant)
{
    auto maybe_return_constant_if = [&](uint64_t cmp) -> Ast * {
        if (cmp == constant && !has_side_effects(variable_ast)) {
            delete variable_ast;
            delete binary;
            return constant_ast;
        }
        return nullptr;
    };
    auto return_variable_if = [&](uint64_t cmp) -> Ast * {
        if (cmp == constant) {
            delete static_cast<AstLiteral *>(constant_ast);
            delete binary;
            return variable_ast;
        }
        return nullptr;
    };

    switch (binary->operation) {
        case Operation::And:
            return maybe_return_constant_if(0);
        case Operation::Multiply: {
            auto *ptr = maybe_return_constant_if(0);
            return ptr ? ptr : return_variable_if(1);
        }
        case Operation::Or:
        case Operation::Xor:
        case Operation::Add:
            return return_variable_if(0);
        default:
            TODO();
    }
    return nullptr;
}

Ast *try_partial_fold_commutative(
    AstBinary *binary, Ast *constant_ast, Ast *variable_ast, uint64_t constant, Type *expected)
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
    return partial_fold_commutative(binary, flattened, binary->operation, expected);
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

bool is_logical_operation(Operation);

Ast *apply_de_morgan_laws(Ast *ast)
{
    if (ast->type != AstType::Binary) {
        return ast;
    }

    auto *outer = static_cast<AstBinary *>(ast);
    auto *operand = outer->left;
    if (!is_logical_operation(operand->operation)) {
        return ast;
    }

    if (operand->operation == Operation::LogicalAnd || operand->operation == Operation::LogicalOr) {
        // Multiple operands.
        bool is_and = operand->operation == Operation::LogicalAnd;
        Operation opposite = is_and ? Operation::LogicalOr : Operation::LogicalAnd;
        std::vector<Ast *> operands;
        flatten_binary(operand, operand->operation, operands);
        // If another inner operand is of a different chain type than this one, give up.
        if (std::ranges::any_of(
                operands, [opposite](Ast *a) { return a->operation == opposite; })) {
            return ast;
        }
        std::vector<Ast *> operations;
        get_binary_operations(operand, operand->operation, operations);
        if (is_equals_x(ast, false)) {
            for (auto *op : operands) {
                invert_logical(op);
            }
            for (auto *op : operations) {
                op->operation = opposite;
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
    type = get_unaliased_type(type);
    bool is_unsigned = type->is_unsigned();
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
    Compiler &cc, AstBinary *binary, Integer &result, Type *&expected, TypeOverridable overridable)
{
    if (overridable == TypeOverridable::Yes) {
        expected = get_fitting_int_type(result.value);
    } else {
        result.value = truncate(expected, result.value);
        diag::ast_warning(cc, binary, "constant expression overflows type {}", to_string(expected));
    }
}

enum class Overflow {
    No,
    SignBitOnly,
    Yes,
};

Overflow check_overflow(Integer left_const, Integer right_const, Integer result, Type *&expected)
{
    if (expected->is_unsigned()) {
        if (result.value < right_const.value || result.value > max_for_type(expected)) {
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

uint64_t fold_sub_and_diagnose_overflow(Compiler &cc, AstBinary *binary, Integer left_const,
    Integer right_const, Type *&expected, TypeOverridable overridable)
{
    Integer result{};
    result.value = left_const.value - right_const.value;
    result.is_signed = expected->is_signed();

    if (expected->is_unsigned()) {
        // `0 - 0xffff_ffff` resolves to `1` (of type unsigned int) in C/C++. In this
        // language, we either warn or cast up to s64.
        if (left_const.value < right_const.value) {
            if (expected->size < 8 && overridable == TypeOverridable::Yes) {
                expected = s64_type();
            } else {
                handle_overflow(cc, binary, result, expected, overridable);
            }
        }
        return result.value;
    }

    int64_t i64;
    bool overflows_i64 = __builtin_sub_overflow(
        static_cast<int64_t>(left_const.value), static_cast<int64_t>(right_const.value), &i64);
    result.value = static_cast<uint64_t>(i64);
    if ((expected->size < 8 && i64 < S32Min) || overflows_i64) {
        handle_overflow(cc, binary, result, expected, overridable);
    }
    return result.value;
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
    bool is_signed = expected->is_signed();
    const char *shift_type = binary->operation == Operation::LeftShift ? "left" : "right";
    auto overflow = [&, func = __func__]() {
        switch (get_unaliased_type(expected)->size) {
            case 8:
                return check_shift_overflow(
                    static_cast<int8_t>(left_const.value), static_cast<int>(right_const.value));
            case 16:
                return check_shift_overflow(
                    static_cast<int16_t>(left_const.value), static_cast<int>(right_const.value));
            case 32:
                return check_shift_overflow(
                    static_cast<int32_t>(left_const.value), static_cast<int>(right_const.value));
            case 64:
                return check_shift_overflow(
                    static_cast<int64_t>(left_const.value), static_cast<int>(right_const.value));
            default:
                todo(func, __FILE__, __LINE__);
        }
    }();
    if (is_signed && overflow == Overflow::SignBitOnly) {
        // HACK: This condition exists so we don't warn twice (getting the expr type and then during
        // actual folding). If we were calling handle_overflow() it would override the type and the
        // warning wouldn't fire again.
        if (overridable == TypeOverridable::No) {
            // NOTE: This is special cased because "x := 1 << 31" should == INT32_MIN instead of
            // casting to u32.
            diag::ast_warning(cc, binary,
                "{} shift on signed type {} changes its sign bit to negative", shift_type,
                to_string(expected));
        }
        return;
    }

    bool overflows_u64 = right_const.value >= u64_type()->size;
    if (overflow != Overflow::Yes && !overflows_u64) {
        return;
    }

    handle_overflow(cc, binary, result, expected, overridable);
}

Ast *try_fold_constants(Compiler &cc, AstBinary *binary, Integer left_const, Integer right_const,
    Type *&expected, TypeOverridable overridable)
{
    Integer result{};

    if (is_logical_operation(binary->operation)) {
        if (overridable == TypeOverridable::Yes) {
            expected = bool_type();
        }
        switch (binary->operation) {
            case Operation::Equals:
                result.value = left_const.value == right_const.value;
                break;
            case Operation::NotEquals:
                result.value = left_const.value != right_const.value;
                break;
            case Operation::Less:
                result.value = left_const.value < right_const.value;
                break;
            case Operation::LessEquals:
                result.value = left_const.value <= right_const.value;
                break;
            case Operation::Greater:
                result.value = left_const.value > right_const.value;
                break;
            case Operation::GreaterEquals:
                result.value = left_const.value >= right_const.value;
                break;
            case Operation::LogicalAnd:
                result.value = left_const.value && right_const.value;
                break;
            case Operation::LogicalOr:
                result.value = left_const.value || right_const.value;
                break;
            default:
                TODO();
        }
    } else {
        switch (binary->operation) {
            case Operation::Add: {
                result.value = left_const.value + right_const.value;
                if (check_overflow(left_const, right_const, result, expected) == Overflow::Yes) {
                    handle_overflow(cc, binary, result, expected, overridable);
                }
                break;
            }
            case Operation::Subtract: {
                result.value = fold_sub_and_diagnose_overflow(
                    cc, binary, left_const, right_const, expected, overridable);
                break;
            }
            case Operation::Multiply: {
                result.value = left_const.value * right_const.value;
                if (check_overflow(left_const, right_const, result, expected) == Overflow::Yes) {
                    handle_overflow(cc, binary, result, expected, overridable);
                }
                break;
            }
            case Operation::Divide:
                result.value = left_const.value / right_const.value;
                break;
            case Operation::Modulo:
                result.value = left_const.value % right_const.value;
                break;
            case Operation::And:
                result.value = left_const.value & right_const.value;
                break;
            case Operation::Or:
                result.value = left_const.value | right_const.value;
                break;
            case Operation::Xor:
                result.value = left_const.value ^ right_const.value;
                break;
            case Operation::LeftShift: {
                result.value = left_const.value << right_const.value;
                diagnose_shift_overflow(
                    cc, binary, left_const, right_const, result, expected, overridable);
                break;
            }
            case Operation::RightShift:
                if (expected->is_signed()) {
                    result.value = static_cast<int64_t>(left_const.value)
                        >> static_cast<int64_t>(right_const.value);
                } else {
                    result.value = left_const.value >> right_const.value;
                }
                diagnose_shift_overflow(
                    cc, binary, left_const, right_const, result, expected, overridable);
                break;
            case Operation::LeftRotate:
                switch (expected->size) {
                    case 8:
                        result.value = std::rotl(static_cast<uint8_t>(left_const.value),
                            static_cast<int>(right_const.value));
                        break;
                    case 16:
                        result.value = std::rotl(static_cast<uint16_t>(left_const.value),
                            static_cast<int>(right_const.value));
                        break;
                    case 32:
                        result.value = std::rotl(static_cast<uint32_t>(left_const.value),
                            static_cast<int>(right_const.value));
                        break;
                    case 64:
                        result.value
                            = std::rotl(left_const.value, static_cast<int>(right_const.value));
                        break;
                    default:
                        TODO();
                }
                break;
            case Operation::RightRotate:
                switch (expected->size) {
                    case 8:
                        result.value = std::rotr(static_cast<uint8_t>(left_const.value),
                            static_cast<int>(right_const.value));
                        break;
                    case 16:
                        result.value = std::rotr(static_cast<uint16_t>(left_const.value),
                            static_cast<int>(right_const.value));
                        break;
                    case 32:
                        result.value = std::rotr(static_cast<uint32_t>(left_const.value),
                            static_cast<int>(right_const.value));
                        break;
                    case 64:
                        result.value
                            = std::rotr(left_const.value, static_cast<int>(right_const.value));
                        break;
                    default:
                        TODO();
                }
                break;
            default:
                return binary; // Do nothing
        }
    }

    if (get_unaliased_type(expected) == bool_type()) {
        result.value = std::clamp<uint64_t>(result.value, 0, 1);
    }

    auto loc = binary->location;
    // can't free anything here because get_binary_expression_type() shouldn't change the ast.
    // free_ast(binary);
    return new AstLiteral(expected, result.value, loc);
}

bool is_commutative_operation(Operation op)
{
    switch (op) {
        case Operation::Add:
        case Operation::Multiply:
        case Operation::And:
        case Operation::Or:
        case Operation::Xor:
            return true;
        default:
            return false;
    }
}

bool exprs_identical(Ast *, Ast *, CheckSideEffects);

Ast *try_simplify_binary_with_negate(AstBinary *binary, Ast *left, Ast *right, Type *expected)
{
    if (has_side_effects(left) || has_side_effects(right)) {
        return binary;
    }

    bool left_negate = left->operation == Operation::Negate;
    bool right_negate = right->operation == Operation::Negate;
    auto *&negated = left_negate ? binary->left : binary->right;
    auto *&other = left_negate ? binary->right : binary->left;

    if (binary->operation == Operation::Add) {
        if (left_negate != right_negate) {
            if (exprs_identical(
                    static_cast<AstUnary *>(negated)->operand, other, CheckSideEffects::No)) {
                // x + -x  or  -x + x  --> 0
                return new AstLiteral(expected, 0, binary->location);
            }
            // x + -y  or  -y + x  --> x - y
            binary->operation = Operation::Subtract;
            if (negated == binary->left) {
                std::swap(binary->left, binary->right);
                binary->right = static_cast<AstNegate *>(binary->right)->operand;
            } else {
                negated = static_cast<AstNegate *>(negated)->operand;
            }
        } else if (left_negate && right_negate) {
            // -x + -x  --> -(x + x)
            // -x + -y  --> -(x + y)
            negated = static_cast<AstNegate *>(negated)->operand;
            other = static_cast<AstNegate *>(other)->operand;
            return new AstUnary(Operation::Negate, binary, binary->location);
        }
    } else if (binary->operation == Operation::Subtract) {
        if (left_negate && right_negate) {
            if (exprs_identical(negated, other, CheckSideEffects::No)) {
                // -x - -x  --> 0
                return new AstLiteral(expected, 0, binary->location);
            }
            // -x - -y  --> -x + y  --> y - x
            binary->left = static_cast<AstNegate *>(left)->operand;
            binary->right = static_cast<AstNegate *>(right)->operand;
            std::swap(binary->left, binary->right);
        } else if (right_negate && !left_negate) {
            // x - -x  --> x + x
            binary->operation = Operation::Add;
            binary->right = static_cast<AstNegate *>(right)->operand;
        }
    }
    return binary;
}

Ast *try_fold_binary(Compiler &cc, AstBinary *binary, Type *&expected, TypeOverridable overridable)
{
    // Figure out which parts of the expression are constant.
    auto *left = try_constant_fold(cc, binary->left, expected, overridable);
    auto *right = try_constant_fold(cc, binary->right, expected, overridable);

    // We can do some simplifications on binary ops with negation.
    if (left->operation == Operation::Negate || right->operation == Operation::Negate) {
        auto *ret = try_simplify_binary_with_negate(binary, left, right, expected);
        if (ret->type != AstType::Binary) {
            return ret;
        }
    }

    left = binary->left, right = binary->right;
    bool left_is_const = left->type == AstType::Integer || left->type == AstType::Boolean;
    bool right_is_const = right->type == AstType::Integer || right->type == AstType::Boolean;
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
        if (right_is_const && right_const.value == 0) {
            diag::ast_warning(cc, binary->right, "trying to divide by 0");
            return binary;
        }
    }

    if (left_is_const && right_is_const) {
        // Easy case: if both sides are constants, fold unless the operation is illegal.
        return try_fold_constants(cc, binary, left_const, right_const, expected, overridable);
    }

    if (is_commutative_operation(binary->operation)) {
        // Only one side is a constant, and the operation is commutative.
        auto *constant_ast = left_is_const ? left : right;
        auto *variable_ast = left_is_const ? right : left;
        auto constant = left_is_const ? left_const : right_const;
        return try_partial_fold_commutative(
            binary, constant_ast, variable_ast, constant.value, expected);
    }

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
