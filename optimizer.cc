#include "compiler.hh"

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
    return new AstBinary(
        operation, ret, new AstLiteral(expected, AstType::Integer, accumulator, {}), {});
}

Ast *try_fold_identities(
    AstBinary *binary, Ast *constant_ast, Ast *variable_ast, int64_t constant, Type *expected)
{
    if (binary->operation == Operation::Add) {
        if (constant == 0) {
            // x+0=x and 0+x=x
            delete static_cast<AstLiteral *>(constant_ast);
            delete static_cast<AstBinary *>(binary);
            // TODO - delete var->type if not builtin
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
            // TODO - delete var->type if not builtin
            static_cast<AstIdentifier *>(variable_ast)->var->type = expected;
            return variable_ast;
        }
    }
    return nullptr;
}

Ast *try_partial_fold_associative(
    AstBinary *binary, Ast *constant_ast, Ast *variable_ast, int64_t constant, Type *expected)
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

Ast *try_constant_fold(Compiler &, Ast *, Type *);

Ast *try_fold_unary(Compiler &cc, Ast *unary, Type *expected)
{
    if (unary->operation == Operation::Cast) {
        assert(static_cast<AstCast *>(unary)->cast_type == expected);
        return static_cast<AstCast *>(unary)->expr;
    }

    if (unary->operation != Operation::Negate) {
        return unary;
    }

    auto *leaf = try_constant_fold(cc, static_cast<AstNegate *>(unary)->operand, expected);
    if (leaf && leaf->type == AstType::Integer) {
        auto lhs = static_cast<AstLiteral *>(leaf)->u.s64;
        auto result = -lhs;
        delete static_cast<AstLiteral *>(leaf);
        delete static_cast<AstNegate *>(unary);
        return new AstLiteral(expected, AstType::Integer, result, {});
    }

    return unary;
}

Ast *try_fold_constants(
    Compiler &cc, AstBinary *binary, int64_t left_const, int64_t right_const, Type *expected)
{
    int64_t result;
    // TODO - warn if expr overflows expected type
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
    return new AstLiteral(expected, AstType::Integer, result, loc);
}

Ast *try_fold_binary(Compiler &cc, AstBinary *binary, Type *expected)
{
    // Figure out which parts of the expression are constant.
    auto *left = try_constant_fold(cc, binary->left, expected);
    auto *right = try_constant_fold(cc, binary->right, expected);
    bool left_is_const = left->type == AstType::Integer;
    bool right_is_const = right->type == AstType::Integer;
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

    if (left_is_const && right_is_const) {
        // Easy case: if both sides are constants, fold unless the operation is illegal.
        return try_fold_constants(cc, binary, left_const, right_const, expected);
    }

    // Only one side is a constant, and the operation is associative.
    if (binary->operation == Operation::Multiply || binary->operation == Operation::Add) {
        auto *constant_ast = left_is_const ? left : right;
        auto *variable_ast = left_is_const ? right : left;
        auto constant = left_is_const ? left_const : right_const;
        return try_partial_fold_associative(binary, constant_ast, variable_ast, constant, expected);
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

Ast *try_constant_fold(Compiler &cc, Ast *ast, Type *expected)
{
    if (!ast) {
        return nullptr;
    }
    if (ast->type == AstType::Unary) {
        return try_fold_unary(cc, static_cast<AstNegate *>(ast), expected);
    }
    if (ast->type == AstType::Binary) {
        return try_fold_binary(cc, static_cast<AstBinary *>(ast), expected);
    }
    return ast;
}
