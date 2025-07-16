#include "compiler.hh"

#define parser_ast_error(ast, msg, ...) \
    diag_error_at(cc, ast->location, ErrorType::Parser, msg __VA_OPT__(, __VA_ARGS__))

#define parser_error(loc, msg, ...) \
    diag_error_at(cc, loc, ErrorType::Parser, msg __VA_OPT__(, __VA_ARGS__))

enum class Associativity {
    Right,
    Left,
};

using Precedence = int;

namespace prec {
    constexpr Precedence Comma = 1, Assignment = 2, LogicalOr = 3, LogicalAnd = 4, Comparison = 5,
                         AdditiveArithmetic = 6, MultiplicativeArithmetic = 7;
    constexpr Precedence Lowest = Comma, Highest = MultiplicativeArithmetic;
} // namespace prec

struct OperatorInfo {
    Precedence precedence;
    Associativity associativity;
};

OperatorInfo get_operator_info(TokenKind kind)
{
    using enum TokenKind;
    switch (kind) {
        case Star:
        case Slash:
        case Percent:
            return { prec::MultiplicativeArithmetic, Associativity::Left };
        case Plus:
        case Minus:
            return { prec::AdditiveArithmetic, Associativity::Left };
        case LAngle:
        case RAngle:
            return { prec::Comparison, Associativity::Left };
        case ColonEquals:
        case Equals:
            return { prec::Assignment, Associativity::Right };
        case Or:
            return { prec::LogicalOr, Associativity::Left };
        case And:
            return { prec::LogicalAnd, Associativity::Left };
        case Comma:
            return { prec::Comma, Associativity::Left };
        case EqualsEquals:
        case ExclEquals:
        case LAngleEquals:
        case RAngleEquals:
            return { prec::Comparison, Associativity::Left };
        default:
            // Invalid operators are handled in parse_atom.
            // Return something big so we make it there.
            return { prec::Highest, Associativity::Left };
    }
}

enum class AllowVarDecl {
    No,
    Yes
};

Ast *parse_expr(Compiler &, AllowVarDecl = AllowVarDecl::No, Precedence = prec::Lowest);

AstCall *parse_call(Compiler &cc, std::string_view function, SourceLocation location)
{
    if (lex(cc).kind == TokenKind::RParen) {
        return new AstCall(function, {}, location); // Call without args
    }

    std::vector<Ast *> args{};
    for (;;) {
        args.emplace_back(parse_expr(cc, AllowVarDecl::No, prec::Comma + 1));
        auto token = lex(cc);
        if (token.kind != TokenKind::Comma) {
            break;
        }
        consume(cc.lexer, token);
    }

    return new AstCall(function, std::move(args), location);
}

enum class IntConversionError {
    None,
    Overflow,
    EmptyString
};

// TODO: floats
IntConversionError string_to_number(std::string_view str, uint64_t &result)
{
    result = 0;
    int base = 10;
    if (str.starts_with("0x")) {
        base = 16;
    } else if (str.starts_with("0b")) {
        base = 2;
    }

    if (base != 10) {
        str = str.substr(2); // Skip the prefix
    }

    if (str.empty()) {
        return IntConversionError::EmptyString;
    }

    for (size_t i = 0; i < str.length(); i++) {
        char c = str[i];
        int digit = 0;
        if (is_digit(c)) {
            digit = c - '0';
        } else if (is_alpha(c) && base > 10) {
            digit = to_upper(c) - 'A' + 10;
        } else if (c == '_') {
            continue;
        }
        // Don't need to check that digit < base, the lexer has already verified it.
        if (__builtin_mul_overflow(result, base, &result)) {
            return IntConversionError::Overflow;
        }
        result += digit;
    }

    return IntConversionError::None;
}

void try_convert_string_to_u64(
    Compiler &cc, SourceLocation loc, std::string_view str, uint64_t &result)
{
    switch (string_to_number(str, result)) {
        case IntConversionError::None:
            return;
        case IntConversionError::Overflow:
            parser_error(loc, "number too big for any type");
        case IntConversionError::EmptyString:
            parser_error(loc, "empty number string");
    }
}

// This also handles unary operations.
Ast *parse_atom(Compiler &cc, AllowVarDecl allow_var_decl)
{
    auto token = lex(cc);

    if (is_group(token.kind, TokenKind::GroupOperator)) {
        if (token.kind == TokenKind::LParen) {
            consume(cc.lexer, token);
            auto ast = parse_expr(cc);
            token = lex(cc);
            consume_expected(cc, TokenKind::RParen, token);
            return ast;
        }
        if (token.kind == TokenKind::Minus) {
            consume(cc.lexer, token);
            auto prev_col = cc.lexer.column;
            auto arg = parse_atom(cc, allow_var_decl);
            // Double negate isn't allowed
            if (arg->operation == Operation::Negate) {
                cc.lexer.column = prev_col;
                parser_ast_error(arg, "only one unary minus is allowed");
            }
            return new AstNegate(Operation::Negate, arg, token.location);
        }
        parser_error(token.location, "unexpected operator");
    }

    if (is_group(token.kind, TokenKind::GroupNumber)) {
        uint64_t number;
        try_convert_string_to_u64(cc, token.location, token.string, number);
        consume(cc.lexer, token);
        if (number > std::numeric_limits<int64_t>::max()) {
            return new AstLiteral(u64_type(), number, token.location);
        }
        if (number > std::numeric_limits<uint32_t>::max()) {
            return new AstLiteral(s64_type(), number, token.location);
        }
        if (number > std::numeric_limits<int32_t>::max()) {
            return new AstLiteral(u32_type(), number, token.location);
        }
        return new AstLiteral(s32_type(), number, token.location);
    }

    if (is_group(token.kind, TokenKind::GroupString)) {
        consume_string(cc.lexer, token);
        return new AstString(*token.real_string, token.location);
    }

    if (is_group(token.kind, TokenKind::GroupIdentifier)) {
        auto prev_col = cc.lexer.column;
        consume(cc.lexer, token);
        auto next_token = lex(cc);
        if (next_token.kind == TokenKind::LParen) {
            consume(cc.lexer, next_token);
            auto *call = parse_call(cc, token.string, token.location);
            consume_expected(cc, TokenKind::RParen, lex(cc));
            return call;
        }
        auto *var_decl = find_variable(current_scope, token.string);
        if (!var_decl) {
            cc.lexer.column = prev_col;
            parser_error(
                token.location, "variable `{}` is not declared in this scope", token.string);
        }
        return new AstIdentifier(token.string, var_decl ? &var_decl->var : nullptr, token.location);
    }

    if (is_group(token.kind, TokenKind::GroupKeyword)) {
        bool is_false = token.kind == TokenKind::False;
        bool is_true = false;
        if (!is_false) {
            is_true = token.kind == TokenKind::True;
            if (!is_true) {
                return nullptr;
            }
        }
        consume(cc.lexer, token);
        return new AstLiteral(is_true, token.location);
    }

    return nullptr;
}

AstBinary *parse_binary(Compiler &cc, const Token &operation_token, Ast *lhs, Ast *rhs)
{
    if (!lhs || !rhs) {
        const char *side = lhs ? "right-hand" : "left-hand";
        parser_error(operation_token.location, "binary operation missing {} operand", side);
    }

    Operation operation = Operation::None;

    switch (operation_token.kind) {
        case TokenKind::Plus:
            operation = Operation::Add;
            break;
        case TokenKind::Minus:
            operation = Operation::Subtract;
            break;
        case TokenKind::Star:
            operation = Operation::Multiply;
            break;
        case TokenKind::Slash:
            operation = Operation::Divide;
            break;
        case TokenKind::Percent:
            operation = Operation::Modulo;
            break;
        case TokenKind::LAngle:
            operation = Operation::Less;
            break;
        case TokenKind::RAngle:
            operation = Operation::Greater;
            break;
        case TokenKind::Equals:
            operation = Operation::Assign;
            break;
        case TokenKind::EqualsEquals:
            operation = Operation::Equals; // Not a bug
            break;
        case TokenKind::ExclEquals:
            operation = Operation::NotEquals;
            break;
        case TokenKind::RAngleEquals:
            operation = Operation::GreaterEquals;
            break;
        case TokenKind::LAngleEquals:
            operation = Operation::LessEquals;
            break;
        case TokenKind::ColonEquals:
            operation = Operation::VariableDecl;
            break;
        case TokenKind::And:
            operation = Operation::LogicalAnd;
            break;
        case TokenKind::Or:
            operation = Operation::LogicalOr;
            break;
        default:
            parser_error(operation_token.location, "unexpected operator `{}` in binary operation",
                operation_token.string);
            return nullptr;
    }

    return new AstBinary(operation, lhs, rhs, operation_token.location);
}

enum class AllowInitExpr {
    No,
    Yes
};

AstVariableDecl *parse_var_decl(Compiler &, AllowInitExpr = AllowInitExpr::Yes);

Ast *parse_expr(Compiler &cc, AllowVarDecl allow_var_decl, Precedence min_precedence)
{
    // HACK - we need this only for parse_var_decl
    auto prev_col = cc.lexer.column;
    auto prev_pos = cc.lexer.position;

    auto root = parse_atom(cc, allow_var_decl);

    for (;;) {
        const auto token = lex(cc);
        if (!is_group(token.kind, TokenKind::GroupOperator) || token.kind == TokenKind::RParen
            || token.kind == TokenKind::LBrace || token.kind == TokenKind::RBrace) {
            break;
        }

        if (token.kind == TokenKind::ColonEquals) {
            if (allow_var_decl == AllowVarDecl::Yes) {
                cc.lexer.column = prev_col;
                cc.lexer.position = prev_pos;
                return parse_var_decl(cc);
            } else {
                break;
            }
        }

        const auto info = get_operator_info(token.kind);
        if (info.precedence < min_precedence) {
            break;
        }

        consume(cc.lexer, token);
        const auto new_precedence = info.precedence + to_underlying(info.associativity);
        // Place the current expression on the left and the next on the right, recursively.
        const auto rhs = parse_expr(cc, allow_var_decl, new_precedence);
        root = parse_binary(cc, token, root, rhs);
    }

    return root;
}

Type *parse_type(Compiler &cc)
{
    auto token = lex(cc);
    if (!is_group(token.kind, TokenKind::GroupIdentifier)) {
        parser_error(
            token.location, "expected a type name, got `{}`", make_printable(token.string));
    }
    consume(cc.lexer, token);
    auto *type = find_type(current_scope, token.string);
    if (type) {
        // FIXME: this is not quite right
        type->location = token.location;
        return type;
    }
    // TODO - unnecessary string creation?
    return new Type{ .name = std::string(token.string),
        .flags = TypeFlags::UNRESOLVED,
        .location = token.location };
}

Token parse_identifier(Compiler &cc)
{
    auto token = lex(cc);
    if (!is_group(token.kind, TokenKind::GroupIdentifier)) {
        parser_error(
            token.location, "expected an identifier, got `{}`", make_printable(token.string));
    }
    consume(cc.lexer, token);
    return token;
}

std::vector<AstVariableDecl *> parse_fn_params(Compiler &cc)
{
    std::vector<AstVariableDecl *> ret{};
    // TODO - let's not allow init exprs for now... maybe this will change
    int idx = 0;
    while (auto *var_decl = parse_var_decl(cc, AllowInitExpr::No)) {
        var_decl->var.param_index = idx++;
        ret.push_back(var_decl);
        auto token = lex(cc);
        if (token.kind != TokenKind::Comma) {
            break;
        }
        consume(cc.lexer, token);
    }

    for (auto &param : ret) {
        current_scope->add_variable(cc, param);
    }

    return ret;
}

AstBlock *parse_block(Compiler &cc, AstFunction *current_function = nullptr)
{
    // TODO: keep track of the last token so we don't have to call lex again
    consume_expected(cc, TokenKind::LBrace, lex(cc));
    cc.lexer.ignore_newlines = false;
    consume_newline_or_eof(cc, lex(cc));
    cc.lexer.ignore_newlines = true;
    std::vector<Ast *> stmts{};
    for (;;) {
        auto token = lex(cc);
        if (token.kind == TokenKind::RBrace) {
            consume(cc.lexer, token);
            break;
        }
        auto *stmt = parse_stmt(cc, current_function);
        if (!stmt) {
            parser_error(cc.lexer.location(), "unexpected end of input. maybe missing a brace?");
        }
        stmts.emplace_back(stmt);
    }
    return new AstBlock(std::move(stmts));
}

AstFunction *parse_function(Compiler &cc)
{
    auto token = lex(cc); // Name
    auto location = token.location;
    if (!is_group(token.kind, TokenKind::GroupIdentifier)) {
        parser_error(token.location, "invalid function name {}", token.string);
    }

    auto name = token.string;
    consume(cc.lexer, token);

    consume_expected(cc, TokenKind::LParen, lex(cc));
    token = lex(cc); // `)` or identifier

    // Function parameters already belong to new scope
    enter_new_scope();
    std::vector<AstVariableDecl *> params{};
    if (token.kind != TokenKind::RParen) {
        params = parse_fn_params(cc);
    }
    consume_expected(cc, TokenKind::RParen, lex(cc));

    auto *ret_type = void_type();
    cc.lexer.ignore_newlines = true;
    token = lex(cc);
    cc.lexer.ignore_newlines = false;
    if (token.kind == TokenKind::Arrow) {
        consume(cc.lexer, token);
        ret_type = parse_type(cc);
        token = lex(cc); // {
    }

    // This expects { + (newline + stmts)? + }
    auto function = new AstFunction(name, ret_type, std::move(params), {}, location);
    current_scope->parent->add_function(cc, function, name);
    function->body = parse_block(cc, function);
    function->scope = current_scope->parent;
    leave_scope();

    return function;
}

AstVariableDecl *parse_var_decl(Compiler &cc, AllowInitExpr allow_init_expr)
{
    auto loc = cc.lexer.location();
    auto name = parse_identifier(cc);
    auto token = lex(cc);
    if (allow_init_expr == AllowInitExpr::Yes && token.kind == TokenKind::ColonEquals) {
        consume(cc.lexer, token);
        if (auto *init_expr = parse_expr(cc)) {
            return new AstVariableDecl(unresolved_type(), name.string, init_expr, loc);
        }
        parser_error(loc, "auto inferred variable must have an initializer expression");
    } else if (token.kind == TokenKind::Colon) {
        consume(cc.lexer, token);
        auto *type = parse_type(cc);
        token = lex(cc);
        Ast *maybe_expr = nullptr;
        if (allow_init_expr == AllowInitExpr::Yes && token.kind == TokenKind::Equals) {
            consume(cc.lexer, token);
            maybe_expr = parse_expr(cc);
            if (!maybe_expr) {
                parser_error(token.location, "missing initializer expression");
            }
        }
        return new AstVariableDecl(type, name.string, maybe_expr, loc);
    }
    // TODO can this be hit?
    assert(!"parse_var_decl");
    return nullptr;
}

AstIf *parse_if(Compiler &cc, AstFunction *current_function)
{
    auto loc = cc.lexer.location();
    Ast *expr = parse_expr(cc, AllowVarDecl::No);
    if (!expr) {
        parser_error(loc, "if statement must have a condition");
    }

    if (expr->operation == Operation::Assign) {
        parser_ast_error(expr, "assignments are not allowed in if statements");
    } else if (expr->operation == Operation::VariableDecl) {
        current_scope->add_variable(cc, static_cast<AstVariableDecl *>(expr));
    }
    enter_new_scope();
    AstBlock *body = parse_block(cc, current_function);
    leave_scope();

    Ast *else_ = nullptr;
    auto token = lex(cc);
    if (token.kind == TokenKind::Else) {
        consume(cc.lexer, token);
        token = lex(cc);
        if (token.kind == TokenKind::If) {
            consume(cc.lexer, token);
            else_ = parse_if(cc, current_function);
        } else {
            else_ = parse_block(cc, current_function);
        }
    }

    return new AstIf(expr, body, else_, loc);
}

AstWhile *parse_while(Compiler &cc, AstFunction *current_function)
{
    auto loc = cc.lexer.location();
    Ast *expr = parse_expr(cc, AllowVarDecl::No);
    if (!expr) {
        parser_error(loc, "while statement must have a condition");
    }

    if (expr->operation == Operation::Assign) {
        parser_ast_error(expr, "assignments are not allowed in while statements");
    }

    enter_new_scope();
    AstBlock *body = parse_block(cc, current_function);
    leave_scope();

    return new AstWhile(expr, body, loc);
}

void parse_error_attribute(Compiler &cc)
{
    static const std::unordered_map<std::string_view, ErrorType> error_attr_map{
        { "lexer", ErrorType::Lexer },
        { "parser", ErrorType::Parser },
        { "verify", ErrorType::Verification },
        { "type", ErrorType::TypeCheck },
    };
    consume_expected(cc, TokenKind::LParen, lex(cc));
    auto token = lex(cc);
    if (auto it = error_attr_map.find(token.string); it != error_attr_map.end()) {
        cc.test_mode.error_type = it->second;
    } else {
        parser_error(token.location, "unknown attribute");
    }
    consume(cc.lexer, token);
    consume_expected(cc, TokenKind::RParen, lex(cc));
    cc.test_mode.test_type = TestType::Error;
    if (!opts.testing) {
        // TODO: output a warning or skip?
    }
}

void parse_attribute_list(Compiler &cc)
{
    auto token = lex(cc);
    if (token.kind == TokenKind::RBrace) {
        consume(cc.lexer, token);
        return;
    }
    token = lex(cc);
    switch (hash(token.string)) {
        case hash("error"):
            consume(cc.lexer, token);
            parse_error_attribute(cc);
            break;
        case hash("returns"):
            consume(cc.lexer, token);
            consume_expected(cc, TokenKind::LParen, lex(cc));
            token = lex(cc);
            if (is_group(token.kind, TokenKind::GroupNumber)) {
                try_convert_string_to_u64(
                    cc, token.location, token.string, cc.test_mode.return_value);
            }
            consume(cc.lexer, token);
            cc.test_mode.test_type = TestType::ReturnsValue;
            consume_expected(cc, TokenKind::RParen, lex(cc));
    }
    consume_expected(cc, TokenKind::RBrace, lex(cc));
}

Ast *parse_stmt(Compiler &cc, AstFunction *current_function)
{
    auto token = lex(cc);

    if (token.kind == TokenKind::Hash) {
        consume(cc.lexer, token);
        consume_expected(cc, TokenKind::LBrace, lex(cc));
        parse_attribute_list(cc);
        token = lex(cc);
    }

    if (is_group(token.kind, TokenKind::GroupKeyword)) {
        if (token.kind == TokenKind::If) {
            consume(cc.lexer, token);
            return parse_if(cc, current_function);
        }
        if (token.kind == TokenKind::While) {
            consume(cc.lexer, token);
            return parse_while(cc, current_function);
        }
        if (token.kind == TokenKind::Fn) {
            consume(cc.lexer, token);
            cc.lexer.ignore_newlines = false;
            auto *function = parse_function(cc);
            // Note we have to set it again because parse_function
            // may recurse into here and some paths can reset it
            cc.lexer.ignore_newlines = false;
            consume_newline_or_eof(cc, lex(cc));
            cc.lexer.ignore_newlines = true;
            return function;
        }
        if (token.kind == TokenKind::Return) {
            consume(cc.lexer, token);
            cc.lexer.ignore_newlines = false;
            auto *ret = new AstReturn(parse_expr(cc), token.location);
            if (ret->expr && ret->expr->operation == Operation::Assign) {
                parser_ast_error(ret, "assignments are not allowed in return statements");
            }
            if (current_function) {
                current_function->return_stmts.push_back(ret);
            }
            consume_newline_or_eof(cc, lex(cc));
            cc.lexer.ignore_newlines = true;
            return ret;
        }
        if (token.kind == TokenKind::Alias) {
            consume(cc.lexer, token);
            auto alias = parse_identifier(cc);
            consume_expected(cc, TokenKind::Colon, lex(cc));
            auto *type = parse_type(cc);
            current_scope->add_alias(cc, type, alias.string, alias.location);
            cc.lexer.ignore_newlines = false;
            consume_newline_or_eof(cc, lex(cc));
            cc.lexer.ignore_newlines = true;
            return nullptr;
        }
        assert(!"unhandled keyword");
    } else if (is_group(token.kind, TokenKind::GroupIdentifier)) {
        const auto prev_pos = cc.lexer.position;
        const auto prev_col = cc.lexer.column;
        consume(cc.lexer, token);
        const auto next_token = lex(cc);
        if (next_token.kind == TokenKind::Colon || next_token.kind == TokenKind::ColonEquals) {
            cc.lexer.ignore_newlines = false;
            // FIXME - make the name be found by parse_var_decl
            cc.lexer.position = prev_pos;
            cc.lexer.column = prev_col;
            auto *var_decl = parse_var_decl(cc);
            current_scope->add_variable(cc, static_cast<AstVariableDecl *>(var_decl));
            consume_newline_or_eof(cc, lex(cc));
            cc.lexer.ignore_newlines = true;
            return var_decl;
        } else {
            // It's not a var decl, go back
            cc.lexer.position = prev_pos;
            cc.lexer.column = prev_col;
        }
    } else if (token.kind == TokenKind::LBrace) {
        enter_new_scope();
        auto block = parse_block(cc, current_function);
        leave_scope();
        return block;
    }
    cc.lexer.ignore_newlines = false;
    auto maybe_expr = parse_expr(cc);
    cc.lexer.ignore_newlines = true;
    if (maybe_expr) {
        if (maybe_expr->operation != Operation::Assign
            && maybe_expr->operation != Operation::Call) {
            // TODO: this is not in the right location, should be at the start of the expression
            diag_ast_warning(cc, maybe_expr, "expression result is unused");
        }
        cc.lexer.ignore_newlines = false;
        consume_newline_or_eof(cc, lex(cc));
        cc.lexer.ignore_newlines = true;
    }
    return maybe_expr;
}

std::string extract_integer_constant(AstLiteral *literal)
{
    auto *type = get_unaliased_type(literal->literal_type);
    if (type->get_kind() == TypeFlags::Integer) {
        if (type->has_flag(TypeFlags::UNSIGNED)) {
            if (type->size == 8) {
                return std::to_string(literal->u.u64);
            }
            return std::to_string(literal->u.u32);
        }
        if (type->size == 8) {
            return std::to_string(literal->u.s64);
        }
        return std::to_string(literal->u.s32);
    }
    if (type->get_kind() == TypeFlags::Boolean) {
        return std::to_string(literal->u.boolean);
    }
    TODO();
}

void diagnose_redeclaration_or_shadowing(Compiler &cc, Scope *scope, std::string_view name,
    std::string_view type, ErrorOnShadowing error_on_shadowing)
{
    Scope *result_scope;
    AstFunction *existing_fn = nullptr;
    Type *existing_type = nullptr;
    AstVariableDecl *existing_var = find_variable(scope, name, &result_scope);
    if (!result_scope) {
        existing_fn = find_function(scope, name, &result_scope);
    }
    if (!result_scope) {
        existing_type = find_type(scope, name, &result_scope);
    }
    if (result_scope) {
        const char *existing_str = existing_var ? "variable" : existing_fn ? "function" : "type";
        auto location = existing_type ? existing_type->location
            : existing_fn             ? existing_fn->location
                                      : existing_var->location;

        if (result_scope == scope || error_on_shadowing == ErrorOnShadowing::Yes) {
            const char *same_scope_str = result_scope == scope ? " in the same scope" : "";
            parser_error(location,
                "{} `{}` cannot be redeclared as a {}{}. this is the existing "
                "declaration: ",
                existing_str, name, type, same_scope_str);
        } else {
            diag_warning_at(cc, location, "{} `{}` is shadowing this {} in an outer scope:", type,
                name, existing_str);
        }
    }
}

void Scope::add_variable(Compiler &cc, AstVariableDecl *var_decl)
{
    diagnose_redeclaration_or_shadowing(
        cc, this, var_decl->var.name, "variable", ErrorOnShadowing::No);
    var_decl->var.index_in_scope = variables.size();
    variables.push_back(var_decl);
}

void Scope::add_function(Compiler &cc, Ast *ast, std::string_view unmangled_name)
{
    assert(ast->operation == Operation::FunctionDecl);
    auto *fn = static_cast<AstFunction *>(ast);
    diagnose_redeclaration_or_shadowing(cc, this, fn->name, "function", ErrorOnShadowing::Yes);
    functions[unmangled_name] = fn;
}

void Scope::add_alias(Compiler &cc, Type *type, std::string_view alias, SourceLocation location)
{
    // TODO - unnecessary string creation?
    diagnose_redeclaration_or_shadowing(cc, this, alias, "type", ErrorOnShadowing::No);
    types[alias] = new Type{
        .name = std::string(alias), .flags = TypeFlags::ALIAS, .real = type, .location = location
    };
}

void free_ast(std::vector<Ast *> &ast_vec)
{
    for (auto *ast : ast_vec) {
        free_ast(ast);
    }
    ast_vec.clear();
}

void free_ast(Ast *ast)
{
    if (!ast) {
        return;
    }

    // Putting a delete at the bottom isn't possible because of mismatched new/delete size values.
    // So all the separate delete + static_casts are necessary.

    switch (ast->type) {
        case AstType::Unary:
            if (ast->operation == Operation::Call) {
                free_ast(static_cast<AstCall *>(ast)->args);
                delete static_cast<AstCall *>(ast);
            } else if (ast->operation == Operation::Negate) {
                free_ast(static_cast<AstNegate *>(ast)->operand);
                delete static_cast<AstNegate *>(ast);
            } else if (ast->operation == Operation::Cast) {
                free_ast(static_cast<AstCast *>(ast)->expr);
                delete static_cast<AstCast *>(ast);
            }
            break;
        case AstType::Binary:
            free_ast(static_cast<AstBinary *>(ast)->left);
            free_ast(static_cast<AstBinary *>(ast)->right);
            delete static_cast<AstBinary *>(ast);
            break;
        case AstType::Statement:
            if (ast->operation == Operation::Return) {
                free_ast(static_cast<AstReturn *>(ast)->expr);
                delete static_cast<AstReturn *>(ast);
            } else if (ast->operation == Operation::FunctionDecl) {
                for (auto *param : static_cast<AstFunction *>(ast)->params) {
                    free_ast(param);
                }
                free_ast(static_cast<AstFunction *>(ast)->body);
                delete static_cast<AstFunction *>(ast);
            } else if (ast->operation == Operation::VariableDecl) {
                free_ast(static_cast<AstVariableDecl *>(ast)->init_expr);
                delete static_cast<AstVariableDecl *>(ast);
            } else if (ast->operation == Operation::If) {
                free_ast(static_cast<AstIf *>(ast)->expr);
                free_ast(static_cast<AstIf *>(ast)->body);
                delete static_cast<AstIf *>(ast);
            }
            break;
        case AstType::Block:
            free_ast(static_cast<AstBlock *>(ast)->stmts);
            delete static_cast<AstBlock *>(ast);
            break;
        case AstType::Integer:
            [[fallthrough]];
        case AstType::Boolean:
            delete static_cast<AstLiteral *>(ast);
            break;
        case AstType::String:
            delete static_cast<AstString *>(ast);
            break;
        case AstType::Identifier:
            delete static_cast<AstIdentifier *>(ast);
            break;
    }
}

template<class T>
T *find__helper(Scope *scope, Scope **result_scope, SearchParents search_parents, auto &&callback)
{
    if (result_scope) {
        *result_scope = nullptr;
    }
    while (scope) {
        if (auto *ret = callback(scope)) {
            if (result_scope) {
                *result_scope = scope;
            }
            return ret;
        }
        if (search_parents == SearchParents::Yes) {
            scope = scope->parent;
        } else {
            break;
        }
    }
    return nullptr;
}

Type *find_type(
    Scope *scope, std::string_view name, Scope **result_scope, SearchParents search_parents)
{
    return find__helper<Type>(scope, result_scope, search_parents, [name](Scope *s) -> Type * {
        if (auto res = s->types.find(name); res != s->types.end()) {
            return res->second;
        }
        return nullptr;
    });
}

AstVariableDecl *find_variable(
    Scope *scope, std::string_view name, Scope **result_scope, SearchParents search_parents)
{
    return find__helper<AstVariableDecl>(
        scope, result_scope, search_parents, [name](Scope *s) -> AstVariableDecl * {
            for (size_t i = 0; i < s->variables.size(); i++) {
                auto *var_decl = s->variables[i];
                // TODO - hash? turn this into a map lookup like the type map already did?
                if (var_decl && var_decl->var.name == name) {
                    return var_decl;
                }
            }
            return nullptr;
        });
}

AstFunction *find_function(Scope *scope, std::string_view unmangled_name, Scope **result_scope,
    SearchParents search_parents)
{
    return find__helper<AstFunction>(
        scope, result_scope, search_parents, [unmangled_name](Scope *s) -> AstFunction * {
            if (auto res = s->functions.find(unmangled_name); res != s->functions.end()) {
                return res->second;
            }
            return nullptr;
        });
}
