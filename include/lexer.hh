#pragma once

#include "file.hh"

#include <stack>

//
// Lexer
//

static constexpr size_t MaxIdentifierLength = 150;

constexpr bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

constexpr bool is_lower(char c)
{
    return c >= 'a' && c <= 'z';
}

constexpr bool is_upper(char c)
{
    return c >= 'A' && c <= 'Z';
}

constexpr bool is_alpha(char c)
{
    return is_lower(c) || is_upper(c);
}

constexpr bool is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
}

constexpr bool is_control_char(char c)
{
    return (c >= 0 && c <= 31) || c == 127;
}

constexpr char to_upper(char c)
{
    return is_lower(c) ? static_cast<char>(c - ('a' - 'A')) : c;
}

#define ENUMERATE_OPERATOR_TOKENS()                 \
    __ENUMERATE_OPERATOR_TOKEN(Plus, "+")           \
    __ENUMERATE_OPERATOR_TOKEN(Minus, "-")          \
    __ENUMERATE_OPERATOR_TOKEN(Star, "*")           \
    __ENUMERATE_OPERATOR_TOKEN(Slash, "/")          \
    __ENUMERATE_OPERATOR_TOKEN(Percent, "%")        \
    __ENUMERATE_OPERATOR_TOKEN(Bar, "|")            \
    __ENUMERATE_OPERATOR_TOKEN(Caret, "^")          \
    __ENUMERATE_OPERATOR_TOKEN(Tilde, "~")          \
    __ENUMERATE_OPERATOR_TOKEN(LParen, "(")         \
    __ENUMERATE_OPERATOR_TOKEN(RParen, ")")         \
    __ENUMERATE_OPERATOR_TOKEN(LBrace, "{")         \
    __ENUMERATE_OPERATOR_TOKEN(RBrace, "}")         \
    __ENUMERATE_OPERATOR_TOKEN(LAngle, "<")         \
    __ENUMERATE_OPERATOR_TOKEN(DoubleLAngle, "<<")  \
    __ENUMERATE_OPERATOR_TOKEN(TripleLAngle, "<<<") \
    __ENUMERATE_OPERATOR_TOKEN(RAngle, ">")         \
    __ENUMERATE_OPERATOR_TOKEN(DoubleRAngle, ">>")  \
    __ENUMERATE_OPERATOR_TOKEN(TripleRAngle, ">>>") \
    __ENUMERATE_OPERATOR_TOKEN(Comma, ",")          \
    __ENUMERATE_OPERATOR_TOKEN(Equals, "=")         \
    __ENUMERATE_OPERATOR_TOKEN(Excl, "!")           \
    __ENUMERATE_OPERATOR_TOKEN(Colon, ":")          \
    __ENUMERATE_OPERATOR_TOKEN(DoubleColon, "::")   \
    __ENUMERATE_OPERATOR_TOKEN(DotDot, "..")        \
    __ENUMERATE_OPERATOR_TOKEN(Hash, "#")           \
    __ENUMERATE_OPERATOR_TOKEN(Ampersand, "&")      \
    __ENUMERATE_OPERATOR_TOKEN(LAngleEquals, "<=")  \
    __ENUMERATE_OPERATOR_TOKEN(RAngleEquals, ">=")  \
    __ENUMERATE_OPERATOR_TOKEN(EqualsEquals, "==")  \
    __ENUMERATE_OPERATOR_TOKEN(ExclEquals, "!=")    \
    __ENUMERATE_OPERATOR_TOKEN(ColonEquals, ":=")   \
    __ENUMERATE_OPERATOR_TOKEN(Arrow, "->")         \
    __ENUMERATE_OPERATOR_TOKEN(And, "and")          \
    __ENUMERATE_OPERATOR_TOKEN(Or, "or")

#define ENUMERATE_KEYWORD_TOKENS()                  \
    __ENUMERATE_KEYWORD_TOKEN(Fn, "fn")             \
    __ENUMERATE_KEYWORD_TOKEN(Return, "return")     \
    __ENUMERATE_KEYWORD_TOKEN(If, "if")             \
    __ENUMERATE_KEYWORD_TOKEN(Else, "else")         \
    __ENUMERATE_KEYWORD_TOKEN(For, "for")           \
    __ENUMERATE_KEYWORD_TOKEN(In, "in")             \
    __ENUMERATE_KEYWORD_TOKEN(While, "while")       \
    __ENUMERATE_KEYWORD_TOKEN(Alias, "alias")       \
    __ENUMERATE_KEYWORD_TOKEN(False, "false")       \
    __ENUMERATE_KEYWORD_TOKEN(True, "true")         \
    __ENUMERATE_KEYWORD_TOKEN(Continue, "continue") \
    __ENUMERATE_KEYWORD_TOKEN(Break, "break")       \
    __ENUMERATE_KEYWORD_TOKEN(Null, "null")         \
    __ENUMERATE_KEYWORD_TOKEN(As, "as")             \
    __ENUMERATE_KEYWORD_TOKEN(Enum, "enum")

enum class TokenKind : uint32_t {
    //
    // Groups:
    //
    GroupEmpty = 1 << 16,
    GroupNewline = 2 << 16,
    GroupNumber = 3 << 16,
    GroupIdentifier = 4 << 16,
    GroupString = 5 << 16,
    GroupOperator = 6 << 16,
    GroupKeyword = 7 << 16,
    group_mask = 0xffff'0000,

    //
    // Kinds:
    // Upper 16 bits is the group, lower 16 bits is the kind.
    //
    operators_start = GroupOperator | 0,
#define __ENUMERATE_OPERATOR_TOKEN(name, string) name,
    ENUMERATE_OPERATOR_TOKENS()
#undef __ENUMERATE_OPERATOR_TOKEN

    keywords_start = GroupKeyword | 0,
#define __ENUMERATE_KEYWORD_TOKEN(name, string) name,
    ENUMERATE_KEYWORD_TOKENS()
#undef __ENUMERATE_KEYWORD_TOKEN
};

struct Lexer;

struct SourceLocation {
    uint32_t line;
    uint32_t column;
    uint32_t end;      // End column.
    uint32_t position; // This is the actual offset in the file, equivalent to Lexer::position.

    static SourceLocation with_lexer(Lexer &, uint32_t end_offset);
};

inline TokenKind get_group(TokenKind kind)
{
    return static_cast<TokenKind>(to_underlying(kind) & to_underlying(TokenKind::group_mask));
}

inline bool is_group(TokenKind kind, TokenKind cmp)
{
    return get_group(kind) == cmp;
}

struct Token {
    explicit constexpr Token(TokenKind _kind, SourceLocation _location)
        : kind(_kind)
        , location(_location)
    {
    }

    explicit constexpr Token(TokenKind _kind, std::string_view _string, SourceLocation _location)
        : string(_string)
        , kind(_kind)
        , location(_location)
    {
    }

    explicit constexpr Token(
        std::unique_ptr<std::string> _string, size_t _length, SourceLocation _location)
        : kind(TokenKind::GroupString)
        , location(_location)
        , real_string(std::move(_string))
        , real_length(_length)
    {
    }

    static constexpr Token make_empty(SourceLocation _location)
    {
        return Token(TokenKind::GroupEmpty, _location);
    }

    static constexpr Token make_newline(SourceLocation _location)
    {
        return Token(TokenKind::GroupNewline, "\n", _location);
    }

    static constexpr Token make_number(std::string_view _string, SourceLocation _location)
    {
        return Token(TokenKind::GroupNumber, _string, _location);
    }

    static constexpr Token make_string(
        std::unique_ptr<std::string> _string, size_t length, SourceLocation _location)
    {
        return Token(std::move(_string), length, _location);
    }

    static constexpr Token make_operator(
        std::string_view _string, TokenKind _kind, SourceLocation _location)
    {
        return Token(_kind, _string, _location);
    }

    static constexpr Token make_keyword(
        std::string_view _string, TokenKind _kind, SourceLocation _location)
    {
        return Token(_kind, _string, _location);
    }

    static constexpr Token make_identifier(std::string_view _string, SourceLocation _location)
    {
        return Token(TokenKind::GroupIdentifier, _string, _location);
    }

    std::string_view string; // use for AstIdentifier?
    TokenKind kind = TokenKind::GroupEmpty;
    SourceLocation location{};

    // For strings
    std::shared_ptr<std::string> real_string;
    size_t real_length{};
};

struct Lexer {
    char get(size_t offset = 0) const
    {
        if (out_of_bounds(offset)) {
            return '\0';
        }
        return string[position + offset];
    }

    bool out_of_bounds(size_t offset = 0) const { return (position + offset >= string.length()); }

    size_t count_while(const std::predicate<char> auto &&cond, size_t offset = 0) const
    {
        size_t count = offset;
        while (position + count < string.length() && cond(get(count))) {
            ++count;
        }
        return count;
    }

    SourceLocation location() const { return { line, column, column + 1, position }; }

    struct UndoState {
        struct {
            size_t cache_size = 0;
            uint32_t position = 0;
            uint32_t column = 0;
        } _private;
    };

    void set_input(const std::string &filename);
    void free_input();

    File input;
    std::string_view string;
    uint32_t position = 0;
    uint32_t line = 1;
    uint32_t column = 0;
    std::stack<SourceLocation> last_lbrace;
    bool ignore_newlines = true;
};

std::string_view get_line(std::string_view source, uint32_t position_in_source);
std::string get_highlighted_line(std::string_view source, uint32_t position_in_source,
    uint32_t highlight_start, uint32_t highlight_end);

struct Compiler;

Token lex(Compiler &);

void expect(Compiler &, const std::string &exp, const Token &);
void expect(Compiler &, TokenKind, const Token &);

inline void advance_column(Lexer &lexer, size_t count = 1)
{
    lexer.position += count;
    lexer.column += count;
}

inline void advance_line(Lexer &lexer, size_t count = 1)
{
    lexer.line += count;
    lexer.position += count;
    lexer.column = 0;
}

Lexer::UndoState make_undo_point(Lexer &);
void undo_lex(Lexer &, Lexer::UndoState);

void consume(Lexer &, const Token &);
void consume_string(Lexer &, const Token &);

void consume_expected(Compiler &, const std::string &exp, const Token &);
void consume_expected(Compiler &, TokenKind, const Token &);

void consume_newline_or_eof(Compiler &, const Token &);
