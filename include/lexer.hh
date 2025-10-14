#pragma once

#include "file.hh"

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
    return is_lower(c) ? (c - ('a' - 'A')) : c;
}

enum class TokenKind : uint32_t {
    //
    // Group
    //
    GroupEmpty = 1 << 16,
    GroupNewline = 1 << 17,
    GroupNumber = 1 << 18,
    GroupIdentifier = 1 << 19,
    GroupString = 1 << 20,
    GroupOperator = 1 << 21,
    GroupKeyword = 1 << 22,
    group_mask = 0xffff'0000,

    //
    // Value
    //
    Plus = 1 | GroupOperator,
    Minus,
    Star,
    Slash,
    Percent,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Comma,
    Equals,
    Excl,
    Colon,
    Hash,
    Ampersand,
    LAngleEquals,
    RAngleEquals,
    EqualsEquals,
    ExclEquals,
    ColonEquals,
    Arrow,
    And,
    Or,

    Fn = 1 | GroupKeyword,
    Return,
    If,
    Else,
    While,
    Alias,
    False,
    True,
    Continue,
    Break,
    Null,
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
    static constexpr Token make_empty()
    {
        return Token{ .kind = TokenKind::GroupEmpty };
    }

    static constexpr Token make_newline(SourceLocation _location)
    {
        return Token{ .string = "\n",

            .kind = TokenKind::GroupNewline,
            .location = _location };
    }

    static constexpr Token make_number(std::string_view _string, SourceLocation _location)
    {
        return Token{ .string = _string, .kind = TokenKind::GroupNumber, .location = _location };
    }

    static constexpr Token make_string(
        std::unique_ptr<std::string> &&_string, size_t length, SourceLocation _location)
    {
        return Token{ .kind = TokenKind::GroupString,
            .location = _location,
            .real_string = std::move(_string),
            .real_length = length };
    }

    static constexpr Token make_operator(
        std::string_view _string, TokenKind _kind, SourceLocation _location)
    {
        return Token{ .string = _string, .kind = _kind, .location = _location };
    }

    static constexpr Token make_identifier(std::string_view _string, SourceLocation _location)
    {
        return Token{
            .string = _string, .kind = TokenKind::GroupIdentifier, .location = _location
        };
    }

    static constexpr Token make_keyword(
        std::string_view _string, TokenKind _kind, SourceLocation _location)
    {
        return Token{ .string = _string, .kind = _kind, .location = _location };
    }

    std::string_view string; // use for AstIdentifier?
    TokenKind kind = TokenKind::GroupEmpty;
    SourceLocation location{};

    // For strings
    std::unique_ptr<std::string> real_string;
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

    bool out_of_bounds(size_t offset = 0) const
    {
        return (position + offset >= string.length());
    }

    size_t count_while(const std::predicate<char> auto &&cond, size_t offset = 0) const
    {
        size_t count = offset;
        while (position + count < string.length() && cond(get(count))) {
            ++count;
        }
        return count;
    }

    SourceLocation location() const
    {
        return { line, column, column + 1, position };
    }

    void set_input(const std::string &filename);
    void free_input();

    File input;
    std::string_view string;
    uint32_t position = 0;
    uint32_t line = 1;
    uint32_t column = 0;
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

inline void consume(Lexer &lexer, const Token &tk)
{
    assert(
        !is_group(tk.kind, TokenKind::GroupNewline) && !is_group(tk.kind, TokenKind::GroupString));
    advance_column(lexer, tk.string.length());
}

inline void consume_string(Lexer &lexer, const Token &tk)
{
    assert(is_group(tk.kind, TokenKind::GroupString));
    advance_column(lexer, tk.real_length);
}

void consume_expected(Compiler &, const std::string &exp, const Token &);
void consume_expected(Compiler &, TokenKind, const Token &);

void consume_newline_or_eof(Compiler &, const Token &);
