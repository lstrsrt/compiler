#include "lexer.hh"
#include "diagnose.hh"
#include "testing.hh"

#include <algorithm>

constexpr bool is_start_of_identifier(char c)
{
    return is_alpha(c) || c == '_' || c == '$';
}

constexpr bool is_valid_char_in_identifier(char c)
{
    return is_alpha(c) || is_digit(c) || c == '_' || c == '$';
}

constexpr bool is_start_of_operator(char c)
{
    constexpr std::array<char, 17> ops{ "+-*/%(){}<>=!,:#" };
    return std::ranges::any_of(ops, [c](char x) { return c == x; });
}

constexpr bool is_newline(Lexer &lexer)
{
    auto c = lexer.get();
    return (c == '\n' || (c == '\r' && lexer.get(1) == '\n'));
}

SourceLocation SourceLocation::with_lexer(Lexer &lexer, uint32_t end_offset)
{
    return { lexer.line, lexer.column, lexer.column + end_offset, lexer.position };
}

enum class Radix {
    Bin,
    Dec,
    Hex,
};

// TODO - floats
Token lex_number(Compiler &cc)
{
    auto &lexer = cc.lexer;
    auto radix = Radix::Dec;
    const auto start = lexer.position;
    size_t count = 0;

    if (lexer.get() == '0') {
        ++count;
        if (to_upper(lexer.get(1)) == 'B') {
            ++count;
            radix = Radix::Bin;
        } else if (to_upper(lexer.get(1)) == 'X') {
            ++count;
            radix = Radix::Hex;
        }
    }

    bool seen_separator = false;
    count = lexer.count_while(
        [radix, &seen_separator](char c) {
            if (c == '_') {
                if (seen_separator) {
                    return false;
                }
                seen_separator = true;
                return true;
            }
            seen_separator = false;
            switch (radix) {
                case Radix::Bin:
                    return c == '0' || c == '1';
                case Radix::Dec:
                    return is_digit(c);
                case Radix::Hex:
                    return is_digit(c) || (to_upper(c) >= 'A' && to_upper(c) <= 'F');
            }
            return false;
        },
        count);

    const auto c = lexer.get(count);
    if (!is_space(c) && c != '\n' && c != '\r' && !is_start_of_operator(c)) {
        advance_column(lexer, count);
        diag::lexer_error(
            cc, "character `{}` is not a digit in this base", diag::make_printable(c));
    }

    return Token::make_number(lexer.string.substr(start, count),
        SourceLocation::with_lexer(lexer, static_cast<uint32_t>(count)));
}

const char *lex_operator_impl(Lexer &lexer, TokenKind &kind)
{
    using enum TokenKind;
    const auto c = lexer.get();
    const auto c2 = lexer.get(1);
    if (c2 == '=') {
        switch (c) {
            case '<':
                kind = LAngleEquals;
                return "<=";
            case '>':
                kind = RAngleEquals;
                return ">=";
            case '=':
                kind = EqualsEquals;
                return "==";
            case '!':
                kind = ExclEquals;
                return "!=";
            case ':':
                kind = ColonEquals;
                return ":=";
        }
    } else if (c2 == '>') {
        if (c == '-') {
            kind = Arrow;
            return "->";
        }
    }
    switch (c) {
        case '+':
            kind = Plus;
            return "+";
        case '-':
            kind = Minus;
            return "-";
        case '*':
            kind = Star;
            return "*";
        case '/':
            kind = Slash;
            return "/";
        case '%':
            kind = Percent;
            return "%";
        case '(':
            kind = LParen;
            return "(";
        case ')':
            kind = RParen;
            return ")";
        case '{':
            kind = LBrace;
            return "{";
        case '}':
            kind = RBrace;
            return "}";
        case '<':
            kind = LAngle;
            return "<";
        case '>':
            kind = RAngle;
            return ">";
        case ',':
            kind = Comma;
            return ",";
        case '=':
            kind = Equals;
            return "=";
        case '!':
            kind = Excl;
            return "!";
        case ':':
            kind = Colon;
            return ":";
        case '#':
            kind = Hash;
            return "#";
    }
    assert(!"lex_operator unhandled operator");
    return "";
}

Token lex_operator(Lexer &lexer)
{
    TokenKind kind;
    std::string_view str = lex_operator_impl(lexer, kind);
    return Token::make_operator(str, kind, SourceLocation::with_lexer(lexer, str.size()));
}

TokenKind get_keyword_or_identifier_kind(std::string_view str)
{
    using enum TokenKind;
    static const std::unordered_map<std::string_view, TokenKind> keyword_map{
        { "fn", Fn },
        { "return", Return },
        { "if", If },
        { "else", Else },
        { "while", While },
        { "alias", Alias },
        { "false", False },
        { "true", True },
        { "and", And },
        { "or", Or },
        { "continue", Continue },
        { "break", Break },
    };
    if (auto it = keyword_map.find(str); it != keyword_map.end()) {
        return it->second;
    }
    return TokenKind::GroupIdentifier;
}

Token lex_identifier_or_keyword(Compiler &cc)
{
    auto &lexer = cc.lexer;
    const auto start = lexer.position;
    const auto count = lexer.count_while(is_valid_char_in_identifier);
    if (count > MaxIdentifierLength) {
        diag::error_at(cc, SourceLocation::with_lexer(lexer, count), ErrorType::Lexer,
            "identifier is {} chars long, which exceeds the maximum allowed length of {}", count,
            MaxIdentifierLength);
    }
    const auto str = lexer.string.substr(start, count);
    const auto kind = get_keyword_or_identifier_kind(str);
    if (kind == TokenKind::GroupIdentifier) {
        return Token::make_identifier(
            str, SourceLocation::with_lexer(lexer, static_cast<uint32_t>(str.size())));
    }
    return Token::make_keyword(
        str, kind, SourceLocation::with_lexer(lexer, static_cast<uint32_t>(str.size())));
}

Token lex_string(Compiler &cc)
{
    Lexer &lexer = cc.lexer;
    auto loc = lexer.location();
    auto str = std::make_unique<std::string>();
    for (size_t i = 1; !lexer.out_of_bounds(i); ++i) {
        char c = lexer.get(i);
        if (c == '\\') {
            // TODO - string must be under x size?
            switch (lexer.get(i + 1)) {
                case '0':
                    // TODO other control chars
                    str->push_back('\0');
                    break;
                case 'a':
                    str->push_back('\a');
                    break;
                case 'b':
                    str->push_back('\b');
                    break;
                case 'e':
                    str->push_back('\e');
                    break;
                case 'f':
                    str->push_back('\f');
                    break;
                case 'n':
                    str->push_back('\n');
                    break;
                case 'r':
                    str->push_back('\r');
                    break;
                case 't':
                    str->push_back('\t');
                    break;
                case 'v':
                    str->push_back('\v');
                    break;
                case '\\':
                    str->push_back('\\');
                    break;
                case '"':
                    str->push_back('\"');
                    break;
                default:
                    // TODO - show warning
            }
            ++i;
        } else if (c == '"') {
            return Token::make_string(
                std::move(str), i + 1, SourceLocation::with_lexer(lexer, i + 1));
        } else {
            str->push_back(c);
        }
    }
    diag::error_at(cc, loc, ErrorType::Lexer, "unterminated string starting at ({},{})", loc.line,
        loc.column + 1);
}

void skip_whitespace(Lexer &lexer)
{
    if (lexer.ignore_newlines) {
        for (;;) {
            advance_column(lexer, lexer.count_while(is_space));
            if (is_newline(lexer)) {
                advance_line(lexer);
            } else {
                break;
            }
        }
    } else {
        advance_column(lexer, lexer.count_while(is_space));
    }
}

void skip_single_line_comment(Lexer &lexer)
{
    advance_column(lexer, 2);
    while (lexer.get() != '\n') {
        advance_column(lexer);
    }
    if (!lexer.ignore_newlines) {
        return;
    }
    advance_line(lexer);
    skip_whitespace(lexer);
}

void skip_multi_line_comment(Compiler &cc)
{
    auto &lexer = cc.lexer;
    size_t nesting = 0;
    auto loc = lexer.location();
    advance_column(lexer, 2);
    ++nesting;
    for (;;) {
        const char c = lexer.get();
        if (c == '/' && lexer.get(1) == '*') {
            ++nesting;
            advance_column(lexer, 2);
            continue;
        }
        if (c == '*' && lexer.get(1) == '/') {
            --nesting;
            advance_column(lexer, 2);
            if (nesting == 0) {
                return;
            }
            continue;
        }
        if (is_newline(lexer)) {
            advance_line(lexer);
            continue;
        }
        if (c == '\n') {
            advance_line(lexer);
            continue;
        }
        advance_column(lexer);
        if (lexer.out_of_bounds()) {
            ++loc.end; // Add one for the star
            diag::error_at(cc, loc, ErrorType::Lexer, "unterminated comment starting at ({},{})",
                loc.line, loc.column + 1);
        }
    }
}

void skip_comments(Compiler &cc)
{
    auto &lexer = cc.lexer;
    for (;;) {
        if (lexer.out_of_bounds()) {
            return;
        }
        if (lexer.get() != '/') {
            return;
        }
        if (lexer.get(1) == '/') {
            do {
                skip_single_line_comment(lexer);
                if (lexer.get() != '/') {
                    break;
                }
            } while (lexer.get(1) == '/');
        } else if (lexer.get(1) == '*') {
            skip_multi_line_comment(cc);
        } else {
            return;
        }
        skip_whitespace(lexer);
    }
}

void expect(Compiler &cc, const std::string &exp, const Token &tk)
{
    if (tk.string != exp) {
        diag::lexer_error(cc, "expected `{}`, got `{}`", diag::make_printable(exp),
            diag::make_printable(tk.string));
    }
}

void expect(Compiler &cc, TokenKind kind, const Token &tk)
{
    if (tk.kind != kind) {
        diag::lexer_error(
            cc, "expected `{}`, got `{}`", to_string(kind), diag::make_printable(tk.string));
    }
}

void consume_expected(Compiler &cc, const std::string &exp, const Token &tk)
{
    expect(cc, exp, tk);
    consume(cc.lexer, tk);
}

void consume_expected(Compiler &cc, TokenKind kind, const Token &tk)
{
    expect(cc, kind, tk);
    consume(cc.lexer, tk);
}

void consume_newline_or_eof(Compiler &cc, const Token &tk)
{
    if (is_group(tk.kind, TokenKind::GroupNewline)) {
        advance_line(cc.lexer);
    } else if (is_group(tk.kind, TokenKind::GroupEmpty)) {
        consume(cc.lexer, tk);
    } else {
        diag::lexer_error(cc,
            "expected `<new line>`, got `{}`.\n"
            "only one statement per line is allowed.",
            diag::make_printable(tk.string));
    }
}

std::string_view get_line(std::string_view source, uint32_t position_in_source)
{
    int32_t x = -1, start, end;
    // This is just get() but with `position_in_source` as the base.
    auto get_at = [source, position_in_source](uint32_t offset) {
        auto off = position_in_source + offset;
        if (off >= source.length()) {
            return '\0';
        }
        return source[position_in_source + offset];
    };
    while (get_at(x) != '\n' && get_at(x) != '\0') {
        --x;
    }
    start = static_cast<int32_t>(position_in_source) + x;
    x = 0;
    while (get_at(x) != '\n' && get_at(x) != '\0') {
        ++x;
    }
    end = static_cast<int32_t>(position_in_source) + x;
    return source.substr(start + 1, end - start - 1);
}

std::string get_highlighted_line(std::string_view source, uint32_t position_in_source,
    uint32_t highlight_start, uint32_t highlight_end)
{
    auto str = std::string(get_line(source, position_in_source));
    assert(highlight_start < highlight_end);
    assert(highlight_start < str.size());
    // Insert at `highlight_end` first so the highlight_start position doesn't have to be fixed
    if (highlight_end > str.size()) {
        str += colors::Default;
    } else {
        str.insert(highlight_end, colors::Default);
    }
    return str.insert(highlight_start, colors::Red);
}

Token lex(Compiler &cc)
{
    auto &lexer = cc.lexer;

    skip_whitespace(lexer);
    skip_comments(cc);
    skip_whitespace(lexer);

    if (lexer.out_of_bounds()) {
        return Token::make_empty();
    }

    const auto c = lexer.get();
    if (is_newline(lexer)) {
        return Token::make_newline(lexer.location());
    }
    if (c == '"') {
        return lex_string(cc);
    }
    if (is_start_of_operator(c)) {
        return lex_operator(lexer);
    }
    if (is_start_of_identifier(c)) {
        return lex_identifier_or_keyword(cc);
    }
    if (is_digit(c)) {
        return lex_number(cc);
    }

    diag::lexer_error(cc, "unknown character `{}`", diag::make_printable(c));
}

void Lexer::set_input(const std::string &filename)
{
    if (!input.open(filename, OpenFlags::Open | OpenFlags::READ)) {
        die("unable to open or read input file '{}'", filename);
    }
    string = input.map;
}

void Lexer::free_input()
{
    input.close();
}
