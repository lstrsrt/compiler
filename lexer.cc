#include "compiler.hh"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

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
    constexpr const char ops[]{ "+-*/%(){}<>=!,:#" };
    for (char x : ops) {
        if (c == x) {
            return true;
        }
    }
    return false;
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
    if (!is_space(c) && c != '\n' && !is_start_of_operator(c)) {
        advance_column(lexer, count);
        diag_lexer_error(cc, "this character is not a digit in this base");
    }

    return Token::make_number(lexer.string.substr(start, count), lexer.location());
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
    const auto str = lex_operator_impl(lexer, kind);
    return Token::make_operator(str, kind, lexer.location());
}

TokenKind get_keyword_or_identifier_kind(std::string_view str)
{
    using enum TokenKind;
    // clang-format off
    static std::unordered_map<std::string_view, TokenKind> keyword_map{
        { "fn", Fn },
        { "return", Return },
        { "if", If },
        { "alias", Alias },
        { "false", False },
        { "true", True }
    };
    // clang-format on
    if (auto it = keyword_map.find(str); it != keyword_map.end()) {
        return it->second;
    }
    return TokenKind::GroupIdentifier;
}

Token lex_identifier_or_keyword(Lexer &lexer)
{
    const auto loc = lexer.location();
    const auto start = lexer.position;
    const auto count = lexer.count_while(is_valid_char_in_identifier);
    const auto str = lexer.string.substr(start, count);
    const auto kind = get_keyword_or_identifier_kind(str);
    if (kind == TokenKind::GroupIdentifier) {
        return Token::make_identifier(str, loc);
    }
    return Token::make_keyword(str, kind, loc);
}

Token lex_string(Compiler &cc)
{
    Lexer &lexer = cc.lexer;
    const auto loc = lexer.location();
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
            return Token::make_string(std::move(str), i + 1, loc);
        } else {
            str->push_back(c);
        }
    }
    diag_error_at(cc, loc, ErrorType::Lexer, "unterminated string starting at ({},{})", loc.line,
        loc.column + 1);
}

void skip_whitespace(Lexer &lexer)
{
    if (lexer.ignore_newlines) {
        for (;;) {
            advance_column(lexer, lexer.count_while(is_space));
            const char c = lexer.get();
            if (c == '\n') { // FIXME handle CRLF
                advance_line(lexer);
            } else {
                break;
            }
        }
    } else {
        advance_column(lexer, lexer.count_while(is_space));
    }
}

void skip_comments(Compiler &cc)
{
    auto &lexer = cc.lexer;

    if (lexer.out_of_bounds()) {
        return;
    }
    if (lexer.get() != '/') {
        return;
    }

    if (lexer.get(1) == '/') {
        advance_column(lexer, 2);
        while (lexer.get() != '\n') {
            advance_column(lexer);
        }
        if (!lexer.ignore_newlines) {
            return;
        }
        advance_line(lexer);
    }

    size_t nesting = 0;
    if (lexer.get(1) == '*') {
        const auto loc = lexer.location();
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
            if (c == '\n') {
                advance_line(lexer);
                continue;
            }
            advance_column(lexer);
            if (lexer.out_of_bounds()) {
                diag_error_at(cc, loc, ErrorType::Lexer, "unterminated comment starting at ({},{})",
                    loc.line, loc.column + 1);
            }
        }
    }
}

void expect(Compiler &cc, const std::string &exp, const Token &tk)
{
    if (tk.string != exp) {
        const auto p1 = make_printable(exp);
        const auto p2 = make_printable(tk.string);
        diag_lexer_error(cc, "expected `{}`, got `{}`", p1, p2);
    }
}

void expect(Compiler &cc, TokenKind kind, const Token &tk)
{
    if (tk.kind != kind) {
        const auto p = make_printable(tk.string);
        diag_lexer_error(cc, "expected `{}`, got `{}`", to_string(kind), p);
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
        const auto printable = make_printable(tk.string);
        diag_lexer_error(cc,
            "expected `<new line>`, got `{}`.\n"
            "only one statement per line is allowed.",
            printable);
    }
}

std::string_view get_line(std::string_view string, ssize_t pos)
{
    ssize_t x = -1, start, end;
    // This is just get() but with pos as the base.
    auto get_at = [string, pos](ssize_t offset) {
        auto off = pos + offset;
        if (off < 0 || off >= static_cast<ssize_t>(string.length())) {
            return '\0';
        }
        return string[pos + offset];
    };
    while (get_at(x) != '\n' && get_at(x) != '\0') {
        --x;
    }
    start = pos + x;
    x = 0;
    while (get_at(x) != '\n' && get_at(x) != '\0') {
        ++x;
    }
    end = pos + x;
    return string.substr(start + 1, end - start - 1);
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
    // FIXME - handle CRLF
    if (c == '\n') {
        return Token::make_newline(lexer.location());
    }
    if (c == '"') {
        return lex_string(cc);
    }
    if (is_start_of_operator(c)) {
        return lex_operator(lexer);
    }
    if (is_start_of_identifier(c)) {
        return lex_identifier_or_keyword(lexer);
    }
    if (is_digit(c)) {
        return lex_number(cc);
    }

    diag_lexer_error(cc, "unknown character `{}`", c);
}

void InputFile::open(std::string_view name)
{
    this->filename = name;
    this->file_handle = ::open(name.data(), O_RDONLY);
    if (file_handle == -1) {
        perror("open");
        exit(1);
    }
    struct stat stat;
    if (fstat(file_handle, &stat) < 0) {
        perror("fstat");
        ::close(file_handle);
        exit(1);
    }
    this->map = static_cast<char *>(mmap(0, stat.st_size, PROT_READ, MAP_SHARED, file_handle, 0));
    if (map == MAP_FAILED) {
        perror("mmap");
        ::close(file_handle);
        exit(1);
    }
    file_size = stat.st_size;
}

void InputFile::close()
{
    if (file_handle != -1) {
        munmap(const_cast<char *>(map), file_size);
        ::close(file_handle);
        filename = {};
        file_handle = -1;
        file_size = 0;
    }
}

void Lexer::set_input(std::string_view filename)
{
    input.open(filename);
    if (input.file_handle != -1) {
        string = input.map;
    }
}

void Lexer::free_input()
{
    input.close();
}
