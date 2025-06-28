#include "compiler.hh"

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
    constexpr const char ops[]{ "+-*/%(){}<>=!,:" };
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
        cc.diag_lexer_error("this character is not a digit in this base");
    }

    return Token{ .string = lexer.string.substr(start, count),
        .kind = TokenKind::Number,
        .location = lexer.location() };
}

const char *lex_operator_impl(Lexer &lexer)
{
    const auto c = lexer.get();
    const auto c2 = lexer.get(1);
    if (c2 == '=') {
        switch (c) {
            case '<':
                return "<=";
            case '>':
                return ">=";
            case '=':
                return "==";
            case '!':
                return "!=";
            case ':':
                return ":=";
        }
    } else if (c2 == '>') {
        if (c == '-') {
            return "->";
        }
    }
    switch (c) {
        case '+':
            return "+";
        case '-':
            return "-";
        case '*':
            return "*";
        case '/':
            return "/";
        case '%':
            return "%";
        case '(':
            return "(";
        case ')':
            return ")";
        case '{':
            return "{";
        case '}':
            return "}";
        case ',':
            return ",";
        case '<':
            return "<";
        case '>':
            return ">";
        case '=':
            return "=";
        case '!':
            return "!";
        case ':':
            return ":";
    }
    assert(!"lex_operator unhandled operator");
    return "";
}

Token lex_operator(Lexer &lexer)
{
    return Token{ .string = lex_operator_impl(lexer),
        .kind = TokenKind::Operator,
        .location = lexer.location() };
}

bool is_keyword(std::string_view str)
{
    constexpr const char *kws[]{ "fn", "return", "if", "alias", "false", "true" };
    for (const char *kw : kws) {
        if (str == kw) {
            return true;
        }
    }
    return false;
}

Token lex_identifier_or_keyword(Lexer &lexer)
{
    const auto loc = lexer.location();
    const auto start = lexer.position;
    const auto count = lexer.count_while(is_valid_char_in_identifier);
    const auto str = lexer.string.substr(start, count);
    if (is_keyword(str)) {
        return Token{ .string = str, .kind = TokenKind::Keyword, .location = loc };
    }
    return Token{ .string = str, .kind = TokenKind::Identifier, .location = loc };
}

Token lex_attribute(Compiler &cc)
{
    Lexer &lexer = cc.lexer;
    const auto loc = lexer.location();
    const auto start = lexer.position;
    const auto count = lexer.count_while(is_valid_char_in_identifier, 1);
    const auto str = lexer.string.substr(start, count);
    constexpr const char *attributes[]{ "#lexer_error", "#parser_error", "#verify_error",
        "#type_error" };
    for (auto *attr : attributes) {
        if (attr == str) {
            return Token{ .string = str, .kind = TokenKind::Attribute, .location = loc };
        }
    }
    cc.diag_lexer_error("unknown attribute");
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
                cc.diag_error_at(loc, ErrorType::LexError,
                    "unterminated comment starting at ({},{})", loc.line, loc.column + 1);
            }
        }
    }
}

void expect(Compiler &cc, const std::string &exp, const Token &tk)
{
    if (tk.string != exp) {
        const auto p1 = make_printable(exp);
        const auto p2 = make_printable(tk.string);
        cc.diag_lexer_error("expected `{}`, got `{}`", p1, p2);
    }
}

void consume_expected(Compiler &cc, const std::string &exp, const Token &tk)
{
    expect(cc, exp, tk);
    consume(cc.lexer, tk);
}

void consume_newline_or_eof(Compiler &cc, const Token &tk)
{
    if (tk.kind == TokenKind::Newline) {
        advance_line(cc.lexer);
    } else if (tk.kind == TokenKind::Empty) {
        consume(cc.lexer, tk);
    } else {
        const auto printable = make_printable(tk.string);
        cc.diag_lexer_error("expected `<new line>`, got `{}`.\n"
                            "only one statement per line is allowed.",
            printable);
    }
}

std::string_view Lexer::get_line(ssize_t pos) const
{
    ssize_t x = -1, start, end;
    // This is just get() but with pos as the base.
    auto get_at = [this, pos](ssize_t offset) {
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
        return Token{ .string = "\n", .kind = TokenKind::Newline, .location = lexer.location() };
    }
    if (c == '#') {
        return lex_attribute(cc);
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

    // TODO - do we want to also return the char?
    // how do we get here?
    return Token{ .kind = TokenKind::Unknown };
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
