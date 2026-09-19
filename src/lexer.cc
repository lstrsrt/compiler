#include "lexer.hh"
#include "diagnose.hh"
#include "testing.hh"

#include <algorithm>
#include <array>

constexpr bool is_ascii(unsigned char c)
{
    return c >= 0x0 && c <= 0x7f;
}

constexpr bool is_valid_char_in_identifier(char c)
{
    return is_alpha(c) || is_digit(c) || c == '_' || c == '$';
}

constexpr bool is_start_of_operator(char c)
{
    constexpr std::array<char, 22> ops{ "+-*/%(){}<>=!,:#&|^~." };
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

std::stack<Token> token_cache{};

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

const char *lex_operator_impl(Compiler &cc, TokenKind &kind)
{
    auto &lexer = cc.lexer;

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
            case '+':
                kind = PlusEquals;
                return "+=";
            case '-':
                kind = MinusEquals;
                return "-=";
            case '*':
                kind = StarEquals;
                return "*=";
            case '/':
                kind = SlashEquals;
                return "/=";
            case '%':
                kind = PercentEquals;
                return "%=";
            case '&':
                kind = AmpersandEquals;
                return "&=";
            case '|':
                kind = BarEquals;
                return "|=";
            case '^':
                kind = CaretEquals;
                return "^=";
        }
    } else if (c2 == '>') {
        if (c == '-') {
            kind = Arrow;
            return "->";
        }
        if (c == '>') {
            const auto c3 = lexer.get(2);
            if (c3 == '>') {
                if (lexer.get(3) == '=') {
                    kind = TripleRAngleEquals;
                    return ">>>=";
                }
                kind = TripleRAngle;
                return ">>>";
            }
            if (c3 == '=') {
                kind = DoubleRAngleEquals;
                return ">>=";
            }
            kind = DoubleRAngle;
            return ">>";
        }
    } else if (c2 == '<') {
        if (c == '<') {
            const auto c3 = lexer.get(2);
            if (c3 == '<') {
                if (lexer.get(3) == '=') {
                    kind = TripleLAngleEquals;
                    return "<<<=";
                }
                kind = TripleLAngle;
                return "<<<";
            }
            if (c3 == '=') {
                kind = DoubleLAngleEquals;
                return "<<=";
            }
            kind = DoubleLAngle;
            return "<<";
        }
    } else if (c2 == ':') {
        if (c == ':') {
            kind = DoubleColon;
            return "::";
        }
    } else if (c2 == '.') {
        if (c == '.') {
            kind = DotDot;
            return "..";
        }
    } else {
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
            case '&':
                kind = Ampersand;
                return "&";
            case '|':
                kind = Bar;
                return "|";
            case '^':
                kind = Caret;
                return "^";
            case '~':
                kind = Tilde;
                return "~";
        }
    }
    auto location = lexer.location();
    location.end += lexer.count_while(is_graph);
    diag::lexer_error(cc, location, "unknown operator");
    return "";
}

Token lex_operator(Compiler &cc)
{
    TokenKind kind;
    std::string_view str = lex_operator_impl(cc, kind);
    auto location = SourceLocation::with_lexer(cc.lexer, str.size());
    if (kind == TokenKind::LBrace) {
        cc.lexer.last_lbrace.push(location);
    }
    return Token::make_operator(str, kind, location);
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
        { "for", For },
        { "in", In },
        { "alias", Alias },
        { "false", False },
        { "true", True },
        { "and", And },
        { "or", Or },
        { "continue", Continue },
        { "break", Break },
        { "null", Null },
        { "as", As },
        { "enum", Enum },
        { "record", Record },
    };
    if (auto it = keyword_map.find(str); it != keyword_map.end()) {
        return it->second;
    }
    return TokenKind::GroupIdentifier;
}

enum class Utf8Result {
    Invalid,
    OneByte,
    TwoBytes,
    ThreeBytes,
    FourBytes,
    End,
};

Utf8Result lex_utf8_char(const Lexer &lexer, size_t offset)
{
    const char c = lexer.get(offset);
    if (is_ascii(static_cast<unsigned char>(c))) [[likely]] {
        return Utf8Result::OneByte;
    }

    const auto uc = static_cast<uint8_t>(c);
    const auto uc2 = static_cast<uint8_t>(lexer.get(offset + 1));
    const auto uc3 = static_cast<uint8_t>(lexer.get(offset + 2));
    const auto uc4 = static_cast<uint8_t>(lexer.get(offset + 3));
    if (uc >= 0xc2 && uc <= 0xdf && uc2 >= 0x80 && uc2 <= 0xbf) {
        return Utf8Result::TwoBytes;
    }
    if ((uc3 >= 0x80 && uc3 <= 0xbf)
        && ((uc == 0xe0 && uc2 >= 0xa0 && uc2 <= 0xbf)
            || (uc >= 0xe1 && uc <= 0xec && uc2 >= 0x80 && uc2 <= 0xbf)
            || (uc == 0xed && uc2 >= 0x80 && uc2 <= 0x9f)
            || ((uc == 0xee || uc == 0xef) && uc2 >= 0x80 && uc2 <= 0xbf))) {
        return Utf8Result::ThreeBytes;
    }
    if ((uc4 >= 0x80 && uc4 <= 0xbf && uc3 >= 0x80 && uc3 <= 0xbf)
        && ((uc == 0xf0 && uc2 >= 0x90 && uc2 <= 0xbf)
            || (uc >= 0xf1 && uc <= 0xf3 && uc2 >= 0x80 && uc2 <= 0xbf)
            || (uc == 0xf4 && uc2 >= 0x80 && uc2 <= 0x8f))) {
        return Utf8Result::FourBytes;
    }
    return Utf8Result::Invalid;
}

Utf8Result lex_utf8_identifier_char(Compiler &cc, size_t offset)
{
    const auto ret = lex_utf8_char(cc.lexer, offset);
    if (ret == Utf8Result::OneByte && !is_valid_char_in_identifier(cc.lexer.get(offset))) {
        return Utf8Result::End;
    }
    return ret;
}

Token lex_string(Compiler &cc)
{
    Lexer &lexer = cc.lexer;
    auto loc = lexer.location();
    auto *str = new std::string;
    bool new_line = false;
    bool multi_line = lexer.get(1) == '"' && lexer.get(2) == '"';
    for (size_t i = multi_line ? 3 : 1; (multi_line || !new_line) && !lexer.out_of_bounds(i); ++i) {
        char c = lexer.get(i);
        switch (lex_utf8_char(lexer, i)) {
            case Utf8Result::Invalid:
                diag::warning_at(cc, loc, "invalid character `{}`", diag::make_printable(c));
                break;
            case Utf8Result::OneByte:
                if (c == '\\') {
                    switch (lexer.get(i + 1)) {
                        case '0':
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
                        case 'x':
                            str->append("\\x");
                            break;
                        case '\\':
                            str->push_back('\\');
                            break;
                        case '"':
                            str->push_back('\"');
                            break;
                        default: {
                            loc.column += i;
                            const auto seq
                                = diag::make_printable(std::string("\\") + lexer.get(i + 1));
                            loc.end = loc.column + seq.size();
                            diag::warning_at(cc, loc, "unknown escape sequence `{}`", seq);
                            str->push_back(lexer.get(i + 1));
                        }
                    }
                    ++i;
                    continue;
                } else if (c == '"') {
                    if (!multi_line || (lexer.get(i + 1) == '"' && lexer.get(i + 2) == '"')) {
                        const auto len = multi_line ? i + 3 : i + 1;
                        return Token::make_string(str, len, SourceLocation::with_lexer(lexer, len));
                    }
                } else if (c == '\r' || c == '\n') {
                    new_line = true;
                } else {
                    str->push_back(c);
                }
                break;
            case Utf8Result::TwoBytes:
                str->push_back(c);
                str->push_back(lexer.get(i + 1));
                i++;
                break;
            case Utf8Result::ThreeBytes:
                str->push_back(c);
                str->push_back(lexer.get(i + 1));
                str->push_back(lexer.get(i + 2));
                i += 2;
                break;
            case Utf8Result::FourBytes:
                str->push_back(c);
                str->push_back(lexer.get(i + 1));
                str->push_back(lexer.get(i + 2));
                str->push_back(lexer.get(i + 3));
                i += 3;
                break;
            case Utf8Result::End:
                std::unreachable();
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
    while (lexer.get() != '\n' && lexer.get() != '\0') {
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
    auto loc = lexer.location();
    size_t nesting = 1;
    advance_column(lexer, 2);
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
        diag::lexer_error(cc, tk.location, "expected `{}`, got `{}`", diag::make_printable(exp),
            diag::make_printable(tk.string));
    }
}

void expect(Compiler &cc, TokenKind kind, const Token &tk)
{
    if (tk.kind != kind) {
        diag::lexer_error(cc, tk.location, "expected `{}`, got `{}`", to_string(kind),
            diag::make_printable(tk.string));
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
        diag::lexer_error(cc, tk.location,
            "expected `<new line>`, got `{}`.\n"
            "only one statement per line is allowed.",
            diag::make_printable(tk.string));
    }
    assert(!token_cache.empty());
    token_cache.pop();
}

std::string_view get_line(std::string_view source, uint32_t position_in_source)
{
    assert(position_in_source < source.size());
    if (source[position_in_source] == '\n') {
        // If this char is a newline, we don't want to skip to the next line.
        --position_in_source;
    }
    auto pos = source.rfind('\n', position_in_source);
    if (pos != std::string_view::npos) {
        source = source.substr(pos + 1);
    }
    auto end = source.find_first_of('\n');
    return source.substr(0, end);
}

std::string get_highlighted_line(std::string_view source, uint32_t position_in_source,
    uint32_t highlight_start, uint32_t highlight_end)
{
    auto str = std::string(get_line(source, position_in_source));

    assert(highlight_start < highlight_end);
    // The highlight might be on a newline which get_line() cuts off,
    // so add 1 to compensate for that.
    assert(highlight_start < str.size() + 1);

    // Insert at `highlight_end` first so `highlight_start` doesn't have to be fixed
    if (highlight_end > str.size()) {
        str += colors::Default;
    } else {
        str.insert(highlight_end, colors::Default);
    }
    return str.insert(highlight_start, colors::Red);
}

Token lex_utf8_identifier(Compiler &cc)
{
    auto &lexer = cc.lexer;
    const auto start = lexer.position;
    size_t cp = 0;
    const auto count = [&] {
        size_t i = 0;
        for (;;) {
            switch (lex_utf8_identifier_char(cc, cp)) {
                case Utf8Result::Invalid:
                    diag::lexer_error(
                        cc, "invalid character `{}`", diag::make_printable(lexer.get(cp)));
                    break;
                case Utf8Result::OneByte:
                    cp++;
                    i++;
                    break;
                case Utf8Result::TwoBytes:
                    cp += 2;
                    i++;
                    break;
                case Utf8Result::ThreeBytes:
                    cp += 3;
                    i++;
                    break;
                case Utf8Result::FourBytes:
                    cp += 4;
                    i++;
                    break;
                case Utf8Result::End:
                    return i;
            }
        }
        return i;
    }();
    if (!count) {
        diag::lexer_error(cc, SourceLocation::with_lexer(lexer, 1), "invalid identifier");
    }
    if (count > MaxIdentifierLength) {
        diag::lexer_error(cc, SourceLocation::with_lexer(lexer, cp),
            "identifier is {} chars long, which exceeds the maximum allowed length of {}", count,
            MaxIdentifierLength);
    }
    const auto str = lexer.string.substr(start, cp);
    const auto kind = get_keyword_or_identifier_kind(str);
    if (kind == TokenKind::GroupIdentifier) {
        return Token::make_identifier(
            str, SourceLocation::with_lexer(lexer, static_cast<uint32_t>(str.size())));
    }
    return Token::make_keyword(
        str, kind, SourceLocation::with_lexer(lexer, static_cast<uint32_t>(str.size())));
}

Token lex_impl(Compiler &cc)
{
    auto &lexer = cc.lexer;

    skip_whitespace(lexer);
    skip_comments(cc);

    if (lexer.out_of_bounds()) {
        auto position = lexer.string.size() - 1;
        auto last_line = get_line(lexer.string, position);
        auto end = last_line.size() + 1;
        auto column = last_line.size();
        return Token::make_empty({
            .line = lexer.line,
            .column = static_cast<uint32_t>(column),
            .end = static_cast<uint32_t>(end),
            .position = static_cast<uint32_t>(position),
        });
    }

    const auto c = lexer.get();
    if (is_newline(lexer)) {
        return Token::make_newline(lexer.location());
    }
    if (c == '"') {
        return lex_string(cc);
    }
    if (is_start_of_operator(c)) {
        return lex_operator(cc);
    }
    if (is_digit(c)) {
        return lex_number(cc);
    }
    return lex_utf8_identifier(cc);
}

Token lex(Compiler &cc)
{
    if (token_cache.empty()) {
        token_cache.push(lex_impl(cc));
    }
    return token_cache.top();
}

void consume(Lexer &lexer, const Token &tk)
{
    assert(
        !is_group(tk.kind, TokenKind::GroupNewline) && !is_group(tk.kind, TokenKind::GroupString));
    advance_column(lexer, tk.string.length());
    assert(!token_cache.empty());
    token_cache.pop();
    if (tk.kind == TokenKind::RBrace) {
        assert(!lexer.last_lbrace.empty());
        lexer.last_lbrace.pop();
    }
}

void consume_string(Lexer &lexer, const Token &tk)
{
    assert(is_group(tk.kind, TokenKind::GroupString));
    advance_column(lexer, tk.real_length);
    assert(!token_cache.empty());
    token_cache.pop();
}

Lexer::UndoState make_undo_point(Lexer &lexer)
{
    return Lexer::UndoState{ ._private{
        .cache_size = token_cache.size(),
        .position = lexer.position,
        .column = lexer.column,
    } };
}

void undo_lex(Lexer &lexer, Lexer::UndoState undo_state)
{
    lexer.position = undo_state._private.position;
    lexer.column = undo_state._private.column;
    while (!token_cache.empty() && token_cache.size() <= undo_state._private.cache_size) {
        token_cache.pop();
    }
}

void Lexer::set_input(const std::string &filename)
{
    if (!input.open(filename, OpenFlags::Open | OpenFlags::READ)) {
        die("unable to open or read input file '{}'", filename);
    }
    string = input.view();
}

void Lexer::free_input()
{
    token_cache = {};
    input.close();
}
