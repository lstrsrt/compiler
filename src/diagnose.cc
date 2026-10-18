#include "diagnose.hh"
#include "base.hh"
#include "lexer.hh"

namespace diag {
void print_line(std::string_view source, SourceLocation location)
{
    if (opts.testing) {
        return;
    }

    using namespace colors;
    const auto line_str = std::format("{}{}{} | ", Blue, location.line, Default);
    const auto fill_width = line_str.size() - (Blue + Default).size();
    // A multi line string's end column is on the line it ends, so only highlight its first line.
    auto end = location.end;
    if (const auto line_end = source.find('\n', location.position);
        line_end != std::string_view::npos) {
        const auto rest = static_cast<uint32_t>(line_end - location.position);
        if (end <= location.column || end - location.column > rest) {
            end = location.column + std::max(rest, 1U);
        }
    }
    std::println(stderr, "{}{}", line_str,
        get_highlighted_line(source, location.position, location.column, end));
    const std::string fill(location.column + fill_width, '~');
    std::println(stderr, "{}{}^{}", fill, Yellow, Default);
}

std::string make_printable(char c)
{
    if (is_graph(c)) {
        return std::string(1, c);
    }

    switch (c) {
        // Return a string instead of a hex byte for more common cases
        case '\t':
            return "<tab>";
        case '\n':
            return "<new line>";
        case '\r':
            return "<carriage return>";
        case ' ':
            return "<space>";
        case 0:
            return "<NUL>";
        default:
            return std::format("\\x{:02X}", static_cast<uint8_t>(c));
    }
}

std::string make_printable(std::string_view s)
{
    if (s.empty()) {
        return "<EOF>";
    }
    if (s.length() == 1) {
        return make_printable(s[0]);
    }
    return std::string(s);
}

std::string make_printable(const Token &tk)
{
    // String tokens don't set `string`, which would otherwise print as <EOF>.
    if (is_group(tk.kind, TokenKind::GroupString)) {
        return "<string>";
    }
    return make_printable(tk.string);
}
} // namespace diag

std::string to_string(TokenKind kind)
{
    using enum TokenKind;
#define __ENUMERATE_OPERATOR_TOKEN(name, string) \
    case name:                                   \
        return string;
#define __ENUMERATE_KEYWORD_TOKEN(name, string) \
    case name:                                  \
        return string;

    switch (kind) {
        ENUMERATE_OPERATOR_TOKENS()
        ENUMERATE_KEYWORD_TOKENS()
        default:
            TODO();
    }

#undef __ENUMERATE_OPERATOR_TOKEN
#undef __ENUMERATE_KEYWORD_TOKEN
}

std::string to_string(Type *type)
{
    auto *real = get_unaliased_type(type);
    auto size = real->byte_size();
    std::string signed_str;
    std::string plural = size == 1 ? "" : "s";

    if (real->is_int()) {
        signed_str = real->is_signed() ? "signed " : "unsigned ";
    }
    if (real == type) {
        return std::format("`{}` ({}{} byte{})", real->get_name(), signed_str, size, plural);
    }

    return std::format("`{}` (alias of `{}`, {}{} byte{})", type->get_name(), real->get_name(),
        signed_str, size, plural);
}
