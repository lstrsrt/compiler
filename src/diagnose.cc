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
    std::println("{}{}", line_str,
        get_highlighted_line(source, location.position, location.column, location.end));
    const std::string fill(location.column + fill_width, '~');
    std::println("{}{}^{}", fill, Yellow, Default);
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
            return std::format("\\x{:X}", static_cast<uint32_t>(c));
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
