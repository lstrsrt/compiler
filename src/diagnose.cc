#include "diagnose.hh"
#include "base.hh"

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
    if (c != ' ' && std::isprint(static_cast<unsigned char>(c))) {
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
    switch (kind) {
        case TokenKind::Plus:
            return "+";
        case TokenKind::Minus:
            return "-";
        case TokenKind::Star:
            return "*";
        case TokenKind::Slash:
            return "/";
        case TokenKind::Percent:
            return "%";
        case TokenKind::LParen:
            return "(";
        case TokenKind::RParen:
            return ")";
        case TokenKind::LBrace:
            return "{";
        case TokenKind::RBrace:
            return "}";
        case TokenKind::LAngle:
            return "<";
        case TokenKind::RAngle:
            return ">";
        case TokenKind::Comma:
            return ",";
        case TokenKind::Equals:
            return "=";
        case TokenKind::Excl:
            return "!";
        case TokenKind::Colon:
            return ":";
        case TokenKind::Hash:
            return "#";
        case TokenKind::LAngleEquals:
            return "<=";
        case TokenKind::RAngleEquals:
            return ">=";
        case TokenKind::EqualsEquals:
            return "==";
        case TokenKind::ExclEquals:
            return "!=";
        case TokenKind::ColonEquals:
            return ":=";
        case TokenKind::Arrow:
            return "->";
        case TokenKind::And:
            return "and";
        case TokenKind::Or:
            return "or";
        case TokenKind::Fn:
            return "fn";
        case TokenKind::Return:
            return "return";
        case TokenKind::If:
            return "if";
        case TokenKind::Else:
            return "else";
        case TokenKind::While:
            return "while";
        case TokenKind::Alias:
            return "alias";
        case TokenKind::False:
            return "false";
        case TokenKind::True:
            return "true";
        case TokenKind::Continue:
            return "continue";
        case TokenKind::Break:
            return "break";
        case TokenKind::Null:
            return "null";
        default:
            TODO();
    }
}
