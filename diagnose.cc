#include "compiler.hh"

void print_diag_line(std::string_view string, SourceLocation loc)
{
    const auto line_str = std::format("{} | ", loc.line);
    std::println("{}{}", line_str, get_line(string, loc.position));
    const std::string fill(loc.column + line_str.length(), '~');
    std::println("{}^", fill);
}

// FIXME - handle more cases
std::string make_printable(std::string_view s)
{
    if (s.empty()) {
        return "<eof>";
    }
    if (s == "\n") {
        return "<new line>";
    }
    if (s == " ") {
        return "<space>";
    }
    return std::string(s);
}

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
        case TokenKind::Fn:
            return "fn";
        case TokenKind::Return:
            return "return";
        case TokenKind::If:
            return "if";
        case TokenKind::Alias:
            return "alias";
        case TokenKind::False:
            return "false";
        case TokenKind::True:
            return "true";
        default:
            TODO();
    }
}
