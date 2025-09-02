#pragma once

#include "base.hh"
#include "compiler.hh"
#include "frontend.hh"
#include "lexer.hh"
#include "testing.hh"

//
// Diagnostics
//

void print_diag_line(std::string_view, SourceLocation);
std::string make_printable(std::string_view);
std::string to_string(TokenKind);

[[noreturn]] void diag_error_at(
    Compiler &cc, SourceLocation location, ErrorType type, std::string_view fmt, auto &&...args)
{
    const auto msg = std::vformat(fmt, std::make_format_args(args...));
    if (!opts.testing) {
        std::println("\033[31;1merror:\033[0m {}({},{}):", cc.lexer.input.filename, location.line,
            location.column);
        std::println("{}", msg);
        print_diag_line(cc.lexer.string, location);
#if _DEBUG
        __builtin_trap();
#else
        exit(1);
#endif
    } else {
        throw TestingException(msg.c_str(), type);
    }
}

[[noreturn]] void diag_lexer_error(Compiler &cc, std::string_view fmt, auto &&...args)
{
    diag_error_at(cc, cc.lexer.location(), ErrorType::Lexer, fmt, args...);
}

void diag_warning_at(Compiler &cc, SourceLocation location, std::string_view fmt, auto &&...args)
{
    // TODO: maybe test warnings too?
    if (!opts.testing) {
        std::println("\033[93;1mwarning:\033[0m {}({},{}):", cc.lexer.input.filename, location.line,
            location.column + 1);
        const auto msg = std::vformat(fmt, std::make_format_args(args...));
        std::println("{}", msg);
        print_diag_line(cc.lexer.string, location);
    }
}

void diag_ast_warning(Compiler &cc, Ast *ast, std::string_view fmt, auto &&...args)
{
    diag_warning_at(cc, ast->location, fmt, args...);
}
