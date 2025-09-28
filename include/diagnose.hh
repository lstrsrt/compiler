#pragma once

#include "base.hh"
#include "compiler.hh"
#include "frontend.hh"
#include "lexer.hh"
#include "testing.hh"
#include <format>

//
// Diagnostics
//

std::string to_string(TokenKind);

namespace diag {
    void print_line(std::string_view, SourceLocation);
    std::string make_printable(std::string_view);

    inline void print_error_header(Compiler &cc, SourceLocation location)
    {
        std::println("{}error:{} {}({},{}):", colors::Red + colors::Bold, colors::Default,
            cc.lexer.input.filename, location.line, location.column + 1);
    }

    inline void print_warning_header(Compiler &cc, SourceLocation location)
    {
        std::println("{}warning:{} {}({},{}):", colors::Yellow + colors::Bold, colors::Default,
            cc.lexer.input.filename, location.line, location.column + 1);
    }

    [[noreturn]] void error_at(
        Compiler &cc, SourceLocation location, ErrorType type, std::string_view fmt, auto &&...args)
    {
        const auto msg = std::vformat(fmt, std::make_format_args(args...));
        if (!opts.testing) {
            print_error_header(cc, location);
            std::println("{}", msg);
            print_line(cc.lexer.string, location);
#if _DEBUG
            __builtin_trap();
#else
            exit(1);
#endif
        } else {
            throw TestingException(msg.c_str(), type);
        }
    }

    [[noreturn]] void lexer_error(Compiler &cc, std::string_view fmt, auto &&...args)
    {
        error_at(cc, cc.lexer.location(), ErrorType::Lexer, fmt, args...);
    }

    void warning_at(
        Compiler &cc, SourceLocation location, std::string_view fmt, auto &&...args)
    {
        // TODO: maybe test warnings too?
        if (!opts.testing) {
            print_warning_header(cc, location);
            const auto msg = std::vformat(fmt, std::make_format_args(args...));
            std::println("{}", msg);
            print_line(cc.lexer.string, location);
        }
    }

    void ast_warning(Compiler &cc, Ast *ast, std::string_view fmt, auto &&...args)
    {
        warning_at(cc, ast->location, fmt, args...);
    }
}
