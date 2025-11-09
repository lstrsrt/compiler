#pragma once

#include "base.hh"
#include "compiler.hh"
#include "frontend.hh"
#include "lexer.hh"
#include "testing.hh"

//
// Diagnostics
//

std::string to_string(TokenKind);

namespace diag {
// This does not get reset because we don't support continuing after an error.
inline bool did_prepare_error = false;
// This does get reset.
inline bool did_prepare_last_warning = false;

void print_line(std::string_view source, SourceLocation);
std::string make_printable(std::string_view);
std::string make_printable(char);

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
        if (!diag::did_prepare_error) {
            print_error_header(cc, location);
        }
        std::println("{}", msg);
        print_line(cc.lexer.string, location);
#if _DEBUG
        __builtin_trap();
#else
        exit(EXIT_FAILURE);
#endif
    } else {
        throw TestingException(msg.c_str(), type);
    }
}

[[noreturn]] void lexer_error(
    Compiler &cc, SourceLocation loc, std::string_view fmt, auto &&...args)
{
    error_at(cc, loc, ErrorType::Lexer, fmt, args...);
}

[[noreturn]] void lexer_error(Compiler &cc, std::string_view fmt, auto &&...args)
{
    error_at(cc, cc.lexer.location(), ErrorType::Lexer, fmt, args...);
}

void warning_at(Compiler &cc, SourceLocation location, std::string_view fmt, auto &&...args)
{
    // TODO: maybe test warnings too?
    if (opts.testing) {
        return;
    }

    if (!did_prepare_last_warning) {
        print_warning_header(cc, location);
    } else {
        did_prepare_last_warning = false;
    }
    const auto msg = std::vformat(fmt, std::make_format_args(args...));
    std::println("{}", msg);
    print_line(cc.lexer.string, location);
}

void ast_warning(Compiler &cc, Ast *ast, std::string_view fmt, auto &&...args)
{
    warning_at(cc, ast->location, fmt, args...);
}

void prepare_error(Compiler &cc, SourceLocation location, std::string_view fmt, auto &&...args)
{
    diag::did_prepare_error = true;
    if (!opts.testing) {
        print_error_header(cc, location);
        std::println("{}", std::vformat(fmt, std::make_format_args(args...)));
    }
}

void prepare_warning(Compiler &cc, SourceLocation location, std::string_view fmt, auto &&...args)
{
    diag::did_prepare_last_warning = true;
    if (!opts.testing) {
        print_warning_header(cc, location);
        std::println("{}", std::vformat(fmt, std::make_format_args(args...)));
    }
}
} // namespace diag
