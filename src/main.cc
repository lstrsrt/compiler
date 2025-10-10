#include "arena-alloc/arena_alloc.h"
#include "compiler.hh"
#include "frontend.hh"

int main(int argc, char **argv)
{
    if (argc < 2) {
        usage(EXIT_FAILURE);
    }

    ArgumentParser arg_parser;
    arg_parser.arguments = std::span<char *>(argv, argc);
    process_cmdline(arg_parser);
    determine_output_names();

    if (opts.testing) {
        auto *dir_or_file = arg_parser.arguments.back();
        if (!fs::exists(dir_or_file)) {
            die("test file or directory '{}' not found", dir_or_file);
        }
        if (fs::is_regular_file(dir_or_file)) {
            run_single_test(dir_or_file);
        } else if (fs::is_directory(dir_or_file)) {
            run_tests(dir_or_file);
        } else {
            die("input '{}' is not a file or directory");
        }
    } else {
        Compiler cc;
        auto *maybe_file = arg_parser.arguments.back();
        if (!fs::exists(maybe_file)) {
            die("input '{}' not found", maybe_file);
        }
        if (!fs::is_regular_file(maybe_file)) {
            die("input '{}' is not a file", maybe_file);
        }
        cc.lexer.set_input(maybe_file);
        // Top level scope is main
        // TODO: give main argc and argv
#ifdef AST_USE_ARENA
        arena::ArenaAllocator<void> allocator(ast_arena());
        StmtVec stmts(allocator);
        VariableDecls params(allocator);
#else
        StmtVec stmts;
        VariableDecls params;
#endif
        auto *main = new AstFunction("main", s32_type(), params, new AstBlock(stmts), {});
        compiler_main(cc, main);
        cc.cleanup(main);
    }
}
