#include "compiler.hh"
#include "frontend.hh"

[[noreturn]] void usage(int errc)
{
    std::println("usage: compiler [switches...] <filename>");
    std::println("COMMANDS:\n"
                 "\t-h, --help: print this message");
    std::println("OPTIONS:\n"
                 "\t--check-only: do not compile input, only check validity\n"
                 "\t--test: run in test mode");
    exit(errc);
}

void process_cmdline(ArgumentParser &ap)
{
    for (size_t i = 1; i < ap.arguments.size(); ++i) {
        switch (hash(ap.arguments[i])) {
            case hash("--test"):
                opts.testing = true;
                break;
            case hash("--check-only"):
                opts.check_only = true;
                break;
            case hash("-h"):
                [[fallthrough]];
            case hash("--help"):
                usage(EXIT_SUCCESS);
                break;
            default:
                if (i < ap.arguments.size() - 1) {
                    std::println(
                        "{}unknown argument{} '{}'", colors::Red, colors::Default, ap.arguments[i]);
                }
        }
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        usage(EXIT_FAILURE);
    }

    ArgumentParser arg_parser;
    arg_parser.arguments = std::span<char *>(argv, argc);
    process_cmdline(arg_parser);

    if (opts.testing) {
        auto *dir_or_file = arg_parser.arguments.back();
        if (!fs::exists(dir_or_file)) {
            die("test file or directory '{}' not found", dir_or_file);
        }
        if (fs::is_regular_file(dir_or_file)) {
            run_single_test(dir_or_file);
        } else if (fs::is_directory(dir_or_file)) {
            run_tests(dir_or_file);
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
        auto *main = new AstFunction("main", s32_type(), {}, new AstBlock({}), {});
        compiler_main(cc, main);
        cc.cleanup(main);
        cc.lexer.free_input();
    }
}
