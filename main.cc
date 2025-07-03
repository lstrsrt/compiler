#include "compiler.hh"

[[noreturn]] void usage(int errc)
{
    std::println("usage: compiler [switches...] <filename>");
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
                usage(0);
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
        usage(1);
    }

    ArgumentParser arg_parser;
    arg_parser.arguments = std::span<char *>(argv, argc);
    process_cmdline(arg_parser);

    if (opts.testing) {
        auto dir = arg_parser.arguments.back();
        if (!fs::exists(dir) || !fs::is_directory(dir)) {
            std::println("not a directory");
            exit(1);
        }
        run_tests(dir);
    } else {
        Compiler cc;
        cc.lexer.set_input(arg_parser.arguments.back());
        // Top level scope is main
        // TODO - give main argc and argv
        auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
        compiler_main(cc, main);
        cc.cleanup(main);
        cc.lexer.free_input();
    }
}
