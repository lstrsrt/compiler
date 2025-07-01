#include "compiler.hh"

void process_cmdline(ArgumentParser &ap)
{
    using namespace std::string_view_literals;

    for (std::string_view s : ap.arguments) {
        switch (fnv1a_hash(s.data())) {
            case fnv1a_hash("--test"sv):
                opts.testing = true;
                break;
        }
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        std::println("USAGE: compiler [switches...] <filename>");
        exit(1);
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
        run_tests(argv[2]);
    } else {
        Compiler cc;
        cc.lexer.set_input(argv[1]);
        // Top level scope is main
        // TODO - give main argc and argv
        auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
        compiler_main(cc, main);
        cc.cleanup(main, true);
    }
}
