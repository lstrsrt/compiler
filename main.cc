#include "compiler.hh"

int main(int argc, char **argv)
{
    if (argc != 2) {
        std::println("USAGE: compiler <filename>");
        exit(1);
    }
#if TESTING
    Compiler cc;
    if (!fs::exists(argv[1]) || !fs::is_directory(argv[1])) {
        std::println("not a directory");
        exit(1);
    }
    run_tests(argv[1]);
#else
    Compiler cc;
    cc.lexer.set_input(argv[1]);
    // Top level scope is main
    // TODO - give main argc and argv
    auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
    compiler_main(cc, main);
    cc.cleanup(main, true);
#endif
}
