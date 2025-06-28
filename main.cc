#include "compiler.hh"

#if TESTING
#include <filesystem>
#include <set>
namespace fs = std::filesystem;
#endif

constexpr std::string cyan = "\033[36;1m";
constexpr std::string default_clr = "\033[0m";
constexpr std::string green = "\033[32;1m";
constexpr std::string red = "\033[31;1m";

#if !TESTING && defined(_DEBUG)
#define DEBUG_SPAM 1
#else
#define DEBUG_SPAM 0
#endif

void compiler_main(Compiler &cc)
{
    Timer timer;
    cc.initialize();
    dbgln("compiling {}", cc.lexer.input.filename);

    // Top level scope is main
    // TODO - give main argc and argv
    auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
    while (!cc.lexer.out_of_bounds()) {
        if (auto *ast = parse_stmt(cc, main)) {
            main->body->stmts.push_back(ast);
        }
    }

#if DEBUG_SPAM
    dbgln("{}pre inference tree:{}", cyan, default_clr);
    print_ast(main);
#endif

    verify_main(cc, main);

#if DEBUG_SPAM
    dbgln("{}post inference tree:{}", cyan, default_clr);
    print_ast(main);
    /*dbgln("{}types:{}", cyan, default_clr);
    for (auto *scope : g_scopes) {
        print_types(scope);
    }*/
#endif

    auto frontend = timer.elapsed();
    timer.reset();

    generate_ir(cc, main);

#if DEBUG_SPAM
    dbgln("{}initial ir:{}", cyan, default_clr);
    for (const auto *fn : cc.ir_builder.functions) {
        dbgln("{}:", fn->ast->name);
        print_ir(*fn);
    }
#endif

#if DEBUG_SPAM
    dbgln("{}assembly:{}", cyan, default_clr);
#endif

    emit_asm(cc);
    auto backend = timer.elapsed();

    cc.lexer.free_input();
    free_ast(main);
    cc.cleanup();

#if AST_ALLOC_PARANOID
    dbgln("=== alloced: {}", ast_alloc_count);
    dbgln("=== freed:   {}", ast_free_count);
    for (const auto &mark : marks) {
        const std::string &clr = mark.deleted ? green : red;
        dbgln("{}{}{} ({})", clr, mark.p, default_clr, mark.size);
        if (!mark.deleted) {
            dbgln("alloced from:");
            char **syms = ::backtrace_symbols(mark.trace.data(), mark.frames);
            for (int i = 0; i < mark.frames; ++i) {
                dbgln("{}:    {}", i, syms[i]);
            }
            free(syms);
        }
    }
#endif

    std::println("{}elapsed time:{}", cyan, default_clr);
    std::println("frontend:    {}{:>6}{}", green, frontend, default_clr);
    std::println("backend:     {}{:>6}{}", green, backend, default_clr);
    std::println("total:       {}{:>6}{}", green, frontend + backend, default_clr);
}

#if TESTING
void run_tests(const fs::path &dir)
{
    std::set<fs::path> sorted;
    for (auto &it : fs::directory_iterator(dir)) {
        if (fs::is_regular_file(it) && it.path().has_filename() && it.path().has_extension()
            && it.path().extension() == ".txt") {
            sorted.insert(it.path());
        }
    }
    for (auto &file : sorted) {
        Compiler cc;
        cc.lexer.set_input(file.string());
        compiler_main(cc);
    }
}
#endif

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
    compiler_main(cc);
#endif
}
