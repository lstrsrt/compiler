#include "compiler.hh"

constexpr std::string cyan = "\033[36;1m";
constexpr std::string default_clr = "\033[0m";
constexpr std::string green = "\033[32;1m";
constexpr std::string red = "\033[31;1m";

int main(int argc, char **argv)
{
    if (argc != 2) {
        std::println("USAGE: compiler <filename>");
        exit(1);
    }
    Compiler cc;
    Timer timer;
    cc.lexer.set_input(argv[1]);
    enter_new_scope();
    cc.add_default_types();
    // Top level scope is main
    // TODO - give main argc and argv
    auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
    while (!cc.lexer.out_of_bounds()) {
        if (auto *ast = parse_stmt(cc, main)) {
            main->body->stmts.push_back(ast);
        }
    }
#ifdef _DEBUG
    dbgln("{}pre inference tree:{}", cyan, default_clr);
    print_ast(main);
#endif
    verify_main(cc, main);
#ifdef _DEBUG
    dbgln("{}post inference tree:{}", cyan, default_clr);
    print_ast(main);
    /*dbgln("{}types:{}", cyan, default_clr);
    for (auto *scope : g_scopes) {
        print_types(scope);
    }*/
#endif
    auto frontend = timer.elapsed();
    generate_ir(cc, main);
#ifdef _DEBUG
    dbgln("{}initial ir:{}", cyan, default_clr);
    for (const auto *fn : cc.ir_builder.functions) {
        dbgln("{}:", fn->ast->name);
        print_ir(*fn);
    }
#endif
    // optimize_ir(cc);
#ifdef _DEBUG
    /*dbgln("{}optimized ir:{}", cyan, default_clr);
    for (const auto &fn : cc.ir_builder.functions) {
        dbgln("{}:", fn.ast->name);
        print_ir(fn.code);
    }*/
#endif
    timer.reset();
    dbgln("{}assembly:{}", cyan, default_clr);
    emit_asm(cc);
    auto backend = timer.elapsed();
    leave_scope();
    cc.lexer.free_input();
    free_ast(main);
    cc.free_types();
    cc.free_ir();
    free_scopes();
#if 0
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

#if 0
template <class T>
struct TemporaryChange {
    TemporaryChange(T &_variable, T value) : variable(_variable), original(std::move(value)) {
        variable = std::move(value);
    }

    ~TemporaryChange() {
        variable = std::move(original);
    }

    T &variable;
    T original;
};
#endif
