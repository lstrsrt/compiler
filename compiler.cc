#include "compiler.hh"

#if !TESTING && defined(_DEBUG)
#define DEBUG_SPAM 1
#else
#define DEBUG_SPAM 0
#endif

void compiler_main(Compiler &cc, AstFunctionDecl *main)
{
    Timer timer;
    cc.initialize();
    dbgln("compiling {}{}{}", default_bold, cc.lexer.input.filename, default_clr);
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

    [[maybe_unused]] auto frontend = timer.elapsed();
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
    [[maybe_unused]] auto backend = timer.elapsed();

    cc.lexer.free_input();
    cc.cleanup(main);

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

#if !TESTING
    std::println("{}elapsed time:{}", cyan, default_clr);
    std::println("frontend:    {}{:>6}{}", green, frontend, default_clr);
    std::println("backend:     {}{:>6}{}", green, backend, default_clr);
    std::println("total:       {}{:>6}{}", green, frontend + backend, default_clr);
#endif
}

void Compiler::initialize()
{
    enter_new_scope();
    add_default_types();
}

// can we have builtin types somewhere else so lookup during verification stage is faster?
void Compiler::add_default_types()
{
    auto *global_scope = g_scopes[0];
    global_scope->types["void"] = void_type();
    global_scope->types["u32"] = u32_type();
    global_scope->types["u64"] = u64_type();
    global_scope->types["s32"] = s32_type();
    global_scope->types["s64"] = s64_type();
    global_scope->types["bool"] = bool_type();
}

void Compiler::free_types()
{
    for (auto *scope : g_scopes) {
        for (auto &[name, ptr] : scope->types) {
            if (!ptr->has_flag(TypeFlags::BUILTIN)) {
                delete ptr;
            }
        }
    }
}

void Compiler::free_ir()
{
    for (auto *fn : ir_builder.functions) {
        ::free_ir(fn);
    }
    ir_builder.functions.clear();
}

void Compiler::cleanup(AstFunctionDecl *root)
{
    free_ast(root);
    leave_scope();
    free_types();
    free_ir();
    free_scopes();
}
