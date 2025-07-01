#include "compiler.hh"

void compiler_main(Compiler &cc, AstFunctionDecl *main)
{
    using namespace colors;

    Timer timer;
    cc.initialize();

    bool testing = opts.testing;

    if (testing) {
        std::println("compiling {}{}{}", DefaultBold, cc.lexer.input.filename, Default);
    }

    while (!cc.lexer.out_of_bounds()) {
        if (auto *ast = parse_stmt(cc, main)) {
            main->body->stmts.push_back(ast);
        }
    }

    if (!testing) {
        dbgln("{}pre inference tree:{}", Cyan, Default);
        print_ast(main);
    };

    verify_main(cc, main);

    if (!testing) {
        dbgln("{}post inference tree:{}", Cyan, Default);
        print_ast(main);
    }

    [[maybe_unused]] auto frontend = timer.elapsed();
    timer.reset();

    generate_ir(cc, main);

    if (!testing) {
        dbgln("{}initial ir:{}", Cyan, Default);
        for (const auto *fn : cc.ir_builder.functions) {
            dbgln("{}:", fn->ast->name);
            print_ir(*fn);
        }
    }

    if (!testing) {
        dbgln("{}assembly:{}", Cyan, Default);
    }

    emit_asm(cc);
    [[maybe_unused]] auto backend = timer.elapsed();

    cc.lexer.free_input();

#if AST_ALLOC_PARANOID
    dbgln("=== alloced: {}", ast_alloc_count);
    dbgln("=== freed:   {}", ast_free_count);
    for (const auto &mark : marks) {
        const std::string &clr = mark.deleted ? Green : Red;
        dbgln("{}{}{} ({})", clr, mark.p, Default, mark.size);
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

    if (!testing) {
        std::println("{}elapsed time:{}", Cyan, Default);
        std::println("frontend:    {}{:>6}{}", Green, frontend, Default);
        std::println("backend:     {}{:>6}{}", Green, backend, Default);
        std::println("total:       {}{:>6}{}", Green, frontend + backend, Default);
    }
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
    global_scope->types["string"] = string_type();
}

void Compiler::free_types(bool skip_builtin)
{
    for (auto *scope : g_scopes) {
        for (auto &[name, ptr] : scope->types) {
            if (!skip_builtin || !ptr->has_flag(TypeFlags::BUILTIN)) {
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

void Compiler::cleanup(AstFunctionDecl *root, bool last)
{
    free_ast(root);
    leave_scope();
    free_types(!last);
    free_ir();
    free_scopes();
}
