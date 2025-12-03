#include "compiler.hh"
#include "asm.hh"
#include "debug.hh"
#include "frontend.hh"
#include "verify.hh"

#ifdef __linux__
#include <unistd.h>
#endif

void compiler_main(Compiler &cc, AstFunction *main)
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
#ifdef _DEBUG
        print_ast(stdout_file(), main);
#endif
    };

    verify_main(cc, main);

    if (!testing) {
        dbgln("\n{}post inference tree:{}", Cyan, Default);
#ifdef _DEBUG
        print_ast(stdout_file(), main);
#endif
    }

    generate_ir(cc, main);

    if (!testing) {
#ifdef _DEBUG
        dbgln("\n{}generated ir:{}", Cyan, Default);
        for (const auto *fn : cc.ir_builder.functions) {
            print_ir(stdout_file(), *fn);
        }
#endif
    }

    optimize_ir(cc);

    if (!testing) {
#ifdef _DEBUG
        dbgln("{}optimized ir:{}", Cyan, Default);
        for (const auto *fn : cc.ir_builder.functions) {
            print_ir(stdout_file(), *fn);
        }
#endif
    }

    // We have to go through IR passes even in check-only mode because it detects missing return
    // statements.
    if (opts.check_only) {
        std::println("{}program passed checks!{}", Green, Default);
        return;
    }

    auto frontend = timer.elapsed();
    timer.reset();

    if (!testing) {
        dbgln("{}assembly:{}", Cyan, Default);
    }

    emit_asm(cc);

    if (opts.full_compile) {
        create_executable(opts.output_name, opts.output_exe_name);
    }

    auto backend = timer.elapsed();

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

#ifndef __linux__
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <Windows.h>
#endif

File &stdout_file()
{
#ifdef __linux__
    static auto s_stdout_file
        = File::from_handle(STDOUT_FILENO, OpenFlags::WRITE, File::Owning::No);
#else
    static auto s_stdout_file
        = File::from_handle(GetStdHandle(STD_OUTPUT_HANDLE), OpenFlags::WRITE, File::Owning::No);
#endif
    return s_stdout_file;
}

// can we have builtin types somewhere else so lookup during verification stage is faster?
void Compiler::add_default_types()
{
    auto *global_scope = g_scopes[0];
    global_scope->types["void"] = void_type();
    global_scope->types["u8"] = u8_type();
    global_scope->types["u16"] = u16_type();
    global_scope->types["u32"] = u32_type();
    global_scope->types["u64"] = u64_type();
    global_scope->types["s8"] = s8_type();
    global_scope->types["s16"] = s16_type();
    global_scope->types["s32"] = s32_type();
    global_scope->types["s64"] = s64_type();
    global_scope->types["bool"] = bool_type();
    global_scope->types["string"] = string_type();

    global_scope->functions["print"] = print_builtin();
}

void free_types()
{
    for (auto *scope : g_scopes) {
        for (auto &[name, ptr] : scope->types) {
            if (!ptr->has_flag(TypeFlags::BUILTIN)) {
                delete ptr;
            }
        }
    }
}

void Compiler::cleanup([[maybe_unused]] AstFunction *root)
{
#ifdef AST_USE_ARENA
    free_ast_arena();
#else
    free_ast(root);
#endif
    leave_scope();
    free_types();
    free_ir(this->ir_builder);
    free_scopes();
    lexer.free_input();
}
