#include "compiler.hh"

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

void Compiler::cleanup()
{
    leave_scope();
    free_types();
    free_ir();
    free_scopes();
}
