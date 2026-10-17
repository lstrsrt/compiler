#pragma once

#include "ir.hh"
#include "lexer.hh"
#include "new-ir.hh"

//
// Main context
//

struct Compiler {
    Lexer lexer;
    ParseState parse_state;
    IRBuilder ir_builder;
    new_ir::IRBuilder new_ir_builder;
    Ast *current_function = nullptr;

    void initialize(AstFunction *main);
    void add_default_types();
    void cleanup();
};

File &stdout_file();

void invoke_compiler(const std::string &, InputMode);
