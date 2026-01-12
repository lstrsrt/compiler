#pragma once

#include "ir.hh"
#include "lexer.hh"
#include "new-ir.hh"
#include "testing.hh"

//
// Main context
//

struct Compiler {
    Lexer lexer;
    ParseState parse_state;
    IRBuilder ir_builder;
    new_ir::IRBuilder new_ir_builder;
    TestMode test_mode;
    Ast *current_function = nullptr;

    void initialize(AstFunction *main);
    void add_default_types();
    void cleanup(AstFunction *root);
};

File &stdout_file();

void compiler_main(Compiler &, AstFunction *main);
