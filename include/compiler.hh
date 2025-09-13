#pragma once

#include "ir.hh"
#include "lexer.hh"
#include "testing.hh"

//
// Main context
//

struct Compiler {
    Lexer lexer;
    IRBuilder ir_builder;
    TestMode test_mode;

    void initialize();
    void add_default_types();
    void free_types();
    void free_ir();
    void cleanup(AstFunction *root);
};

void compiler_main(Compiler &, AstFunction *main);
