#pragma once

#include "ir.hh"
#include "lexer.hh"
#include "testing.hh"

//
// Main context
//

struct Compiler {
    Lexer lexer;
    ParseState parse_state;
    IRBuilder ir_builder;
    TestMode test_mode;

    void initialize();
    void add_default_types();
    void cleanup(AstFunction *root);
};

File &stdout_file();

void compiler_main(Compiler &, AstFunction *main);
