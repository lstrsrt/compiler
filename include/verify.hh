#pragma once

#include "parser.hh"

//
// Verification
//

void verify_main(Compiler &, AstFunction *);

enum class TypeError {
    None,
    Default,
    SignednessMismatch,
    SizeMismatch,
    PointerMismatch,
};

TypeError types_match(Type *, Type *);
uint64_t max_for_type(Type *);
uint64_t get_int_literal(Ast *);
void flatten_binary(Ast *, std::vector<Ast *> &);
void flatten_binary(Ast *, Operation, std::vector<Ast *> &);
