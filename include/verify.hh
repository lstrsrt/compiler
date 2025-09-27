#pragma once

#include "parser.hh"

//
// Verification
//

enum_flags(ExprConstness, uint8_t){
    SawConstant = 1 << 0,
    SawNonConstant = 1 << 1,
};

enum class TypeOverridable {
    No,
    Yes,
};

void verify_main(Compiler &, AstFunction *);

bool types_match(Type *, Type *);
uint64_t max_for_type(Type *);
uint64_t get_int_literal(Ast *);
Type *get_expression_type(Compiler &, Ast *, ExprConstness *, TypeOverridable);
void flatten_binary(Ast *, std::vector<Ast *> &);
void flatten_binary(Ast *, Operation, std::vector<Ast *> &);
