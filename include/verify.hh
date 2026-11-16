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
    EnumMismatch,
};

enum class IgnoreCasts {
    No,
    Yes,
};

enum class CheckSideEffects {
    No,
    Yes,
};

TypeError types_match(Type *, Type *);
uint64_t max_for_type(Type *);
Integer min_for_type(Type *);
Integer max_for_type_exact(Type *);
// The value of a constant expression (through casts) converted to `type`, like a cast would.
Integer get_int_literal_as(Ast *, Type *);
uint64_t get_int_literal(Ast *);
void flatten_binary(Ast *, std::vector<Ast *> &);
void flatten_binary(
    Ast *, Operation, std::vector<Ast *> &, CheckSideEffects = CheckSideEffects::No);
bool has_side_effects(Ast *);
