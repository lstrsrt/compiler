#pragma once

#include "verify.hh"

//
// Optimizer
//

Ast *try_constant_fold(Compiler &, Ast *, Type *&expected, TypeOverridable);
Ast *try_fold_logical_chain(Compiler &, AstBinary *);
Ast *apply_de_morgan_laws(Ast *);
