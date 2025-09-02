#pragma once

#include "ir.hh"
#include "parser.hh"
#include "testing.hh"

//
// Debugging
//

std::string to_string(AstType);
std::string to_string(Operation);
std::string to_string(IRArgType);
std::string type_kind_to_string(TypeFlags);
std::string type_flags_to_string(TypeFlags); // Including the kind
std::string to_string(ErrorType);
void print_ast(Ast *, std::string indent = "");
void print_ast(const std::vector<Ast *> &, std::string indent = "");
void print_types(Scope *);
void print_ir(IR *);
void print_ir(BasicBlock *);
void print_ir(const IRFunction &);
