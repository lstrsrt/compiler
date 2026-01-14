#pragma once

#include "ir.hh"
#include "new-ir.hh"
#include "parser.hh"
#include "testing.hh"

//
// Debugging
//

std::string to_string(AstType);
std::string to_string(Operation);
std::string to_string(IRArgType);
std::string to_string(new_ir::InstType);
std::string type_kind_to_string(TypeFlags);
std::string type_flags_to_string(TypeFlags); // Including the kind
std::string to_string(ErrorType);
void print_ast(File &, Ast *, std::string indent = "");
void print_ast(File &, const std::vector<Ast *> &, std::string indent = "");
void print_types(Scope *);
void print_type(Type *);
void print_ir(File &, IR *);
void print_ir(File &, BasicBlock *);
void print_ir(File &, const IRFunction &);
std::string get_ir_arg_value(const IRArg &);
