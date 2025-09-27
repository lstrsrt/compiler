#pragma once

struct Ast;
struct Compiler;

namespace llvm {
class Value;
}

namespace llvm_gen {
void run(Compiler &, Ast *);
void dump();
} // namespace llvm_gen
