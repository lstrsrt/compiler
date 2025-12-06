#pragma once

#include "parser.hh"

#include <list>

//
// IR
//

#define ENUMERATE_IR_ARG_TYPES()        \
    __ENUMERATE_IR_ARG_TYPE(Empty)      \
    __ENUMERATE_IR_ARG_TYPE(Undef)      \
    __ENUMERATE_IR_ARG_TYPE(Vreg)       \
    __ENUMERATE_IR_ARG_TYPE(SSA)        \
    __ENUMERATE_IR_ARG_TYPE(Constant)   \
    __ENUMERATE_IR_ARG_TYPE(String)     \
    __ENUMERATE_IR_ARG_TYPE(Variable)   \
    __ENUMERATE_IR_ARG_TYPE(Parameter)  \
    __ENUMERATE_IR_ARG_TYPE(Function)   \
    __ENUMERATE_IR_ARG_TYPE(BasicBlock) \
    __ENUMERATE_IR_ARG_TYPE(Type)

enum class IRArgType {
#define __ENUMERATE_IR_ARG_TYPE(type) type,
    ENUMERATE_IR_ARG_TYPES()
#undef __ENUMERATE_IR_ARG_TYPE
};

struct BasicBlock;

struct SSA {
    Variable *source;
    size_t index;

    explicit SSA(Variable *_source, size_t _index)
        : source(_source)
        , index(_index)
    {
    }
};

union IRStorage {
    Variable *variable = nullptr;
    AstLiteral *constant;
    AstString *string;
    AstFunction *function;
    ssize_t vreg;
    SSA *ssa;
    BasicBlock *basic_block;
    Type *type;
    uintptr_t any;
};

struct IRArg {
    IRArgType arg_type;
    size_t string_index = 0;
    IRStorage u;

    static IRArg make_parameter(Variable *variable)
    {
        IRArg arg;
        arg.arg_type = IRArgType::Parameter;
        arg.u.variable = variable;
        return arg;
    }

    static IRArg make_variable(Variable *variable)
    {
        IRArg arg;
        arg.arg_type = IRArgType::Variable;
        arg.u.variable = variable;
        return arg;
    }

    static IRArg make_block(BasicBlock *bb)
    {
        IRArg arg;
        arg.arg_type = IRArgType::BasicBlock;
        arg.u.basic_block = bb;
        return arg;
    }

    static IRArg make_function(AstFunction *function)
    {
        IRArg arg;
        arg.arg_type = IRArgType::Function;
        arg.u.function = function;
        return arg;
    }

    static IRArg make_constant(AstLiteral *constant)
    {
        IRArg arg;
        arg.arg_type = IRArgType::Constant;
        arg.u.constant = constant;
        return arg;
    }

    static IRArg make_vreg(ssize_t vreg)
    {
        IRArg arg;
        arg.arg_type = IRArgType::Vreg;
        arg.u.vreg = vreg;
        return arg;
    }

    static IRArg make_ssa(Variable *src_var, size_t index)
    {
        IRArg arg;
        arg.arg_type = IRArgType::SSA;
        arg.u.ssa = new SSA(src_var, index);
        return arg;
    }

    static IRArg make_string(size_t index, AstString *string)
    {
        IRArg arg;
        arg.arg_type = IRArgType::String;
        arg.string_index = index;
        arg.u.string = string;
        return arg;
    }

    static IRArg make_undef()
    {
        IRArg arg;
        arg.arg_type = IRArgType::Undef;
        arg.u.any = 0;
        return arg;
    }
};

struct IR {
    Ast *ast = nullptr;
    AstType type;
    Operation operation;
    IRArg left;
    IRArg right;
    size_t basic_block_index = 0; // which basic block am I in?
    ssize_t target = -1;

    explicit IR() = default;

    explicit IR(Ast *_ast)
        : ast(_ast)
        , type(_ast->type)
        , operation(_ast->operation)
    {
    }

    explicit IR(AstType _type, Operation _operation)
        : type(_type)
        , operation(_operation)
    {
    }

    virtual ~IR() = default;

    bool has_vreg_target() const
    {
        return operation != Operation::PushArg && target > 0;
    }
};

struct IRCondBranch : IR {
    BasicBlock *true_block{};
    BasicBlock *false_block{};

    ~IRCondBranch() override = default;
};

struct IRCast : IR {
    Type *cast_type;

    explicit IRCast(Ast *_ast, Type *_cast_type)
        : IR(_ast)
        , cast_type(_cast_type)
    {
    }

    ~IRCast() override = default;
};

struct Phi {
    IRArg value;
    BasicBlock *block = nullptr;
};

struct IRPhi : IR {
    std::vector<Phi> phi_operands;

    // FIXME: not really a binary
    explicit IRPhi()
        : IR(AstType::Binary, Operation::Phi)
    {
    }

    ~IRPhi() override = default;
};

struct BasicBlock {
    std::unordered_map<uintptr_t, IRArg> current_def;
    std::list<IR *> code;
    size_t index = 0; // Index in IRFunction
    std::string label_name;
    std::vector<BasicBlock *> successors;
    std::vector<BasicBlock *> predecessors;
    bool reachable = false;
    bool terminal = false;
    bool sealed = true;
};

struct IRFunction {
    std::vector<BasicBlock *> basic_blocks;
    BasicBlock *current_block = nullptr;
    std::unordered_map<uint64_t, int> stack_offsets;
    AstFunction *ast;
    ssize_t temp_regs = 0;
};

inline std::vector<std::string> string_map;

struct IRBuilder {
    std::vector<IRFunction *> functions;
    IRFunction *current_function;
    BasicBlock *while_cmp_block = nullptr;
    BasicBlock *while_after_block = nullptr;
};

void generate_ir(Compiler &, AstFunction *);
void optimize_ir(Compiler &);

std::string demangled_name(const std::string &);

void free_ir_function(IRFunction *);
void free_ir(IRBuilder &);
