#pragma once

#include <utility>

#include "lexer.hh"
#include "parser.hh"

//
// IR rewrite
//

#ifdef SSA_DEBUG // In case ir.hh is ever included here...
#undef SSA_DEBUG
#endif

// #define SSA_DEBUG

#ifdef SSA_DEBUG
#define ssa_dbgln(x, ...) std::println(x __VA_OPT__(, __VA_ARGS__))
#else
#define ssa_dbgln(...) (void)0
#endif

namespace new_ir {

struct BasicBlock;
struct Inst;

struct Stats {
    uint64_t insts_added = 0;
    uint64_t insts_killed = 0;
    uint64_t vars_to_ssa = 0;
};

inline Stats stats;

enum class InstKind {
    Nop,
    Identity,
    // Comment, // TODO: add comments inside IR?
    Const,
    Arg,
    String,
    Undef,
    Var,
    SSA,
    Unary,
    Binary,
    Cast,
    Call,
    Phi,
};

#define ENUMERATE_INST_TYPES(type) \
    __ENUMERATE_INST_TYPE(Void)    \
    __ENUMERATE_INST_TYPE(S1)      \
    __ENUMERATE_INST_TYPE(S8)      \
    __ENUMERATE_INST_TYPE(S16)     \
    __ENUMERATE_INST_TYPE(S32)     \
    __ENUMERATE_INST_TYPE(S64)     \
    __ENUMERATE_INST_TYPE(U8)      \
    __ENUMERATE_INST_TYPE(U16)     \
    __ENUMERATE_INST_TYPE(U32)     \
    __ENUMERATE_INST_TYPE(U64)     \
    __ENUMERATE_INST_TYPE(String)  \
    __ENUMERATE_INST_TYPE(Ptr)

enum class InstType {
#define __ENUMERATE_INST_TYPE(type) type,
    ENUMERATE_INST_TYPES()
#undef __ENUMERATE_INST_TYPE
};

struct Inst {
    std::string name = "???";
    InstKind kind;
    InstType type;
    Operation operation = Operation::None;
    std::vector<Inst *> args;
    size_t index_in_bb = 0;

    explicit Inst(Operation _operation, InstKind _kind, InstType _type, const std::string &_name)
        : name(_name)
        , kind(_kind)
        , type(_type)
        , operation(_operation)
    {
        ++stats.insts_added;
    }

    void add_arg(Inst *arg) { args.push_back(arg); }

    void transform_to_nop();
    void transform_to_identity(Inst *id, const std::string &new_name);
    void transform_to_jump(BasicBlock *cur, BasicBlock *target, const std::string &new_name);
};

struct ConstInst : Inst {
    Integer constant{};

    explicit ConstInst(InstType _type, const std::string &_name)
        : Inst(Operation::None, InstKind::Const, _type, _name)
    {
    }
};

struct ArgInst : Inst {
    int index = -1;

    explicit ArgInst(InstType _type, const std::string &_name)
        : Inst(Operation::None, InstKind::Arg, _type, _name)
    {
    }
};

struct StringInst : Inst {
    std::string *string = nullptr;

    explicit StringInst(const std::string &_name)
        : Inst(Operation::None, InstKind::String, InstType::String, _name)
    {
    }
};

struct AllocaInst : Inst {
    InstType inst_type{};
    bool force_memory = false;

    explicit AllocaInst(const std::string &_name)
        : Inst(Operation::Alloca, InstKind::Unary, InstType::Ptr, _name)
    {
    }
};

// TODO: remove this, we only need SSA and Alloca
struct VarInst : Inst {
    Variable *variable = nullptr;

    explicit VarInst(InstType _type, const std::string &_name)
        : Inst(Operation::None, InstKind::Var, _type, _name)
    {
    }
};

struct SSAInst : Inst {
    Variable *variable = nullptr; // TODO: remove me

    explicit SSAInst(InstType _type, const std::string &_name)
        : Inst(Operation::None, InstKind::SSA, _type, _name)
    {
    }
};

struct Phi {
    BasicBlock *block;
    Inst *value;
};

struct PhiInst : Inst {
    std::vector<Phi> incoming;

    explicit PhiInst(InstType _type, const std::string &_name)
        : Inst(Operation::Phi, InstKind::Phi, _type, _name)
    {
    }
};

struct CastInst : Inst {
    InstType cast = InstType::Void;

    explicit CastInst(InstType _type, const std::string &_name)
        : Inst(Operation::Cast, InstKind::Cast, _type, _name)
    {
    }
};

struct CallInst : Inst {
    AstFunction *function = nullptr;

    explicit CallInst(InstType _type, const std::string &_name)
        : Inst(Operation::Call, InstKind::Call, _type, _name)
    {
    }
};

struct Insertion {
    size_t pos;
    Inst *inst;
};

struct InsertionSet {
    void insert_before(size_t index_in_bb, Inst *);
    void insert_before(Inst *, Inst *);
    void insert_after(size_t index_in_bb, Inst *);
    void insert_after(Inst *, Inst *);
    void execute(BasicBlock *);
    void execute_range(BasicBlock *);

private:
    std::vector<Insertion> insertions;
};

struct InsertionSetMap {
    std::map<BasicBlock *, InsertionSet> insertion_sets;

    auto &operator[](BasicBlock *bb) { return insertion_sets[bb]; }

    void execute_all()
    {
        for (auto &[block, insertion_set] : insertion_sets) {
            insertion_set.execute(block);
        }
        insertion_sets.clear();
    }
};

template<class T>
requires std::formattable<T, char>
struct UniqueNameGenerator {
    std::unordered_map<T, int> counter;

    std::string get(const T &key) { return std::format("{}{}", key, ++counter[key]); }
};

inline std::unordered_map<struct Function *, UniqueNameGenerator<std::string>> name_gen;

struct BasicBlock {
    std::string name;
    std::vector<Inst *> code;
    std::vector<BasicBlock *> successors;
    std::vector<BasicBlock *> predecessors;
    size_t index_in_fn = 0;
    bool reachable = false;
    bool terminal = false;
    bool sealed = true;

    explicit BasicBlock() = default;

    explicit BasicBlock(Function *fn, const std::string &_name)
        : name(name_gen[fn].get(_name))
    {
    }

    void add_successor(BasicBlock *succ)
    {
        successors.push_back(succ);
        succ->predecessors.push_back(this);
        succ->reachable = reachable;
    }
};

struct Function {
    std::string name;
    std::vector<BasicBlock *> blocks;
    BasicBlock *current_block = nullptr;
    AstFunction *ast = nullptr;
    ptrdiff_t last_alloca = 0;
    SourceLocation location;
};

struct BlockInsertion {
    size_t pos;
    BasicBlock *block;
};

struct BlockInsertionSet {
    void insert_after(size_t index_in_fn, BasicBlock *);
    void insert_before(size_t index_in_fn, BasicBlock *);

    void execute(Function *);
    void execute_range(Function *);

    std::vector<BlockInsertion> insertions;
};

struct IRBuilder {
    std::vector<Function *> fns;
    Function *current_fn = nullptr;
    BasicBlock *loop_cmp_block = nullptr;
    BasicBlock *loop_merge_block = nullptr;
    std::stack<InsertionSet> alloca_sets;
    BlockInsertionSet *block_insertion_set = nullptr;
    size_t block_insertion_point = 0;

    // Insert an instruction at the current position.
    // Use InsertionSet for anything more complicated.
    Inst *add(Inst *) const;
};

InstType to_inst_type(Type *);

inline InstType to_inst_type(Ast *ast)
{
    return to_inst_type(ast->expr_type);
}

void generate(Compiler &, AstFunction *);
Inst *generate(IRBuilder &, Ast *);
Inst *generate_jump(IRBuilder &, BasicBlock *);
Inst *generate_alloca(IRBuilder &, Variable *);
Inst *generate_alloca(IRBuilder &, InstType, const std::string &);
Inst *make_load(Function *, Inst *);

std::string demangled_name(const std::string &);

// Cleanup passes
void replace_identities(IRBuilder &);
void dce_sweep(IRBuilder &);

void free(IRBuilder &);

void print(File &, IRBuilder &);
void print(File &, Function *);
void consume_stats(File &);

} // namespace new_ir
