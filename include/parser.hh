#pragma once

#define AST_USE_ARENA

#ifdef AST_USE_ARENA
#include "arena-alloc/arena_alloc.h"
#endif

#include "base.hh"
#include "lexer.hh"

#include <map>
#include <unordered_map>

//
// Parser
//

#define ENUMERATE_AST_TYPES()        \
    __ENUMERATE_AST_TYPE(Integer)    \
    __ENUMERATE_AST_TYPE(Boolean)    \
    __ENUMERATE_AST_TYPE(String)     \
    __ENUMERATE_AST_TYPE(Enum)       \
    __ENUMERATE_AST_TYPE(Identifier) \
    __ENUMERATE_AST_TYPE(Unary)      \
    __ENUMERATE_AST_TYPE(Binary)     \
    __ENUMERATE_AST_TYPE(Statement)  \
    __ENUMERATE_AST_TYPE(Block)

enum class AstType {
#define __ENUMERATE_AST_TYPE(type) type,
    ENUMERATE_AST_TYPES()
#undef __ENUMERATE_AST_TYPE
};

#define ENUMERATE_OPERATIONS()           \
    __ENUMERATE_OPERATION(None)          \
    __ENUMERATE_OPERATION(AddressOf)     \
    __ENUMERATE_OPERATION(Dereference)   \
    __ENUMERATE_OPERATION(Load)          \
    __ENUMERATE_OPERATION(Store)         \
    __ENUMERATE_OPERATION(Negate)        \
    __ENUMERATE_OPERATION(Call)          \
    __ENUMERATE_OPERATION(Add)           \
    __ENUMERATE_OPERATION(Subtract)      \
    __ENUMERATE_OPERATION(Multiply)      \
    __ENUMERATE_OPERATION(Divide)        \
    __ENUMERATE_OPERATION(Modulo)        \
    __ENUMERATE_OPERATION(And)           \
    __ENUMERATE_OPERATION(Or)            \
    __ENUMERATE_OPERATION(Xor)           \
    __ENUMERATE_OPERATION(Not)           \
    __ENUMERATE_OPERATION(LeftShift)     \
    __ENUMERATE_OPERATION(RightShift)    \
    __ENUMERATE_OPERATION(LeftRotate)    \
    __ENUMERATE_OPERATION(RightRotate)   \
    __ENUMERATE_OPERATION(Equals)        \
    __ENUMERATE_OPERATION(NotEquals)     \
    __ENUMERATE_OPERATION(Greater)       \
    __ENUMERATE_OPERATION(GreaterEquals) \
    __ENUMERATE_OPERATION(Less)          \
    __ENUMERATE_OPERATION(LessEquals)    \
    __ENUMERATE_OPERATION(Assign)        \
    __ENUMERATE_OPERATION(LogicalAnd)    \
    __ENUMERATE_OPERATION(LogicalOr)     \
    __ENUMERATE_OPERATION(LogicalNot)    \
    __ENUMERATE_OPERATION(Return)        \
    __ENUMERATE_OPERATION(VariableDecl)  \
    __ENUMERATE_OPERATION(FunctionDecl)  \
    __ENUMERATE_OPERATION(EnumDecl)      \
    __ENUMERATE_OPERATION(EnumMember)    \
    __ENUMERATE_OPERATION(If)            \
    __ENUMERATE_OPERATION(While)         \
    __ENUMERATE_OPERATION(For)           \
    __ENUMERATE_OPERATION(Break)         \
    __ENUMERATE_OPERATION(Continue)      \
    __ENUMERATE_OPERATION(Cast)          \
    __ENUMERATE_OPERATION(PushArg)       \
    __ENUMERATE_OPERATION(Branch)        \
    __ENUMERATE_OPERATION(BranchEq)      \
    __ENUMERATE_OPERATION(BranchNe)      \
    __ENUMERATE_OPERATION(BranchUGt)     \
    __ENUMERATE_OPERATION(BranchUGe)     \
    __ENUMERATE_OPERATION(BranchULt)     \
    __ENUMERATE_OPERATION(BranchULe)     \
    __ENUMERATE_OPERATION(BranchSGt)     \
    __ENUMERATE_OPERATION(BranchSGe)     \
    __ENUMERATE_OPERATION(BranchSLt)     \
    __ENUMERATE_OPERATION(BranchSLe)     \
    __ENUMERATE_OPERATION(Alloca)        \
    __ENUMERATE_OPERATION(Phi)

enum class Operation {
#define __ENUMERATE_OPERATION(operation) operation,
    ENUMERATE_OPERATIONS()
#undef __ENUMERATE_OPERATION
};

struct Scope;
inline Scope *current_scope;

enum_flags(AstFlags, uint64_t){
    // Prevent optimizer from trying to fold this tree multiple times.
    FOLDED = 1 << 0,
    // Tell the typechecker that this tree can mix integers and pointers in operations.
    PTR_ARITH = 1 << 1,
    // This is a function that directly calls itself.
    RECURSIVE = 1 << 2,
};

constexpr uint64_t KiB(uint64_t bytes)
{
    return bytes * 1024;
}

#ifdef AST_USE_ARENA
inline auto *ast_arena()
{
    static arena::Arena arena(KiB(1024), ARENA_FL_GROW);
    return &arena;
}

inline auto &ast_vec_allocator()
{
    static arena::ArenaAllocator<void> allocator(ast_arena());
    return allocator;
}
#endif

struct Ast {
    Scope *scope;
    AstType type;
    Operation operation;
    SourceLocation location;
    Type *expr_type = nullptr; // Set during verification.
    AstFlags flags{};

    explicit Ast(
        AstType _type, Operation _operation = Operation::None, SourceLocation _location = {})
        : scope(current_scope)
        , type(_type)
        , operation(_operation)
        , location(_location)
    {
    }

#ifdef AST_USE_ARENA
    static void *operator new(size_t size) { return ast_arena()->alloc_raw(size); }

    static void operator delete(void *) { }
#endif
};

#ifdef AST_USE_ARENA
inline void free_ast_arena()
{
    ast_arena()->reset();
}
#endif

Type *bool_type();

struct Integer {
    uint64_t value;
    bool is_signed;

    explicit Integer() = default;

    explicit Integer(uint64_t _value, bool _is_signed)
        : value(_value)
        , is_signed(_is_signed)
    {
    }

    int64_t as_signed() const { return static_cast<int64_t>(value); }

    bool is_negative() const { return is_signed && (as_signed() & (uint64_t(1) << 63)); }

    bool is_nonnegative() const { return !is_negative(); }

    auto operator<=>(const Integer &rhs) const
    {
        // TODO: what if is_signed != rhs.is_signed?
        // can we remove this entirely and just compare the raw values??
        // maybe not because of type sizes?
        if (is_signed) {
            return as_signed() <=> rhs.as_signed();
        }
        return value <=> rhs.value;
    }

    bool operator==(const Integer &rhs) const
    {
        // TODO: what if is_signed != rhs.is_signed?
        if (is_signed) {
            return as_signed() == rhs.as_signed();
        }
        return value == rhs.value;
    }

    auto operator-(uint64_t rhs) const
    {
        Integer ret = *this;
        ret.value -= rhs;
        return ret;
    }

    auto operator+(uint64_t rhs) const
    {
        Integer ret = *this;
        ret.value += rhs;
        return ret;
    }

    static auto min(const Integer &lhs, const Integer &rhs)
    {
        if (lhs.is_signed) {
            if (lhs.as_signed() < rhs.as_signed()) {
                return lhs;
            }
            return rhs;
        }

        if (lhs.value < rhs.value) {
            return lhs;
        }
        return rhs;
    }

    static auto max(const Integer &lhs, const Integer &rhs)
    {
        if (lhs.is_signed) {
            if (lhs.as_signed() > rhs.as_signed()) {
                return lhs;
            }
            return rhs;
        }

        if (lhs.value > rhs.value) {
            return lhs;
        }
        return rhs;
    }
};

enum_flags(TypeFlags, int){
    //
    // Type kind
    //
    Unknown,
    Void,
    Integer,
    Boolean,
    String,
    // Float,
    // Char, // should this be considered separate from Integer?
    // Struct,
    kind_mask = 0b1111,

    //
    // Flags: zero, one or multiple of these can be set
    //
    UNRESOLVED = 1 << 4,
    ALIAS = 1 << 5,
    BUILTIN = 1 << 6,
    ENUM = 1 << 7,

    // Integer flags
    UNSIGNED = 1 << 8,
};

struct Type {
    std::string name;
    TypeFlags flags{};
    uint8_t size = 0;
    uint8_t pointer = 0;

    // If this type is an alias or a pointer, this points to the underlying
    // type (which may also be an alias or pointer)
    Type *real = nullptr;

    // Used by AstEnum, and in the future also AstRecord.
    Ast *decl = nullptr;

    SourceLocation location{};

    TypeFlags get_kind() const { return flags & TypeFlags::kind_mask; }

    bool is_int() const { return get_kind() == TypeFlags::Integer; }

    bool is_bool() const { return get_kind() == TypeFlags::Boolean; }

    // Is this a integer or an integer-like type?
    // NOTE: Booleans may not be used in arithmetic operations.
    bool is_integral() const { return is_int() || is_bool(); }

    bool is_string() const { return get_kind() == TypeFlags::String; }

    TypeFlags get_flags() const { return flags & ~TypeFlags::kind_mask; }

    size_t byte_size() const
    {
        if (size == 1) {
            return 1;
        }
        return size / 8;
    }

    bool has_flag(TypeFlags flag) const { return ::has_flag(flags, flag); }

    bool is_unsigned() const { return ::has_flag(flags, TypeFlags::UNSIGNED); }

    bool is_signed() const { return !is_unsigned(); }

    bool is_pointer() const { return pointer > 0; }

    std::string get_name() const
    {
        std::string s;
        if (is_pointer()) {
            s += std::string(pointer, '*');
        }
        s += name;
        return s;
    }
};

struct AstLiteral : Ast {
    union {
        Integer any;
        uint32_t u32;
        uint64_t u64;
        int32_t s32;
        int64_t s64;
        bool boolean;
    } u;

    explicit AstLiteral(Type *_type, uint64_t _literal, SourceLocation _location)
        : Ast(AstType::Integer, Operation::None, _location)
    {
        expr_type = _type;
        u.any.value = _literal;
        u.any.is_signed = !_type->is_unsigned();
    }

    explicit AstLiteral(bool _literal, SourceLocation _location)
        : Ast(AstType::Boolean, Operation::None, _location)
    {
        expr_type = bool_type();
        u.boolean = _literal;
    }
};

struct AstString : Ast {
    std::string string;

    explicit AstString(std::string_view _string, SourceLocation _location)
        : Ast(AstType::String, Operation::None, _location)
        , string(_string)
    {
    }
};

std::string extract_integer_constant(Ast *);

struct Variable;

struct AstIdentifier : Ast {
    std::string string;
    Variable *var;

    explicit AstIdentifier(std::string_view _string, Variable *_var, SourceLocation _location)
        : Ast(AstType::Identifier, Operation::None, _location)
        , string(_string)
        , var(_var)
    {
    }
};

struct AstUnary : Ast {
    Ast *operand;

    explicit AstUnary(Operation _op, Ast *_operand, SourceLocation _location)
        : Ast(AstType::Unary, _op, _location)
        , operand(_operand)
    {
    }
};

using AstNegate = AstUnary;
using AstAddressOf = AstUnary;
using AstDereference = AstUnary;
using AstLogicalNot = AstUnary;
using AstNot = AstUnary;

struct AstCast : AstUnary {
    Type *cast_type;

    explicit AstCast(Ast *_expr, Type *_cast_type, SourceLocation _location)
        : AstUnary(Operation::Cast, _expr, _location)
        , cast_type(_cast_type)

    {
    }
};

struct AstFunction;

#ifdef AST_USE_ARENA
using FunctionArgs = std::vector<Ast *, arena::ArenaAllocator<Ast *>>;
#else
using FunctionArgs = std::vector<Ast *>;
#endif

struct AstCall : Ast {
    std::string_view name;
    FunctionArgs args;
    AstFunction *fn = nullptr; // Use get_callee() instead of accessing this directly

    explicit AstCall(std::string_view _name, FunctionArgs _args, SourceLocation _location)
        : Ast(AstType::Unary, Operation::Call, _location)
        , name(_name)
        , args(std::move(_args))
    {
    }
};

AstFunction *get_callee(Compiler &, AstCall *);

struct AstBinary : Ast {
    Ast *left;
    Ast *right;

    explicit AstBinary(Operation _op, Ast *_left, Ast *_right, SourceLocation _location)
        : Ast(AstType::Binary, _op, _location)
        , left(_left)
        , right(_right)
    {
    }
};

struct AstReturn : Ast {
    Ast *expr;

    explicit AstReturn(Ast *_expr, SourceLocation _location)
        : Ast(AstType::Statement, Operation::Return, _location)
        , expr(_expr)
    {
    }
};

Type *void_type();
Type *u8_type();
Type *u16_type();
Type *u32_type();
Type *u64_type();
Type *s8_type();
Type *s16_type();
Type *s32_type();
Type *s64_type();
Type *string_type();
Type *null_type();
Type *unresolved_type();

Type *get_unaliased_type(Type *);

struct Variable {
    Type *type;
    std::string name;
    // If this is a function parameter...
    int param_index = -1;
    bool is_unresolved = false;

    bool force_stack = false;

    bool is_parameter() const { return param_index > -1; }

    explicit Variable(Type *_type, std::string_view _name, bool _is_unresolved)
        : type(_type)
        , name(_name)
        , is_unresolved(_is_unresolved)
    {
    }
};

Variable *unresolved_var(std::string_view name);

struct AstVariableDecl : Ast {
    Variable var;
    Ast *init_expr;

    explicit AstVariableDecl(
        Type *_type, std::string_view _name, Ast *_init_expr, SourceLocation _location)
        : Ast(AstType::Statement, Operation::VariableDecl, _location)
        , var(_type, _name, false)
        , init_expr(_init_expr)
    {
    }
};

#ifdef AST_USE_ARENA
using StmtVec = std::vector<Ast *, arena::ArenaAllocator<Ast *>>;
#else
using StmtVec = std::vector<Ast *>;
#endif

struct AstBlock : Ast {
    StmtVec stmts;

    explicit AstBlock(StmtVec _stmts)
        : Ast(AstType::Block)
        , stmts(std::move(_stmts))
    {
    }
};

struct AstIf : Ast {
    Ast *expr;
    AstBlock *body;
    Ast *else_body;

    explicit AstIf(Ast *_expr, AstBlock *_body, Ast *_else_body, SourceLocation _location)
        : Ast(AstType::Statement, Operation::If, _location)
        , expr(_expr)
        , body(_body)
        , else_body(_else_body)
    {
    }
};

struct AstWhile : Ast {
    Ast *expr;
    AstBlock *body = nullptr;

    explicit AstWhile(Ast *_expr, SourceLocation _location)
        : Ast(AstType::Statement, Operation::While, _location)
        , expr(_expr)
    {
    }
};

struct AstFor : Ast {
    AstVariableDecl *var_decl;
    Ast *ident;
    Ast *body = nullptr;
    Ast *end;
    Ast *cmp;
    Ast *change;

    explicit AstFor(AstVariableDecl *_var_decl, AstIdentifier *_ident, Ast *_end, Ast *_cmp,
        Ast *_change, const SourceLocation &_location)
        : Ast(AstType::Statement, Operation::For, _location)
        , var_decl(_var_decl)
        , ident(_ident)
        , end(_end)
        , cmp(_cmp)
        , change(_change)
    {
    }
};

struct AstBreak : Ast {
    explicit AstBreak(SourceLocation _location)
        : Ast(AstType::Statement, Operation::Break, _location)
    {
    }
};

struct AstContinue : Ast {
    explicit AstContinue(SourceLocation _location)
        : Ast(AstType::Statement, Operation::Continue, _location)
    {
    }
};

enum_flags(FunctionAttributes, int){
    DUMP_AST = 1 << 0,
    DUMP_IR = 1 << 1,
    DUMP_ASM = 1 << 2,
    FORCE_USE = 1 << 3,
    BUILTIN_PRINT = 1 << 4,
};

#ifdef AST_USE_ARENA
using VariableDecls = std::vector<AstVariableDecl *, arena::ArenaAllocator<AstVariableDecl *>>;
#else
using VariableDecls = std::vector<AstVariableDecl *>;
#endif

struct AstFunction : Ast {
    std::string name;
    Type *return_type;
    VariableDecls params;
    AstBlock *body;
    uint64_t call_count = 0;
    FunctionAttributes attributes{};

    explicit AstFunction(std::string_view _name, Type *_return_type, VariableDecls _params,
        AstBlock *_body, SourceLocation _location)
        : Ast(AstType::Statement, Operation::FunctionDecl, _location)
        , name(_name)
        , return_type(_return_type)
        , params(std::move(_params))
        , body(_body)
    {
    }

    bool returns_void() const
    {
        return get_unaliased_type(return_type)->get_kind() == TypeFlags::Void;
    }
};

struct EnumMembers {
    std::unordered_map<std::string, struct AstEnumMember *> map;
    std::vector<Ast *> vector;
};

struct AstEnumDecl : Ast {
    std::string name;
    Type *enum_type = nullptr;
    EnumMembers *members;
    uint64_t next_int = 0;

    explicit AstEnumDecl(std::string_view _name, SourceLocation _location)
        : Ast(AstType::Statement, Operation::EnumDecl, _location)
        , name(_name)
        , members(new EnumMembers)
    {
    }
};

struct AstEnumMember : Ast {
    std::string name;
    std::string enum_name;
    AstEnumDecl *enum_decl;
    Ast *expr;

    explicit AstEnumMember(
        std::string_view _name, AstEnumDecl *_enum_decl, Ast *_ast, SourceLocation _location)
        : Ast(AstType::Enum, Operation::EnumMember, _location)
        , name(_name)
        , enum_name(_enum_decl->name)
        , enum_decl(_enum_decl)
        , expr(_ast)
    {
    }

    explicit AstEnumMember(
        std::string_view _enum_name, std::string_view _name, SourceLocation _location)
        : Ast(AstType::Enum, Operation::EnumMember, _location)
        , name(_name)
        , enum_name(_enum_name)
        , enum_decl(nullptr)
        , expr(nullptr)
    {
    }
};

Ast *parse_stmt(Compiler &, AstFunction *);

AstFunction *print_builtin();

void free_ast(Ast *);
void free_ast(std::vector<Ast *> &);

struct Scope {
    Scope *parent;
    AstFunction *function;
    std::unordered_map<std::string_view, Type *> types;
    std::unordered_map<std::string_view, AstEnumDecl *> enums;
    std::unordered_map<std::string_view, AstFunction *> functions;
    std::unordered_map<std::string, AstVariableDecl *> variables;

    explicit Scope(Scope *_parent, AstFunction *_function)
        : parent(_parent)
        , function(_function)
    {
    }

    void add_variable(Compiler &, AstVariableDecl *);
    void add_function(Compiler &, Ast *, std::string_view unmangled_name);
    void add_alias(Compiler &, Type *, std::string_view alias, SourceLocation);
    void add_enum(Compiler &, AstEnumDecl *);
};

inline std::vector<Scope *> g_scopes;

inline void enter_new_scope(AstFunction *function)
{
    g_scopes.emplace_back(new Scope(current_scope, function));
    current_scope = g_scopes.back();
}

inline void leave_scope()
{
    if (current_scope) {
        current_scope = current_scope->parent;
    }
}

struct AutoScope {
    explicit AutoScope(AstFunction *main) { enter_new(main); }

    void enter_new(AstFunction *function)
    {
        ++count;
        enter_new_scope(function);
    }

    AutoScope(AutoScope &) = delete;
    AutoScope(AutoScope &&) = delete;
    auto operator=(AutoScope &) = delete;
    auto operator=(AutoScope &&) = delete;

    ~AutoScope()
    {
        while (count--) {
            leave_scope();
        }
    }

    int count = 0;
};

inline bool at_top_level()
{
    return !current_scope->parent;
}

inline void free_scopes()
{
    assert(!current_scope);
    for (auto *scope : g_scopes) {
        delete scope;
    }
    g_scopes.clear();
}

enum class SearchParents {
    No,
    Yes
};

Type *find_type(Scope *, std::string_view name, Scope **result_scope = nullptr,
    SearchParents = SearchParents::Yes);

AstEnumDecl *find_enum(Scope *scope, std::string_view name, Scope **result_scope = nullptr,
    SearchParents search_parents = SearchParents::Yes);

AstVariableDecl *find_variable(Scope *, const std::string &name, Scope **result_scope = nullptr,
    SearchParents = SearchParents::Yes);

AstFunction *find_function(Scope *, std::string_view name, Scope **result_scope = nullptr,
    SearchParents = SearchParents::Yes);

enum class ErrorOnShadowing {
    No,
    Yes
};

void diagnose_redeclaration_or_shadowing(Compiler &, Scope *, std::string_view name,
    std::string_view type, SourceLocation, ErrorOnShadowing);

struct ParseState {
    bool in_call = false;
    AstEnumDecl *current_enum = nullptr;
    Ast *current_loop = nullptr;
};
