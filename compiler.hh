#pragma once

#include <cassert>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <print>
#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace fs = std::filesystem;
namespace ch = std::chrono;
using namespace std::string_view_literals;

int spawn_and_wait(const fs::path &exe_path, const std::vector<std::string> &_cmdline);

namespace colors {
    constexpr std::string Cyan = "\033[36;1m";
    constexpr std::string Default = "\033[0m";
    constexpr std::string DefaultBold = Default + "\033[1m";
    constexpr std::string Green = "\033[32;1m";
    constexpr std::string Red = "\033[31;1m";
} // namespace colors

template<typename Fn>
struct Defer {
    Defer(Fn _fn)
        : fn(std::move(_fn))
    {
    }

    ~Defer()
    {
        fn();
    }

    Fn fn;
};

#define AST_ALLOC_PARANOID 0

#if AST_ALLOC_PARANOID
#include <execinfo.h>
#endif

enum class TestType {
    CanCompile,
    ReturnsValue,
    Error,
};
#define ENUMERATE_ERROR_TYPES()          \
    __ENUMERATE_ERROR_TYPE(Lexer)        \
    __ENUMERATE_ERROR_TYPE(Parser)       \
    __ENUMERATE_ERROR_TYPE(Verification) \
    __ENUMERATE_ERROR_TYPE(TypeCheck)

enum class ErrorType {
#define __ENUMERATE_ERROR_TYPE(type) type,
    ENUMERATE_ERROR_TYPES()
#undef __ENUMERATE_ERROR_TYPE
};

//
// Utils
//

using std::to_underlying;

#ifdef _DEBUG
#define dbgln(fmt, ...) std::println(fmt __VA_OPT__(, __VA_ARGS__))
#define dbg(fmt, ...) std::print(fmt __VA_OPT__(, __VA_ARGS__))
#else
#define dbgln(...) (void)0
#define dbg(fmt, ...) (void)0
#endif

struct Timer {
    Timer()
    {
        reset();
    }

    void reset()
    {
        start = ch::system_clock::now();
    }

    auto elapsed()
    {
        return ch::duration<double>(ch::system_clock::now() - start);
    }

    ch::time_point<ch::system_clock> start;
};

constexpr size_t align_up(size_t value, size_t alignment)
{
    return (value + (alignment - 1)) & ~(alignment - 1);
}

[[noreturn]] inline void todo(const char *func)
{
    std::println("\nTODO: {}", func);
#ifdef _DEBUG
    __builtin_trap();
#else
    exit(1);
#endif
}

#define TODO() todo(__func__)

using hash_t = uint32_t;

inline constexpr hash_t hash(const char *s, size_t len)
{
    constexpr hash_t basis = 0x811c9dc5, prime = 0x1000193;
    auto hash = basis;
    for (size_t i = 0; i < len; ++i) {
        hash ^= s[i];
        hash *= prime;
    }
    return hash;
}

inline constexpr hash_t hash(std::string_view s)
{
    return hash(s.data(), s.length());
}

struct ArgumentParser {
    std::span<char *> arguments;
};

struct Options {
    bool testing = false;
    bool check_only = false;
};

inline Options opts;

void process_cmdline(ArgumentParser &);

//
// Lexer
//

inline constexpr bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

inline constexpr bool is_lower(char c)
{
    return c >= 'a' && c <= 'z';
}

inline constexpr bool is_upper(char c)
{
    return c >= 'A' && c <= 'Z';
}

inline constexpr bool is_alpha(char c)
{
    return is_lower(c) || is_upper(c);
}

inline constexpr bool is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
}

inline constexpr bool is_control_char(char c)
{
    return (c >= 0 && c <= 31) || c == 127;
}

inline constexpr char to_upper(char c)
{
    return is_lower(c) ? (c - ('a' - 'A')) : c;
}

enum class TokenKind : uint32_t {
    //
    // Group
    //
    GroupEmpty = 1 << 16,
    GroupNewline = 1 << 17,
    GroupNumber = 1 << 18,
    GroupIdentifier = 1 << 19,
    GroupString = 1 << 20,
    GroupOperator = 1 << 21,
    GroupKeyword = 1 << 22,
    group_mask = 0xffff'0000,

    //
    // Value
    //
    Plus = 1 | GroupOperator,
    Minus,
    Star,
    Slash,
    Percent,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Comma,
    Equals,
    Excl,
    Colon,
    Hash,
    LAngleEquals,
    RAngleEquals,
    EqualsEquals,
    ExclEquals,
    ColonEquals,
    Arrow,
    And,
    Or,

    Fn = 1 | GroupKeyword,
    Return,
    If,
    Else,
    While,
    Alias,
    False,
    True,
};

struct SourceLocation {
    size_t line;
    size_t column;
    size_t position; // This is the actual offset in the file, equivalent to Lexer::position.
};

inline TokenKind get_group(TokenKind kind)
{
    return static_cast<TokenKind>(to_underlying(kind) & to_underlying(TokenKind::group_mask));
}

inline bool is_group(TokenKind kind, TokenKind cmp)
{
    return get_group(kind) == cmp;
}

struct Token {
    static constexpr Token make_empty()
    {
        return Token{ .kind = TokenKind::GroupEmpty };
    }

    static constexpr Token make_newline(SourceLocation _location)
    {
        return Token{ .string = "\n",

            .kind = TokenKind::GroupNewline,
            .location = _location };
    }

    static constexpr Token make_number(std::string_view _string, SourceLocation _location)
    {
        return Token{ .string = _string, .kind = TokenKind::GroupNumber, .location = _location };
    }

    static constexpr Token make_string(
        std::unique_ptr<std::string> &&_string, size_t length, SourceLocation _location)
    {
        return Token{ .kind = TokenKind::GroupString,
            .location = _location,
            .real_string = std::move(_string),
            .real_length = length };
    }

    static constexpr Token make_operator(
        std::string_view _string, TokenKind _kind, SourceLocation _location)
    {
        return Token{ .string = _string, .kind = _kind, .location = _location };
    }

    static constexpr Token make_identifier(std::string_view _string, SourceLocation _location)
    {
        return Token{
            .string = _string, .kind = TokenKind::GroupIdentifier, .location = _location
        };
    }

    static constexpr Token make_keyword(
        std::string_view _string, TokenKind _kind2, SourceLocation _location)
    {
        return Token{ .string = _string, .kind = _kind2, .location = _location };
    }

    std::string_view string{}; // use for AstIdentifier?
    TokenKind kind = TokenKind::GroupEmpty;
    SourceLocation location{};

    // For strings
    std::unique_ptr<std::string> real_string{};
    size_t real_length{};
};

struct InputFile {
    void open(std::string_view name);
    void close();

    std::string filename;
    int file_handle = -1;
    off_t file_size;
    char *map;
};

// TODO - finish de-OOPing
struct Lexer {
    char get(size_t offset = 0) const
    {
        if (out_of_bounds(offset)) {
            return '\0';
        }
        return string[position + offset];
    }

    bool out_of_bounds(size_t offset = 0) const
    {
        // TODO - properly avoid overflow
        return (position + offset >= string.length());
    }

    size_t count_while(const std::predicate<char> auto &&cond, size_t offset = 0) const
    {
        size_t count = offset;
        while (position + count < string.length() && cond(get(count))) {
            ++count;
        }
        return count;
    }

    SourceLocation location() const
    {
        return { line, column, position };
    }

    void set_input(std::string_view filename);
    void free_input();

    InputFile input;
    std::string_view string;
    size_t position = 0;
    size_t line = 1;
    size_t column = 0;
    bool ignore_newlines = true;
};

std::string_view get_line(std::string_view, ssize_t pos);

struct Compiler;

Token lex(Compiler &);

void expect(Compiler &, const std::string &exp, const Token &);
void expect(Compiler &, TokenKind, const Token &);

inline void advance_column(Lexer &lexer, size_t count = 1)
{
    lexer.position += count;
    lexer.column += count;
}

inline void advance_line(Lexer &lexer, size_t count = 1)
{
    lexer.line += count;
    lexer.position += count;
    lexer.column = 0;
}

inline void consume(Lexer &lexer, const Token &tk)
{
    assert(
        !is_group(tk.kind, TokenKind::GroupNewline) && !is_group(tk.kind, TokenKind::GroupString));
    advance_column(lexer, tk.string.length());
}

inline void consume_string(Lexer &lexer, const Token &tk)
{
    assert(is_group(tk.kind, TokenKind::GroupString));
    advance_column(lexer, tk.real_length);
}

void consume_expected(Compiler &, const std::string &exp, const Token &);
void consume_expected(Compiler &, TokenKind, const Token &);

void consume_newline_or_eof(Compiler &, const Token &);

//
// Parser
//

#define ENUMERATE_AST_TYPES()        \
    __ENUMERATE_AST_TYPE(Integer)    \
    __ENUMERATE_AST_TYPE(Boolean)    \
    __ENUMERATE_AST_TYPE(String)     \
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
    __ENUMERATE_OPERATION(Negate)        \
    __ENUMERATE_OPERATION(Call)          \
    __ENUMERATE_OPERATION(Add)           \
    __ENUMERATE_OPERATION(Subtract)      \
    __ENUMERATE_OPERATION(Multiply)      \
    __ENUMERATE_OPERATION(Divide)        \
    __ENUMERATE_OPERATION(Modulo)        \
    __ENUMERATE_OPERATION(Equals)        \
    __ENUMERATE_OPERATION(NotEquals)     \
    __ENUMERATE_OPERATION(Greater)       \
    __ENUMERATE_OPERATION(GreaterEquals) \
    __ENUMERATE_OPERATION(Less)          \
    __ENUMERATE_OPERATION(LessEquals)    \
    __ENUMERATE_OPERATION(Assign)        \
    __ENUMERATE_OPERATION(LogicalAnd)    \
    __ENUMERATE_OPERATION(LogicalOr)     \
    __ENUMERATE_OPERATION(Return)        \
    __ENUMERATE_OPERATION(VariableDecl)  \
    __ENUMERATE_OPERATION(FunctionDecl)  \
    __ENUMERATE_OPERATION(If)            \
    __ENUMERATE_OPERATION(While)         \
    __ENUMERATE_OPERATION(Cast)          \
    __ENUMERATE_OPERATION(PushArg)       \
    __ENUMERATE_OPERATION(Branch)        \
    __ENUMERATE_OPERATION(CondBranch)

enum class Operation {
#define __ENUMERATE_OPERATION(operation) operation,
    ENUMERATE_OPERATIONS()
#undef __ENUMERATE_OPERATION
};

struct Scope;
inline Scope *current_scope;

inline size_t ast_alloc_count, ast_free_count;

struct AstAllocMark {
    void *p;
    size_t size;
    std::array<void *, 16> trace;
    int frames;
    bool deleted;
};

inline std::vector<AstAllocMark> marks;

#define EC_ENUM_OPERATOR(type, op)                                                \
    constexpr type operator op(const type lhs, const type rhs) noexcept           \
    {                                                                             \
        return static_cast<type>(to_underlying(lhs) op to_underlying(rhs));       \
    }                                                                             \
    constexpr type &operator op##=(type & lhs, const type rhs) noexcept           \
    {                                                                             \
        return lhs = static_cast<type>(to_underlying(lhs) op to_underlying(rhs)); \
    }

#define EC_ENUM_BIT_OPS(type)                        \
    EC_ENUM_OPERATOR(type, &)                        \
    EC_ENUM_OPERATOR(type, |)                        \
    EC_ENUM_OPERATOR(type, ^)                        \
    EC_ENUM_OPERATOR(type, >>)                       \
    EC_ENUM_OPERATOR(type, <<)                       \
    constexpr type operator~(const type x) noexcept  \
    {                                                \
        return static_cast<type>(~to_underlying(x)); \
    }                                                \
    constexpr auto operator!(const type x) noexcept  \
    {                                                \
        return !to_underlying(x);                    \
    }

#define enum_flags(name, underlying) \
    enum class name : underlying;    \
    EC_ENUM_BIT_OPS(name)            \
    enum class name : underlying

enum_flags(AstFlags, uint64_t){
    FOLDED = 1 << 0, // Prevent verifier trying to constant fold this tree multiple times.
};

struct Ast {
    Scope *scope;
    AstType type;
    Operation operation;
    SourceLocation location;
    AstFlags flags{};

    Ast(AstType _type, Operation _operation = Operation::None, SourceLocation _location = {})
        : scope(current_scope)
        , type(_type)
        , operation(_operation)
        , location(_location)
    {
    }

#if AST_ALLOC_PARANOID
    static void *operator new(size_t size)
    {
        auto p = malloc(size);
        // dbgln("new ast @ {} ({})", p, size);
        ++ast_alloc_count;
        std::array<void *, 16> trace;
        int frames = ::backtrace(trace.data(), trace.size());
        marks.emplace_back(p, size, trace, frames, false);
        return p;
    }

    static void operator delete(void *p)
    {
        // dbgln("delete {}", p);
        ++ast_free_count;
        if (auto it = std::ranges::find(marks, p, &AstAllocMark::p); it != marks.end()) {
            it->deleted = true;
        }
        free(static_cast<Ast *>(p));
    }
#endif
};

struct Type;

struct AstLiteral : Ast {
    Type *literal_type = nullptr;

    union {
        uint32_t u32;
        uint64_t u64;
        int32_t s32;
        int64_t s64;
        bool boolean;
    } u;

    explicit AstLiteral(Type *_type, AstType _ast_type, uint64_t _literal, SourceLocation _location)
        : Ast(_ast_type, Operation::None, _location)
        , literal_type(_type)
    {
        u.u64 = _literal;
    }

    explicit AstLiteral(Type *_type, AstType _ast_type, uint32_t _literal, SourceLocation _location)
        : Ast(_ast_type, Operation::None, _location)
        , literal_type(_type)
    {
        u.u32 = _literal;
    }

    explicit AstLiteral(Type *_type, AstType _ast_type, int32_t _literal, SourceLocation _location)
        : Ast(_ast_type, Operation::None, _location)
        , literal_type(_type)
    {
        u.s32 = _literal;
    }

    explicit AstLiteral(Type *_type, AstType _ast_type, int64_t _literal, SourceLocation _location)
        : Ast(_ast_type, Operation::None, _location)
        , literal_type(_type)
    {
        u.s64 = _literal;
    }

    explicit AstLiteral(Type *_type, AstType _ast_type, bool _literal, SourceLocation _location)
        : Ast(_ast_type, Operation::None, _location)
        , literal_type(_type)
    {
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

std::string extract_integer_constant(AstLiteral *);

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

struct AstNegate : Ast {
    Ast *operand;

    explicit AstNegate(Operation _op, Ast *_operand, SourceLocation _location)
        : Ast(AstType::Unary, _op, _location)
        , operand(_operand)
    {
    }
};

struct AstCast : Ast {
    Ast *expr;
    Type *cast_type;

    explicit AstCast(Ast *_expr, Type *_cast_type, SourceLocation _location)
        : Ast(AstType::Unary, Operation::Cast, _location)
        , expr(_expr)
        , cast_type(_cast_type)

    {
    }
};

struct AstFunctionDecl;

struct AstCall : Ast {
    std::string_view name;
    std::vector<Ast *> args;
    AstFunctionDecl *fn = nullptr; // Use get_callee() instead of accessing this directly

    explicit AstCall(
        std::string_view _name, const std::vector<Ast *> &_args, SourceLocation _location)
        : Ast(AstType::Unary, Operation::Call, _location)
        , name(_name)
        , args(_args)
    {
    }
};

AstFunctionDecl *get_callee(Compiler &, AstCall *);

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

enum class TypeFlags {
    //
    // Type kind
    //
    Void = 1,
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
    UNRESOLVED = (1 << 4),
    ALIAS = (1 << 5),
    BUILTIN = (1 << 6),

    // Integer flags
    UNSIGNED = (1 << 7),
};

constexpr TypeFlags operator|(const TypeFlags lhs, const TypeFlags rhs)
{
    return static_cast<TypeFlags>(to_underlying(lhs) | to_underlying(rhs));
}

constexpr TypeFlags &operator|=(TypeFlags &lhs, const TypeFlags rhs)
{
    return lhs = static_cast<TypeFlags>(to_underlying(lhs) | to_underlying(rhs));
}

constexpr TypeFlags operator&(const TypeFlags lhs, const TypeFlags rhs)
{
    return static_cast<TypeFlags>(to_underlying(lhs) & to_underlying(rhs));
}

constexpr TypeFlags &operator&=(TypeFlags &lhs, const TypeFlags rhs)
{
    return lhs = static_cast<TypeFlags>(to_underlying(lhs) & to_underlying(rhs));
}

constexpr TypeFlags operator~(TypeFlags rhs)
{
    return static_cast<TypeFlags>(~to_underlying(rhs));
}

template<class E>
requires std::is_enum_v<E>
inline bool has_flag(E value, E flag)
{
    return static_cast<bool>(to_underlying(value) & to_underlying(flag));
}

struct Type {
    std::string name{};
    TypeFlags flags{};
    uint8_t size = 0;
    Type *real = nullptr; // If this type is an alias, this points to the underlying type (which may
                          // also be an alias)
    SourceLocation location{};

    TypeFlags get_kind() const
    {
        return flags & TypeFlags::kind_mask;
    }

    TypeFlags get_flags() const
    {
        return flags & ~TypeFlags::kind_mask;
    }

    bool has_flag(TypeFlags flag) const
    {
        return ::has_flag(flags, flag);
    }
};

Type *void_type();
Type *u32_type();
Type *u64_type();
Type *s32_type();
Type *s64_type();
Type *bool_type();
Type *string_type();
Type *unresolved_type();

Type *get_unaliased_type(Type *);

struct Variable {
    Type *type;
    std::string name;
    // Index into the associated scope's variables array.
    ssize_t index_in_scope = -1;
    // If this is a function parameter...
    int param_index = -1;

    bool is_parameter() const
    {
        return param_index > -1;
    }

    explicit Variable(Type *_type, std::string_view _name)
        : type(_type)
        , name(_name)
    {
    }
};

struct AstVariableDecl : Ast {
    Variable var;
    Ast *init_expr;

    explicit AstVariableDecl(
        Type *_type, std::string_view _name, Ast *_init_expr, SourceLocation _location)
        : Ast(AstType::Statement, Operation::VariableDecl, _location)
        , var(_type, _name)
        , init_expr(_init_expr)
    {
    }
};

struct AstBlock : Ast {
    std::vector<Ast *> stmts;

    explicit AstBlock(const std::vector<Ast *> &_stmts)
        : Ast(AstType::Block)
        , stmts(_stmts)
    {
    }
};

struct AstIf : Ast {
    Ast *expr;
    AstBlock *body;
    AstBlock *else_body;

    explicit AstIf(Ast *_expr, AstBlock *_body, AstBlock *_else_body, SourceLocation _location)
        : Ast(AstType::Statement, Operation::If, _location)
        , expr(_expr)
        , body(_body)
        , else_body(_else_body)
    {
    }
};

struct AstWhile : Ast {
    Ast *expr;
    AstBlock *body;

    explicit AstWhile(Ast *_expr, AstBlock *_body, SourceLocation _location)
        : Ast(AstType::Statement, Operation::While, _location)
        , expr(_expr)
        , body(_body)
    {
    }
};

struct AstFunctionDecl : Ast {
    std::string name;
    Type *return_type;
    std::vector<AstVariableDecl *> params;
    AstBlock *body;
    // Return statements are all contained somewhere in the body, but to speed up verification
    // we also put them into this vector.
    std::vector<AstReturn *> return_stmts;
    uint64_t call_count = 0;

    explicit AstFunctionDecl(std::string_view _name, Type *_return_type,
        const std::vector<AstVariableDecl *> &_params, AstBlock *_body, SourceLocation _location)
        : Ast(AstType::Statement, Operation::FunctionDecl, _location)
        , name(_name)
        , return_type(_return_type)
        , params(_params)
        , body(_body)
    {
    }

    bool returns_void() const
    {
        return get_unaliased_type(return_type)->get_kind() == TypeFlags::Void;
    }
};

Ast *parse_stmt(Compiler &, AstFunctionDecl *);

void free_ast(Ast *);
void free_ast(std::vector<Ast *> &);

enum class TypeOverridable {
    No,
    Yes
};
Ast *try_constant_fold(Compiler &, Ast *, Type *&expected, TypeOverridable);

struct Scope {
    Scope *parent;
    std::unordered_map<std::string_view, Type *> types{};
    std::unordered_map<std::string_view, AstFunctionDecl *> functions{};
    std::vector<AstVariableDecl *> variables{};

    Scope(Scope *_parent)
        : parent(_parent)
    {
    }

    void add_variable(Compiler &, AstVariableDecl *);
    void add_function(Compiler &, Ast *, std::string_view unmangled_name);
    void add_alias(Compiler &, Type *, std::string_view alias, SourceLocation);
};

inline std::vector<Scope *> g_scopes;

inline void enter_new_scope()
{
    g_scopes.emplace_back(new Scope(current_scope));
    current_scope = g_scopes.back();
}

inline void leave_scope()
{
    if (current_scope) {
        current_scope = current_scope->parent;
    }
}

inline void free_scopes()
{
    for (auto *scope : g_scopes) {
        delete scope;
    }
    g_scopes.clear();
    current_scope = nullptr;
}

enum class SearchParents {
    No,
    Yes
};

AstVariableDecl *find_variable(Scope *, std::string_view name, Scope **result_scope = nullptr,
    SearchParents = SearchParents::Yes);

AstFunctionDecl *find_function(Scope *, std::string_view name, Scope **result_scope = nullptr,
    SearchParents = SearchParents::Yes);

Type *find_type(Scope *, std::string_view name, Scope **result_scope = nullptr,
    SearchParents = SearchParents::Yes);

enum class ErrorOnShadowing {
    No,
    Yes
};

void diagnose_redeclaration_or_shadowing(
    Compiler &, Scope *, std::string_view name, std::string_view type, ErrorOnShadowing);

//
// Verification
//

void verify_main(Compiler &, AstFunctionDecl *);

bool has_top_level_return(AstBlock *);
bool types_match(Type *t1, Type *t2);

//
// IR
//

#define ENUMERATE_IR_ARG_TYPES()        \
    __ENUMERATE_IR_ARG_TYPE(Empty)      \
    __ENUMERATE_IR_ARG_TYPE(Vreg)       \
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

struct IRArg {
    IRArgType arg_type;
    size_t string_index = 0;

    union {
        Variable *variable = nullptr;
        AstLiteral *constant;
        AstString *string;
        AstFunctionDecl *function;
        ssize_t vreg;
        BasicBlock *basic_block;
        Type *type;
    };
};

struct IR {
    Ast *ast;
    AstType type;
    Operation operation;
    IRArg left;
    IRArg right;
    size_t basic_block_index;         // which basic block am I in?
    ssize_t target = -1;
    std::vector<IR *> target_used_by; // which IRs link to this target?

    bool has_vreg_target() const
    {
        return operation != Operation::PushArg && target > 0;
    }
};

struct IRBranch : IR {
    size_t cond_vreg;
};

struct BasicBlock {
    std::vector<IR *> code;
    size_t index = 0; // Index in IRFunction
    std::string label_name;
};

struct IRFunction {
    std::vector<BasicBlock *> basic_blocks;
    BasicBlock *current_block = nullptr;
    std::unordered_map<uint64_t, int> stack_offsets;
    AstFunctionDecl *ast;
    ssize_t temp_regs = 0;
};

inline std::vector<std::string> string_map;

struct IRBuilder {
    std::vector<IRFunction *> functions;
    IRFunction *current_function;
};

void generate_ir(Compiler &, AstFunctionDecl *);
void optimize_ir(Compiler &);

void free_ir(IRFunction *);

//
// Backend
//

void emit_asm(Compiler &);

//
// Testing
//

void run_tests(const fs::path &dir);
void run_single_test(const fs::path &file);

struct TestingException : std::runtime_error {
    TestingException(const char *const msg, ErrorType _type) throw()
        : std::runtime_error(msg)
        , type(_type)
    {
    }

    ErrorType type;
};

struct TestMode {
    TestType test_type = TestType::CanCompile;
    ErrorType error_type;
    uint64_t return_value;
};

//
// Main context
//

struct Compiler {
    Lexer lexer;
    IRBuilder ir_builder;
    TestMode test_mode;

    void initialize();
    void add_default_types();
    void free_types();
    void free_ir();
    void cleanup(AstFunctionDecl *root);
};

void compiler_main(Compiler &, AstFunctionDecl *root);

//
// Diagnostics
//

void print_diag_line(std::string_view, SourceLocation);
std::string make_printable(std::string_view);
std::string to_string(TokenKind);

[[noreturn]] void diag_error_at(Compiler &cc, [[maybe_unused]] SourceLocation location,
    [[maybe_unused]] ErrorType type, std::string_view fmt, auto &&...args)
{
    const auto msg = std::vformat(fmt, std::make_format_args(args...));
    if (!opts.testing) {
        std::println("\033[31;1merror:\033[0m {}({},{}):", cc.lexer.input.filename, location.line,
            location.column);
        std::println("{}", msg);
        print_diag_line(cc.lexer.string, location);
#if _DEBUG
        __builtin_trap();
#else
        exit(1);
#endif
    } else {
        throw TestingException(msg.c_str(), type);
    }
}

[[noreturn]] void diag_lexer_error(Compiler &cc, std::string_view fmt, auto &&...args)
{
    diag_error_at(cc, cc.lexer.location(), ErrorType::Lexer, fmt, args...);
}

void diag_warning_at(Compiler &cc, [[maybe_unused]] SourceLocation location,
    [[maybe_unused]] std::string_view fmt, [[maybe_unused]] auto &&...args)
{
    // TODO: maybe test warnings too?
    if (!opts.testing) {
        std::println("\033[93;1mwarning:\033[0m {}({},{}):", cc.lexer.input.filename, location.line,
            location.column + 1);
        const auto msg = std::vformat(fmt, std::make_format_args(args...));
        std::println("{}", msg);
        print_diag_line(cc.lexer.string, location);
    }
}

void diag_ast_warning(Compiler &cc, Ast *ast, std::string_view fmt, auto &&...args)
{
    diag_warning_at(cc, ast->location, fmt, args...);
}

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
void print_ir(const std::vector<IR *> &);
void print_ir(const IRFunction &);
