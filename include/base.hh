#pragma once

#include <cassert>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <print>
#include <string>
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
    explicit Defer(Fn _fn)
        : fn(std::move(_fn))
    {
    }

    ~Defer()
    {
        if (!disabled) {
            fn();
        }
    }

    void disable()
    {
        disabled = true;
    }

    void enable()
    {
        disabled = false;
    }

    Fn fn;
    bool disabled = false;
};

#define AST_ALLOC_PARANOID 0

#if AST_ALLOC_PARANOID
#include <execinfo.h>
#endif

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
    std::println("\n{}TODO{}: {}", colors::Red, colors::Default, func);
#ifdef _DEBUG
    __builtin_trap();
#else
    exit(1);
#endif
}

#define TODO() todo(__func__)

template<typename... Args>
[[noreturn]] void die(std::string_view msg, Args &&...args)
{
    if constexpr (sizeof...(args)) {
        std::println(
            "{}", std::vformat(msg, std::make_format_args(std::forward<decltype(args)>(args)...)));
    } else {
        std::println("{}", msg);
    }
    exit(EXIT_FAILURE);
}

#define DEFINE_ENUM_OPERATOR(type, op)                                            \
    constexpr type operator op(const type lhs, const type rhs) noexcept           \
    {                                                                             \
        return static_cast<type>(to_underlying(lhs) op to_underlying(rhs));       \
    }                                                                             \
    constexpr type &operator op##=(type & lhs, const type rhs) noexcept           \
    {                                                                             \
        return lhs = static_cast<type>(to_underlying(lhs) op to_underlying(rhs)); \
    }

#define DEFINE_ENUM_BIT_OPS(type)                    \
    DEFINE_ENUM_OPERATOR(type, &)                    \
    DEFINE_ENUM_OPERATOR(type, |)                    \
    DEFINE_ENUM_OPERATOR(type, ^)                    \
    DEFINE_ENUM_OPERATOR(type, >>)                   \
    DEFINE_ENUM_OPERATOR(type, <<)                   \
    constexpr type operator~(const type x) noexcept  \
    {                                                \
        return static_cast<type>(~to_underlying(x)); \
    }                                                \
    constexpr auto operator!(const type x) noexcept  \
    {                                                \
        return !to_underlying(x);                    \
    }

#ifndef enum_flags
#define enum_flags(name, underlying) \
    enum class name : underlying;    \
    DEFINE_ENUM_BIT_OPS(name)        \
    enum class name : underlying
#endif

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

//
// Forward some types
//

struct Compiler;
struct Ast;
struct Scope;
struct Type;
