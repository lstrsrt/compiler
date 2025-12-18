#pragma once

#include <cassert>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <limits>
#include <print>
#include <string>
#include <utility>
#include <vector>

namespace fs = std::filesystem;
namespace ch = std::chrono;
using namespace std::string_view_literals;

#ifndef __linux__
// ssize_t is POSIX.
using ssize_t = std::make_signed<size_t>::type;
#endif
using std::size, std::ssize;

namespace colors {
static const std::string Bold = "\033[1m";
static const std::string Cyan = "\033[36;1m";
static const std::string Default = "\033[0m";
static const std::string DefaultBold = Default + Bold;
static const std::string Red = "\033[31;1m";
static const std::string Green = "\033[32;1m";
static const std::string Yellow = "\033[33;1m";
static const std::string Blue = "\033[38;5;39m";
} // namespace colors

static constexpr auto S8Min = std::numeric_limits<int8_t>::min();
static constexpr auto S16Min = std::numeric_limits<int16_t>::min();
static constexpr auto S32Min = std::numeric_limits<int32_t>::min();
static constexpr auto S64Min = std::numeric_limits<int64_t>::min();

static constexpr auto S8Max = std::numeric_limits<int8_t>::max();
static constexpr auto S16Max = std::numeric_limits<int16_t>::max();
static constexpr auto S32Max = std::numeric_limits<int32_t>::max();
static constexpr auto S64Max = std::numeric_limits<int64_t>::max();

static constexpr auto U8Max = std::numeric_limits<uint8_t>::max();
static constexpr auto U16Max = std::numeric_limits<uint16_t>::max();
static constexpr auto U32Max = std::numeric_limits<uint32_t>::max();
static constexpr auto U64Max = std::numeric_limits<uint64_t>::max();

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

    auto elapsed() const
    {
        return ch::duration<double>(ch::system_clock::now() - start);
    }

    ch::time_point<ch::system_clock> start;
};

constexpr size_t align_up(size_t value, size_t alignment)
{
    return (value + (alignment - 1)) & ~(alignment - 1);
}

[[noreturn]] inline void todo(const char *func, const char *file, int line)
{
    std::println("\n{}TODO ({}:{}){}: {}", colors::Red, file, line, colors::Default, func);
#ifdef _DEBUG
    // This produces a SIGILL (ud2), not a SIGTRAP because it's impossible to continue after here.
    __builtin_trap();
#endif
    exit(EXIT_FAILURE);
}

#define TODO() todo(__func__, __FILE__, __LINE__)

inline void trap()
{
    asm volatile("int $3");
}

template<typename... Args>
[[noreturn]] void die(std::string_view msg, Args &&...args)
{
    std::print("{}error:{} ", colors::Red, colors::Default);
    if constexpr (sizeof...(args)) {
        std::println(
            "{}", std::vformat(msg, std::make_format_args(std::forward<decltype(args)>(args)...)));
    } else {
        std::println("{}", msg);
    }
    exit(EXIT_FAILURE);
}

template<typename... Args>
void print_error(std::string_view msg, Args &&...args)
{
    std::print("{}error:{} ", colors::Red, colors::Default);
    if constexpr (sizeof...(args)) {
        std::println(
            "{}", std::vformat(msg, std::make_format_args(std::forward<decltype(args)>(args)...)));
    } else {
        std::println("{}", msg);
    }
}

// clang-format off
#define DEFINE_ENUM_OPERATOR(type, op)                                            \
    constexpr type operator op(const type lhs, const type rhs) noexcept           \
    {                                                                             \
        return static_cast<type>(to_underlying(lhs) op to_underlying(rhs));       \
    }                                                                             \
    constexpr type &operator op##=(type &lhs, const type rhs) noexcept            \
    {                                                                             \
        return lhs = static_cast<type>(to_underlying(lhs) op to_underlying(rhs)); \
    }
// clang-format on

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

template<typename E>
requires std::is_enum_v<E>
inline bool has_flag(E value, E flag)
{
    return static_cast<bool>(to_underlying(value) & to_underlying(flag));
}

template<typename E, typename... Es>
requires std::is_enum_v<E>
inline bool has_flags(E value, Es... flags)
{
    auto e = (to_underlying(flags) | ...);
    return (to_underlying(value) & e) == e;
}

using hash_t = uint32_t;

constexpr hash_t hash(const char *s, size_t len)
{
    constexpr hash_t basis = 0x811c9dc5, prime = 0x1000193;
    auto hash = basis;
    for (size_t i = 0; i < len; ++i) {
        hash ^= s[i];
        hash *= prime;
    }
    return hash;
}

constexpr hash_t hash(std::string_view s)
{
    return hash(s.data(), s.length());
}

int spawn_blocking_process(const fs::path &exe_path, const std::vector<std::string> &_cmdline);
void create_executable(const std::string &asm_file, const std::string &output_name);

//
// Forward some types
//

struct Compiler;
struct Ast;
struct Scope;
struct Type;
