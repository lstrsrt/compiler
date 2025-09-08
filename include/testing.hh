#pragma once

#include "base.hh"

//
// Testing
//

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

void run_tests(const fs::path &);
void run_single_test(const fs::path &);

struct TestingException : std::runtime_error {
    TestingException(const char *const msg, ErrorType _type) noexcept
        : std::runtime_error(msg)
        , type(_type)
    {
    }

    ErrorType type;
};

struct TestMode {
    TestType test_type = TestType::CanCompile;
    ErrorType error_type{};
    uint64_t return_value = 0;
};
