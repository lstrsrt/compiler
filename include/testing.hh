#pragma once

#include "base.hh"
#include "file.hh"

//
// Testing
//

enum class TestType {
    CanCompile,
    ReturnsValue,
    Error,
};

#define ENUMERATE_ERROR_TYPES()          \
    __ENUMERATE_ERROR_TYPE(None)         \
    __ENUMERATE_ERROR_TYPE(Lexer)        \
    __ENUMERATE_ERROR_TYPE(Parser)       \
    __ENUMERATE_ERROR_TYPE(Verification) \
    __ENUMERATE_ERROR_TYPE(TypeCheck)

enum class ErrorType {
#define __ENUMERATE_ERROR_TYPE(type) type,
    ENUMERATE_ERROR_TYPES()
#undef __ENUMERATE_ERROR_TYPE
};

enum class CompareType {
    None,
    Ast,
    IR,
    Asm,
};

void run_tests(const fs::path &, bool root = true);
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
    ErrorType error_type = ErrorType::None;
    CompareType compare_type = CompareType::None;
    std::string reference_file;
    std::string compare_file;
    uint64_t return_value = 0;
};

template<class T>
std::string write_comparison_file(
    const std::string &file_ext, T object, std::invocable<File &, T> auto &&dump_callback)
{
    auto file = File::make_temporary(file_ext, OpenFlags::WRITE);
    if (file.is_valid()) {
        dump_callback(file, object);
        if (!file.close()) {
            die("failed to write to comparison file");
        }
    } else {
        die("failed to create comparison file");
    }
    return file.filename;
}
