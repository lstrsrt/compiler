#include "testing.hh"
#include "compiler.hh"
#include "debug.hh"
#include "frontend.hh"
#include "parser.hh"

#include <algorithm>
#include <utility>

#ifdef __linux__
#include <linux/limits.h>
#include <spawn.h>
#include <sys/wait.h>
#else
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <Windows.h>
#include <numeric>
#include <process.h>
#include <ranges>
#include <stdio.h>
#endif

struct TestData {
    size_t current_test = 0;
    size_t passed_tests = 0;
    size_t total_tests = 0;
    bool printed_result = false;
    fs::path prev_wd;
};

void switch_to_tmp(TestData &td)
{
    td.prev_wd = fs::current_path();
    fs::current_path(fs::temp_directory_path());
}

void switch_back(TestData &td)
{
    fs::current_path(td.prev_wd);
}

int spawn_blocking_process(const fs::path &exe_path, const std::vector<std::string> &_cmdline)
{
#ifdef __linux__
    std::vector<char *> cmdline;
    // Put the path first
    auto path = exe_path.string();
    cmdline.push_back(path.data());
    for (const auto &arg : _cmdline) {
        cmdline.push_back(const_cast<char *>(arg.c_str()));
    }
    // argv has to be terminated with 0
    cmdline.push_back(nullptr);

    pid_t pid;
    if (posix_spawn(&pid, path.data(), nullptr, nullptr, cmdline.data(), environ)) {
        std::println("error spawning {}", path);
        perror("posix_spawn");
        return -1;
    }

    int status;
    do {
        if (waitpid(pid, &status, 0) == -1) {
            return -1;
        }
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
        return -1;

    } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    return -1;
#else
    std::string cmd = exe_path.string() + " ";
    cmd += std::ranges::to<std::string>(_cmdline | std::views::join_with(' '));
    STARTUPINFOA si{};
    PROCESS_INFORMATION pi{};

    si.cb = sizeof(si);

    if (!CreateProcessA(
            nullptr, cmd.data(), nullptr, nullptr, FALSE, 0, nullptr, nullptr, &si, &pi)) {
        perror("CreateProcess");
        return -1;
    }

    CloseHandle(pi.hThread);
    WaitForSingleObject(pi.hProcess, INFINITE);
    DWORD exit_code;
    if (!GetExitCodeProcess(pi.hProcess, &exit_code)) {
        perror("GetExitCodeProcess");
        CloseHandle(pi.hProcess);
        return -1;
    }

    CloseHandle(pi.hProcess);
    return static_cast<int>(exit_code);
#endif
}

bool files_are_equal(const std::string &path1, const std::string &path2)
{
    File file1;
    File file2;

    if (!file1.open(path1, OpenFlags::Open | OpenFlags::READ)) {
        std::println("couldn't open file '{}'", path1);
        return false;
    }
    if (!file2.open(path2, OpenFlags::Open | OpenFlags::READ)) {
        std::println("couldn't open file '{}'", path2);
        return false;
    }

    auto s1 = file1.view();
    auto s2 = file2.view();
    bool equals = std::equal(s1.begin(), s1.end(), s2.begin());
    return equals;
}

bool extensions_match(const fs::path &a, const fs::path &b)
{
    if (a.has_extension() ^ b.has_extension()) {
        // Only one has an extension
        return false;
    }
    if (!a.has_extension()) {
        // Both have no extension
        return true;
    }
    // Both have an extension, check if they're equal
    return a.extension() == b.extension();
}

bool run_compare_test()
{
    if (test_mode.compare_type == CompareType::None) {
        return true;
    }

    auto cmp_file = fs::path(test_mode.compare_file);
    auto ref_file = fs::path(test_mode.reference_file);
    if (!fs::exists(ref_file)) {
        std::println("{}testing error{}: reference file '{}' not found.\n"
                     "place the reference file in the working directory.",
            colors::Red, colors::Default, ref_file.string());
    }
    if (!extensions_match(cmp_file, ref_file)) {
        std::println("{}test warning{}: extensions for files '{}' and '{}' don't match",
            colors::Yellow, colors::Default, cmp_file.string(), ref_file.string());
    }
    if (files_are_equal(cmp_file.string(), ref_file.string())) {
        fs::remove(cmp_file);
        return true;
    }

    // TODO: print diff
    std::println("{}testing error{}: files '{}' and '{}' not equal", colors::Red, colors::Default,
        cmp_file.string(), ref_file.string());

    return false;
}

void run_test(TestData &td, const fs::path &file)
{
    using namespace colors;
    Defer reset{ [] { test_mode = {}; } };

    const auto report_single_test_result = [&]() {
        if (td.total_tests == 1) {
            auto color = td.passed_tests ? Green : Red;
            const auto *str = td.passed_tests ? "passed" : "failed";
            std::println("{}{}{} {}{}", color, str, DefaultBold, file.string(), Default);
        }
    };

    if (td.total_tests == 1) {
        switch_to_tmp(td);
    }

    bool compiled = false;
    try {
        ++td.current_test;
        invoke_compiler(file.string(), InputMode::File);
        compiled = true;
    } catch (TestingException &te) {
        if (test_mode.test_type == TestType::Error) {
            if (te.type != test_mode.error_type) {
                std::println("{}testing error{}: non-matching exception {} (expected {})", Red,
                    Default, to_string(te.type), to_string(test_mode.error_type));
            } else {
                ++td.passed_tests;
            }
        } else {
            std::println("{}testing error{}: failed compilation", Red, Default);
        }
    } catch (std::exception &e) {
        std::println("unexpected exception: {}", e.what());
    }

    if (!compiled) {
        return report_single_test_result();
    }

    // The new IR has no backend yet, so we can't check return values.
    if (test_mode.test_type == TestType::CanCompile
        || (opts.new_ir && test_mode.test_type == TestType::ReturnsValue)) {
        if (run_compare_test()) {
            ++td.passed_tests;
        }
    } else if (test_mode.test_type == TestType::Error) {
        std::println("{}testing error{}: should have failed compilation", Red, Default);
    } else if (test_mode.test_type == TestType::ReturnsValue) {
        bool ok = run_compare_test();
        create_executable(opts.output_name, opts.output_exe_name);
        int status = spawn_blocking_process(fs::current_path() / opts.output_exe_name, {});
        if (std::cmp_equal(status, test_mode.return_value)) {
            if (ok) {
                ++td.passed_tests;
            }
        } else {
            std::println("{}testing error{}: non-matching return value {} (expected {})", Red,
                Default, status, test_mode.return_value);
        }
    }

    if (td.total_tests == 1) {
        switch_back(td);
    }

    report_single_test_result();
}

void run_single_test(TestData &td, const fs::path &path)
{
    td.total_tests = 1;
    run_test(td, fs::canonical(path));
}

void run_single_test(const fs::path &path)
{
    TestData td;
    run_single_test(td, path);
}

void run_tests(TestData &td, const fs::path &path, bool root)
{
    using namespace colors;

    if (!fs::exists(path) || !fs::is_directory(path)) {
        return;
    }

    std::vector<fs::path> subdirs;
    std::vector<fs::path> files;
    for (const auto &entry : fs::directory_iterator(path)) {
        if (entry.is_directory()) {
            subdirs.push_back(fs::canonical(entry));
        } else if (entry.path().has_filename() && entry.path().filename().has_extension()
            && entry.path().filename().extension() == ".txt") {
            files.push_back(fs::canonical(entry));
            ++td.total_tests;
        }
    }

    std::ranges::sort(files);
    std::ranges::sort(subdirs);

    switch_to_tmp(td);

    for (const auto &file : files) {
        run_test(td, file);
    }

    for (const auto &dir : subdirs) {
        run_tests(td, dir, false);
    }

    switch_back(td);

    if (root && !td.printed_result && td.current_test == td.total_tests) {
        auto color = (td.passed_tests == td.total_tests) ? Green : Red;
        std::println("{}passed {}{}/{}{} tests{}", DefaultBold, color, td.passed_tests,
            td.total_tests, DefaultBold, Default);
        td.printed_result = true;
    }
}

void run_tests(const fs::path &path, bool root)
{
    TestData td;
    run_tests(td, path, root);
}

std::string to_string(ErrorType type)
{
    using enum ErrorType;
    switch (type) {
#define __ENUMERATE_ERROR_TYPE(type) \
    case type:                       \
        return #type;
        ENUMERATE_ERROR_TYPES()
#undef __ENUMERATE_ERROR_TYPE
    }
}
