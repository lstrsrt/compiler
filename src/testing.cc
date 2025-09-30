#include "testing.hh"
#include "compiler.hh"
#include "debug.hh"
#include "frontend.hh"
#include "parser.hh"

#include <algorithm>

#include <filesystem>
#include <linux/limits.h>
#include <spawn.h>
#include <sys/wait.h>

size_t current_test;
size_t passed_tests;
size_t total_tests;
bool printed_result;
fs::path prev_wd;

void switch_to_tmp()
{
    prev_wd = fs::current_path();
    fs::current_path(fs::temp_directory_path());
}

void switch_back()
{
    fs::current_path(prev_wd);
}

int spawn_blocking_process(const fs::path &exe_path, const std::vector<std::string> &_cmdline)
{
    std::vector<char *> cmdline;
    // Put the path first
    cmdline.push_back(const_cast<char *>(exe_path.string().data()));
    for (const auto &arg : _cmdline) {
        cmdline.push_back(const_cast<char *>(arg.c_str()));
    }
    // argv has to be terminated with 0
    cmdline.push_back(nullptr);

    pid_t pid;
    if (posix_spawn(&pid, exe_path.string().data(), nullptr, nullptr, cmdline.data(), environ)) {
        std::println("error spawning {}", exe_path.string());
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
}

bool files_are_equal(const std::string &path1, const std::string &path2)
{
    File file1, file2;

    if (!file1.open(path1, OpenFlags::Open | OpenFlags::READ)) {
        std::println("couldn't open file '{}'", path1);
        return false;
    }
    if (!file2.open(path2, OpenFlags::Open | OpenFlags::READ)) {
        std::println("couldn't open file '{}'", path2);
        file1.close();
        return false;
    }

    std::string_view s1 = file1.map;
    std::string_view s2 = file2.map;
    bool equals = std::equal(s1.begin(), s1.end(), s2.begin());
    file1.close();
    file2.close();
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

bool run_compare_test(Compiler &cc)
{
    if (cc.test_mode.compare_type == CompareType::None) {
        return true;
    }

    auto cmp_file = fs::path(cc.test_mode.compare_file);
    auto ref_file = fs::path(cc.test_mode.reference_file);
    if (!fs::exists(ref_file)) {
        // Maybe it's in the input path?
        auto alt = fs::path(cc.lexer.input.filename).replace_filename(ref_file);
        if (!fs::exists(alt)) {
            std::println("{}testing error{}: reference file '{}' not found.\n"
                         "place the reference file in the working directory or in the directory of "
                         "the file being compiled.",
                colors::Red, colors::Default, ref_file.string());
        } else {
            ref_file = alt;
        }
    }
    if (!extensions_match(cmp_file, ref_file)) {
        std::println("{}test warning{}: extensions for files '{}' and '{}' don't match",
            colors::Yellow, colors::Default, cmp_file.string(), ref_file.string());
    }
    if (files_are_equal(cmp_file, ref_file)) {
        fs::remove(cmp_file);
        return true;
    }

    // TODO: print diff
    std::println("{}testing error{}: files '{}' and '{}' not equal", colors::Red, colors::Default,
        cmp_file.string(), ref_file.string());

    return false;
}

void run_test(const fs::path &file)
{
    using namespace colors;

    const auto report_single_test_result = [&]() {
        if (total_tests == 1) {
            auto color = passed_tests ? Green : Red;
            const auto *str = passed_tests ? "passed" : "failed";
            std::println("{}{}{} {}{}", color, str, DefaultBold, file.string(), Default);
        }
    };

    if (total_tests == 1) {
        switch_to_tmp();
    }

    Compiler cc;
    bool compiled = false;
    cc.lexer.set_input(file);
    auto *main = new AstFunction("main", s32_type(), {}, new AstBlock({}), {});
    try {
        ++current_test;
        compiler_main(cc, main);
        cc.cleanup(main);
        cc.lexer.free_input();
        compiled = true;
    } catch (TestingException &te) {
        if (cc.test_mode.test_type == TestType::Error) {
            if (te.type != cc.test_mode.error_type) {
                std::println("{}testing error{}: non-matching exception {} (expected {})", Red,
                    Default, to_string(te.type), to_string(cc.test_mode.error_type));
            } else {
                ++passed_tests;
            }
        } else {
            std::println("{}testing error{}: failed compilation", Red, Default);
        }
        cc.cleanup(main);
        cc.lexer.free_input();
    } catch (std::exception &e) {
        std::println("unexpected exception: {}", e.what());
    }

    if (!compiled) {
        return report_single_test_result();
    }

    if (cc.test_mode.test_type == TestType::CanCompile) {
        if (run_compare_test(cc)) {
            ++passed_tests;
        }
    } else if (cc.test_mode.test_type == TestType::Error) {
        std::println("{}testing error{}: should have failed compilation", Red, Default);
    } else if (cc.test_mode.test_type == TestType::ReturnsValue) {
        bool ok = run_compare_test(cc);
        compile_to_exe(opts.output_name, opts.output_exe_name);
        int status = spawn_blocking_process(fs::current_path() / opts.output_exe_name, {});
        if (status == static_cast<int>(cc.test_mode.return_value)) {
            if (ok) {
                ++passed_tests;
            }
        } else {
            std::println("{}testing error{}: non-matching return value {} (expected {})", Red,
                Default, status, cc.test_mode.return_value);
        }
    }

    if (total_tests == 1) {
        switch_back();
    }

    report_single_test_result();
}

void run_single_test(const fs::path &path)
{
    total_tests = 1;
    run_test(fs::canonical(path));
}

void run_tests(const fs::path &path, bool root)
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
            ++total_tests;
        }
    }

    std::ranges::sort(files);
    std::ranges::sort(subdirs);

    switch_to_tmp();

    for (const auto &file : files) {
        run_test(file);
    }

    for (const auto &dir : subdirs) {
        run_tests(dir, false);
    }

    switch_back();

    if (root && !printed_result && current_test == total_tests) {
        auto color = (passed_tests == total_tests) ? Green : Red;
        std::println("{}passed {}{}/{}{} tests{}", DefaultBold, color, passed_tests, total_tests,
            DefaultBold, Default);
        printed_result = true;
    }
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
