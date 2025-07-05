#include "compiler.hh"

#include <algorithm>

#include <filesystem>
#include <linux/limits.h>
#include <spawn.h>
#include <sys/wait.h>

size_t current_test;
size_t passed_tests;
size_t total_tests;
bool printed_result;

int spawn_and_wait(const fs::path &exe_path, const std::vector<std::string> &_cmdline)
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
    extern char **environ;
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
        } else {
            std::println("signal or continue");
        }
    } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    return -1;
}

void run_test(const fs::path &file)
{
    using namespace colors;

    ++total_tests;
    Compiler cc;
    cc.lexer.set_input(file.string());
    auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
    try {
        ++current_test;
        compiler_main(cc, main);
        cc.cleanup(main);
        cc.lexer.free_input();
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
        return;
    } catch (std::exception &e) {
        std::println("unexpected exception: {}", e.what());
        return;
    }

    if (cc.test_mode.test_type == TestType::Error) {
        std::println("{}testing error{}: should have failed compilation", Red, Default);
    } else if (cc.test_mode.test_type == TestType::CanCompile) {
        ++passed_tests;
    } else if (cc.test_mode.test_type == TestType::ReturnsValue) {
        auto prev_wd = fs::current_path();
        Defer _{ [&] {
            fs::current_path(prev_wd);
        } };
        fs::current_path(fs::temp_directory_path());
        if (spawn_and_wait("/usr/bin/nasm",
                { "-f elf64", "-o output.o", (prev_wd / "output.asm").string() })) {
            std::println("{}testing error{}: nasm failure", Red, Default);
            return;
        }
        if (spawn_and_wait("/usr/bin/gcc", { "output.o", "-ooutput" })) {
            std::println("{}testing error{}: gcc failure", Red, Default);
            return;
        }
        int status = spawn_and_wait("./output", {});
        if (status == static_cast<int>(cc.test_mode.return_value)) {
            ++passed_tests;
        } else {
            std::println("{}testing error{}: non-matching return value {} (expected {})", Red,
                Default, status, cc.test_mode.return_value);
        }
        //  TODO: add AST walk verification, etc
    }

    // TODO: when only running one test, print success/failure
    if (total_tests == 1) {
        auto color = passed_tests ? Green : Red;
        auto str = passed_tests ? "passed" : "failed";
        std::println("{}{}{} {}{}", color, str, DefaultBold, file.string(), Default);
    }
}

void run_tests(const fs::path &path)
{
    using namespace colors;

    if (!fs::exists(path) || !fs::is_directory(path)) {
        return;
    }

    std::vector<fs::directory_entry> subdirs;
    std::vector<fs::directory_entry> files;
    for (const auto &entry : fs::directory_iterator(path)) {
        if (entry.is_directory()) {
            subdirs.push_back(entry);
        } else if (entry.path().has_filename() && entry.path().filename().has_extension()
            && entry.path().filename().extension() == ".txt") {
            files.push_back(entry);
        }
    }

    std::ranges::sort(subdirs, [](const fs::directory_entry &a, const fs::directory_entry &b) {
        return a.path().filename() < b.path().filename();
    });

    for (const auto &file : files) {
        run_test(file.path());
    }

    for (const auto &dir : subdirs) {
        run_tests(dir.path());
    }

    if (!printed_result && current_test == total_tests) {
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
