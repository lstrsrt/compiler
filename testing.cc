#include "compiler.hh"

#include <algorithm>

size_t current_test;
size_t passed_tests;
size_t total_tests;
bool printed_result;

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
            ++total_tests;
        }
    }

    std::ranges::sort(subdirs, [](const fs::directory_entry &a, const fs::directory_entry &b) {
        return a.path().filename() < b.path().filename();
    });

    for (const auto &file : files) {
        Compiler cc;
        cc.lexer.set_input(file.path().string());
        auto *main = new AstFunctionDecl("main", s32_type(), {}, new AstBlock({}), {});
        try {
            ++current_test;
            compiler_main(cc, main);
            cc.cleanup(main);
            cc.lexer.free_input();
        } catch (TestingException &te) {
            if (cc.test_mode.test_type == TestType::Error) {
                if (te.type != cc.test_mode.error_type) {
                    std::println("{}testing error{}: non-matching exception {}{}{}", Red, Default,
                        DefaultBold, to_underlying(te.type), Default);
                } else {
                    ++passed_tests;
                }
            } else {
                std::println("{}testing error{}: failed compilation", Red, Default);
            }
            cc.cleanup(main);
            cc.lexer.free_input();
            continue;
        } catch (std::exception &e) {
            std::println("unexpected exception: {}", e.what());
            continue;
        }

        if (cc.test_mode.test_type == TestType::Error) {
            std::println("{}testing error{}: should have failed compilation", Red, Default);
        } else if (cc.test_mode.test_type == TestType::CanCompile) {
            ++passed_tests;
        } else {
            // TODO - actually run program and check return value
            // TODO - add AST walk verification, etc
        }
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
