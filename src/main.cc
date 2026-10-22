#include "compiler.hh"
#include "frontend.hh"
#include "testing.hh"

#ifdef FUZZING
#include <unistd.h>
__AFL_FUZZ_INIT();
#endif

int main([[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
#ifdef FUZZING
#ifdef __AFL_HAVE_MANUAL_CONTROL
    __AFL_INIT();
#endif
    opts.check_only = true;
    opts.new_ir = true;
    opts.ssa = false;
    unsigned char *buf = __AFL_FUZZ_TESTCASE_BUF;
    while (__AFL_LOOP(10000)) {
        size_t len = __AFL_FUZZ_TESTCASE_LEN;
        std::string src{ reinterpret_cast<const char *>(buf), len };
        try {
            invoke_compiler(src, InputMode::String);
        } catch (FuzzerException &) {
            // this is ok
        }
    }
#else
    if (argc < 2) {
        usage(EXIT_FAILURE);
    }

    ArgumentParser arg_parser;
    arg_parser.arguments = std::span<char *>(argv, argc);
    process_cmdline(arg_parser);
    determine_output_names();

    if (opts.testing) {
        auto *dir_or_file = arg_parser.arguments.back();
        if (!fs::exists(dir_or_file)) {
            die("test file or directory '{}' not found", dir_or_file);
        }
        bool passed = false;
        if (fs::is_regular_file(dir_or_file)) {
            passed = run_single_test(dir_or_file);
        } else if (fs::is_directory(dir_or_file)) {
            passed = run_tests(dir_or_file);
        } else {
            die("input '{}' is not a file or directory", dir_or_file);
        }
        return passed ? EXIT_SUCCESS : EXIT_FAILURE;
    } else {
        auto *maybe_file = arg_parser.arguments.back();
        if (!fs::exists(maybe_file)) {
            die("input '{}' not found", maybe_file);
        }
        if (!fs::is_regular_file(maybe_file)) {
            die("input '{}' is not a file", maybe_file);
        }
        invoke_compiler(maybe_file, InputMode::File);
    }
#endif
}
