#include "frontend.hh"

[[noreturn]] void usage(int errc)
{
    std::println("usage: compiler [options...] <filename>");
    std::println("COMMANDS:\n"
                 "\t-h, --help: print this message");
    std::println("OPTIONS:\n"
                 "\t--check-only: do not compile input, only check validity\n"
                 "\t--test: run in test mode\n"
                 "\t--exe: compile to executable instead of generating assembler\n"
                 "         (requires nasm and gcc to be installed)\n"
                 "\t-o: set output filename (default: output)");
    exit(errc);
}

void process_cmdline(ArgumentParser &ap)
{
    std::vector<size_t> unknown_args;

    for (size_t i = 1; i < ap.arguments.size(); ++i) {
        switch (hash(ap.arguments[i])) {
            case hash("--test"):
                opts.testing = true;
                break;
            case hash("--check-only"):
                opts.check_only = true;
                break;
            case hash("-h"):
                [[fallthrough]];
            case hash("--help"):
                usage(EXIT_SUCCESS);
                break;
            case hash("--exe"):
                opts.full_compile = true;
                break;
            case hash("-o"):
                if ((i + 1) == ap.arguments.size()) {
                    die("missing output name");
                }
                opts.output_name = ap.arguments[i + 1];
                ++i;
                break;
            default:
                if (i < ap.arguments.size() - 1) {
                    unknown_args.push_back(i);
                }
        }
    }

    if (!unknown_args.empty()) {
        for (auto i : unknown_args) {
            print_error(
                "unknown argument '{}{}{}'", colors::DefaultBold, ap.arguments[i], colors::Default);
        }
        exit(EXIT_FAILURE);
    }

    if (opts.testing && !opts.output_name.empty()) {
        die("--test and -o are incompatible");
    }
}

void determine_output_names()
{
    if (opts.output_name.empty()) {
        opts.output_exe_name = "output";
        opts.output_name = "output.asm";
    } else if (opts.full_compile) {
        // Use -o for the executable name
        opts.output_exe_name = opts.output_name;
        opts.output_name = opts.output_name + ".asm";
    }
}
