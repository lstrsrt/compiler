#include "frontend.hh"

[[noreturn]] void usage(int errc)
{
    std::println("usage: compiler [options...] <filename>");
    std::println("COMMANDS:\n"
                 "\t-h, --help: print this message");
    std::println("OPTIONS:\n"
                 "\t--check-only: do not compile input, only check validity\n"
                 "\t--test: run in test mode. <filename> can also be a directory.\n"
                 "\t--new-ir: use new ir (no codegen supported, implies --check-only)\n"
                 "\t--exe: compile to executable instead of generating assembler\n"
                 "\t  (requires nasm and gcc to be installed)\n"
                 "\t-o: set output filename (default: output)\n"
                 "\t--[no-]ssa: toggle ssa optimizations (default: on)\n"
                 "\t--[no-]inline: toggle inlining pass (default: off)");
    exit(errc);
}

struct UnknownArg {
    std::string string;
    bool list_arg = false;
};

struct UnknownArgsHandler {
    explicit UnknownArgsHandler(ArgumentParser &ap)
        : arguments(ap.arguments)
    {
    }

    void add_unknown_arg(size_t index) { unknown.emplace_back(arguments[index], false); }

    void add_unknown_list_arg(const std::string &s) { unknown.push_back({ s, true }); }

    void handle()
    {
        if (unknown.empty()) {
            return;
        }

        for (const auto &s : unknown) {
            const auto *list = s.list_arg ? "list " : "";
            print_error("unknown {}argument '{}{}{}'", list, colors::DefaultBold, s.string,
                colors::Default);
        }
        exit(EXIT_FAILURE);
    }

    std::span<char *> arguments;
    std::vector<UnknownArg> unknown;
};

void process_cmdline(ArgumentParser &ap)
{
    UnknownArgsHandler unknown_args(ap);

    for (size_t i = 1; i < ap.arguments.size(); ++i) {
        switch (hash(ap.arguments[i])) {
            case hash("--test"):
                opts.testing = true;
                break;
            case hash("--new-ir"):
                opts.new_ir = true;
                opts.check_only = true;
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
            case hash("--ssa"):
                opts.ssa = true;
                break;
            case hash("--no-ssa"):
                opts.ssa = false;
                break;
            case hash("--inline"):
                opts.inliner = true;
                break;
            case hash("--no-inline"):
                opts.inliner = false;
                break;
            default:
                if (i < ap.arguments.size() - 1) {
                    unknown_args.add_unknown_arg(i);
                }
        }
    }

    unknown_args.handle();

    if (opts.testing && !opts.output_name.empty()) {
        die("--test and -o are incompatible");
    }

    if (opts.new_ir && opts.full_compile) {
        die("--new-ir is not supported by the backend yet");
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
