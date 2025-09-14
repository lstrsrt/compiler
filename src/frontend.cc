#include "frontend.hh"

[[noreturn]] void usage(int errc)
{
    std::println("usage: compiler [switches...] <filename>");
    std::println("COMMANDS:\n"
                 "\t-h, --help: print this message");
    std::println("OPTIONS:\n"
                 "\t--check-only: do not compile input, only check validity\n"
                 "\t--test: run in test mode");
    exit(errc);
}

void process_cmdline(ArgumentParser &ap)
{
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
            default:
                if (i < ap.arguments.size() - 1) {
                    std::println(
                        "{}unknown argument{} '{}'", colors::Red, colors::Default, ap.arguments[i]);
                }
        }
    }
}
