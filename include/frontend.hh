#pragma once

#include "base.hh"

#include <span>

struct ArgumentParser {
    std::span<char *> arguments;
};

struct Options {
    bool testing = false;
    bool check_only = false;
    bool full_compile = false;
    std::string output_name;
    std::string output_exe_name;
};

inline Options opts;

void process_cmdline(ArgumentParser &);
[[noreturn]] void usage(int errc);
void determine_output_names();
