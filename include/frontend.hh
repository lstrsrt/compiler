#pragma once

#include "base.hh"

#include <span>

struct ArgumentParser {
    std::span<char *> arguments;
};

struct Options {
    bool testing = false;
    bool check_only = false;
};

inline Options opts;

void process_cmdline(ArgumentParser &);
[[noreturn]] void usage(int errc);
