#pragma once

#include "base.hh"

struct InputFile {
    void open(const std::string &name);
    void close();

    std::string filename;
    int file_handle = -1;
    off_t file_size;
    char *map;
};

// #define ONLY_PRINT_ASM

#ifdef ONLY_PRINT_ASM
struct OutputFile {
    inline void create(std::string_view) { }

    inline void write(std::string_view) { }

    inline void close() { }
};
#else
struct OutputFile {
    void create(std::string_view _name);
    void write(std::string_view str);
    void close();

    std::string name;
    int file_handle = -1;
    std::string write_buffer;
};
#endif
