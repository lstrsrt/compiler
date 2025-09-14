#pragma once

#include "base.hh"

enum_flags(OpenFlags, int){
    //
    // Open behavior
    //
    Open,         // Open if file exists, otherwise return error
    OpenOrCreate, // Open if file exists, otherwise create
    Create,       // Return error if file exists, otherwise create
    behavior_mask = 0b1111,

    //
    // Access flags
    //
    READ = (1 << 4),
    WRITE = (1 << 5),
    APPEND = (1 << 6),
    TRUNCATE = (1 << 7),
};

struct File {
    enum Commit {
        No,
        Yes,
    };

    bool open(const std::string &name, OpenFlags);

    // This function only appends to the write buffer,
    // it does not actually write to the file yet.
    // Call commit() to flush the write buffer to the file.
    void write(std::string_view str);

    // Commit changes to file. This clears the write buffer!
    bool commit();

    // Passing No loses uncommited changes.
    bool close(Commit = Commit::Yes);

    std::string filename;
    int file_handle = -1;
    off_t file_size = 0;
    char *map = nullptr;
    std::string write_buffer;
    OpenFlags flags = static_cast<OpenFlags>(0);
};
