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
    APPEND = WRITE | (1 << 6),
    TRUNCATE = WRITE | (1 << 7),
};

struct File {
#ifdef __linux__
    using Handle = int;
#else
    // HANDLE but without having to pull in Windows headers here.
    using Handle = void *;
#endif
    enum class Commit : uint8_t {
        No,
        Yes,
    };
    enum class Owning : uint8_t {
        No,
        Yes,
    };
    enum class Buffered : uint8_t {
        No,
        Yes,
    };

    // If 0 is passed for `access`, fcntl(F_GETFL) is used on Linux and READ is assumed on Windows.
    // If `owning` is Owning::Yes, `handle` is closed when the file is closed.
    static File from_handle(Handle handle, OpenFlags access, Owning);

    static File make_temporary(const std::string &extension, OpenFlags);

    ~File();

    // Did a factory function/open() call succeed?
    bool is_valid() const
    {
        return valid && is_valid_handle();
    }

    bool is_valid_handle() const;

    size_t file_size() const;

    bool open(const std::string &name, OpenFlags);

    // In buffered mode (default), this function only appends to the write buffer and returns true
    // without actually writing to the file.
    // Call commit() to flush the write buffer to the file.
    bool write(std::string_view);

    template<class... Args>
    bool fwrite(std::string_view fmt, Args... args)
    {
        if constexpr (sizeof...(args)) {
            auto s = std::vformat(fmt, std::make_format_args(args...));
            return write(s);
        } else {
            return write(std::format("{}", fmt));
        }
    }

    template<class... Args>
    bool fwriteln(std::string_view fmt, Args... args)
    {
        if constexpr (sizeof...(args)) {
            auto s = std::vformat(fmt, std::make_format_args(args...));
            s += '\n';
            return write(s);
        } else {
            return write(std::format("{}\n", fmt));
        }
    }

    // Commit changes to file. This clears the write buffer!
    bool commit();

    // Passing No loses uncommited changes.
    bool close(Commit = Commit::Yes);

#ifdef __linux__
    static constexpr Handle InvalidHandle = -1;
#else
    static constexpr Handle InvalidHandle = nullptr;
#endif
    std::string filename;
    Handle file_handle = InvalidHandle;
    char *map = nullptr;
    std::string write_buffer;
    OpenFlags flags{};
    Buffered buffered = Buffered::Yes;
    Owning owning = Owning::Yes;
    bool valid = false;
};
