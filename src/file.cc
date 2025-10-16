#include "file.hh"

#include <random>

#include <fcntl.h>
#include <linux/limits.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

static int open_flags_to_posix(OpenFlags flags)
{
    using enum OpenFlags;
    int posix = 0;
    auto behavior = flags & behavior_mask;
    // TODO: warn on contradictory flags
    if (behavior == OpenOrCreate || behavior == Create) {
        posix |= O_CREAT;
    }
    if (has_flags(flags, READ, WRITE)) {
        posix |= O_RDWR;
    } else if (has_flag(flags, READ)) {
        posix |= O_RDONLY;
    } else if (has_flag(flags, WRITE)) {
        posix |= O_WRONLY;
    }
    if (has_flag(flags, TRUNCATE)) {
        posix |= O_TRUNC;
    } else if (has_flag(flags, APPEND)) {
        posix |= O_APPEND;
    }
    return posix;
}

File File::from_fd(int fd)
{
    using enum OpenFlags;
    File ret;
    auto perms = fcntl(fd, F_GETFL);
    switch (perms & O_ACCMODE) {
        case O_RDWR:
            ret.flags |= (READ | WRITE);
            break;
        case O_RDONLY:
            ret.flags |= READ;
            break;
        case O_WRONLY:
            ret.flags |= WRITE;
            break;
    }
    std::string path(PATH_MAX, '\0');
    auto fd_path = std::format("/proc/self/fd/{}", fd);
    auto len = readlink(fd_path.c_str(), path.data(), path.size() - 1);
    if (len != -1) {
        ret.filename = path;
    }
    ret.file_handle = fd;
    // TODO file size, mmap
    return ret;
}

File File::make_temporary(const std::string &extension, OpenFlags flags)
{
    std::mt19937 rng{ std::random_device{}() };
    std::string out = "compiler-";
    constexpr const auto &chars = "0123456789"
                                  "abcdefghijklmnopqrstuvwxyz"
                                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    std::uniform_int_distribution<size_t> dist(0, std::size(chars) - 2);
    for (size_t i = 0; i <= 6; ++i) {
        out += chars[dist(rng)];
    }
    out = fs::temp_directory_path() / (out + extension);
    File ret;
    ret.open(out, OpenFlags::Create | (flags & ~OpenFlags::behavior_mask));
    return ret;
}

File::~File()
{
    close();
}

bool File::is_valid() const
{
    return file_handle != -1 && fcntl(file_handle, F_GETFD) >= 0;
}

bool File::open(const std::string &name, OpenFlags flags)
{
    using enum OpenFlags;
    bool exists = fs::exists(name);
    auto behavior = flags & behavior_mask;
    if (exists && behavior == Create) {
        return false;
    }
    if (!exists && behavior == Open) {
        return false;
    }

    filename = name;
    file_handle = ::open(name.data(), open_flags_to_posix(flags), 0644);
    if (file_handle == -1) {
        return false;
    }

    Defer fail_defer{ [&] { close(); } };

    struct stat stat{};
    if (fstat(file_handle, &stat) < 0) {
        return false;
    }
    file_size = stat.st_size;

    if (has_flag(flags, READ)) {
        if (!file_size && has_flags(flags, WRITE)) {
            // Do this even if we don't have TRUNCATE. The file is empty so we're not deleting
            // anything, and not doing this would cause mmap to fail.
            ftruncate(file_handle, 4096);
        }
        map = static_cast<char *>(
            mmap(nullptr, stat.st_size, PROT_READ, MAP_SHARED, file_handle, 0));
        if (map == MAP_FAILED) {
            map = nullptr;
            return false;
        }
    } else {
        map = nullptr;
    }

    fail_defer.disable();
    if (has_flag(flags, WRITE) && buffered) {
        write_buffer.reserve(4096);
    }
    this->flags = flags;
    return true;
}

bool File::write(std::string_view str)
{
    if (!has_flag(flags, OpenFlags::WRITE)) {
        return false;
    }
    if (buffered) {
        write_buffer += str;
        return true;
    }
    return ::write(file_handle, str.data(), str.length()) == ssize(str);
}

bool File::commit()
{
    if (!buffered) {
        return true;
    }
    if (!is_valid() || !has_flag(flags, OpenFlags::WRITE)
        || ::write(file_handle, write_buffer.c_str(), write_buffer.length())
            < ssize(write_buffer)) {
        return false;
    }
    write_buffer.clear();
    return true;
}

bool File::close(File::Commit commit)
{
    bool ret = true;
    if (commit == Commit::Yes) {
        ret = this->commit();
    }
    // Keep the filename on purpose
    if (is_valid()) {
        ::close(file_handle);
    }
    file_handle = -1;
    file_size = 0;
    if (map) {
        munmap(map, file_size);
    }
    map = nullptr;
    write_buffer = {};
    flags = static_cast<OpenFlags>(0);
    buffered = true;
    return ret;
}
