#include "file.hh"

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

    this->filename = name;
    this->file_handle = ::open(name.data(), open_flags_to_posix(flags), 0644);
    if (file_handle == -1) {
        return false;
    }

    Defer fail_defer{ [&] {
        this->close();
    } };

    struct stat stat{};
    if (fstat(file_handle, &stat) < 0) {
        return false;
    }
    this->file_size = stat.st_size;

    if (has_flag(flags, READ)) {
        if (!this->file_size && has_flags(flags, WRITE)) {
            // Do this even if we don't have TRUNCATE. The file is empty so we're not deleting
            // anything, and not doing this would cause mmap to fail.
            ftruncate(this->file_handle, 4096);
        }
        this->map = static_cast<char *>(
            mmap(nullptr, stat.st_size, PROT_READ, MAP_SHARED, file_handle, 0));
        if (this->map == MAP_FAILED) {
            this->map = nullptr;
            return false;
        }
    } else {
        this->map = nullptr;
    }

    fail_defer.disable();
    if (has_flag(flags, WRITE)) {
        this->write_buffer.reserve(4096);
    }
    this->flags = flags;
    return true;
}

void File::write(std::string_view str)
{
    if (!has_flag(this->flags, OpenFlags::WRITE)) {
        return;
    }
    this->write_buffer += str;
}

bool File::commit()
{
    if (!has_flag(this->flags, OpenFlags::WRITE)
        || ::write(this->file_handle, this->write_buffer.c_str(), this->write_buffer.length())
            < 0) {
        return false;
    }

    this->write_buffer.clear();
    return true;
}

bool File::close(File::Commit commit)
{
    bool ret = true;
    if (has_flag(this->flags, OpenFlags::WRITE) && !write_buffer.empty() && commit == Commit::Yes) {
        if (::write(file_handle, write_buffer.c_str(), write_buffer.length()) < 0) {
            ret = false;
        }
    }
    this->filename = {};
    ::close(this->file_handle);
    this->file_handle = -1;
    this->file_size = 0;
    if (this->map) {
        munmap(map, file_size);
    }
    this->map = nullptr;
    this->write_buffer = {};
    this->flags = static_cast<OpenFlags>(0);
    return ret;
}
