#include "file.hh"

#include <random>

#ifdef __linux__
#include <fcntl.h>
#include <linux/limits.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#else
#include <utility>
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <Windows.h>
#endif

static int open_flags_to_native(OpenFlags flags)
{
    using enum OpenFlags;
#ifdef __linux__
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
#else
    int win32 = 0;
    if (has_flag(flags, READ)) {
        win32 |= GENERIC_READ;
    }
    if (has_flag(flags, WRITE)) {
        // Write-only mappings don't exist on Windows, unfortunately.
        win32 |= GENERIC_READ | GENERIC_WRITE;
    }
    return win32;
#endif
}

static size_t get_file_size(File::Handle handle)
{
#ifdef __linux__
    struct stat stat{};
    if (fstat(handle, &stat) < 0) {
        return 0;
    }
    return stat.st_size;
#else
    LARGE_INTEGER ret;
    if (!GetFileSizeEx(handle, &ret)) {
        return 0;
    }
    return ret.QuadPart;
#endif
}

static char *map_file(File::Handle handle, [[maybe_unused]] size_t size, OpenFlags flags)
{
    using enum OpenFlags;
#ifdef __linux__
    int prot = PROT_NONE;
    if (has_flags(flags, READ, WRITE)) {
        prot = PROT_READ | PROT_WRITE;
    } else if (has_flag(flags, WRITE)) {
        prot = PROT_WRITE;
    } else if (has_flag(flags, READ)) {
        prot = PROT_READ;
    }
    auto *map = static_cast<char *>(mmap(nullptr, size, prot, MAP_SHARED, handle, 0));
    if (map == MAP_FAILED) {
        return nullptr;
    }
    return map;
#else
    DWORD prot = PAGE_NOACCESS;
    DWORD desired_access = FILE_MAP_READ;
    if (has_flag(flags, WRITE)) {
        // Force readable even if we don't have the flag.
        // See open_flags_to_native().
        prot = PAGE_READWRITE;
        desired_access = FILE_MAP_READ | FILE_MAP_WRITE;
    } else if (has_flag(flags, READ)) {
        prot = PAGE_READONLY;
        desired_access = FILE_MAP_READ;
    }
    auto mapping = CreateFileMappingA(handle, nullptr, prot, 0, 0, nullptr);
    if (mapping) {
        auto *map = static_cast<char *>(MapViewOfFile(mapping, desired_access, 0, 0, 0));
        CloseHandle(mapping);
        return map;
    } else {
        return nullptr;
    }
#endif
}

static void unmap_file(char *map, [[maybe_unused]] size_t size)
{
#ifdef __linux__
    if (map) {
        munmap(map, size);
    }
#else
    if (map) {
        // The mapping has already been closed, just the view is left.
        UnmapViewOfFile(map);
    }
#endif
}

static bool close_file(File::Handle handle)
{
#ifdef __linux__
    return close(handle) == 0;
#else
    return CloseHandle(handle);
#endif
}

static bool write_file(File::Handle handle, std::string_view str)
{
    // NOTE: could use the map here but this is simpler.
#ifdef __linux__
    return write(handle, str.data(), str.length()) == ssize(str);
#else
    DWORD written;
    return WriteFile(handle, str.data(), static_cast<DWORD>(size(str)), &written, nullptr)
        && std::cmp_greater_equal(written, size(str));
#endif
}

bool seek_in_file(File::Handle handle, long position, int whence)
{
#ifdef __linux__
    return lseek(handle, position, whence) >= 0;
#else
    return SetFilePointer(handle, position, nullptr, whence) != INVALID_SET_FILE_POINTER;
#endif
}

enum class Seek {
    No,
    Yes,
};

bool truncate_file(File::Handle handle, long length, Seek seek)
{
#ifdef __linux__
    if (ftruncate(handle, length) == -1) {
        return false;
    }
    if (seek == Seek::Yes) {
        return seek_in_file(handle, length, SEEK_SET);
    }
#else
    LONG low;
    LONG high = 0;
    if (seek == Seek::No) {
        // Get the current pos first.
        low = SetFilePointer(handle, 0, &high, SEEK_CUR);
        if (std::cmp_equal(low, INVALID_SET_FILE_POINTER)) {
            return false;
        }
    }

    if (SetFilePointer(handle, length, nullptr, FILE_BEGIN) == INVALID_SET_FILE_POINTER) {
        return false;
    }
    if (!SetEndOfFile(handle)) {
        return false;
    }

    if (seek == Seek::No) {
        // Go back to where we were before.
        if (SetFilePointer(handle, low, &high, FILE_BEGIN) == INVALID_SET_FILE_POINTER) {
            return false;
        }
    }
#endif
    return true;
}

bool make_file_mappable(File::Handle handle, size_t size, OpenFlags flags)
{
    using enum OpenFlags;
    // If the file is empty, we need to extend it so it becomes mappable for reading.
    // FIXME: If READ is not set, don't do this so there won't be garbage chars at the end. This
    // means the file won't be mapped but it will still be writable.
    bool need_to_extend = size == 0 && has_flags(flags, READ, WRITE);
    if (need_to_extend) {
        if (!truncate_file(handle, 1024, Seek::No)) {
            return false;
        }
    } else if (has_flags(flags, TRUNCATE)) {
        // FIXME: has_flags() is used (not has_flag()) because TRUNCATE consists of two flags.
        // Seems bugprone!!!

        // Truncate to 0 first and then extend again so it's mappable.
        if (!truncate_file(handle, 0, Seek::Yes)) {
            return false;
        }
        if (!truncate_file(handle, 1024, Seek::No)) {
            return false;
        }
    } else if (has_flag(flags, WRITE)) {
        // We probably want to append to the file.
        // TODO: make configurable
        if (!seek_in_file(handle, 0, SEEK_END)) {
            return false;
        }
    }
    return true;
}

File File::from_handle(File::Handle handle, OpenFlags access, Owning owning)
{
    using enum OpenFlags;
    File ret;

#ifdef __linux__
    if (!access) {
        int perms = fcntl(handle, F_GETFL);
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
    } else {
        ret.flags = access & ~behavior_mask;
    }
    std::string path(PATH_MAX, '\0');
    auto fd_path = std::format("/proc/self/fd/{}", handle);
    auto len = readlink(fd_path.c_str(), path.data(), path.size() - 1);
    if (len != -1) {
        ret.filename = path;
    }
    ret.file_handle = handle;
    ret.map = map_file(handle, get_file_size(handle), ret.flags);
#else
    ret.file_handle = handle;

    // DWORD type = GetFileType(handle);
    // TODO: Enable this? stdout is FILE_TYPE_UNKNOWN which would have to be whitelisted, making
    // this check kind of useless.
    // assert(type == FILE_TYPE_CHAR || type == FILE_TYPE_DISK);

    if (!access) {
        ret.flags = READ;
    } else {
        // No equivalent to fcntl(F_GETFL) exists on Windows so just set it to READ.
        ret.flags = access & ~behavior_mask;
    }

    char path[MAX_PATH];
    DWORD len = GetFinalPathNameByHandleA(handle, path, MAX_PATH, FILE_NAME_NORMALIZED);
    if (len && len < MAX_PATH /* not <= because null terminator is not included */) {
        ret.filename = path;
    }

    ret.file_size = get_file_size(handle);
    // NOTE: we don't call make_file_mappable() here because it only makes sense for text files,
    // but we don't know what this file is. The caller has to check `map`.
    ret.map = map_file(handle, ret.file_size, ret.flags);
#endif
    ret.owning = owning;
    ret.valid = true;
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
    out = (fs::temp_directory_path() / (out + extension)).string();
    File ret;
    ret.open(out, OpenFlags::Create | (flags & ~OpenFlags::behavior_mask));
    return ret;
}

File::~File()
{
    close();
}

bool File::is_valid_handle() const
{
#ifdef __linux__
    return file_handle != InvalidHandle && fcntl(file_handle, F_GETFD) >= 0;
#else
    // DWORD flags;
    return file_handle != InvalidHandle; // && GetHandleInformation(file_handle, &flags);
#endif
}

size_t File::file_size() const
{
    return get_file_size(file_handle);
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

#ifdef __linux__
    file_handle = ::open(name.data(), open_flags_to_native(flags), 0644);
#else
    int disposition = 0;
    if (behavior == Create) {
        disposition = CREATE_NEW;
    } else if (behavior == Open) {
        disposition = OPEN_EXISTING;
    } else if (behavior == OpenOrCreate) {
        disposition = OPEN_ALWAYS;
    }
    file_handle = CreateFileA(name.data(), open_flags_to_native(flags), FILE_SHARE_READ, nullptr,
        disposition, FILE_ATTRIBUTE_NORMAL, nullptr);
#endif
    if (!is_valid_handle()) {
        return false;
    }

    Defer fail_defer{ [&] { close(); } };

    auto file_size = this->file_size();

    if (!make_file_mappable(file_handle, file_size, flags)) {
        return false;
    }

    map = map_file(file_handle, file_size, flags);
    if (has_flag(flags, READ) && !map) {
        // If the user intends to read, fail here.
        // For write-only it is not an issue because write_file() uses the handle.
        return false;
    }

    fail_defer.disable();
    if (has_flag(flags, WRITE) && buffered == Buffered::Yes) {
        write_buffer.reserve(4096);
    }
    this->flags = flags;
    valid = true;
    return true;
}

bool File::write(std::string_view str)
{
    if (!has_flag(flags, OpenFlags::WRITE)) {
        return false;
    }
    if (buffered == Buffered::Yes) {
        write_buffer += str;
        return true;
    }
    return write_file(file_handle, str);
}

bool File::commit()
{
    if (buffered == Buffered::No) {
        return true;
    }
    if (!is_valid() || !has_flag(flags, OpenFlags::WRITE)
        || !write_file(file_handle, write_buffer)) {
        return false;
    }
    write_buffer.clear();
    return true;
}

bool File::close(Commit commit)
{
    bool ret = true;
    if (commit == Commit::Yes) {
        ret = this->commit();
    }
    if (map) {
        unmap_file(map, file_size());
    }
    if (owning == Owning::Yes && is_valid_handle()) {
        close_file(file_handle);
    }
    // Even if we don't own this file, reset the handle.
    file_handle = InvalidHandle;

    map = nullptr;
    write_buffer = {};
    flags = {};
    valid = false;

    // Keep the filename on purpose

    return ret;
}
