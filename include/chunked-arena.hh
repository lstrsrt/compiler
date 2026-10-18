#pragma once

#include "arena-alloc/arena_alloc.h"
#include "base.hh"

#include <algorithm>
#include <memory>
#include <vector>

// Wraps arena::Arena so that allocated memory never moves.
// ARENA_FL_GROW uses realloc, which invalidates every pointer into the arena when the block moves.
// Instead, when a block is full we start a new fixed-size block and keep the old ones alive.
class ChunkedArena {
public:
    explicit ChunkedArena(size_t chunk_size)
        : chunk_size(chunk_size)
    {
        add_chunk(chunk_size);
    }

    void *alloc_raw(size_t nbytes, size_t alignment = ARENA_DEFAULT_ALIGNMENT)
    {
        if (void *ptr = chunks[current]->alloc_raw(nbytes, alignment)) {
            return ptr;
        }
        // Reuse chunks left over from before a reset(), otherwise make a new one.
        while (++current < chunks.size()) {
            if (void *ptr = chunks[current]->alloc_raw(nbytes, alignment)) {
                return ptr;
            }
        }
        if (chunks.size() * chunk_size > MiB(1024)) {
            internal_error("exceeded 1024 mib memory limit!");
        }
        add_chunk(std::max(chunk_size, nbytes + alignment));
        void *ptr = chunks[current]->alloc_raw(nbytes, alignment);
        if (!ptr) {
            internal_error("out of memory!");
        }
        return ptr;
    }

    // Keeps the chunks around so they can be reused.
    void reset()
    {
        for (auto &chunk : chunks) {
            (*chunk).reset();
        }
        current = 0;
    }

private:
    void add_chunk(size_t size)
    {
        chunks.push_back(std::make_unique<arena::Arena>(size));
        current = chunks.size() - 1;
    }

    size_t chunk_size;
    size_t current = 0;
    std::vector<std::unique_ptr<arena::Arena>> chunks;
};

template<typename T>
class ChunkedArenaAllocator {
public:
    using value_type = T;

    explicit ChunkedArenaAllocator(ChunkedArena *arena) noexcept
        : arena(arena)
    {
    }

    template<typename U>
    // NOLINTNEXTLINE(google-explicit-constructor)
    ChunkedArenaAllocator(const ChunkedArenaAllocator<U> &other) noexcept
        : arena(other.arena)
    {
    }

    T *allocate(size_t n) { return static_cast<T *>(arena->alloc_raw(n * sizeof(T), alignof(T))); }

    void deallocate(T *, size_t) noexcept { }

    template<typename U>
    bool operator==(const ChunkedArenaAllocator<U> &other) const noexcept
    {
        return arena == other.arena;
    }

private:
    template<typename U>
    friend class ChunkedArenaAllocator;

    ChunkedArena *arena;
};
