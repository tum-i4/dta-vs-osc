#pragma once
#include <fmt/format.h>
#include <taint/utils.h>

#include <stddef.h>
#include <vector>
#include <string>
#include <cstdio>

struct saved_context_t {
    size_t instr_num;

    size_t xdi;
    size_t xsi;
    size_t xbp;
    size_t xsp;
    size_t xbx;
    size_t xdx;
    size_t xcx;
    size_t xax;

    size_t r8;
    size_t r9;
    size_t r10;
    size_t r11;
    size_t r12;
    size_t r13;
    size_t r14;
    size_t r15;

    size_t xflags;

    size_t xip;

    size_t fs;
    static std::vector<saved_context_t*> get_all(char *buf, size_t len);
    static std::vector<saved_context_t*> deserialize(uint8_t *buf, size_t num, size_t* bytesConsumed);
};

struct saved_memory_t {
    size_t instr_num;
    size_t start_addr;
    size_t size;
    const char *data;

    static saved_memory_t *from_buf(uint8_t **buf);
    static std::vector<saved_memory_t*> get_all(char *buf, size_t len);
    static std::vector<saved_memory_t*> deserialize(uint8_t *buf, size_t len, size_t *bytesConsumed);
};

struct saved_module_t {
    size_t start;
    size_t end;
    std::string path;
    bool is_main;

    bool init(const std::string &name, const char *path);
};

bool read_trace_from_file(const char *filename, std::vector<uint64_t> &output);

std::vector<uint64_t*> deserialize_traces(uint8_t *buf, size_t len, size_t *bytesConsumed);

template<typename T, std::vector<T*> (*Deserialize)(uint8_t *buf, size_t len, size_t *bytesConsumed)>
class LazyLoader {
private:
    // path to the file
    std::string path;
    // stream used to read from the file
    file_unique_ptr file = nullptr;
    // buffer of data
    std::unique_ptr<uint8_t[]> data;
    // current position in the buffer
    int cur_pos;

    // if we hit eof
    bool hit_eof = false;

    int buf_cnt;
    std::vector<T*> buf;

    void read_next() {
        if (hit_eof)
            return;

        // file has not been opened yet
        if (!file) {
            FILE *f = fopen(path.c_str(), "rb");
            if (!f) {
                fmt::print("LazyLoader: could not open file {:s}\n", path);
                fmt::print("  file: {:s}\n", path);
                exit(1);
            }
            file.reset(f);
        }
        
        // allocate buffer if we need to
        if (!data)
            data.reset(new uint8_t[buf_cnt * sizeof(T)]);
        if (!data) {
            fmt::print("LazyLoader: could not allocate buffer of size {:d}", buf_cnt * sizeof(T));
            fmt::print("  file: {:s}\n", path);
            exit(1);
        }

        // read new data into our buffer
        size_t bytes_to_read = buf_cnt * sizeof(T);
        auto num_read = fread(data.get(), 1, bytes_to_read, file.get());

        // check how much we read
        if (num_read < bytes_to_read) {
            // make sure we hit eof if that is the case
            if (!feof(file.get())) {
                fmt::print("LazyLoader: error reading file {:s}", path);
                fmt::print("  file: {:s}\n", path);
                exit(1);
            }
            hit_eof = true;
        }

        size_t bytesConsumed = 0;
        // use the Deserialize function to populate the vector with elements
        buf = std::move(Deserialize(data.get(), num_read, &bytesConsumed));
        // check if we need to rewind because we couldn't deserialize enough
        if (bytesConsumed < num_read) {
            if (bytesConsumed == 0) {
                fmt::print("LazyLoader: Deserialize did not consume any bytes. Buf size might be too low\n");
                fmt::print("  file: {:s}\n", path);
                exit(1);
            }
            int bytesToRewind = (int)bytesConsumed - (int)num_read;
            if (!fseek(file.get(), bytesToRewind, SEEK_CUR)) {
                fmt::print("LazyLoader: fseek (..., {:d}, SEEK_CUR) failed (read {}) bytes\n", bytesToRewind, num_read);
                fmt::print("  file: {:s}\n", path);
                exit(1);
            }
        }
        cur_pos = -1;
    }

public:
    LazyLoader(const std::string &path, int buf_cnt = 1024 * 1024)
        : path(path), cur_pos(-1), buf_cnt(buf_cnt)
    {
        read_next();
        next();
    }

    // set by next to the current element
    // is nullptr if no more elements exist
    T *current;

    // advances the current position
    // return false when there is no next element
    bool next() {
        // check if we need to load more data
        if (cur_pos + 1 >= buf.size()) {
            // if we successfully load more data it will be set to something valid
            current = nullptr;
            // we already loaded all of the data
            if (hit_eof) {
                return false;
            }
            // actually load the next chunck
            read_next();
            // sanity check that we actually read something
            if (buf.size() == 0) {
                return false;
            }
        }
        current = buf[++cur_pos];
        return true;
    }

};


using LazyContextLoader = LazyLoader<saved_context_t, saved_context_t::deserialize>;
using LazyMemoryLoader = LazyLoader<saved_memory_t, saved_memory_t::deserialize>;
using LazyTraceLoader = LazyLoader<uint64_t, deserialize_traces>;
