#include <fmt/format.h>
#include <taint/utils.h>
#include <fstream>

bool read_whole_file(const char *filename, std::vector<char> &output) {
    std::ifstream file(filename, std::ifstream::binary);
    if (!file) {
        fmt::print(stderr, "could not open file for reading\n  '{:s}'\n", filename);
        return false;
    }

    file.seekg(0, file.end);
    size_t len = file.tellg();
    file.seekg(0, file.beg);

    auto buffer = std::vector<char>(len);
    if (!file.read(buffer.data(), buffer.size())) {
        fmt::print(stderr, "could not fully read file\n  '{:s}'\n", filename);
        return false;
    }
    output.swap(buffer);
    file.close();

    return true;
}