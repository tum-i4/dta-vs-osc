#pragma once
#include <vector>

bool read_whole_file(const char *filename, std::vector<char> &output);

struct FileDeleter
{
    void operator()(FILE *ptr) const
    {
        fclose(ptr);
        ptr = nullptr;
    }
};
using file_unique_ptr = std::unique_ptr<FILE, FileDeleter>;
