#pragma once
#include <string>

struct cmd_args {
    bool verbose = false;
    bool print_all_instructions = false;
    bool fail_emulation_allowed = false;
    // std::string output_path;
    // std::string input_binary;
    std::string text_section_raw;
    std::string log_dir;
    std::string output_path;
};

bool parse_args(int argc, char **argv, cmd_args &args);
