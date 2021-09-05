#pragma once
#include <triton/api.hpp>
#include <triton/x86Specifications.hpp>
#include <vector>
#include <memory>
#include <taint/serialization.h>

struct CollectedData {
    bool unexpected;
    bool bypassed;
    size_t instructions;
    size_t diverged;
    size_t skipped;
    size_t xsavec;
};

class TaintAnalysis
{
private:
    bool debug = false;
    bool print_all_instructions = false;
    triton::API api;
    std::vector<saved_module_t> modules;
    std::vector<saved_module_t> main_modules;

    static std::vector<triton::arch::x86::instruction_e> skipped_instructions;

    void set_context(const saved_context_t *context, bool set_ip = false);
    void print_context();

public:
    void set_debug(bool dbg);
    void set_print_all_instructions(bool dbg);
    void set_code_arr_taint(triton::uint64 arr_start, triton::uint64 arr_length, bool taint);
    bool setup_context(std::vector<saved_module_t> &modules, size_t txt_start, size_t txt_end);
    CollectedData emulate(LazyTraceLoader &trace, LazyContextLoader &contexts,
                                  LazyMemoryLoader &memories);
};
