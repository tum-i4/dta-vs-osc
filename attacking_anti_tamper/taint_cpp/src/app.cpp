#include <fmt/format.h>

#include <taint/serialization.h>
#include <taint/cmdline.h>
#include <taint/utils.h>
#include <taint/analysis.h>

#include <iostream>
#include <fstream>

int main(int argc, char **argv)
{

    // parse command line args
    cmd_args args;
    if (!parse_args(argc, argv, args))
        return 1;

    // reading saved contexts
    auto contexts_path = args.log_dir + "saved_contexts.bin";
    fmt::print(stdout, "[*] reading contexts\n");
    LazyContextLoader saved_contexts(contexts_path);

    // reading trace
    auto trace_path = args.log_dir + "instrace.log";
    fmt::print(stdout, "[*] reading trace\n");
    LazyTraceLoader trace(trace_path);

    // reading saved memories
    auto memories_path = args.log_dir + "saved_memories.bin";
    fmt::print(stdout, "[*] reading memories\n");
    LazyMemoryLoader saved_memories(memories_path);

    // reading modules
    fmt::print(stdout, "[*] reading modules\n");
    // module names are in a file modules.txt, one per line
    auto modules_names_path = args.log_dir + "modules.txt";
    std::ifstream modules_names_file(modules_names_path);
    if (!modules_names_file)
    {
        fmt::print(stderr, "cannot open module names path\n  '{:s}'\n", modules_names_path);
        return 1;
    }
    auto modules_path = args.log_dir + "modules/";
    std::vector<saved_module_t> modules;
    std::string mod_name;
    // read one by one and parse the name for the info
    while (std::getline(modules_names_file, mod_name))
    {
        // if (args.verbose)
        //     fmt::print("module: {:s}\n", mod_name);
        auto mod_path = modules_path + mod_name;
        saved_module_t mod;
        if (!mod.init(mod_name, mod_path.c_str()))
        {
            fmt::print(stderr, "failed initializing module from path\n  '{:s}'\n", mod_path);
            return 1;
        }
        // if (args.verbose)
        // {
        //     fmt::print("module: 0x{:#x} - 0x{:#x}, main = {}\n",
        //                mod.start, mod.end, mod.is_main);
        // }
        modules.push_back(std::move(mod));
    }

    size_t text_section_start = 0;
    size_t text_section_end = 0;
    if (!args.text_section_raw.empty()) {
        auto delim_pos = args.text_section_raw.find(',');
        text_section_start = stoull(args.text_section_raw.substr(0, delim_pos));
        auto size_str = args.text_section_raw.substr(delim_pos + 1);
        text_section_end = text_section_start + stoull(size_str);
    }

    TaintAnalysis analysis;
    analysis.set_debug(args.verbose);
    analysis.set_print_all_instructions(args.print_all_instructions);
    // setup the memory/taint
    fmt::print("[*] setting up the context\n");
    if (!analysis.setup_context(modules, text_section_start, text_section_end)) // modules are now owned by analysis
    {
        fmt::print(stderr, "failed setting up the context\n");
        return 1;
    }

    // running the emulation
    fmt::print("[*] running the emulation\n");
    CollectedData result = analysis.emulate(trace, saved_contexts, saved_memories);

    // write output file
    std::ofstream output_file(args.output_path);
    if (!output_file)
    {
        fmt::print(stderr, "cannot open output path\n  '{:s}'\n", args.output_path);
        return 1;
    }
    output_file << "exception: " << (result.unexpected ? "yes" : "no") << "\n";
    output_file << "bypassed: " << (result.bypassed ? "yes" : "no") << "\n";
    output_file << "instructions: " << result.instructions << "\n";
    output_file << "diverged: " << result.diverged << "\n";
    output_file << "skipped: " << result.skipped << "\n";
    output_file << "xsavec: " << result.xsavec << "\n";

    return 0;
}
