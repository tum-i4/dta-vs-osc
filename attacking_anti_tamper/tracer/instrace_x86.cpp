/* ******************************************************************************
 * Copyright (c) 2011-2018 Google, Inc.  All rights reserved.
 * Copyright (c) 2010 Massachusetts Institute of Technology  All rights reserved.
 * ******************************************************************************/

/*
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * * Neither the name of VMware, Inc. nor the names of its contributors may be
 *   used to endorse or promote products derived from this software without
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL VMWARE, INC. OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/* Code Manipulation API Sample:
 * instrace_x86.c
 *
 * Collects a dynamic instruction trace and dumps it to a file.
 * This is an x86-specific implementation of an instruction tracing client.
 * For a simpler (and slower) arch-independent version, please see instrace_simple.c.
 *
 * Illustrates how to create generated code in a local code cache and
 * perform a lean procedure call to that generated code.
 *
 * (1) Fills a buffer and dumps the buffer when it is full.
 * (2) Inlines the buffer filling code to avoid a full context switch.
 * (3) Uses a lean procedure call for clean calls to reduce code cache size.
 *
 */

#include <cstdio>
#include <cstring> /* for memset */
#include <cstddef> /* for offsetof */
#include "dr_api.h"
#include "drmgr.h"
#include "drreg.h"
#include "dr_tools.h"
#include "drutil.h"
// #include "droption.h"
#include "utils.h"

#include <string>
#include <sstream>
#include <vector>
#include <iomanip>

#include <unistd.h>
#include <asm/ldt.h>   
#include <sys/syscall.h>
#include <sys/prctl.h>
#include <asm/prctl.h>

// static droption_t<std::string> op_logdir
// (DROPTION_SCOPE_CLIENT, "logdir", "", "Directory where log files and other artifacts will be written to",
//  "");

static std::string logdir = "";
/* Each ins_ref_t describes an executed instruction. */
typedef struct _ins_ref_t {
    app_pc pc;
    // int opcode;
} ins_ref_t;

/* Max number of ins_ref a buffer can have */
#define MAX_NUM_INS_REFS 8192
/* The size of the memory buffer for holding ins_refs. When it fills up,
 * we dump data from the buffer to the file.
 */
#define INS_BUF_SIZE (sizeof(ins_ref_t) * MAX_NUM_INS_REFS)

typedef struct {
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
} saved_context_t;
#define MAX_NUM_CTX 1024
#define CTX_BUF_SIZE (sizeof(saved_context_t) * MAX_NUM_CTX)

typedef struct {
    size_t trace_addr;
    size_t start_addr;
    size_t size;
    // must be freed with dr_global_free(data, len)
    const char *data;
} saved_memory_t;
#define MAX_NUM_MEM 256
#define MEM_BUF_SIZE (sizeof(saved_memory_t) * MAX_NUM_MEM)

/* Thread-private data */
typedef struct {
    char *buf_ptr;
    char *buf_base;
    /* buf_end holds the negative value of real address of buffer end. */
    ptr_int_t buf_end;
    file_t log;
    size_t instr_count;

    file_t saved_contexts_file;
    saved_context_t *context_buf;
    int context_buf_cnt;

    file_t saved_memories_file;
    saved_memory_t *memory_buf;
    int memory_buf_cnt;

} per_thread_t;


static thread_id_t instrumented_thread_id = 0;
static size_t page_size;
static client_id_t client_id;
static int tls_index;
static std::string ctx_fname;
static std::string mem_fname;
// static file_t saved_contexts_file = INVALID_FILE;
// static file_t saved_memories_file = INVALID_FILE;
// static std::vector<saved_context_t> saved_contexts;
// static std::vector<saved_memory_t> saved_memories;
static std::vector<std::string> saved_module_names;
static bool initial_state_recorded = false;
#ifdef MEMORY_SAVE_STACK_ONLY
static size_t saved_stack_start = -1;
static size_t saved_stack_end = -1;
#endif
static app_pc main_entry_pc = 0;

static void
event_exit(void);
static void
event_module_load(void *drcontext, const module_data_t * info, bool loaded);
static void
event_thread_init(void *drcontext);
static void
event_thread_exit(void *drcontext);

static dr_emit_flags_t
event_bb_insert(void *drcontext, void *tag, instrlist_t *bb, instr_t *instr,
                bool for_trace, bool translating, void *user_data);

static void
save_context(void*, dr_mcontext_t*);
static void
flush_saved_memories(per_thread_t*);
static void
flush_saved_contexts(per_thread_t*);
static void
flush_saved_modules();
static bool
event_filter_syscall(void*, int);
static bool
event_pre_syscall(void*, int);
static void
event_post_syscall(void*, int);
static void
clean_call_save_memory(size_t, uint16_t);
static void
clean_call_save_context(size_t);
static void
clean_call_xgetbv_callback(size_t pc);
static void
clean_call_cpuid_callback_before(size_t pc);
static void
clean_call_cpuid_callback_after(size_t pc);
static void
clean_call_trace(size_t);
static void
flush_saved_traces(void *drcontext);


DR_EXPORT void
dr_client_main(client_id_t id, int argc, const char *argv[])
{
    dr_fprintf(STDERR, "parsing args\n");
    for (int i = 1; i < argc; ++i) {
        const char *arg = argv[i];
        dr_fprintf(STDERR, "%s\n", arg);
        if (strcmp("-logdir", arg) == 0) {
            if (i + 1 < argc) {
                logdir = std::string{ argv[i+1] };
                if (logdir[logdir.size() - 1] != PATHSEP)
                    logdir += PATHSEP;
                ++i;
            } else {
                dr_fprintf(STDERR, "-logdir specified but no directory specified\n");
                dr_abort();
            }
        } else {
            dr_fprintf(STDERR, "argument ignored: '%s'\n", *arg);
        }
        dr_fprintf(STDERR, "loop end\n");
    }
    // // droption_parser_t::parse_argv crashes...
    // std::string parse_err;
    // int last_idx = 0;
    // if (!droption_parser_t::parse_argv(DROPTION_SCOPE_CLIENT, argc, argv, &parse_err, &last_idx)) {
    //     dr_fprintf(STDERR, "Usage error: %s\n", parse_err.c_str());
    //     dr_abort();
    // }
    dr_fprintf(STDERR, "parsing args done\n");
    if (logdir.empty()) {
        dr_fprintf(STDERR, "fatal error: missing argument -logdir {path}\n");
        dr_abort();
    }

    // checks if the logfile exists already which serves as check whether this
    // process has been forked (we don't want to trace forks)
    if (dr_file_exists((logdir + "instrace.log").c_str()))
        return;

    // if (!op_logdir.get_value().empty()) {
    //     dr_printf("logdir: %s\n", op_logdir.get_value().c_str());
    // }
    /* We need 2 reg slots beyond drreg's eflags slots => 3 slots */
    // TODO: set third option, drreg_options_t.conservative to false for better performance
    // set to true right now to turn off lazy restores of registers which leads to
    // wrong values in the save_context clean call
    drreg_options_t ops = { sizeof(ops), 3, false, NULL };
    /* Specify priority relative to other instrumentation operations: */
    drmgr_priority_t priority = { sizeof(priority), /* size of struct */
                                  "instrace",       /* name of our operation */
                                  NULL, /* optional name of operation we should precede */
                                  NULL, /* optional name of operation we should follow */
                                  0 };  /* numeric priority */
    dr_set_client_name("DynamoRIO Sample Client 'instrace'",
                       "http://dynamorio.org/issues");
    disassemble_set_syntax(DR_DISASM_INTEL);
    page_size = dr_page_size();
    if (!drmgr_init() || drreg_init(&ops) != DRREG_SUCCESS || !drutil_init())
        DR_ASSERT(false);
    client_id = id;
    dr_register_exit_event(event_exit);
    dr_register_filter_syscall_event(event_filter_syscall);
    if (!drmgr_register_thread_init_event(event_thread_init) ||
        !drmgr_register_thread_exit_event(event_thread_exit) ||
        !drmgr_register_bb_instrumentation_event(NULL /*analysis func*/, event_bb_insert,
                                                 &priority) ||
        !drmgr_register_post_syscall_event(event_post_syscall) ||
        !drmgr_register_pre_syscall_event(event_pre_syscall) ||
        !drmgr_register_module_load_event(event_module_load)) {
        /* something is wrong: can't continue */
        DR_ASSERT(false);
        return;
    }
    tls_index = drmgr_register_tls_field();
    DR_ASSERT(tls_index != -1);

    module_data_t *main_module = dr_get_main_module();
    main_entry_pc = main_module->entry_point;
    dr_free_module_data(main_module);


    dr_log(NULL, DR_LOG_ALL, 1, "Client 'instrace' initializing\n");
#ifdef SHOW_RESULTS
    if (dr_is_notify_on()) {
#    ifdef WINDOWS
        /* Ask for best-effort printing to cmd window.  Must be called at init. */
        dr_enable_console_printing();
#    endif
        dr_fprintf(STDERR, "Client instrace is running\n");
    }
#endif
}

static void
event_module_load(void *drcontext, const module_data_t *info, bool loaded)
{
    (void)drcontext;
    std::string name = dr_module_preferred_name(info);
    if (name.find("ntdll") == std::string::npos) {
        return;
    }
    size_t start_addr = (size_t)info->start;
    size_t end_addr = (size_t)info->end;
    // size_t mon_addr = 0x00007ffccbc5d070;
    // if (start_addr <= mon_addr && mon_addr < end_addr) {
    //     dr_printf("monitored_value at module_load : %#zx = %#zx\n", mon_addr, *(size_t*)mon_addr);
    // }
    dr_printf("module_load: %s, loaded %d, from %#zx to %#zx\n", name.c_str(), loaded, start_addr, end_addr);
    dr_flush_file(STDOUT);
    std::stringstream ss;
    // ss << op_logdir.get_value() << '\\';
    ss << logdir << "modules" << PATHSEP;
    ss << "0x" << std::setfill('0') << std::setw(16) << std::hex << start_addr;
    ss << "-";
    ss << "0x" << std::setfill('0') << std::setw(16) << std::hex << end_addr;
    ss << "_" << name.c_str();
    std::string fname { ss.str() };
    file_t f = dr_open_file(fname.c_str(), DR_FILE_WRITE_OVERWRITE);
    if (f == INVALID_FILE) {
        dr_fprintf(STDERR, "Could not open file %s\n", fname.c_str());
        return;
    }
    size_t buf_size = end_addr - start_addr;
    size_t num_written = dr_write_file(f, (void*)start_addr, buf_size);
    if (num_written != buf_size) {
        dr_fprintf(STDERR, "Failed writing to file %s (%#zx/%#zx)\n", fname.c_str(), num_written, buf_size);
    }
    dr_close_file(f);
}

static void
event_exit()
{
    dr_printf("event_exit\n");
#ifdef SHOW_RESULTS
    char msg[512];
    int len;
    len = dr_snprintf(msg, sizeof(msg) / sizeof(msg[0]),
                      "Instrumentation results:\n"
                      "  saw %llu memory references\n",
                      num_refs);
    DR_ASSERT(len > 0);
    NULL_TERMINATE_BUFFER(msg);
    DISPLAY_STRING(msg);
#endif /* SHOW_RESULTS */

    drutil_exit();
    if (!drmgr_unregister_tls_field(tls_index) ||
        !drmgr_unregister_thread_init_event(event_thread_init) ||
        !drmgr_unregister_thread_exit_event(event_thread_exit) ||
        !drmgr_unregister_bb_insertion_event(event_bb_insert) ||
        !dr_unregister_filter_syscall_event(event_filter_syscall) ||
        // !drmgr_unregister_post_syscall_event(event_post_syscall) ||
        !drmgr_unregister_pre_syscall_event(event_pre_syscall) ||
        !drmgr_unregister_module_load_event(event_module_load) ||
        drreg_exit() != DRREG_SUCCESS)
        DR_ASSERT(false);
    dr_fprintf(STDERR, "tracer_run_success\n");

    drmgr_exit();
}

#ifdef WINDOWS
#    define IF_WINDOWS(x) x
#else
#    define IF_WINDOWS(x) /* nothing */
#endif

static void
event_thread_init(void *drcontext)
{
    if (instrumented_thread_id == 0)
        instrumented_thread_id = dr_get_thread_id(drcontext);
    else
        return;

    dr_printf("tracing only thread with id: %d\n", dr_get_thread_id(drcontext));
    /* allocate thread private data */
    per_thread_t *data = (per_thread_t*)dr_thread_alloc(drcontext, sizeof(per_thread_t));
    drmgr_set_tls_field(drcontext, tls_index, data);
    data->buf_base = (char*)dr_thread_alloc(drcontext, INS_BUF_SIZE);
    data->buf_ptr = data->buf_base;
    /* set buf_end to be negative of address of buffer end for the lea later */
    data->buf_end = -(ptr_int_t)(data->buf_base + INS_BUF_SIZE);

    data->saved_contexts_file = INVALID_FILE;
    data->context_buf = (saved_context_t*)dr_thread_alloc(drcontext, CTX_BUF_SIZE);
    data->context_buf_cnt = 0;

    data->saved_memories_file = INVALID_FILE;
    data->memory_buf = (saved_memory_t*)dr_thread_alloc(drcontext, MEM_BUF_SIZE);
    data->memory_buf_cnt = 0;

    data->instr_count = 0;

    /* We're going to dump our data to a per-thread file.
     * On Windows we need an absolute path so we place it in
     * the same directory as our library. We could also pass
     * in a path as a client argument.
     */

    ctx_fname = logdir + "saved_contexts.bin";
    if (data->saved_contexts_file == INVALID_FILE) {
        file_t f = dr_open_file(ctx_fname.c_str(), DR_FILE_WRITE_OVERWRITE);
        if (f == INVALID_FILE) {
            dr_fprintf(STDERR, "Could not open file %s\n", ctx_fname.c_str());
            return;
        }
        data->saved_contexts_file = f;
    }

    mem_fname = logdir + "saved_memories.bin";
    if (data->saved_memories_file == INVALID_FILE) {
        file_t f = dr_open_file(mem_fname.c_str(), DR_FILE_WRITE_OVERWRITE);
        if (f == INVALID_FILE) {
            dr_fprintf(STDERR, "Could not open file %s\n", mem_fname.c_str());
            return;
        }
        data->saved_memories_file = f;
    }

    std::string logfile = logdir + "instrace.log";
    data->log = dr_open_file(logfile.c_str(), DR_FILE_WRITE_OVERWRITE |
    // data->log = log_file_open(client_id, drcontext,
    //     logdir.c_str(),
    //     // op_logdir.get_value().empty()
    //     // ? NULL /* using client lib path */
    //     // : op_logdir.get_value().c_str(),
    //     "instrace",
#ifndef WINDOWS
                      DR_FILE_CLOSE_ON_FORK |
#endif
                          DR_FILE_ALLOW_LARGE);
    if (data->log == INVALID_FILE) {
        dr_fprintf(STDERR, "Could not open log file %s\n", logfile.c_str());
        dr_abort();
    }
}

static void
event_thread_exit(void *drcontext)
{
    thread_id_t id = dr_get_thread_id(drcontext);
    dr_printf("event_thread_exit %d\n", id);
    if (id != instrumented_thread_id)
        return;
    dr_printf("event_thread_exit in main thread\n");
    per_thread_t *data;

    flush_saved_traces(drcontext);
    data = (per_thread_t*)drmgr_get_tls_field(drcontext, tls_index);

    // write mcontext structs
    flush_saved_contexts(data);
    if (data->saved_contexts_file != INVALID_FILE)
        dr_close_file(data->saved_contexts_file);

    // write memory snapshots
    flush_saved_memories(data);
    if (data->saved_memories_file != INVALID_FILE)
        dr_close_file(data->saved_memories_file);
    
    // write list of modules
    flush_saved_modules();

    log_file_close(data->log);
    dr_thread_free(drcontext, data->buf_base, INS_BUF_SIZE);
    dr_thread_free(drcontext, data->context_buf, CTX_BUF_SIZE);
    dr_thread_free(drcontext, data->memory_buf, MEM_BUF_SIZE);
    dr_thread_free(drcontext, data, sizeof(per_thread_t));
}

static size_t get_thread_base() {
    unsigned long addr;
    int ret = syscall(SYS_arch_prctl, ARCH_GET_FS, &addr);
    if (ret != 0) {
        dr_fprintf(STDERR, "arch_prctl failed: %s\n", strerror(errno));
        return -1;
    }
    return addr;
}

static void
dump_mapped_memory()
{
    void *drcontext = dr_get_current_drcontext();
    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;

    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_ALL;
    
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("dr_get_mcontext failed\n");
        dr_abort();
    }

    byte *addr = 0;
    dr_mem_info_t mem_info = { 0 };
    while (dr_query_memory_ex(addr, &mem_info) && mem_info.type != DR_MEMTYPE_ERROR_WINKERNEL && addr != (byte*)-1) {
        // dr_printf("dr_query_memory_ex = %#zx, type %x,  prot %x, internal %d\n", addr, mem_info.type, mem_info.prot, dr_memory_is_dr_internal(addr));
        addr = mem_info.base_pc + mem_info.size;
        // only interested in data that can be read and written
        if ((mem_info.prot & (DR_MEMPROT_GUARD)) != 0 || (mem_info.prot & DR_MEMPROT_READ) == 0 || dr_memory_is_dr_internal(addr))
            continue;
        
        size_t size = (size_t)mem_info.size;
        size_t start_addr = (size_t)mem_info.base_pc;
        size_t end_addr = start_addr + size;

#ifdef MEMORY_SAVE_STACK_ONLY
        // might be too naive to only save it at start since it could grow during runtime
        if (start_addr < mcontext.xsp && mcontext.xsp < end_addr) {
            saved_stack_start = start_addr;
            saved_stack_end = end_addr;
        }
#endif

        if (!dr_memory_is_readable((byte*)start_addr, size)) {
            // dr_printf("Memory is not readable, won't dump. Protections: %x\n", mbi.Protect);
            continue;
        }

        std::string full_name { "" };
        module_data_t *module = dr_lookup_module((byte*)start_addr);
        if (module) {
            full_name = dr_module_preferred_name(module);
            dr_free_module_data(module);
            module = nullptr;
        }

        size_t last_backslash_pos = full_name.rfind('\\');
        std::string short_name = full_name.substr(last_backslash_pos + 1);
        if (short_name.find("ntdll") != std::string::npos) {
            // size_t *addr_to_read = (size_t*)0x00007ffccbb9aff0;
            // dr_printf("dump_mapped_memory: monitored_addr: %#zx = %#zx\n", addr_to_read, *addr_to_read);
            // return;
            continue;
        }

        dr_printf("%#018zx - %#018zx, %s\n", start_addr, end_addr, short_name.c_str());

        // dump memory
        std::stringstream ss;
        // ss << op_logdir.get_value() << '\\';
        ss << logdir << "modules" << PATHSEP;
        std::stringstream ss_fname;
        ss_fname << "0x" << std::setfill('0') << std::setw(16) << std::hex << start_addr;
        ss_fname << '-';
        ss_fname << "0x" << std::setfill('0') << std::setw(16) << std::hex << end_addr;
        ss_fname << '-';
        // mark main module
        if (start_addr <= (size_t)main_entry_pc && (size_t)main_entry_pc < end_addr)
            ss_fname << "main";
        else
            ss_fname << "other";
        
        ss_fname << '_' << short_name;
        ss << ss_fname.str();
        std::string fname { ss.str() };
        file_t f = dr_open_file(fname.c_str(), DR_FILE_WRITE_OVERWRITE);
        if (f == INVALID_FILE) {
            dr_fprintf(STDERR, "Could not open file %s\n", fname.c_str());
            dr_abort();
        }
        saved_module_names.push_back(ss_fname.str());
        size_t buf_size = end_addr - start_addr;
        dr_switch_to_app_state(drcontext);
        size_t num_written = dr_write_file(f, (void*)start_addr, buf_size);
        dr_switch_to_dr_state(drcontext);
        if (num_written != buf_size) {
            dr_fprintf(STDERR, "Failed writing to file %s (%#zx/%#zx)\n", fname.c_str(), num_written, buf_size);
        }
        dr_close_file(f);
    }
}

static void
insert_save_memory(void *drcontext, instrlist_t *ilist, instr_t *where, instr_t *mem_inst)
{
    reg_id_t regaddr;
    reg_id_t scratch;
    bool success;

    if (drreg_reserve_aflags(drcontext, ilist, where) != DRREG_SUCCESS ||
        drreg_reserve_register(drcontext, ilist, where, nullptr, &regaddr) !=
            DRREG_SUCCESS ||
        drreg_reserve_register(drcontext, ilist, where, nullptr, &scratch) !=
            DRREG_SUCCESS)
    {
        DR_ASSERT(false);
        return;
    }

    opnd_t dst = instr_get_dst(mem_inst, 0);
    success = drutil_insert_get_mem_addr(drcontext, ilist, where, dst, regaddr,
                                         scratch);
    DR_ASSERT(success);
    dr_insert_clean_call(drcontext, ilist, where, (void *)clean_call_save_memory, false,
        // pass as argument the address of the written memory + the size
        2,
        opnd_create_reg(regaddr),
        OPND_CREATE_INT16(drutil_opnd_mem_size_in_bytes(dst, mem_inst)));

    if (drreg_unreserve_register(drcontext, ilist, where, regaddr) !=
            DRREG_SUCCESS ||
        drreg_unreserve_register(drcontext, ilist, where, scratch) !=
            DRREG_SUCCESS ||
        drreg_unreserve_aflags(drcontext, ilist, where) != DRREG_SUCCESS)
    {
        DR_ASSERT(false);
    }
}

static void
insert_save_context(void *drcontext, instrlist_t *ilist, instr_t *where, app_pc pc)
{
    dr_insert_clean_call(drcontext, ilist, where, (void *)clean_call_save_context, false,
        // pass as argument the PC of the instruction
        1, OPND_CREATE_INTPTR(pc));
}


static app_pc context_save_instr_pc = 0;
static instr_t *memory_save_instr = nullptr;
static int memory_save_instr_op = 0;
static app_pc xgetbv_instr_pc = 0;
static app_pc cpuid_instr_pc = 0;
static int instr_count = 0;
/* event_bb_insert calls instrument_instr to instrument every
 * application memory reference.
 */
static dr_emit_flags_t
event_bb_insert(void *drcontext, void *tag, instrlist_t *bb, instr_t *instr,
                bool for_trace, bool translating, void *user_data)
{
    (void)user_data; (void)tag; (void)for_trace; (void)translating;

    app_pc pc = instr_get_app_pc(instr);
    if (pc == NULL || !instr_is_app(instr))
        return DR_EMIT_DEFAULT;
    
    // dump state at entry point
    // DO NOT RETURN EARLY HERE. We still need to instrument every basic block
    // if (!initial_state_recorded && pc == (app_pc)0x4411a0) {
    if (!initial_state_recorded && pc == main_entry_pc) {
        initial_state_recorded = true;

        dr_insert_clean_call(drcontext, bb, instr, (void *)dump_mapped_memory, false,
            0, NULL);
        insert_save_context(drcontext, bb, instr, (app_pc)-1);
    }
    
    // insert context/memory save call in case e.g. the last instruction was
    // unsupported by the analysis tool
    if (context_save_instr_pc) {
        insert_save_context(drcontext, bb, instr, context_save_instr_pc);
        context_save_instr_pc = 0;
    }
    if (memory_save_instr) {
        insert_save_memory(drcontext, bb, instr, memory_save_instr);
        memory_save_instr = nullptr;
    }
    if (xgetbv_instr_pc) {
        dr_insert_clean_call(drcontext, bb, instr, (void *)clean_call_xgetbv_callback, false,
            // pass as argument the PC of the instruction
            1, OPND_CREATE_INTPTR(pc));
        xgetbv_instr_pc = 0;
    }
    if (cpuid_instr_pc) {
        dr_insert_clean_call(drcontext, bb, instr, (void *)clean_call_cpuid_callback_after, false,
            // pass as argument the PC of the instruction
            1, OPND_CREATE_INTPTR(pc));
        cpuid_instr_pc = 0;
    }

    // determine whether a context/memory save is required and signal it by
    // setting context_save_instr_pc/memory_save_instr_pc to save the state
    // after the current instruction
    int opc = instr_get_opcode(instr);
    bool dbg_dump = false; // pc == (app_pc)0x7ffccbb56e16;
    if (opc == OP_cpuid || opc == OP_xgetbv //|| uses_tls_segment || writes_tls_segment
        // || opc == OP_syscall || opc == OP_sysenter
        || opc == OP_rdtsc || opc == OP_rdtscp
        || opc == OP_rdrand
        // || opc == OP_vpmovmskb
        || opc == OP_xsave32
        || opc == OP_xrstor32
        // || opc == OP_pslld
        // || opc == OP_psllq
        || opc == OP_ucomisd
        || opc == OP_vmovd
        || opc == OP_vpxor
        || opc == OP_vpbroadcastb
        || opc == OP_fnstcw
        || opc == OP_cvtsd2ss
        || dbg_dump) {
        if (opc == OP_xgetbv)
            xgetbv_instr_pc = pc;
        if (opc == OP_cpuid) {
            dr_insert_clean_call(drcontext, bb, instr, (void *)clean_call_cpuid_callback_before, false,
                // pass as argument the PC of the instruction
                1, OPND_CREATE_INTPTR(pc));
            cpuid_instr_pc = pc;
        }
        char buf[128];
        instr_disassemble_to_buffer(drcontext, instr, buf, sizeof(buf));
        context_save_instr_pc = pc;
        memory_save_instr = opc == instr_writes_memory(instr) || dbg_dump ? instr : nullptr;
        // if (opc == OP_syscall || opc == OP_sysenter) {
        //     dr_insert_clean_call(drcontext, bb, instr, (void *)clean_call_prepare_syscall, false,
        //         // pass as argument the PC of the instruction
        //         0);
        // }
        memory_save_instr_op = opc;
        dr_printf("context%s_save_instr %#zx %d %s\n",
            memory_save_instr != nullptr ? "/memory" : "", context_save_instr_pc, instr_count, buf);
    }

    dr_insert_clean_call(drcontext, bb, instr, (void *)clean_call_trace, false,
        // pass as argument the PC of the instruction
        1, OPND_CREATE_INTPTR(pc));

    ++instr_count;
    return DR_EMIT_DEFAULT;
}

static void
save_context(void *drcontext, dr_mcontext_t *mcontext)
{
    per_thread_t *data = (per_thread_t*)drmgr_get_tls_field(drcontext, tls_index);
    saved_context_t s;
    s.instr_num = data->instr_count - 1;
    dr_printf("runtime save_context instr_count: %lld rip: %#p\n", data->instr_count - 1, mcontext->rip);

    s.xdi = (size_t)mcontext->xdi;
    s.xsi = (size_t)mcontext->xsi;
    s.xbp = (size_t)mcontext->xbp;
    s.xsp = (size_t)mcontext->xsp;
    s.xbx = (size_t)mcontext->xbx;
    s.xdx = (size_t)mcontext->xdx;
    s.xcx = (size_t)mcontext->xcx;
    s.xax = (size_t)mcontext->xax;

    s.r8 = (size_t)mcontext->r8;
    s.r9 = (size_t)mcontext->r9;
    s.r10 = (size_t)mcontext->r10;
    s.r11 = (size_t)mcontext->r11;
    s.r12 = (size_t)mcontext->r12;
    s.r13 = (size_t)mcontext->r13;
    s.r14 = (size_t)mcontext->r14;
    s.r15 = (size_t)mcontext->r15;

    s.xflags = mcontext->xflags;

    s.xip = (size_t)mcontext->xip;

    dr_switch_to_app_state(drcontext);
    size_t tb = get_thread_base();
    // size_t tb_val = ((size_t*)tb)[0x28/sizeof(size_t)];
    dr_switch_to_dr_state(drcontext);
    // dr_printf("fs addr %#zx [0x28] = %#zx, rax = %#zx\n", tb, tb_val, mcontext->rax);
    s.fs  = tb;

    // check if the buffer is full
    if (data->context_buf_cnt >= MAX_NUM_CTX)
        flush_saved_contexts(data);
    // save context in buffer
    data->context_buf[data->context_buf_cnt++] = s;
}

static void
flush_saved_memories(per_thread_t *data)
{
    if (data->memory_buf_cnt == 0)
        return;

    for (saved_memory_t *saved_memory = data->memory_buf; saved_memory < data->memory_buf + data->memory_buf_cnt; ++saved_memory) {
        size_t buf_size = sizeof(*saved_memory);
        size_t num_written = dr_write_file(data->saved_memories_file, saved_memory, buf_size);
        if (num_written != buf_size) {
            dr_fprintf(STDERR, "Failed writing to file %s (%#zx/%#zx)\n", mem_fname.c_str(), num_written, buf_size);
        }
        buf_size = saved_memory->size;
        num_written = dr_write_file(data->saved_memories_file, saved_memory->data, buf_size);
        if (num_written != buf_size) {
            dr_fprintf(STDERR, "Failed writing to file %s (%#zx/%#zx)\n", mem_fname.c_str(), num_written, buf_size);
        }
        dr_global_free((void*)saved_memory->data, saved_memory->size);
    }
    data->memory_buf_cnt = 0;
}

static void
flush_saved_contexts(per_thread_t* data)
{
    if (data->context_buf_cnt == 0)
        return;

    size_t buf_size = data->context_buf_cnt * sizeof(data->context_buf[0]);
    size_t num_written = dr_write_file(data->saved_contexts_file, data->context_buf, buf_size);
    if (num_written != buf_size) {
        dr_fprintf(STDERR, "Failed writing to file %s (%#zx/%#zx)\n", ctx_fname.c_str(), num_written, buf_size);
    }
    data->context_buf_cnt = 0;
}

static void
flush_saved_modules()
{
    std::string modules_path = logdir + "modules.txt";
    file_t f = dr_open_file(modules_path.c_str(), DR_FILE_WRITE_OVERWRITE);
    if (f == INVALID_FILE) {
        dr_fprintf(STDERR, "Could not open file %s\n", modules_path.c_str());
        return;
    }
    for (auto &mod : saved_module_names) {
        dr_write_file(f, mod.c_str(), mod.size());
        dr_write_file(f, "\n", 1);
    }
    dr_close_file(f);
}

static void
flush_saved_traces(void *drcontext)
{
    per_thread_t *data;
    ins_ref_t *ins_ref;

    data = (per_thread_t*)drmgr_get_tls_field(drcontext, tls_index);
    ins_ref = (ins_ref_t *)data->buf_base;

    dr_write_file(data->log, data->buf_base, (size_t)(data->buf_ptr - data->buf_base));

    memset(data->buf_base, 0, INS_BUF_SIZE);
    data->buf_ptr = data->buf_base;
}

static void
clean_call_trace(size_t pc)
{
    // only record traces once the entry point has been reached
    if (!initial_state_recorded)
        return;

    void *drcontext = dr_get_current_drcontext();

    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;

    per_thread_t *data = (per_thread_t*)drmgr_get_tls_field(drcontext, tls_index);

    data->instr_count += 1;
    *(size_t*)(data->buf_ptr) = pc;
    data->buf_ptr += sizeof(ins_ref_t);
    if ((size_t)data->buf_ptr >= (size_t)-data->buf_end)
        flush_saved_traces(drcontext);
}

size_t mem_to_save_addr = 0;
size_t mem_to_save_size = 0;
static void
clean_call_save_memory(size_t mem_to_save_addr, uint16_t mem_to_save_size)
{
    // only save memory once the entry point has been reached
    if (!initial_state_recorded)
        return;
    
    // if no save memory addr/size has been specified in the prepare syscall
    // step we don't save memory
    if (mem_to_save_addr == 0 || mem_to_save_size == 0)
        return;

    void *drcontext = dr_get_current_drcontext();

    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;

    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_ALL;
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("dr_get_mcontext failed\n");
        return;
    }

    dr_printf("saving memory\n");
    per_thread_t *data = (per_thread_t*)drmgr_get_tls_field(drcontext, tls_index);
#ifndef MEMORY_SAVE_STACK_ONLY
#if 1
    // query the entire address space and save anything that looks like data
    byte *addr = (byte*)mem_to_save_addr;
    byte *max_addr = addr + mem_to_save_size;
    dr_mem_info_t mem_info = { 0 };
    while (dr_query_memory_ex(addr, &mem_info) && mem_info.type != DR_MEMTYPE_ERROR_WINKERNEL && addr != (byte*)-1 && addr < max_addr) {
        // dr_printf("dr_query_memory_ex = %#zx, type %x,  prot %x, internal %d\n", addr, mem_info.type, mem_info.prot, dr_memory_is_dr_internal(addr));
        addr = mem_info.base_pc + mem_info.size;
        // only interested in data that can be written to since read only data should not change
        // if (mem_info.type != DR_MEMTYPE_DATA || mem_info.prot != (DR_MEMPROT_READ | DR_MEMPROT_WRITE) || dr_memory_is_dr_internal(addr))
        if ((mem_info.prot & (DR_MEMPROT_GUARD | DR_MEMPROT_EXEC | DR_MEMPROT_VDSO)) != 0 || (mem_info.prot & DR_MEMPROT_READ) == 0 || dr_memory_is_dr_internal(addr))
            continue;
        
        saved_memory_t saved_memory = { 0 };
        saved_memory.trace_addr = data->instr_count - 1;
        saved_memory.start_addr = mem_to_save_addr;
        // only save as much as we need to in case the max address is inside the current region
        if (mem_info.base_pc + mem_info.size >= max_addr)
            saved_memory.size = mem_to_save_size;
        // in case it contains more than the current segment
        else
            saved_memory.size = mem_info.size - (mem_to_save_addr - (size_t)mem_info.base_pc);
        saved_memory.data = (const char*)dr_global_alloc(saved_memory.size);

        dr_switch_to_app_state(drcontext);
        memcpy((void*)saved_memory.data, (void*)saved_memory.start_addr, saved_memory.size);
        dr_switch_to_dr_state(drcontext);

        // saved_memories.push_back(saved_memory);
        // check if the buffer is full
        if (data->memory_buf_cnt >= MAX_NUM_MEM)
            flush_saved_memories(data);
        // save context in buffer
        data->memory_buf[data->memory_buf_cnt++] = saved_memory;
    }
#else
    // query the entire address space and save anything that looks like data
    byte *addr = 0;
    dr_mem_info_t mem_info = { 0 };
    while (dr_query_memory_ex(addr, &mem_info) && mem_info.type != DR_MEMTYPE_ERROR_WINKERNEL && addr != (byte*)-1) {
        // dr_printf("dr_query_memory_ex = %#zx, type %x,  prot %x, internal %d\n", addr, mem_info.type, mem_info.prot, dr_memory_is_dr_internal(addr));
        addr = mem_info.base_pc + mem_info.size;
        // only interested in data that can be written to since read only data should not change
        // if (mem_info.type != DR_MEMTYPE_DATA || mem_info.prot != (DR_MEMPROT_READ | DR_MEMPROT_WRITE) || dr_memory_is_dr_internal(addr))
        if ((mem_info.prot & (DR_MEMPROT_GUARD | DR_MEMPROT_EXEC | DR_MEMPROT_VDSO)) != 0 || (mem_info.prot & DR_MEMPROT_READ) == 0 || dr_memory_is_dr_internal(addr))
            continue;
        
        saved_memory_t saved_memory = { 0 };
        saved_memory.trace_addr = data->instr_count - 1;
        saved_memory.start_addr = (size_t)mem_info.base_pc;
        saved_memory.size = mem_info.size;
        saved_memory.data = (const char*)dr_global_alloc(saved_memory.size);

        dr_switch_to_app_state(drcontext);
        memcpy((void*)saved_memory.data, (void*)saved_memory.start_addr, saved_memory.size);
        dr_switch_to_dr_state(drcontext);

        // saved_memories.push_back(saved_memory);
        // check if the buffer is full
        if (data->memory_buf_cnt >= MAX_NUM_MEM)
            flush_saved_memories(data);
        // save context in buffer
        data->memory_buf[data->memory_buf_cnt++] = saved_memory;
    }
#endif
#else
    if (saved_stack_start == -1 || saved_stack_end == -1) {
        dr_printf("saved_stack bounds invalid: %#zx - %#zx\n", saved_stack_start, saved_stack_end);
    }

    saved_memory_t saved_memory = { 0 };
    saved_memory.trace_addr = data->instr_count - 1;
    saved_memory.start_addr = saved_stack_start;
    saved_memory.size = saved_stack_end - saved_stack_start;
    saved_memory.data = (const char*)dr_global_alloc(saved_memory.size);

    memcpy((void*)saved_memory.data, (void*)saved_memory.start_addr, saved_memory.size);

    saved_memories.push_back(saved_memory);
#endif

    if (!dr_set_mcontext(drcontext, &mcontext)) {
        dr_printf("dr_set_mcontext failed\n");
        dr_abort();
    }
    mem_to_save_addr = 0;
    mem_to_save_size = 0;
}

static bool
event_filter_syscall(void *drcontext, int sysnum)
{
    (void)drcontext;
    (void)sysnum;
    // we need to filter all of the syscalls for context saving and sometimes memory saving
    return true;
}

#include <sys/stat.h>
static bool
event_pre_syscall(void *drcontext, int sysnum)
{
    (void)drcontext;
    // only save memory once the entry point has been reached
    // if (!initial_state_recorded)
    //     return false;

    // if (instrumented_thread_id != dr_get_thread_id(drcontext))
    //     return false;

    // bool ret = false;
    switch (sysnum) {
        // read
        case 0: {
            dr_mcontext_t mcontext = { 0 };
            mcontext.size = sizeof(dr_mcontext_t);
            mcontext.flags = DR_MC_ALL;
            if (!dr_get_mcontext(drcontext, &mcontext)) {
                dr_printf("dr_get_mcontext failed\n");
                break;
            }
            mem_to_save_addr = mcontext.rsi;
            mem_to_save_size = mcontext.rdx;
            // ret = true;
            break;
        }
        // stat, fstat, lstat
        case 4:
        case 5:
        case 6: {
            dr_mcontext_t mcontext = { 0 };
            mcontext.size = sizeof(dr_mcontext_t);
            mcontext.flags = DR_MC_ALL;
            if (!dr_get_mcontext(drcontext, &mcontext)) {
                dr_printf("dr_get_mcontext failed\n");
                break;
            }
            mem_to_save_addr = mcontext.rsi;
            mem_to_save_size = sizeof(struct stat);
            // ret = true;
            break;
        }
        default:
            break;
    }
    // dr_printf("event_pre_syscall %d = %d\n", sysnum, ret);
    return true;
}

static bool
sys_is_memory_save(int sysnum)
{
    switch (sysnum) {
        // read
        case 0:
        // stat, fstat, lstat
        case 4:
        case 5:
        case 6:
            return true;
        default:
            return false;
    }
}

static void
event_post_syscall(void *drcontext, int sysnum)
{
    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_CONTROL;
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("dr_get_mcontext failed\n");
        return;
    }
    // not a nice way to handle passing the PC but sysenter and syscall are both
    // 2 bytes long so it works
    clean_call_save_context((size_t)mcontext.rip - 2);
    if (!sys_is_memory_save(sysnum))
        return;
    dr_printf("event_post_syscall %d\n", sysnum);
    clean_call_save_memory(mem_to_save_addr, mem_to_save_size);
}

static void
clean_call_save_context(size_t pc)
{
    if (!initial_state_recorded)
        return;
    
    void *drcontext = dr_get_current_drcontext();

    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;

    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_ALL;
    
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("%s: dr_get_mcontext failed\n", __FUNCTION__);
        return;
    }

    // mcontext seems to contain a wrong value so set it manually
    mcontext.xip = (byte*)pc;
    save_context(drcontext, &mcontext);
}

static void
clean_call_xgetbv_callback(size_t pc)
{
    // if (!initial_state_recorded)
    //     return;

    (void)pc;
    void *drcontext = dr_get_current_drcontext();
    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;
    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_INTEGER;
    
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("%s: dr_get_mcontext failed\n", __FUNCTION__);
        return;
    }

    if ((mcontext.rax & 0x6) == 0x6) {
        dr_printf("eax & 0x6, removing bit\n");
        mcontext.rax = mcontext.rax & ~0x6;
        dr_set_mcontext(drcontext, &mcontext);
    }
    if ((mcontext.rax & 0x7) == 0x7) {
        dr_printf("eax & 0x7, removing bit\n");
        mcontext.rax = mcontext.rax & ~0x7;
        dr_set_mcontext(drcontext, &mcontext);
    }
}

static uint32_t cpuid_eax_arg = 0;
static void
clean_call_cpuid_callback_before(size_t pc)
{
    (void)pc;
    void *drcontext = dr_get_current_drcontext();
    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;
    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_INTEGER;
    
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("%s: dr_get_mcontext failed\n", __FUNCTION__);
        return;
    }

    cpuid_eax_arg = (uint32_t)mcontext.rax;
}

// https://wiki.osdev.org/SSE
#define SSE2_BIT (1 << 26)
#define SSE3_BIT (1 << 0)
#define SSSE3_BIT (1 << 9)
#define SSE41_BIT (1 << 19)
#define SSE42_BIT (1 << 20)
static void
clean_call_cpuid_callback_after(size_t pc)
{
    (void)pc;
    if (cpuid_eax_arg != 1)
        return;

    void *drcontext = dr_get_current_drcontext();
    if (instrumented_thread_id != dr_get_thread_id(drcontext))
        return;
    dr_mcontext_t mcontext = { 0 };
    mcontext.size = sizeof(dr_mcontext_t);
    mcontext.flags = DR_MC_INTEGER;
    
    if (!dr_get_mcontext(drcontext, &mcontext)) {
        dr_printf("%s: dr_get_mcontext failed\n", __FUNCTION__);
        return;
    }

    bool modified = false;
    if ((mcontext.rdx & SSE2_BIT) == SSE2_BIT) {
        dr_printf("SSE2_BIT set, removing it\n");
        mcontext.rdx = mcontext.rdx & ~SSE2_BIT;
        modified = true;
    }
    if ((mcontext.rcx & SSE3_BIT) == SSE3_BIT) {
        dr_printf("SSE3_BIT set, removing it\n");
        mcontext.rcx = mcontext.rcx & ~SSE3_BIT;
        modified = true;
    }
    if ((mcontext.rcx & SSSE3_BIT) == SSSE3_BIT) {
        dr_printf("SSSE3_BIT set, removing it\n");
        mcontext.rcx = mcontext.rcx & ~SSSE3_BIT;
        modified = true;
    }
    if ((mcontext.rcx & SSE41_BIT) == SSE41_BIT) {
        dr_printf("SSE41_BIT set, removing it\n");
        mcontext.rcx = mcontext.rcx & ~SSE41_BIT;
        modified = true;
    }
    if ((mcontext.rcx & SSE42_BIT) == SSE42_BIT) {
        dr_printf("SSE42_BIT set, removing it\n");
        mcontext.rcx = mcontext.rcx & ~SSE42_BIT;
        modified = true;
    }
    if (modified)
        dr_set_mcontext(drcontext, &mcontext);
}
