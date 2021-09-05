#include <taint/analysis.h>
#include <taint/utils.h>
#include <fmt/format.h>
#include <keystone/keystone.h>

#include <signal.h>

#include <unordered_map>
#include <sstream>

std::vector<triton::arch::x86::instruction_e> TaintAnalysis::skipped_instructions =
    {
        triton::arch::x86::ID_INS_XGETBV,
        triton::arch::x86::ID_INS_RDTSCP,
        triton::arch::x86::ID_INS_RDRAND,
        triton::arch::x86::ID_INS_XSAVE,
        triton::arch::x86::ID_INS_XSTORE,
        triton::arch::x86::ID_INS_XRSTOR,
        triton::arch::x86::ID_INS_FNSTCW,
        triton::arch::x86::ID_INS_CVTSD2SS,
};

void TaintAnalysis::set_debug(bool dbg)
{
    this->debug = dbg;
}

void TaintAnalysis::set_print_all_instructions(bool print_all)
{
    this->print_all_instructions = print_all;
}

void TaintAnalysis::set_context(const saved_context_t *context, bool set_ip)
{
	api.setTaintRegister(api.registers.x86_rip, false);
	if (set_ip) {
		api.setConcreteRegisterValue(api.registers.x86_rip, context->xip);
	}

	if (api.getConcreteRegisterValue(api.registers.x86_fs) != context->fs)
		api.setTaintRegister(api.registers.x86_fs, false);
	api.setConcreteRegisterValue(api.registers.x86_fs, context->fs);

	// if (api.getConcreteRegisterValue(api.registers.x86_eflags) != context->xflags)
	// always clear taint of eflags since it probably happens too often that
	// they didn't change but they were assigned to
	fmt::print("clearing eflags taint\n");
	api.setTaintRegister(api.registers.x86_eflags, false);
	api.setTaintRegister(api.registers.x86_zf, false);
	api.setTaintRegister(api.registers.x86_cf, false);
	api.setConcreteRegisterValue(api.registers.x86_eflags, context->xflags);

	// set x86 regs
	if (api.getConcreteRegisterValue(api.registers.x86_rdi) != context->xdi)
		api.setTaintRegister(api.registers.x86_rdi, false);
	api.setConcreteRegisterValue(api.registers.x86_rdi, context->xdi);

	if (api.getConcreteRegisterValue(api.registers.x86_rsi) != context->xsi)
		api.setTaintRegister(api.registers.x86_rsi, false);
	api.setConcreteRegisterValue(api.registers.x86_rsi, context->xsi);

	if (api.getConcreteRegisterValue(api.registers.x86_rbp) != context->xbp)
		api.setTaintRegister(api.registers.x86_rbp, false);
	api.setConcreteRegisterValue(api.registers.x86_rbp, context->xbp);

	if (api.getConcreteRegisterValue(api.registers.x86_rsp) != context->xsp)
		api.setTaintRegister(api.registers.x86_rsp, false);
	api.setConcreteRegisterValue(api.registers.x86_rsp, context->xsp);

	if (api.getConcreteRegisterValue(api.registers.x86_rbx) != context->xbx)
		api.setTaintRegister(api.registers.x86_rbx, false);
	api.setConcreteRegisterValue(api.registers.x86_rbx, context->xbx);

	if (api.getConcreteRegisterValue(api.registers.x86_rdx) != context->xdx)
		api.setTaintRegister(api.registers.x86_rdx, false);
	api.setConcreteRegisterValue(api.registers.x86_rdx, context->xdx);

	if (api.getConcreteRegisterValue(api.registers.x86_rcx) != context->xcx)
		api.setTaintRegister(api.registers.x86_rcx, false);
	api.setConcreteRegisterValue(api.registers.x86_rcx, context->xcx);

	if (api.getConcreteRegisterValue(api.registers.x86_rax) != context->xax)
		api.setTaintRegister(api.registers.x86_rax, false);
	api.setConcreteRegisterValue(api.registers.x86_rax, context->xax);

	// set amd64 regs
	if (api.getConcreteRegisterValue(api.registers.x86_r8) != context->r8)
		api.setTaintRegister(api.registers.x86_r8, false);
	api.setConcreteRegisterValue(api.registers.x86_r8, context->r8);

	if (api.getConcreteRegisterValue(api.registers.x86_r9) != context->r9)
		api.setTaintRegister(api.registers.x86_r9, false);
	api.setConcreteRegisterValue(api.registers.x86_r9, context->r9);

	if (api.getConcreteRegisterValue(api.registers.x86_r10) != context->r10)
		api.setTaintRegister(api.registers.x86_r10, false);
	api.setConcreteRegisterValue(api.registers.x86_r10, context->r10);

	if (api.getConcreteRegisterValue(api.registers.x86_r11) != context->r11)
		api.setTaintRegister(api.registers.x86_r11, false);
	api.setConcreteRegisterValue(api.registers.x86_r11, context->r11);

	if (api.getConcreteRegisterValue(api.registers.x86_r12) != context->r12)
		api.setTaintRegister(api.registers.x86_r12, false);
	api.setConcreteRegisterValue(api.registers.x86_r12, context->r12);

	if (api.getConcreteRegisterValue(api.registers.x86_r13) != context->r13)
		api.setTaintRegister(api.registers.x86_r13, false);
	api.setConcreteRegisterValue(api.registers.x86_r13, context->r13);

	if (api.getConcreteRegisterValue(api.registers.x86_r14) != context->r14)
		api.setTaintRegister(api.registers.x86_r14, false);
	api.setConcreteRegisterValue(api.registers.x86_r14, context->r14);

	if (api.getConcreteRegisterValue(api.registers.x86_r15) != context->r15)
		api.setTaintRegister(api.registers.x86_r15, false);
	api.setConcreteRegisterValue(api.registers.x86_r15, context->r15);
}

void TaintAnalysis::print_context()
{
	fmt::print("[C] {: <3} = {:#018x}\n", "rip", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rip));
	fmt::print("[C] {: <3} = {:#018x}\n", "rsp", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rsp));

	fmt::print("[C] {: <3} = {:#018x}\n", "rdi", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rdi));
	fmt::print("[C] {: <3} = {:#018x}\n", "rsi", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rsi));
	fmt::print("[C] {: <3} = {:#018x}\n", "rbp", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rbp));
	fmt::print("[C] {: <3} = {:#018x}\n", "rbx", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rbx));
	fmt::print("[C] {: <3} = {:#018x}\n", "rdx", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rdx));
	fmt::print("[C] {: <3} = {:#018x}\n", "rcx", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rcx));
	fmt::print("[C] {: <3} = {:#018x}\n", "rax", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rax));

	fmt::print("[C] {: <3} = {:#018x}\n", "r8", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r8));
	fmt::print("[C] {: <3} = {:#018x}\n", "r9", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r9));
	fmt::print("[C] {: <3} = {:#018x}\n", "r10", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r10));
	fmt::print("[C] {: <3} = {:#018x}\n", "r11", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r11));
	fmt::print("[C] {: <3} = {:#018x}\n", "r12", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r12));
	fmt::print("[C] {: <3} = {:#018x}\n", "r13", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r13));
	fmt::print("[C] {: <3} = {:#018x}\n", "r14", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r14));
	fmt::print("[C] {: <3} = {:#018x}\n", "r15", (uint64_t)api.getConcreteRegisterValue(api.registers.x86_r15));
}

void TaintAnalysis::set_code_arr_taint(triton::uint64 arr_start, triton::uint64 arr_length, bool taint)
{
    triton::uint64 element_size = 2;
    fmt::print("[*] Setting taint of memory interval [{:#x}, {:#x}) to {}\n", arr_start, arr_start + arr_length*element_size, taint);
    for (triton::uint64 i = 0; i < arr_length; i++)
    {
	    api.setTaintMemory(triton::arch::MemoryAccess(arr_start + i*element_size, element_size), taint);
    }
}

bool TaintAnalysis::setup_context(std::vector<saved_module_t> &modules, size_t text_section_start, size_t text_section_end)
{
    api.reset();
    api.setArchitecture(triton::arch::ARCH_X86_64);
    api.enableSymbolicEngine(false);
    api.enableTaintEngine(true);

    for (auto &mod : modules)
    {
        std::vector<char> mod_data;
        if (!read_whole_file(mod.path.c_str(), mod_data))
            return false;

        api.setConcreteMemoryAreaValue(mod.start, reinterpret_cast<triton::uint8 *>(mod_data.data()), mod_data.size());
    }
    this->modules.swap(modules);

    return true;
}

bool stop_requested = false;

CollectedData TaintAnalysis::emulate(LazyTraceLoader &trace,
		LazyContextLoader &contexts,
		LazyMemoryLoader &memories)
{
	size_t trace_pos = 0;
  size_t diverged = 0;
  size_t skipped = 0;
  size_t xsavec = 0;
	auto pc = *trace.current;
	auto last_pc = pc;

	contexts.current->xip = pc;
	set_context(contexts.current, true);
	size_t context_pos = 1;
	size_t memory_pos = 0;

	auto inst = triton::arch::Instruction();
	struct sigaction sigIntHandler;
	sigIntHandler.sa_handler = [](int sig) {
		fmt::print("Caught signal {:d}\n", sig);
		stop_requested = true;
	};
	sigemptyset(&sigIntHandler.sa_mask);
	sigIntHandler.sa_flags = 0;
	sigaction(SIGINT, &sigIntHandler, NULL);

  std::map<triton::arch::register_e, int> reg_occurrences;
  // part of the attack: if the last instruction was movabs rdi {immediate}, the code array might be its second operand
  bool last_inst_relevant = false;
  triton::uint64 possible_code_arr = 0;
  triton::uint64 code_arr_address = 0;
  triton::uint64 code_arr_length = 0;
  bool guard_bypassed = false;

    while (true)
    {
	    do
        {
            // set instruction to emulate
            inst.clear();
            auto opcodes = api.getConcreteMemoryAreaValue(pc, 16, false);
            inst.setOpcode(opcodes.data(), opcodes.size());
            inst.setAddress(pc);

            bool skip_instruction = false;
            bool xsavec_instruction = false;
            bool successfully_executed = false;
            try {
            		// for some reason, tetris flatten.10 seems to throw a cannot
            		// disassemble instruction sometimes, we simply catch it here
            		// and proceed
                successfully_executed = api.processing(inst);
            } catch (const triton::exceptions::Exception &e) {
                // we don't want to terminate
                // just clear the instruction
                inst.clear();
                // fmt::print("exception in processing:\n{}", e.what());
                // for (int blub = 0; blub < 16; ++blub) {
                //     fmt::print("{:02x} ", opcodes[blub]);
                // }
                // fmt::print("\n");
            }
            // execute the instruction
            if (!successfully_executed)
            {
                // uncomment if you care about hitting unsupported instructions
                // if (false) {
                //     // in case it is not a known instruction to skip, error out
                //     if (std::find(skipped_instructions.begin(), skipped_instructions.end(), inst.getType()) == skipped_instructions.end())
                //     {
                //         fmt::print("[-] unsupported instruction {}\n", inst.getDisassembly());
                //         return CollectedData { true, false, ++trace_pos, diverged, skipped, xsavec };
                //     }
                // }

                // check for xsavec as it is not recognized by Triton
                if (opcodes[0]==0x0f && opcodes[1]==0xc7 && opcodes[2]>=0x60 && opcodes[2]<0x68)
                {
                    ++xsavec;
                    xsavec_instruction = true;
                }
                ++skipped;
                skip_instruction = true;
            }

            /*for (int blub = 0; blub < 16; ++blub) {
                fmt::print("{:02x} ", opcodes[blub]);
            }
            fmt::print("\n");*/

            triton::uint32 TYPE_MOVABS = 443;
            triton::uint32 TYPE_MOV    = 442;
            triton::uint32 TYPE_CMP    =  93;
            triton::arch::register_e REG_RDI = static_cast<triton::arch::register_e>(5);
            triton::arch::register_e REG_EAX = static_cast<triton::arch::register_e>(42);
            if(last_inst_relevant && inst.getType() == TYPE_MOV && inst.operands.size() == 2
            		&& inst.operands[0].getType() == triton::arch::OP_REG && inst.operands[1].getType() == triton::arch::OP_IMM
            		&& inst.operands[0].getRegister().getId() == REG_EAX)
            {
            	// fmt::print("CODE ARRAY BEGINNING: {:#x}\n", possible_code_arr);
            	code_arr_address = possible_code_arr;
            	code_arr_length = inst.operands[1].getImmediate().getValue();
            	set_code_arr_taint(code_arr_address, code_arr_length, true);
            }
            if(inst.getType() == TYPE_MOVABS && inst.operands.size() == 2
            		&& inst.operands[0].getType() == triton::arch::OP_REG && inst.operands[1].getType() == triton::arch::OP_IMM
            		&& inst.operands[0].getRegister().getId() == REG_RDI)
            {
            	last_inst_relevant = true;
            	possible_code_arr = inst.operands[1].getImmediate().getValue();
            }
            else
            {
            	last_inst_relevant = false;
            }
            if(inst.getType() == TYPE_CMP && inst.isTainted())
            {
            	// fmt::print("[*] Guard cmp found\n");
            	set_code_arr_taint(code_arr_address, code_arr_length, false);
            	//fmt::print("Eflags before: {}\n", api.getConcreteRegisterValue(api.registers.x86_eflags));
            	// std::cout << "Eflags before: " << api.getConcreteRegisterValue(api.registers.x86_eflags) << std::endl;
            	// std::cout << "Zero F before: " << api.getConcreteRegisterValue(api.registers.x86_zf) << std::endl;
            	triton::uint512 eflags_before_setting_flag = api.getConcreteRegisterValue(api.registers.x86_eflags);
            	triton::uint512 eflags_with_zero_flag      = eflags_before_setting_flag | 0x40;
            	if(eflags_before_setting_flag != eflags_with_zero_flag)
            	{
            		fmt::print("[*] Guard bypassed\n");
            		api.setConcreteRegisterValue(api.registers.x86_eflags, eflags_with_zero_flag);
            		guard_bypassed = true;
                return CollectedData { false, true, ++trace_pos, diverged, skipped, xsavec };
            	}
            	//fmt::print("Eflags after: {}\n", api.getConcreteRegisterValue(api.registers.x86_eflags));
            	// std::cout << "Eflags after:  " << api.getConcreteRegisterValue(api.registers.x86_eflags) << std::endl;
            	// std::cout << "Zero F after:  " << api.getConcreteRegisterValue(api.registers.x86_zf) << std::endl;
            }
            // fmt::print("Tainted: {:<7}|  ", inst.isTainted());
            //std::cout << "Eflags: " << api.getConcreteRegisterValue(api.registers.x86_eflags) << "  |  ";

            /* for (int i=0; i<inst.operands.size(); i++) {
            	auto operand = inst.operands[i];
            	if(operand.getType() == triton::arch::OP_REG) {
            		triton::arch::register_e regId = operand.getRegister().getId();
            		std::string regName = operand.getRegister().getName();
            		// fmt::print("Name: {}, Id: {}, Value: {}\n", regName, regId, (uint64_t)api.getConcreteRegisterValue(operand.getRegister()));
            		if(!reg_occurrences.count(regId)) {
            			// reg_occurrences.insert(std::pair<triton::arch::register_e,int>()
            			reg_occurrences[regId] = 1;
            		} else {
            			reg_occurrences[regId]++;
            		}
            	}
            }*/

            // fmt::print("Address: {:>14x}  |  ", inst.getAddress());
            // fmt::print("Instruction: {:<48}  |  Type: {:>8}  |  rax: {:>20}  |  rcx: {:>20}  |  ", inst.getDisassembly(), inst.getType(), (uint64_t)api.getConcreteRegisterValue(api.getRegister(static_cast<triton::arch::register_e>(1))), (uint64_t)api.getConcreteRegisterValue(api.getRegister(static_cast<triton::arch::register_e>(3))));
            /*for(triton::arch::OperandWrapper operand: inst.operands) {
            	if (operand.getType() == triton::arch::OP_MEM)
            		fmt::print("  Memory:    {:>16x}  |", operand.getMemory().getDisplacement().getValue());
            	else if (operand.getType() == triton::arch::OP_REG)
            		fmt::print("  Register:  {:>16}  |", operand.getRegister().getId());
            	else if (operand.getType() == triton::arch::OP_IMM)
            		fmt::print("  Immediate: {:>16}  |", operand.getImmediate().getValue());
            	else
            		fmt::print("  INVALID    {:>16}  |", "");
            }*/
            // fmt::print("\n");

            if (this->print_all_instructions)
            {
                fmt::print("[D] {0:07} {3:#x} {1} {2}\n", trace_pos, inst.getDisassembly(), inst.isTainted() ? "tainted" : "", inst.getAddress());
            }

            // check if we need to restore the register context
            // if (context_pos < contexts.size() && trace_pos == contexts[context_pos]->instr_num)
            if (contexts.current && trace_pos == contexts.current->instr_num)
            {
                auto ctx = contexts.current;
                if (this->debug)
                    fmt::print("[D] saved context {}\n", context_pos);
                // sanity check
                if (pc != ctx->xip)
                {
                    fmt::print("[-] saved context wrong pc: {:#x} != {:#x}\n", pc, ctx->xip);
                    // auto rsp_mem = api.getConcreteMemoryAreaValue(
                    //     (uint64_t)api.getConcreteRegisterValue(api.registers.x86_rsp), 8, false);
                    // fmt::print("[D] rsp[0]: {:#x}\n", *(size_t*)rsp_mem.data());
                    return CollectedData { true, false, ++trace_pos, diverged, skipped, xsavec };
                }
                set_context(ctx);
                contexts.next();
                ++context_pos;
            }

            // check if we need to restore memory
            while (memories.current && trace_pos == memories.current->instr_num)
            {
                saved_memory_t &mem = *memories.current;
                if (this->debug)
                    fmt::print("[D] saved memory {}: {:#x} - {:#x}\n",
                               memory_pos, mem.start_addr, mem.start_addr + mem.size);
                api.setConcreteMemoryAreaValue(mem.start_addr, (uint8_t *)mem.data, mem.size);
                memories.next();
                ++memory_pos;
            }

            // rip is not restored through context
            if (xsavec_instruction)
                api.setConcreteRegisterValue(api.registers.x86_rip, api.getConcreteRegisterValue(api.registers.x86_rip) + 5);
            else if (skip_instruction)
                api.setConcreteRegisterValue(api.registers.x86_rip, inst.getNextAddress());

            last_pc = pc;
            pc = static_cast<uint64_t>(api.getConcreteRegisterValue(api.registers.x86_rip));

            // there are instructions with e.g. rep prefix that are executed multiple times
            // in triton but are counted as single instruction in the trace
        } while (last_pc == pc);

        // check if we are done
        ++trace_pos;
        trace.next();

        if(trace_pos % 100000 == 0)
        {
          fmt::print("Currently emulated trace length: {:>12}\n", trace_pos);
        }

        if (!guard_bypassed && !trace.current)
        {
            // for(std::map<triton::arch::register_e, int>::iterator it = reg_occurrences.begin(); it != reg_occurrences.end(); it++) {
            // 	fmt::print("Register {:3}: {}\n", it->first, it->second);
            // }
            fmt::print("[+] reached end of trace. emulation done\n");
            return CollectedData { false, false, trace_pos, diverged, skipped, xsavec };
        }

        if (stop_requested)
        {
            fmt::print("[*] Stop requested. Stopping emulation\n");
            return CollectedData { true, false, trace_pos, diverged, skipped, xsavec };
        }

	      // only get back to trace if no guard has been bypassed yet
        if (!guard_bypassed && pc != *trace.current)
        {
            if (false) {
                fmt::print("[-] execution diverged at {:#x}, trace {:#x}\n",
                        pc, *trace.current);
                print_context();
                fmt::print("[*] next trace instructions:\n");
                for (int i = 0; i < 10; ++i)
                {
                    if (!trace.next())
                        break;
                    fmt::print("{:#018x}\n", *trace.current);
                }
                return CollectedData { true, false, trace_pos, diverged, skipped, xsavec };
            }
            else {
                // fmt::print("[*] execution diverged at {:#x}, trace {:#x}\n", pc, *trace.current);
                // fmt::print("[*] execution diverged at: {}\n", inst.getDisassembly());
                ++diverged;
                pc = *trace.current;
                api.setConcreteRegisterValue(api.registers.x86_rip, pc);
            }
        }
    }

    return CollectedData { true, false, trace_pos, diverged, skipped, xsavec };
}
