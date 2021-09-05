#ifndef LLVM_SCVIRT_HPP
#define LLVM_SCVIRT_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <vector>

#include "llvm/Transforms/ScVirt/ISATranslator.hpp"
#include "llvm/Transforms/ScVirt/VirtualizationBuilder.hpp"

struct FunctionInfo {
  llvm::Function *Func;
  CodeMap CodeMapping;
  DataMap DataMapping;
};

struct ConnectivityMap {
  llvm::Function *Func;
  std::vector<llvm::StringRef> ToCheck;
};

/// Self-checksumming with virtualization obfuscation pass.
/// Runs over all functions that are marked with the "scvirt" attribute
/// or are passed via command line option.
class ScVirt : public llvm::ModulePass {
  /// Prefix for the code arrays.
  std::string CodeArrPrefix = "scvirt_";

  /// The module's data layout.
  const llvm::DataLayout *Layout { nullptr };

  /// Used to call abort() on invalid code instruction.
  llvm::Function *AbortFunc { nullptr };

  /// Used to call malloc().
  llvm::Function *MallocFunc { nullptr };

  /// Used to call free()
  llvm::Function *FreeFunc { nullptr };

  /// Used to calculate the checksum.
  llvm::Function *XorFunc { nullptr };

  /// Creates the function to xor the elements of an array.
  /// \param M The current module.
  /// \return The newly created function.
  llvm::Function *createXORFunction(llvm::Module &M);

  /// Collects non-empty functions of interest either by
  /// checking the "scvirt" attribute or the passed list of function names.
  /// \param M The current module.
  /// \return Vector of functions to be transformed by this pass.
  std::vector<llvm::Function *> getFunctions(llvm::Module &M);

  /// Debug method to print index <-> value mapping of the data array.
  /// \param Data Mapping from index to value.
  void printDataArrayMapping(const std::map<uint64_t, llvm::Value *> &Data);
  void printCodeMapping(const std::vector<CodeMap::InstToOpcode> &CodeMap);

  /// Inserts a new global variable holding a function's instruction array.
  /// \param Insts The instruction mapping.
  /// \param Name The name of the func.
  /// \param M The current module.
  /// \return The newly created global variable.
  llvm::GlobalVariable *createCodeArray(const std::vector<CodeMap::InstToOpcode> &Insts,
                                        const llvm::StringRef Name,
                                        llvm::Module &M);

  /// Inserts a new global variable holding a function's data array.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Data Mapping from index to data.
  /// \param M The current module.
  /// \return The newly created global variable.
  llvm::AllocaInst *createEmptyDataArray(llvm::IRBuilder<> &Builder,
                                         const std::map<uint64_t, llvm::Value *> &Data,
                                         const uint64_t Size,
                                         llvm::Module &M);

  /// Clones \param TargetFunc with all attributes.
  /// \param TargetFunc The function to clone.
  /// \param M The current module.
  /// \return The newly created function.
  llvm::Function *createNewFunction(llvm::Function &TargetFunc, llvm::Module &M);

  /// Calculates the checksum for the given instructions.
  /// \tparam TFunc Type of the operation function.
  /// \param Insts The list of instructions.
  /// \param Operator Function that takes two values and calculates the result (in our case: xor).
  /// \return The calculated checksum.
  template<typename TFunc>
  uint16_t calculateChecksum(const std::vector<CodeMap::InstToOpcode> &Insts, TFunc Operator);

  uint16_t calculateChecksumXor(const std::vector<CodeMap::InstToOpcode> &Insts);

  /// Virtualizes a single instruction.
  /// \param VirtBuilder Handles virtualization.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Elem The current instruction and new opcode.
  /// \param NewFunc The new function to insert into.
  /// \param BBSwitchHead The block to jump back to.
  /// \param Switch The virtualization switch statement.
  /// \param CaseValue The case value for this instruction.
  /// \param IdxAlloca The instruction pointer allocation.
  void virtualizeInstruction(VirtualizationBuilder &VirtBuilder,
                             llvm::IRBuilder<> &Builder,
                             const CodeMap::InstToOpcode &Elem,
                             llvm::Function *NewFunc,
                             llvm::BasicBlock *BBSwitchHead,
                             llvm::SwitchInst *Switch,
                             llvm::ConstantInt *CaseValue,
                             llvm::Value *IdxAlloca);

  /// Creates the emulator function for the generated ISA.
  /// \param Insts The instructions to emulate.
  /// \param Data A mapping of index to Value*. Corresponds to the generated data array.
  /// \param TargetFunc The original function.
  /// \param CodeArray Array of opcodes.
  /// \param M The current module.
  /// \return A new function that emulates \param TargetFunc.
  llvm::Function *createEmulator(const std::vector<CodeMap::InstToOpcode> &Insts,
                                 const std::map<uint64_t, llvm::Value *> &Data,
                                 const std::vector<llvm::StringRef> &ToCheck,
                                 const std::map<uint16_t, llvm::StringRef> &CSMap,
                                 llvm::Function &TargetFunc,
                                 llvm::GlobalVariable *CodeArray,
                                 llvm::Module &M);

  /// Replaces the \param Func by a virtualized function that emulates its behaviour.
  /// \param Func The function to emulate.
  /// \param M The current module.
  /// \return A virtualized function emulating \p Func.
  llvm::Function *virtualizeFunction(FunctionInfo &FInfo, const std::vector<llvm::StringRef> &ToCheck, const std::map<uint16_t, llvm::StringRef> &CSMap, llvm::Module &M);

  /// Move all AllocaInst to the entry point of the function \param Func.
  /// \param Func The function to order AllocaInsts in.
  void moveAllocaUp(llvm::Function &Func);

  /// NOTE: Will be removed later!
  /// Convenience function implemented in my C test file. Prints the content of the data array.
  /// \param Builder IRBuilder to insert instructions.
  /// \param DataArray The array to print.
  /// \param Size The number of elements.
  void printDataArray(llvm::IRBuilder<> &Builder, llvm::AllocaInst *DataArray, uint32_t Size);

  std::vector<ConnectivityMap> generateDAG(std::vector<llvm::Function *> &&Funcs);
public:
  /// This pass' ID.
  static char ID;

  /// Default constructor.
  ScVirt() : ModulePass(ID) {}

  /// Initializes the module. Used to insert the function abort().
  /// \param M The current module.
  /// \return True if the module was changed. Otherwise false.
  bool doInitialization(llvm::Module &M) override;

  /// Creates virtualization obfuscation emulators for each marked function.
  /// \param M The current module.
  /// \return True if the module was changed. Otherwise false.
  bool runOnModule(llvm::Module &M) override;
};

#endif