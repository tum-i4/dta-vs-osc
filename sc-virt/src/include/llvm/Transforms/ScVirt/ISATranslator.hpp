#ifndef LLVM_TRANSFORMS_ISATRANSLATOR_HPP
#define LLVM_TRANSFORMS_ISATRANSLATOR_HPP

#include "llvm/ADT/Optional.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include <map>
#include <vector>

#include "llvm/Transforms/ScVirt/CodeMap.hpp"
#include "llvm/Transforms/ScVirt/DataMap.hpp"
#include "llvm/Transforms/ScVirt/InstrOperandTypes.hpp"

/// This class translates the given function into an uint64_t list of new opcodes.
class ISATranslator {
  /// The function to translate.
  llvm::Function *Func;

  const std::vector<uint16_t> &Checksums;
  const std::vector<llvm::StringRef> &ToCheck;

  /// Instruction x Operand type combinations we already handled.
  std::vector<InstrOperandTypes> HandledTypes;

  /// List of generated function <-> opcode mappings.
  CodeMap CodeMapping;

  /// List of generated data <-> index mappings.
  DataMap DataMapping;

  std::map<uint16_t, llvm::StringRef> ChecksumMap;

  /// Inserts safe guards into the code map.
  void generateSafeGuards();

  /// Translates \var Func and fills \var CodeMapping and \var DataArrayMapping.
  void translateFunc();

  /// Checks if we should ignore the \param Inst.
  /// \param Inst The instruction to check.
  /// \return True, if the \param Inst should be ignored. Otherwise false.
  bool shouldIgnore(const llvm::Instruction *Inst);

  /// Translates the \param Inst into a list of opcodes.
  /// \param Inst The instruction to translate.
  /// \return List of opcodes.
  std::vector<uint16_t> translateInst(llvm::Instruction *Inst);

public:
  /// Constructor.
  /// \param Func The function to translate.
  explicit ISATranslator(llvm::Function *Func, const std::vector<llvm::StringRef> &ToCheck, const std::vector<uint16_t> &Checksums);

  /// Gets the generated opcodes.
  /// \return List of generated opcodes and original instruction.
  CodeMap getCodeMap() const;

  /// Gets the generated data map.
  /// \return Mapping of index to original value.
  DataMap getDataMap() const;

  std::map<uint16_t, llvm::StringRef> getChecksumMap() const;
};

#endif