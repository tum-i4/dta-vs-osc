#ifndef LLVM_CODEMAP_HPP
#define LLVM_CODEMAP_HPP

#include "llvm/IR/Instructions.h"

class CodeMap {
public:
  struct InstToOpcode {
    llvm::Instruction *OldInst;
    std::vector<uint16_t> Opcode;
  };

  void insertInst(llvm::Instruction *OldInst, const std::vector<uint16_t> &Opcode);
  void insertSafeGuard(const size_t Index, const std::vector<uint16_t> &Opcode);

  uint64_t size() const {
    return Elements.size();
  }

  void wireTargets();

  const std::vector<CodeMap::InstToOpcode> &getElements() const;
  std::vector<CodeMap::InstToOpcode> &getElements();
  const CodeMap::InstToOpcode &at(const uint64_t Idx) const;

  static bool hasFunctionArg(llvm::Instruction *Inst, llvm::Function *Func);

private:
  std::vector<CodeMap::InstToOpcode> Elements;

  bool HasWiredTargets { false };

  template<typename T>
  std::vector<CodeMap::InstToOpcode *> collect();

  uint16_t indexOfInst(const llvm::Instruction *Inst) const;
  void wireIndirectBranches();
  void wireBranches();
  void wireCleanupRet();
  void wireCatchRet();
  void wireCatchSwitch();
  void wireSwitch();
  void wireInvoke();

};

#endif //LLVM_CODEMAP_HPP
