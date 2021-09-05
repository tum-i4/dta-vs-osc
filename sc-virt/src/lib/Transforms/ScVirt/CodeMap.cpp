#include "llvm/Transforms/ScVirt/CodeMap.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "CodeMap"

using namespace llvm;

void CodeMap::insertInst(Instruction *OldInst, const std::vector<uint16_t> &Opcode) {
  assert(!HasWiredTargets);
  assert(OldInst);
  Elements.emplace_back(InstToOpcode { OldInst, Opcode });
}

void CodeMap::insertSafeGuard(const size_t Index, const std::vector<uint16_t> &Opcode) {
  assert(!HasWiredTargets);
  assert(Index < Elements.size());
  Elements.emplace(Elements.begin() + Index, InstToOpcode { nullptr, Opcode });
}

const std::vector<CodeMap::InstToOpcode> &CodeMap::getElements() const {
  assert(HasWiredTargets);
  return Elements;
}

std::vector<CodeMap::InstToOpcode> &CodeMap::getElements() {
  assert(HasWiredTargets);
  return Elements;
}

const CodeMap::InstToOpcode &CodeMap::at(const uint64_t Idx) const {
  return Elements.at(Idx);
}

bool CodeMap::hasFunctionArg(llvm::Instruction *Inst, llvm::Function *Func) {
  assert(Inst);
  assert(Func);
  const auto NumOperands = Inst->getNumOperands();
  for(auto I = 0u; I < NumOperands; ++I) {
    const auto *CurrentOperand = Inst->getOperand(I);

    for(const auto &Arg : Func->getArgumentList()) {
      if(&Arg == CurrentOperand) {
        DEBUG(dbgs() << "Found instruction with function argument!\n" << *Inst << "\n");
        return true;
      }
    }
  }

  return false;
}

template<typename T>
inline std::vector<CodeMap::InstToOpcode *> CodeMap::collect() {
  std::vector<InstToOpcode *> Worklist;
  for(auto &Elem : Elements) {
    if(Elem.OldInst && isa<T>(Elem.OldInst)) {
      Worklist.emplace_back(&Elem);
    }
  }

  return Worklist;
}

uint16_t CodeMap::indexOfInst(const Instruction *Inst) const {
  assert(Inst);
  uint16_t Idx = 0u;

  for(const auto &Elem : Elements) {
    if(Elem.OldInst == Inst) {
      return Idx;
    }

    Idx += Elem.Opcode.size();
  }

  return Idx;
}

void CodeMap::wireIndirectBranches() {
  auto Worklist = collect<IndirectBrInst>();

  for(auto *Elem : Worklist) {
    auto *IBr = cast<IndirectBrInst>(Elem->OldInst);

    const auto NumDests = IBr->getNumDestinations();
    for(auto Idx = 0u; Idx < NumDests; ++Idx) {
      auto *Succ = &*IBr->getDestination(Idx)->getFirstInsertionPt();
      Elem->Opcode.at(2u + Idx) = indexOfInst(Succ);
    }
  }
}

void CodeMap::wireBranches() {
  auto Worklist = collect<BranchInst>();

  for(auto *Elem : Worklist) {
    auto *BR = cast<BranchInst>(Elem->OldInst);

    if(BR->isUnconditional()) {
      auto *Succ = &*BR->getSuccessor(0)->getFirstInsertionPt();
      Elem->Opcode.at(1) = indexOfInst(Succ);
    } else {
      auto *True = &*BR->getSuccessor(0)->getFirstInsertionPt();
      Elem->Opcode.at(2) = indexOfInst(True);

      auto *False = &*BR->getSuccessor(1)->getFirstInsertionPt();
      Elem->Opcode.at(3) = indexOfInst(False);
    }
  }
}

void CodeMap::wireCleanupRet() {
  auto Worklist = collect<CleanupReturnInst>();

  for(auto *Elem : Worklist) {
    auto *CleanupRet = cast<CleanupReturnInst>(Elem->OldInst);
    auto *Succ = &*CleanupRet->getUnwindDest()->getFirstInsertionPt();
    Elem->Opcode.at(2) = indexOfInst(Succ);
  }
}

void CodeMap::wireCatchRet() {
  auto Worklist = collect<CatchReturnInst>();

  for(auto *Elem : Worklist) {
    auto *CatchRet = cast<CatchReturnInst>(Elem->OldInst);
    auto *Succ = &*CatchRet->getSuccessor()->getFirstInsertionPt();
    Elem->Opcode.at(2) = indexOfInst(Succ);
  }
}

void CodeMap::wireCatchSwitch() {
  auto Worklist = collect<CatchSwitchInst>();

  for(auto *Elem : Worklist) {
    auto *CS = cast<CatchSwitchInst>(Elem->OldInst);

    auto *UnwindSucc = &*CS->getUnwindDest()->getFirstInsertionPt();
    const auto UnwindIdx = indexOfInst(UnwindSucc);
    Elem->Opcode.at(3) = UnwindIdx;

    const auto StartIdx = 4;
    for(auto Idx = 0u; Idx < CS->getNumSuccessors(); ++Idx) {
      auto *Succ = &*CS->getSuccessor(Idx)->getFirstInsertionPt();
      const auto SuccIdx = indexOfInst(Succ);
      Elem->Opcode.at(StartIdx + Idx) = SuccIdx;
    }
  }
}

void CodeMap::wireSwitch() {
  auto Worklist = collect<SwitchInst>();

  for(auto *Elem : Worklist) {
    auto *SW = cast<SwitchInst>(Elem->OldInst);
    auto *DefaultSucc = &*SW->getDefaultDest()->getFirstInsertionPt();
    Elem->Opcode.at(2) = indexOfInst(DefaultSucc);

    auto CurrentCase = 0u;
    for(auto Iter = SW->case_begin(), End = SW->case_end(); Iter != End; ++Iter) {
      auto *Targ = &*Iter.getCaseSuccessor()->getFirstInsertionPt();
      Elem->Opcode.at(3u + CurrentCase) = indexOfInst(Targ);
      ++CurrentCase;
    }
  }
}

void CodeMap::wireInvoke() {
  auto Worklist = collect<InvokeInst>();

  for(auto *Elem : Worklist) {
    auto *Inv = cast<InvokeInst>(Elem->OldInst);

    auto StartIdx = 1 + Inv->getNumArgOperands();
    if(!Inv->getCalledFunction()) {
      ++StartIdx;
    }

    assert(Elem->Opcode.at(StartIdx) == 0x0);

    auto *Succ = &*Inv->getNormalDest()->getFirstInsertionPt();
    Elem->Opcode.at(StartIdx) = indexOfInst(Succ);
    ++StartIdx;

    assert(Elem->Opcode.at(StartIdx) == 0x0);

    auto *Lpad = Inv->getLandingPadInst()->getNextNode();
    Elem->Opcode.at(StartIdx) = indexOfInst(Lpad);
  }
}

void CodeMap::wireTargets() {
  wireIndirectBranches();
  wireBranches();
  wireCleanupRet();
  wireCatchRet();
  wireCatchSwitch();
  wireSwitch();
  wireInvoke();

  HasWiredTargets = true;
}