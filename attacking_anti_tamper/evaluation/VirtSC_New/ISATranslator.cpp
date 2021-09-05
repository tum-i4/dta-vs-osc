#include "llvm/Transforms/ScVirt/ISATranslator.hpp"

#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/Transforms/ScVirt/CallConstantParameterIndices.hpp"
#include "llvm/Transforms/ScVirt/OpcodeGenerator.hpp"

#define DEBUG_TYPE "Translator"

using namespace llvm;

ISATranslator::ISATranslator(Function *Func,
                             const std::vector<StringRef> &ToCheck,
                             const std::vector<uint16_t> &Checksums)
  : Func(Func), ToCheck(ToCheck), Checksums(Checksums) {
  translateFunc();
}

std::vector<uint16_t> ISATranslator::translateInst(Instruction *Inst) {
  auto &OpGen = OpcodeGenerator::get();

  std::vector<uint16_t> Opcode;
  const auto &Layout = Inst->getModule()->getDataLayout();

  switch (Inst->getOpcode()) {
  case Instruction::Alloca: {
    Opcode.emplace_back(OpGen.generate());

    const auto DstIdx = DataMapping.getOrInsert(Inst, Layout);
    Opcode.emplace_back(DstIdx);
    break;
  }
  case Instruction::Store: {
    auto *SI = cast<StoreInst>(Inst);

    auto *Src = SI->getValueOperand();
    auto *Dst = SI->getPointerOperand();

    Opcode.emplace_back(OpGen.generate());

    if (!isa<Function>(Src)) {
      // handle operands
      const auto SrcIdx = DataMapping.getOrInsert(Src, Layout);
      Opcode.emplace_back(SrcIdx);
    }

    const auto DestIdx = DataMapping.getOrInsert(Dst, Layout);
    Opcode.emplace_back(DestIdx);

    break;
  }
  case Instruction::Load: {
    auto *LI = cast<LoadInst>(Inst);

    auto *Src = LI->getPointerOperand();

    Opcode.emplace_back(OpGen.generate());

    // handle operands
    const auto SrcIdx = DataMapping.getOrInsert(Src, Layout);
    Opcode.emplace_back(SrcIdx);

    const auto LIIdx = DataMapping.getOrInsert(LI, Layout);
    Opcode.emplace_back(LIIdx);

    break;
  }
  case Instruction::Unreachable: {
    auto *UR = cast<UnreachableInst>(Inst);
    DEBUG(dbgs() << "Unreachable:\t" << *UR << "\n");

    Opcode.emplace_back(OpGen.generate());

    break;
  }
  case Instruction::AddrSpaceCast:
  case Instruction::BitCast:
  case Instruction::IntToPtr:
  case Instruction::PtrToInt:
  case Instruction::SIToFP:
  case Instruction::UIToFP:
  case Instruction::FPToSI:
  case Instruction::FPToUI:
  case Instruction::Trunc:
  case Instruction::FPTrunc:
  case Instruction::FPExt:
  case Instruction::ZExt:
  case Instruction::SExt: {
    auto *CI = cast<CastInst>(Inst);
    DEBUG(dbgs() << "CastInst:\t" << *CI << "\n");

    Opcode.emplace_back(OpGen.generate());
    auto *Val = CI->getOperand(0);

    const auto OpIdx = DataMapping.getOrInsert(Val, Layout);
    Opcode.emplace_back(OpIdx);

    const auto ResultIdx = DataMapping.getOrInsert(CI, Layout);
    Opcode.emplace_back(ResultIdx);

    break;
  }
  case Instruction::SRem:
  case Instruction::URem:
  case Instruction::FRem:
  case Instruction::And:
  case Instruction::Or:
  case Instruction::Xor:
  case Instruction::LShr:
  case Instruction::AShr:
  case Instruction::Shl:
  case Instruction::SDiv:
  case Instruction::UDiv:
  case Instruction::FDiv:
  case Instruction::Mul:
  case Instruction::FMul:
  case Instruction::Sub:
  case Instruction::FSub:
  case Instruction::Add:
  case Instruction::FAdd: {
    auto *OI = cast<BinaryOperator>(Inst);

    DEBUG(dbgs() << "BinaryOp:\t" << *OI << "\n");

    Opcode.emplace_back(OpGen.generate());

    auto *Lhs = OI->getOperand(0);
    auto *Rhs = OI->getOperand(1);

    // handle operands
    const auto LhsIdx = DataMapping.getOrInsert(Lhs, Layout);
    Opcode.emplace_back(LhsIdx);

    const auto RhsIdx = DataMapping.getOrInsert(Rhs, Layout);
    Opcode.emplace_back(RhsIdx);

    const auto ResultIdx = DataMapping.getOrInsert(OI, Layout);
    Opcode.emplace_back(ResultIdx);

    break;
  }
  case Instruction::Br: {
    auto *BR = cast<BranchInst>(Inst);
    DEBUG(dbgs() << "Branch:\t" << *BR << "\n");

    Opcode.emplace_back(OpGen.generate());

    // add dummy values
    if (BR->isUnconditional()) {
      Opcode.emplace_back(0x0);
    } else {
      const auto CondIdx = DataMapping.getOrInsert(BR->getCondition(), Layout);
      Opcode.emplace_back(CondIdx);
      Opcode.emplace_back(0x0);
      Opcode.emplace_back(0x0);
    }

    break;
  }
  case Instruction::IndirectBr: {
    auto *IBr = cast<IndirectBrInst>(Inst);
    DEBUG(dbgs() << "IndirectBranch:\t" << *IBr << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto AddrIdx = DataMapping.getOrInsert(IBr->getAddress(), Layout);
    Opcode.emplace_back(AddrIdx);

    for (auto Idx = 0u; Idx < IBr->getNumDestinations(); ++Idx) {
      Opcode.emplace_back(0x0);
    }

    break;
  }
  case Instruction::FCmp:
  case Instruction::ICmp: {
    auto *Cmp = cast<CmpInst>(Inst);
    DEBUG(dbgs() << "ICmp:\t" << *Cmp << "\n");

    Opcode.emplace_back(OpGen.generate());

    auto *Lhs = Cmp->getOperand(0);
    DEBUG(dbgs() << "LHS:\t" << *Lhs << "\n");
    auto *Rhs = Cmp->getOperand(1);
    DEBUG(dbgs() << "RHS:\t" << *Rhs << "\n");

    const auto LhsIdx = DataMapping.getOrInsert(Lhs, Layout);
    Opcode.emplace_back(LhsIdx);

    const auto RhsIdx = DataMapping.getOrInsert(Rhs, Layout);
    Opcode.emplace_back(RhsIdx);

    const auto ResIdx = DataMapping.getOrInsert(Cmp, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::Switch: {
    auto *SW = cast<SwitchInst>(Inst);
    DEBUG(dbgs() << "Switch:\t" << *SW << "\n");

    Opcode.emplace_back(OpGen.generate());

    // condition, then targets
    const auto CondIdx = DataMapping.getOrInsert(SW->getCondition(), Layout);
    Opcode.emplace_back(CondIdx);

    // default target dummy
    Opcode.emplace_back(0);

    // we don't need to save the case values because those have to be ConstantInts
    // therefore we will re-use the ones from the old instruction
    for (auto I = 0u; I < SW->getNumCases(); ++I) {
      // case target
      Opcode.emplace_back(0x0);
    }

    break;
  }
  case Instruction::Select: {
    auto *Sel = cast<SelectInst>(Inst);
    Opcode.emplace_back(OpGen.generate());

    DEBUG(dbgs() << "Select:\t" << *Sel << "\n");
    const auto CondIdx = DataMapping.getOrInsert(Sel->getCondition(), Layout);
    Opcode.emplace_back(CondIdx);
    const auto TrueIdx = DataMapping.getOrInsert(Sel->getTrueValue(), Layout);
    Opcode.emplace_back(TrueIdx);
    const auto FalseIdx = DataMapping.getOrInsert(Sel->getFalseValue(), Layout);
    Opcode.emplace_back(FalseIdx);

    const auto ResIdx = DataMapping.getOrInsert(Sel, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::GetElementPtr: {
    auto *GEP = cast<GetElementPtrInst>(Inst);
    Opcode.emplace_back(OpGen.generate());

    DEBUG(dbgs() << "GEP:\t" << *GEP << "\n");

    // add ptr operand
    const auto PtrIdx = DataMapping.getOrInsert(GEP->getPointerOperand(), Layout);
    Opcode.emplace_back(PtrIdx);

    // don't add constant because they have to be constant for some types (e.g. structs)
    for (auto I = 0u; I < GEP->getNumIndices(); ++I) {
      auto *Op = GEP->getOperand(1 + I);

      if (isa<Constant>(Op)) {
        continue;
      }

      const auto OpIdx = DataMapping.getOrInsert(Op, Layout);
      Opcode.emplace_back(OpIdx);
    }

    const auto ResultIdx = DataMapping.getOrInsert(GEP, Layout);
    Opcode.emplace_back(ResultIdx);

    break;
  }
  case Instruction::Call: {
    auto *Call = cast<CallInst>(Inst);

    Opcode.emplace_back(OpGen.generate());

    DEBUG(dbgs() << "Call:\t" << *Call << "\n");

    Value *CallTarget = Call->getCalledFunction();

    if (!CallTarget) {
      const auto FuncIdx = DataMapping.getOrInsert(Call->getCalledValue(), Layout);
      Opcode.emplace_back(FuncIdx);

      CallTarget = Call->getCalledValue();
    }

    for (auto OpIdx = 0u; OpIdx < Call->getNumArgOperands(); ++OpIdx) {
      auto *Arg = Call->getArgOperand(OpIdx);
      DEBUG(dbgs() << "Argument:\t\t" << *Arg << "\n");

      // don't add Metadata into the data array
      if(Arg->getType()->isMetadataTy()) {
        continue;
      }

      if (auto *CalledFunc = dyn_cast<Function>(CallTarget)) {
        // some intrinsics (e.g. llvm.eh.typeid) check that their value is a GlobalVariable
        // thus, we cannot load them into the data array
        if (CalledFunc->isIntrinsic()) {
          if (isa<GlobalVariable>(Arg->stripPointerCasts())) {
            continue;
          }

          // FIX: DataAlignment and Volatile parameter have to be constant
          //      ==> cannot load parameters from data array
          if (CalledFunc->getName().startswith("llvm.memcpy")
            && (OpIdx == ConstantIndices::MEMCPY_ALIGNMENT_INDEX
              || OpIdx == ConstantIndices::MEMCPY_VOLATILE_INDEX)) {
            continue;
          } else if(CalledFunc->getName().startswith("llvm.memset")
            && (OpIdx == ConstantIndices::MEMSET_ALIGNMENT_INDEX
            || OpIdx == ConstantIndices::MEMSET_VOLATILE_INDEX)) {
            continue;
          } else if(CalledFunc->getName().startswith("llvm.memmove")
            && (OpIdx == ConstantIndices::MEMMOVE_ALIGNMENT_INDEX
                || OpIdx == ConstantIndices::MEMMOVE_VOLATILE_INDEX)) {
            continue;
          }
        }
      }

      const auto ArgIdx = DataMapping.getOrInsert(Arg, Layout);
      Opcode.emplace_back(ArgIdx);
    }

    // add the return index last
    auto *RetTy = Call->getFunctionType()->getReturnType();
    if (!RetTy->isVoidTy()) {
      const auto RetValIdx = DataMapping.getOrInsert(Call, Layout);
      Opcode.emplace_back(RetValIdx);
    }

    break;
  }
  case Instruction::Invoke: {
    auto *Inv = cast<InvokeInst>(Inst);

    Opcode.emplace_back(OpGen.generate());

    DEBUG(dbgs() << "Invoke:\t" << *Inv << "\n");

    // virtual function calls load the called value beforehand
    // thus we don't have a llvm::Function object for that
    if (!Inv->getCalledFunction()) {
      DEBUG(dbgs() << "Virtual call!\n");

      const auto FuncIdx = DataMapping.getOrInsert(Inv->getCalledValue(), Layout);
      Opcode.emplace_back(FuncIdx);
    }

    for (auto OpIdx = 0u; OpIdx < Inv->getNumArgOperands(); ++OpIdx) {
      auto *Arg = Inv->getArgOperand(OpIdx);
      DEBUG(dbgs() << "Argument:\t\t" << *Arg << "\n");

      const auto ArgIdx = DataMapping.getOrInsert(Arg, Layout);
      Opcode.emplace_back(ArgIdx);
    }

    // cont label
    Opcode.emplace_back(0x0);

    // lpad successor label
    Opcode.emplace_back(0x0);

    const auto LpadResIdx = DataMapping.getOrInsert(Inv->getLandingPadInst(), Layout);
    Opcode.emplace_back(LpadResIdx);

    // add the return index last
    auto *RetTy = Inv->getFunctionType()->getReturnType();
    if (!RetTy->isVoidTy()) {
      const auto RetValIdx = DataMapping.getOrInsert(Inv, Layout);
      Opcode.emplace_back(RetValIdx);
    }

    break;
  }
  case Instruction::ExtractElement: {
    auto *EE = cast<ExtractElementInst>(Inst);
    DEBUG(dbgs() << "ExtractElement:\t" << *EE << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto VecIdx = DataMapping.getOrInsert(EE->getVectorOperand(), Layout);
    Opcode.emplace_back(VecIdx);

    const auto IdxOp = DataMapping.getOrInsert(EE->getIndexOperand(), Layout);
    Opcode.emplace_back(IdxOp);

    const auto ResIdx = DataMapping.getOrInsert(EE, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::InsertElement: {
    auto *IE = cast<InsertElementInst>(Inst);
    DEBUG(dbgs() << "InsertElement:\t" << *IE << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto VecIdx = DataMapping.getOrInsert(IE->getOperand(0), Layout);
    Opcode.emplace_back(VecIdx);

    const auto ValIdx = DataMapping.getOrInsert(IE->getOperand(1), Layout);
    Opcode.emplace_back(ValIdx);

    const auto IdxOp = DataMapping.getOrInsert(IE->getOperand(2), Layout);
    Opcode.emplace_back(IdxOp);

    const auto ResIdx = DataMapping.getOrInsert(IE, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::ExtractValue: {
    auto *EV = cast<ExtractValueInst>(Inst);
    DEBUG(dbgs() << "ExtractValue:\t" << *EV << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto AggOpIdx = DataMapping.getOrInsert(EV->getAggregateOperand(), Layout);
    Opcode.emplace_back(AggOpIdx);

    const auto ResIdx = DataMapping.getOrInsert(EV, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::InsertValue: {
    auto *IV = cast<InsertValueInst>(Inst);
    DEBUG(dbgs() << "InsertValue:\t" << *IV << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto AggIdx = DataMapping.getOrInsert(IV->getAggregateOperand(), Layout);
    Opcode.emplace_back(AggIdx);

    const auto ValIdx = DataMapping.getOrInsert(IV->getInsertedValueOperand(), Layout);
    Opcode.emplace_back(ValIdx);

    const auto ResIdx = DataMapping.getOrInsert(IV, Layout);
    Opcode.emplace_back(ResIdx);
    break;
  }
  case Instruction::ShuffleVector: {
    auto *SV = cast<ShuffleVectorInst>(Inst);
    DEBUG(dbgs() << "ShuffleVector:\t" << *SV << "\n");
    Opcode.emplace_back(OpGen.generate());

    DEBUG(dbgs() << "Operand0:\t" << *(SV->getOperand(0)) << "\n");
    DEBUG(dbgs() << "Operand1:\t" << *(SV->getOperand(1)) << "\n");
    DEBUG(dbgs() << "Operand2:\t" << *(SV->getOperand(2)) << "\n");

    const auto Vec1Idx = DataMapping.getOrInsert(SV->getOperand(0), Layout);
    Opcode.emplace_back(Vec1Idx);

    const auto Vec2Idx = DataMapping.getOrInsert(SV->getOperand(1), Layout);
    Opcode.emplace_back(Vec2Idx);

    // shuffle mask has to be a constant

    const auto ResIdx = DataMapping.getOrInsert(SV, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::Resume: {
    auto *RS = cast<ResumeInst>(Inst);
    Opcode.emplace_back(OpGen.generate());

    const auto ValIdx = DataMapping.getOrInsert(RS->getValue(), Layout);
    Opcode.emplace_back(ValIdx);

    DEBUG(dbgs() << "Resume:\t" << *RS << "\n");
    break;
  }
  case Instruction::Ret: {
    auto *RetInst = cast<ReturnInst>(Inst);
    if (auto *RetVal = RetInst->getReturnValue()) {
      DEBUG(dbgs() << "Got non-void return!\n");
      Opcode.emplace_back(OpGen.generate());

      const auto RetIdx = DataMapping.getOrInsert(RetVal, Layout);
      Opcode.emplace_back(RetIdx);
    } else {
      DEBUG(dbgs() << "Got void return!\n");
      Opcode.emplace_back(OpGen.generate());
    }

    break;
  }
  case Instruction::CatchSwitch: {
    auto *CS = cast<CatchSwitchInst>(Inst);
    DEBUG(dbgs() << "CatchSwitch: " << *CS << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto ParentIdx = DataMapping.getOrInsert(CS->getParentPad(), Layout);
    Opcode.emplace_back(ParentIdx);

    const auto ResIdx = DataMapping.getOrInsert(CS, Layout);
    Opcode.emplace_back(ResIdx);

    if (CS->hasUnwindDest()) {
      Opcode.emplace_back(0x0);
    }

    for (auto Idx = 0u; Idx < CS->getNumSuccessors(); ++Idx) {
      Opcode.emplace_back(0x0);
    }

    break;
  }
  case Instruction::CleanupRet:
  case Instruction::CatchRet: {
    auto *CR = cast<CatchReturnInst>(Inst);
    DEBUG(dbgs() << "CatchReturn: " << *CR << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto ParentIdx = DataMapping.getOrInsert(CR->getCatchPad(), Layout);
    Opcode.emplace_back(ParentIdx);

    Opcode.emplace_back(0x0);

    break;
  }
  case Instruction::CleanupPad:
  case Instruction::CatchPad: {
    auto *CP = cast<CatchPadInst>(Inst);
    DEBUG(dbgs() << "CatchPad: " << *CP << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto ParentPadIdx = DataMapping.getOrInsert(CP->getParentPad(), Layout);
    Opcode.emplace_back(ParentPadIdx);

    for (auto Idx = 0u; Idx < CP->getNumArgOperands(); ++Idx) {
      const auto ArgIdx = DataMapping.getOrInsert(CP->getArgOperand(Idx), Layout);
      Opcode.emplace_back(ArgIdx);
    }

    const auto ResIdx = DataMapping.getOrInsert(CP, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::Fence: {
    Opcode.emplace_back(OpGen.generate());
    break;
  }
  case Instruction::AtomicRMW: {
    auto *ARMW = cast<AtomicRMWInst>(Inst);
    DEBUG(dbgs() << "AtomicRMW: " << *ARMW << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto PtrIdx = DataMapping.getOrInsert(ARMW->getPointerOperand(), Layout);
    Opcode.emplace_back(PtrIdx);

    const auto ValIdx = DataMapping.getOrInsert(ARMW->getValOperand(), Layout);
    Opcode.emplace_back(ValIdx);

    const auto ResIdx = DataMapping.getOrInsert(ARMW, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  case Instruction::AtomicCmpXchg: {
    auto *ACX = cast<AtomicCmpXchgInst>(Inst);
    DEBUG(dbgs() << "AtomicCmpXchgq: " << *ACX << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto PtrIdx = DataMapping.getOrInsert(ACX->getPointerOperand(), Layout);
    Opcode.emplace_back(PtrIdx);

    const auto CmpIdx = DataMapping.getOrInsert(ACX->getCompareOperand(), Layout);
    Opcode.emplace_back(CmpIdx);

    const auto ValIdx = DataMapping.getOrInsert(ACX->getNewValOperand(), Layout);
    Opcode.emplace_back(ValIdx);

    break;
  }
  case Instruction::VAArg: {
    auto *VA = cast<VAArgInst>(Inst);
    DEBUG(dbgs() << "VAArg: " << *VA << "\n");
    Opcode.emplace_back(OpGen.generate());

    const auto PtrIdx = DataMapping.getOrInsert(VA->getPointerOperand(), Layout);
    Opcode.emplace_back(PtrIdx);

    const auto ResIdx = DataMapping.getOrInsert(VA, Layout);
    Opcode.emplace_back(ResIdx);

    break;
  }
  default: {
    DEBUG(dbgs() << "No handler for: " << *Inst << "\n");
    break;
  }
  }

  return Opcode;
}

bool ISATranslator::shouldIgnore(const Instruction *Inst) {
  // landing pads are handled together with the invoke
  if (isa<LandingPadInst>(Inst)) {
    DEBUG(dbgs() << "Ignoring: " << *Inst << "\n");
    return true;
  }

  return false;
}

// Is not called in updated VirtSC but remains to simplify switching between different versions
void ISATranslator::generateSafeGuards() {
  auto &OpGen = OpcodeGenerator::get();

  std::random_device RandDevice;
  std::mt19937 Generator{RandDevice()};
  std::uniform_int_distribution<uint64_t> Distr{0u, CodeMapping.size() - 1};

  auto &Context = Func->getContext();
  auto *Int64Ty = Type::getInt64Ty(Context);
  const auto &Layout = Func->getParent()->getDataLayout();

  std::map<uint16_t, uint64_t> Tmp;

  for (auto I = 0u; I < ToCheck.size(); ++I) {
    const auto Idx = Distr(Generator);

    auto *CSum = ConstantInt::get(Int64Ty, Checksums.at(I));
    const auto DataIdx = DataMapping.getOrInsert(CSum, Layout);

    std::vector<uint16_t> Opcode;
    Opcode.emplace_back(OpGen.generate());
    Opcode.emplace_back(DataIdx);

    CodeMapping.insertSafeGuard(Idx, Opcode);
    Tmp.emplace(std::make_pair(Opcode.at(0), I));
  }

  auto Idx = 0u;
  for(auto I = 0u; I < ToCheck.size(); ++I) {
    while(CodeMapping.at(Idx).OldInst) {
      ++Idx;
    }

    const auto Opcode = CodeMapping.at(Idx).Opcode.at(0);
    const auto CheckIdx = Tmp.at(Opcode);

    ChecksumMap.emplace(std::make_pair(Idx, ToCheck.at(CheckIdx)));
    ++Idx;
  }
}
void ISATranslator::translateFunc() {
  for (auto Iter = inst_begin(Func), End = inst_end(Func); Iter != End; ++Iter) {
    auto *Inst = &*Iter;

    if (shouldIgnore(Inst)) {
      DEBUG(dbgs() << "Ignoring " << *Inst << "\n");
      continue;
    }

    auto Opcode = translateInst(Inst);

    auto HandledIter = std::find_if(HandledTypes.begin(), HandledTypes.end(), [&Inst](const InstrOperandTypes &Other) {
      for (auto Op = 0u; Op < Inst->getNumOperands(); ++Op) {
        if (isa<Function>(Inst->getOperand(Op))) {
          return false;
        }
      }

      return Other.isSameInstType(Inst);
    });

    // if we did handle it, overwrite the opcode and free it up again
    if (HandledIter != HandledTypes.end() && !isa<GetElementPtrInst>(Inst) && !isa<ExtractValueInst>(Inst)) {
      DEBUG(dbgs() << "We already looked at instruction x type combination: " << *Inst << "\n");

      const auto NewOpcode = Opcode.at(0);
      Opcode.at(0) = HandledIter->getOpcode();
      OpcodeGenerator::get().release(NewOpcode);
    }

    CodeMapping.insertInst(Inst, Opcode);
    HandledTypes.emplace_back(InstrOperandTypes{Inst, Opcode.at(0)});
  }

  CodeMapping.wireTargets();
}

CodeMap ISATranslator::getCodeMap() const {
  return CodeMapping;
}

DataMap ISATranslator::getDataMap() const {
  return DataMapping;
}

std::map<uint16_t, StringRef> ISATranslator::getChecksumMap() const {
  return ChecksumMap;
}
