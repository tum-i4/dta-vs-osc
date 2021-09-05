#include "llvm/Transforms/ScVirt/InstrOperandTypes.hpp"

#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "InstrOperandTypes"

using namespace llvm;

InstrOperandTypes::InstrOperandTypes(const Instruction *Inst, const uint16_t Opcode)
    : Inst(Inst), InstOpcode(Inst->getOpcode()), NewOpcode(Opcode) { }

bool InstrOperandTypes::isSameInstType(const Instruction *Other) const {
  const auto IsSame = Other->isSameOperationAs(Inst);

  if(!IsSame) {
    return false;
  }

  for(auto NumOp = 0u; NumOp < Inst->getNumOperands(); ++NumOp) {
    const auto *ThisOp = Inst->getOperand(NumOp);

    if(isa<ConstantInt>(ThisOp) || isa<Function>(ThisOp)) {
      return false;
    }
  }

  return true;
}

uint16_t InstrOperandTypes::getOpcode() const {
  return NewOpcode;
}