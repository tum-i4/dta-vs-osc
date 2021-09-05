#ifndef LLVM_INSTROPERANDTYPES_HPP
#define LLVM_INSTROPERANDTYPES_HPP

#include "llvm/IR/Instructions.h"

/// This is needed because we have to be able to differentiate between two XYZInsts with different types.
/// For example:
///   store i32, i32
/// is different from
///   store f32, f32
class InstrOperandTypes {
  /// The actual instruction.
  const llvm::Instruction *Inst;

  /// The LLVM instruction's opcode.
  uint32_t InstOpcode;

  /// Our generated opcode.
  uint16_t NewOpcode;

public:
  /// Constructor.
  /// \param Inst The instruction to store opcode and operand types of.
  /// \param Opcode The generated opcode for this opcode x operand type combination.
  InstrOperandTypes(const llvm::Instruction *Inst, const uint16_t Opcode);

  /// Checks if instruction \param Other is equal to this one.
  /// \param Other The instruction to compare against.
  /// \return True if both instructions have the same opcode and operand types.
  bool isSameInstType(const llvm::Instruction *Other) const;

  /// Gets this instruction's \var opcode for re-use.
  /// \return This instruction's opcode.
  uint16_t getOpcode() const;
};

#endif