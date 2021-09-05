#ifndef LLVM_VIRTUALIZATIONBUILDER_HPP
#define LLVM_VIRTUALIZATIONBUILDER_HPP

#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/Transforms/ScVirt/ISATranslator.hpp"

class VirtualizationBuilder {
private:
  /// Map to re-use allocations.
  llvm::DenseMap<llvm::Type *, llvm::AllocaInst *> AllocMap;

  /// The module's data layout.
  const llvm::DataLayout *Layout;

  /// The code array.
  llvm::GlobalVariable *CodeArray;

  /// The data array.
  llvm::LoadInst *DataArray;

  /// To clean up the allocated data array.
  llvm::Function *FreeFunc;

  llvm::AllocaInst *getOrCreateAlloca(llvm::IRBuilder<> &Builder, llvm::Type *Ty);

  /// Checks whether the value \param Val is a function argument.
  /// \param Val The value to check.
  /// \param TargetFunc Used to compare to \param Val.
  /// \param NewFunc Used to get a pointer to the correct argument.
  /// \return The argument of \param NewFunc that is stored.
  llvm::Argument *getAsArgument(const llvm::Value *Val, llvm::Function &TargetFunc, llvm::Function *NewFunc) const;

  /// Copies all call and parameter attributes from \param From to \param To.
  /// \tparam TCallSite Either CallInst or InvokeInst.
  /// \param Context The current LLVMContext.
  /// \param From The CallSite to copy from.
  /// \param To The CallSite to copy to.
  template<typename TCallSite>
  void copyCallAttributes(llvm::LLVMContext &Context, const TCallSite *From, TCallSite *To);

  /// Increment the instruction pointer.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Prev The previous value of the instruction pointer.
  /// \return Incremented instruction pointer.
  llvm::Value *incrementCodeIndex(llvm::IRBuilder<> &Builder, llvm::Value *Prev);

  /// Gets the GEP at \param CurrentIdx from the \property DataArray.
  /// \param Builder IRBuilder to insert instructions.
  /// \param CurrentIdx The index to load from.
  /// \return Loaded address at \param CurrentIdx from the \property DataArray.
  llvm::Value *loadDataIndex(llvm::IRBuilder<> &Builder, llvm::Value *CurrentIdx);

  /// Loads an actual value from the \property DataArray.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Ty The type of the value to load.
  /// \param CurrentIdx The index to load from.
  /// \return Loaded value of type \param Ty.
  llvm::LoadInst *loadValueFromDataArray(llvm::IRBuilder<> &Builder, llvm::Type *Ty, llvm::Value *CurrentIdx);

  /// Stores \param Result directly to location \param Dest.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Result The value to store.
  /// \param Dest The location to store to.
  void storeResultDest(llvm::IRBuilder<> &Builder, llvm::Value *Result, llvm::Value *Dest);

  /// Stores \param Result to the location indicated by the \property CodeArray value at \param CurrentIdx
  /// \param Builder IRBuilder to insert instructions.
  /// \param Result The value to store.
  /// \param CurrentIdx The index to get the location from the \property CodeArray.
  /// \return Updated instruction pointer.
  llvm::Value *storeResult(llvm::IRBuilder<> &Builder, llvm::Value *Result, llvm::Value *CurrentIdx);

  /// Handles conversion operations such as SExt, ZExt, Trunc, ... .
  /// \tparam TFunc Type of passed lambda function \param Convert
  /// \param Builder IRBuilder to insert instructions.
  /// \param Cst The original instruction for type information.
  /// \param Convert The operation to perform.
  /// \param CurrentIdx The index into the \param CodeArray.
  template<typename TFunc>
  llvm::Value *handleConversion(llvm::IRBuilder<> &Builder,
                                const llvm::CastInst &Cst,
                                const TFunc Convert,
                                llvm::Value *CurrentIdx);

  /// Handles arithmetic operations such as Add, Sub, Mul, ... .
  /// \tparam TFunc Type of passed lambda function \param Calculate.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The original instruction for type information.
  /// \param Calculate The operation to perform.
  /// \param CurrentIdx The index into the \param CodeArray.
  template<typename TFunc>
  llvm::Value *handleArithmetic(llvm::IRBuilder<> &Builder,
                                const llvm::BinaryOperator &BinOp,
                                const TFunc Calculate,
                                llvm::Value *CurrentIdx);

  /// If an instruction operates on a constant value there is no StoreInst for it.
  /// Therefore, we have to store these ourselves.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Val The constant value to insert.
  /// \param DstIndex The index to store to.
  void storeConstant(llvm::IRBuilder<> &Builder, llvm::Value *Val, llvm::Value *DstIndex);

public:
  /// Stores constants into the data array.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Data The data array map.
  void storeConstants(llvm::IRBuilder<> &Builder, const std::map<uint64_t, llvm::Value *> &Data);

  /// Stores arguments passed to \param NewFunc into the \property DataArray.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Insts List of instructions.
  /// \param Data Mapping of \property DataArray index to original instruction.
  /// \param TargetFunc The original function.
  /// \param NewFunc The new function / emulator.
  void storeFunctionArguments(llvm::IRBuilder<> &Builder,
                              const std::vector<CodeMap::InstToOpcode> &Insts,
                              const std::map<uint64_t, llvm::Value *> &Data,
                              llvm::Function &TargetFunc,
                              llvm::Function *NewFunc);

  //-----------------------------------------------------------------------------------------

  /// Handles UnreachableInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Unreach The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  void handleUnreachable(llvm::IRBuilder<> &Builder, const llvm::UnreachableInst *Unreach, llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles AllocaInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Alloca The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAlloca(llvm::IRBuilder<> &Builder, const llvm::AllocaInst *Alloca, llvm::Value *IdxAlloca);

  /// Handles LoadInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Load The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleLoad(llvm::IRBuilder<> &Builder, const llvm::LoadInst *Load, llvm::Value *IdxAlloca);

  /// Handles StoreInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Store The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleStore(llvm::IRBuilder<> &Builder, const llvm::StoreInst *Store, llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles floating point comparisons.
  /// \param Builder IRBuilder to insert instructions.
  /// \param FCmp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFCmp(llvm::IRBuilder<> &Builder, const llvm::FCmpInst *FCmp, llvm::Value *IdxAlloca);

  /// Handles integer comparisons.
  /// \param Builder IRBuilder to insert instructions.
  /// \param ICmp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleICmp(llvm::IRBuilder<> &Builder, const llvm::ICmpInst *ICmp, llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles SwitchInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Switch The original SwitchInst.
  /// \param NewFunc The emulator.
  /// \param SwitchHead The block to jump back to after this instruction.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSwitch(llvm::IRBuilder<> &Builder,
                            const llvm::SwitchInst *Switch,
                            llvm::Function *NewFunc,
                            llvm::BasicBlock *SwitchHead,
                            llvm::Value *IdxAlloca);

  /// Handles BranchInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Branch The original BranchInst.
  /// \param NewFunc The emulator.
  /// \param SwitchHead The block to jump back to after this instruction.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleBr(llvm::IRBuilder<> &Builder,
                        const llvm::BranchInst *Branch,
                        llvm::Function *NewFunc,
                        llvm::BasicBlock *SwitchHead,
                        llvm::Value *IdxAlloca);

  /// Handles IndirectBranchInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param IndBranch The original IndirectBranchInst.
  /// \param NewFunc The emulator.
  /// \param SwitchHead The block to jump back to after this instruction.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleIndirectBr(llvm::IRBuilder<> &Builder,
                                const llvm::IndirectBrInst *IndBranch,
                                llvm::Function *NewFunc,
                                llvm::BasicBlock *SwitchHead,
                                llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles SExtInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param SExt The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSExt(llvm::IRBuilder<> &Builder,
                          const llvm::SExtInst *SExt,
                          llvm::Value *IdxAlloca);

  /// Handles ZExtInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param ZExt The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleZExt(llvm::IRBuilder<> &Builder,
                          const llvm::ZExtInst *ZExt,
                          llvm::Value *IdxAlloca);

  /// Handles TruncInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Trunc The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleTrunc(llvm::IRBuilder<> &Builder,
                           const llvm::TruncInst *Trunc,
                           llvm::Value *IdxAlloca);

  /// Handles FPTruncInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param FPTrunc The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFPTrunc(llvm::IRBuilder<> &Builder,
                             const llvm::FPTruncInst *FPTrunc,
                             llvm::Value *IdxAlloca);

  /// Handles FPExtInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param FPExt The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFPExt(llvm::IRBuilder<> &Builder,
                           const llvm::FPExtInst *FPExt,
                           llvm::Value *IdxAlloca);

  /// Handles FPToSIInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param FPToSI The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFPToSI(llvm::IRBuilder<> &Builder,
                            const llvm::FPToSIInst *FPToSI,
                            llvm::Value *IdxAlloca);

  /// Handles FPToUIInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param FPToUI The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFPToUI(llvm::IRBuilder<> &Builder,
                            const llvm::FPToUIInst *FPToUI,
                            llvm::Value *IdxAlloca);

  /// Handles SIToFPInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param SIToFP The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSIToFP(llvm::IRBuilder<> &Builder,
                            const llvm::SIToFPInst *SIToFP,
                            llvm::Value *IdxAlloca);

  /// Handles UIToFPInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param UIToFP The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleUIToFP(llvm::IRBuilder<> &Builder,
                            const llvm::UIToFPInst *UIToFP,
                            llvm::Value *IdxAlloca);

  /// Handles BitCastInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BitCast The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleBitCast(llvm::IRBuilder<> &Builder,
                             const llvm::BitCastInst *BitCast,
                             llvm::Value *IdxAlloca);

  /// Handles AddrSpaceCastInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param AddrCast The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAddrSpaceCast(llvm::IRBuilder<> &Builder,
                                   const llvm::AddrSpaceCastInst *AddrCast,
                                   llvm::Value *IdxAlloca);

  /// Handles PtrToIntInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param PtrToInt The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handlePtrToInt(llvm::IRBuilder<> &Builder,
                              const llvm::PtrToIntInst *PtrToInt,
                              llvm::Value *IdxAlloca);

  /// Handles IntToPtr.
  /// \param Builder IRBuilder to insert instructions.
  /// \param IntToPtr The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleIntToPtr(llvm::IRBuilder<> &Builder,
                              const llvm::IntToPtrInst *IntToPtr,
                              llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles addition instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAdd(llvm::IRBuilder<> &Builder,
                         const llvm::BinaryOperator *BinOp,
                         llvm::Value *IdxAlloca);

  /// Handles floating point addition instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFAdd(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles subtraction instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSub(llvm::IRBuilder<> &Builder,
                         const llvm::BinaryOperator *BinOp,
                         llvm::Value *IdxAlloca);

  /// Handles floating point subtraction instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFSub(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles multiplication instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleMul(llvm::IRBuilder<> &Builder,
                         const llvm::BinaryOperator *BinOp,
                         llvm::Value *IdxAlloca);

  /// Handles floating point multiplication instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFMul(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles unsigned division instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleUDiv(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles signed division instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSDiv(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles floating point division instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFDiv(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles binary and instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAnd(llvm::IRBuilder<> &Builder,
                         const llvm::BinaryOperator *BinOp,
                         llvm::Value *IdxAlloca);

  /// Handles binary or instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleOr(llvm::IRBuilder<> &Builder,
                        const llvm::BinaryOperator *BinOp,
                        llvm::Value *IdxAlloca);

  /// Handles binary xor instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleXor(llvm::IRBuilder<> &Builder,
                         const llvm::BinaryOperator *BinOp,
                         llvm::Value *IdxAlloca);

  /// Handles shift left instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleShl(llvm::IRBuilder<> &Builder,
                         const llvm::BinaryOperator *BinOp,
                         llvm::Value *IdxAlloca);

  /// Handles logical shift right instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleLShr(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles arithmetic shift right instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAShr(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles unsigned remainder instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleURem(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles signed remainder instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSRem(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  /// Handles floating point remainder instructions.
  /// \param Builder IRBuilder to insert instructions.
  /// \param BinOp The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFRem(llvm::IRBuilder<> &Builder,
                          const llvm::BinaryOperator *BinOp,
                          llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles GetElementPtrInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param GEP The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleGetElementPtr(llvm::IRBuilder<> &Builder,
                                   const llvm::GetElementPtrInst *GEP,
                                   llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles CallInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Call The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleCall(llvm::IRBuilder<> &Builder, const llvm::CallInst *Call, llvm::Value *IdxAlloca);

  /// Handles InvokeInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Invoke The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleInvoke(llvm::IRBuilder<> &Builder,
                            const llvm::InvokeInst *Invoke,
                            llvm::Function *NewFunc,
                            llvm::BasicBlock *SwitchHead,
                            llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles ResumeInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Resume The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleResume(llvm::IRBuilder<> &Builder, const llvm::ResumeInst *Resume, llvm::Value *IdxAlloca);

  /// Handles CatchReturnInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param CatchReturn The instruction to handle.
  /// \param NewFunc The emulator.
  /// \param SwitchHead The block to jump back to after this instruction.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleCatchReturn(llvm::IRBuilder<> &Builder,
                                 const llvm::CatchReturnInst *CatchReturn,
                                 llvm::Function *NewFunc,
                                 llvm::BasicBlock *SwitchHead,
                                 llvm::Value *IdxAlloca);

  /// Handles CatchPadInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param CatchPad The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleCatchPad(llvm::IRBuilder<> &Builder,
                              const llvm::CatchPadInst *CatchPad,
                              llvm::Value *IdxAlloca);

  /// Handles CatchSwitchInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param CatchSwitch The instruction to handle.
  /// \param NewFunc The emulator.
  /// \param SwitchHead The block to jump back to after this instruction.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleCatchSwitch(llvm::IRBuilder<> &Builder,
                                 const llvm::CatchSwitchInst *CatchSwitch,
                                 llvm::Function *NewFunc,
                                 llvm::BasicBlock *SwitchHead,
                                 llvm::Value *IdxAlloca);

  /// Handles CleanupPadInsts
  /// \param Builder IRBuilder to insert instructions.
  /// \param CleanupPad The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleCleanupPad(llvm::IRBuilder<> &Builder,
                                const llvm::CleanupPadInst *CleanupPad,
                                llvm::Value *IdxAlloca);

  /// Handles CleanupRetInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param CleanupRet The instruction to handle.
  /// \param NewFunc The emulator.
  /// \param SwitchHead The block to jump back to after this instruction.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleCleanupRet(llvm::IRBuilder<> &Builder,
                                const llvm::CleanupReturnInst *CleanupRet,
                                llvm::Function *NewFunc,
                                llvm::BasicBlock *SwitchHead,
                                llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles ShuffleVectorInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param ShuffleVector The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleShuffleVector(llvm::IRBuilder<> &Builder,
                                   const llvm::ShuffleVectorInst *ShuffleVector,
                                   llvm::Value *IdxAlloca);

  /// Handles ExtractElementInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param ExtractElement The current instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleExtractElement(llvm::IRBuilder<> &Builder,
                                    const llvm::ExtractElementInst *ExtractElement,
                                    llvm::Value *IdxAlloca);

  /// Handles InsertElementInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param InsertElement The current instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleInsertElement(llvm::IRBuilder<> &Builder,
                                   const llvm::InsertElementInst *InsertElement,
                                   llvm::Value *IdxAlloca);

  /// Handles ExtractValueInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param ExtractValue The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleExtractValue(llvm::IRBuilder<> &Builder,
                                  const llvm::ExtractValueInst *ExtractValue,
                                  llvm::Value *IdxAlloca);

  /// Handles InsertValueInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param InsertValue The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleInsertValue(llvm::IRBuilder<> &Builder,
                                 const llvm::InsertValueInst *InsertValue,
                                 llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles FenceInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Fence The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleFence(llvm::IRBuilder<> &Builder, const llvm::FenceInst *Fence, llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles AtomicRMWInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param AtomicRMW The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAtomicRMW(llvm::IRBuilder<> &Builder,
                               const llvm::AtomicRMWInst *AtomicRMW,
                               llvm::Value *IdxAlloca);

  /// Handles AtomicCmpXchgInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param AtomicRMW The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleAtomicCmpXchg(llvm::IRBuilder<> &Builder,
                                   const llvm::AtomicCmpXchgInst *AtomicCmp,
                                   llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles VAArgInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param VAArg The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleVAArg(llvm::IRBuilder<> &Builder, const llvm::VAArgInst *VAArg, llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles SelectInst.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Select The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleSelect(llvm::IRBuilder<> &Builder,
                            const llvm::SelectInst *Select,
                            llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Handles ReturnInsts.
  /// \param Builder IRBuilder to insert instructions.
  /// \param Return The instruction to handle.
  /// \param IdxAlloca The current instruction pointer.
  /// \return Updated instruction pointer.
  llvm::Value *handleRet(llvm::IRBuilder<> &Builder,
                         const llvm::ReturnInst *Return,
                         llvm::Function *NewFunc,
                         llvm::Value *IdxAlloca);

  llvm::Value *handleSafeGuard(llvm::IRBuilder<> &Builder,
                               llvm::GlobalVariable *ToCheck,
                               llvm::Function *XorFunc,
                               llvm::BasicBlock *Unreach,
                               const uint64_t Len,
                               llvm::BasicBlock *SwitchHead,
                               llvm::Value *IdxAlloca);

  //-----------------------------------------------------------------------------------------

  /// Constructor.
  /// \param Layout The module's data layout.
  /// \param CodeArray The code array.
  /// \param DataArray The data array.
  /// \param FreeFunc Function to call to free the data array on return.
  VirtualizationBuilder(const llvm::DataLayout *Layout,
                        llvm::GlobalVariable *CodeArray,
                        llvm::LoadInst *DataArray,
                        llvm::Function *FreeFunc)
    : Layout(Layout), CodeArray(CodeArray), DataArray(DataArray), FreeFunc(FreeFunc) {}

};

#endif //LLVM_VIRTUALIZATIONBUILDER_HPP
