#include "llvm/Transforms/ScVirt/VirtualizationBuilder.hpp"

#include "llvm/Transforms/ScVirt/CallConstantParameterIndices.hpp"

#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <type_traits>

#define DEBUG_TYPE "VirtualizationBuilder"

using namespace llvm;



AllocaInst *VirtualizationBuilder::getOrCreateAlloca(IRBuilder<> &Builder, Type *Ty) {
  auto Iter = AllocMap.find(Ty);
  if(Iter != AllocMap.end()) {
    return Iter->second;
  }

  auto *AI = Builder.CreateAlloca(Ty);
  AllocMap.insert({Ty, AI});

  return AI;
}

void VirtualizationBuilder::storeConstants(IRBuilder<> &Builder, const std::map<uint64_t, Value *> &Data) {
  for (const auto &Pair : Data) {
    auto *Val = Pair.second;
    assert(Val);
    if (isa<Constant>(Val)) {
      storeConstant(Builder, Val, Builder.getInt64(Pair.first));
    }
  }
}

void VirtualizationBuilder::storeConstant(IRBuilder<> &Builder, Value *Val, Value *DstIndex) {
  assert(Val);
  assert(DstIndex);

  DEBUG(dbgs() << "Got a constant:\t" << *Val << "\n");
  DEBUG(dbgs() << "storing to " << *DstIndex << "\n");
  DEBUG(dbgs() << "ValueType:\t" << *(Val->getType()) << "\n");

  // write parameter into DataArray at DstIndex
  auto *ToGEP = Builder.CreateInBoundsGEP(DataArray, {DstIndex});

  auto *AI = getOrCreateAlloca(Builder, Val->getType());
  Builder.CreateStore(Val, AI);

  // copy value to its respective position in the DataArray
  Builder.CreateMemCpy(ToGEP, AI, Layout->getTypeAllocSize(Val->getType()), DataArray->getAlignment());
}

Argument *VirtualizationBuilder::getAsArgument(const Value *Val, Function &TargetFunc, Function *NewFunc) const {
  assert(Val);
  assert(NewFunc);

  Argument *FuncArg = nullptr;
  auto *Iter = &*NewFunc->arg_begin();
  for (const auto &Arg : TargetFunc.getArgumentList()) {
    if (&Arg == Val) {
      FuncArg = Iter;
      break;
    }

    Iter = NewFunc->getArgumentList().getNext(Iter);
  }

  return FuncArg;
}

void VirtualizationBuilder::storeFunctionArguments(IRBuilder<> &Builder,
                                                   const std::vector<CodeMap::InstToOpcode> &Insts,
                                                   const std::map<uint64_t, Value *> &Data,
                                                   Function &TargetFunc,
                                                   Function *NewFunc) {
  assert(NewFunc);

  auto *ConstantZero = Builder.getInt64(0);
  for (const auto &Elem : Insts) {
    const auto IsFuncArg = Elem.OldInst && CodeMap::hasFunctionArg(Elem.OldInst, &TargetFunc);
    if (!IsFuncArg) {
      continue;
    }

    // NOTE: we can't simply iterate over the total length of the opcode for invoke
    const auto Len = [&Elem]() -> uint64_t {
      if(auto *Inv = dyn_cast<InvokeInst>(Elem.OldInst)) {
        return Inv->getNumArgOperands();
      } else {
        return Elem.Opcode.size();
      }
    }();

    const auto StartIdx = [&Elem]() -> uint64_t {
      if(auto *Inv = dyn_cast<InvokeInst>(Elem.OldInst)) {
        return 1 + (Inv->getCalledFunction() != nullptr ? 0 : 1);
      } else {
        return 1;
      }
    }();

    for (auto Idx = StartIdx; Idx < Len; ++Idx) {
      const auto SrcIndex = Elem.Opcode.at(Idx);
      DEBUG(dbgs() << "Checking data index: " << SrcIndex << "\n");
      auto *SrcVal = Data.at(SrcIndex);
      auto *SrcTy = SrcVal->getType();

      auto *DestPtrTy = PointerType::get(SrcTy, 0);

      auto DataIter = std::find_if(Data.begin(), Data.end(), [&SrcVal](const std::pair<uint64_t, Value *> &Pair) {
        return Pair.second == SrcVal;
      });

      assert(DataIter != Data.end() && "Did not find the function argument!");

      const auto DstIndex = DataIter->first;
      auto *FuncArg = getAsArgument(SrcVal, TargetFunc, NewFunc);
      if (!FuncArg) {
        continue;
      }

      auto *GEP = Builder.CreateInBoundsGEP(DataArray, {Builder.getInt64(DstIndex)});
      GEP = Builder.CreateBitCast(GEP, DestPtrTy);

      auto *ArgAI = getOrCreateAlloca(Builder, SrcTy);
      Builder.CreateStore(FuncArg, ArgAI);
      auto *ArgLd = Builder.CreateLoad(ArgAI);

      Builder.CreateStore(ArgLd, GEP);
    }
  }
}

template<typename TCallSite>
inline void VirtualizationBuilder::copyCallAttributes(LLVMContext &Context, const TCallSite *From, TCallSite *To) {
  assert(From);
  assert(To);

  // get old attributes
  const auto &OldAttributes = From->getAttributes();

  To->setCallingConv(From->getCallingConv());

  // copy non-parameter attributes
  AttributeSet NonParamAttributes;
  NonParamAttributes =
    NonParamAttributes.addAttributes(Context, AttributeSet::ReturnIndex, OldAttributes.getRetAttributes());
  NonParamAttributes =
    NonParamAttributes.addAttributes(Context, AttributeSet::FunctionIndex, OldAttributes.getFnAttributes());
  To->setAttributes(NonParamAttributes);

  const auto ArgSize = From->getNumArgOperands();

  // copy all parameter attributes
  for (auto ArgOpIdx = 0u; ArgOpIdx < ArgSize; ++ArgOpIdx) {
    for (auto AttrKind = Attribute::None; AttrKind != Attribute::EndAttrKinds;
         AttrKind = Attribute::AttrKind(AttrKind + 1)) {
      if (!OldAttributes.hasAttribute(ArgOpIdx, AttrKind)) {
        continue;
      }

      switch (AttrKind) {
      case Attribute::Dereferenceable: {
        const auto DerefBytes = From->getDereferenceableBytes(ArgOpIdx);
        To->addDereferenceableAttr(ArgOpIdx, DerefBytes);
        break;
      }
      case Attribute::DereferenceableOrNull: {
        const auto DerefOrNull = From->getDereferenceableOrNullBytes(ArgOpIdx);
        To->addDereferenceableOrNullAttr(ArgOpIdx, DerefOrNull);
        break;
      }
      case Attribute::Alignment: {
        auto Attr = OldAttributes.getAttribute(ArgOpIdx, AttrKind);
        const auto NewAttr = Attribute::get(Context, AttrKind, Attr.getAlignment());
        To->addAttribute(ArgOpIdx, NewAttr);
        break;
      }
      default: {
        To->addAttribute(ArgOpIdx, AttrKind);
        break;
      }
      }
    }
  }
}

Value *VirtualizationBuilder::incrementCodeIndex(IRBuilder<> &Builder, Value *Prev) {
  assert(Prev);

  auto *ConstOne = Builder.getInt16(1);
  return Builder.CreateAdd(ConstOne, Prev);
}

Value *VirtualizationBuilder::loadDataIndex(IRBuilder<> &Builder, Value *CurrentIdx) {
  assert(CurrentIdx);

  auto &Context = Builder.getContext();
  auto *Int16Ty = Type::getInt16Ty(Context);
  auto *ConstantZero = Builder.getInt16(0);

  auto *CodeSourceGEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, CurrentIdx});
  auto *CodeSourceLoad = Builder.CreateLoad(Int16Ty, CodeSourceGEP);
  return Builder.CreateInBoundsGEP(DataArray, {CodeSourceLoad});
}

LoadInst *VirtualizationBuilder::loadValueFromDataArray(IRBuilder<> &Builder, Type *Ty, Value *CurrentIdx) {
  assert(Ty);
  assert(CurrentIdx);

  // pointer into the array
  auto *GEP = loadDataIndex(Builder, CurrentIdx);
  auto *PtrTy = PointerType::get(Ty, 0);
  GEP = Builder.CreateBitCast(GEP, PtrTy);
  return Builder.CreateLoad(GEP);
}

void VirtualizationBuilder::storeResultDest(IRBuilder<> &Builder, Value *Result, Value *Dest) {
  assert(Result);
  assert(Dest);

  auto *DestPtrTy = PointerType::get(Result->getType(), 0);
  Dest = Builder.CreateBitCast(Dest, DestPtrTy);

  auto *ArgAI = getOrCreateAlloca(Builder, Result->getType());
  Builder.CreateStore(Result, ArgAI);
  auto *ArgLd = Builder.CreateLoad(ArgAI);

  Builder.CreateStore(ArgLd, Dest);
}

Value *VirtualizationBuilder::storeResult(IRBuilder<> &Builder, Value *Result, Value *CurrentIdx) {
  assert(Result);
  assert(CurrentIdx);

  // load where to store the result
  auto *ResData = loadDataIndex(Builder, CurrentIdx);
  CurrentIdx = incrementCodeIndex(Builder, CurrentIdx);

  storeResultDest(Builder, Result, ResData);

  return CurrentIdx;
}

template<typename TFunc>
inline Value *VirtualizationBuilder::handleConversion(IRBuilder<> &Builder,
                                                      const CastInst &Cst,
                                                      const TFunc Convert,
                                                      Value *CurrentIdx) {
  assert(CurrentIdx);

  auto *SrcTy = Cst.getSrcTy();
  auto *ValLoad = loadValueFromDataArray(Builder, SrcTy, CurrentIdx);
  CurrentIdx = incrementCodeIndex(Builder, CurrentIdx);

  DEBUG(dbgs() << "\tVal to be casted: " << *ValLoad << "\n");
  DEBUG(dbgs() << "\tSrc Type: " << *SrcTy << "\n");
  DEBUG({
    auto *DestTy = Cst.getDestTy();
    dbgs() << "\tDest Type: " << *DestTy << "\n";
  });

  // cast the value
  auto *Res = Convert(Builder, Cst, ValLoad);

  // if we didn't need to cast this will be a load inst
  if (auto *ResCst = dyn_cast<CastInst>(Res)) {
    if (isa<FPMathOperator>(Cst)) {
      ResCst->copyFastMathFlags(&Cst);
    }
  } else {
    DEBUG(dbgs() << "Not a CastInst: " << *Res << "\n");
  }

  Res->setName(Cst.getName());

  // store result back into the data array
  return storeResult(Builder, Res, CurrentIdx);
}

template<typename TFunc>
inline Value *VirtualizationBuilder::handleArithmetic(IRBuilder<> &Builder,
                                                      const BinaryOperator &BinOp,
                                                      const TFunc Calculate,
                                                      Value *CurrentIdx) {
  assert(CurrentIdx);

  // load LHS and RHS operand
  auto *LHS = loadValueFromDataArray(Builder, BinOp.getOperand(0)->getType(), CurrentIdx);
  CurrentIdx = incrementCodeIndex(Builder, CurrentIdx);

  auto *RHS = loadValueFromDataArray(Builder, BinOp.getOperand(1)->getType(), CurrentIdx);
  CurrentIdx = incrementCodeIndex(Builder, CurrentIdx);

  auto *Result = Calculate(Builder, BinOp, LHS, RHS);
  auto *BinOpRes = cast<BinaryOperator>(Result);
  if (isa<FPMathOperator>(BinOp)) {
    BinOpRes->copyFastMathFlags(&BinOp);
  }

  Result->setName(BinOp.getName());

  // store result back into data array
  return storeResult(Builder, Result, CurrentIdx);
}

void VirtualizationBuilder::handleUnreachable(IRBuilder<> &Builder, const UnreachableInst *Unreach, Value *IdxAlloca) {
  Builder.CreateUnreachable();
}

Value *VirtualizationBuilder::handleAlloca(IRBuilder<> &Builder, const AllocaInst *Alloca, Value *IdxAlloca) {
  assert(Alloca);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *AITy = Alloca->getType();
  auto *AllocedTy = Alloca->getAllocatedType();

  DEBUG(dbgs() << "\tOldAlloca: " << *Alloca << "\n");
  DEBUG(dbgs() << "\t\t-- Alloca Type: " << *AITy << "\n");
  DEBUG(dbgs() << "\t\t-- Alloca Alloc'd Type: " << *AllocedTy << "\n");

  auto *DestPtrTy = PointerType::get(AITy, 0);

  auto *DestGEP = loadDataIndex(Builder, Add);
  Add = incrementCodeIndex(Builder, Add);
  DEBUG(dbgs() << "\t\t-- Dest GEP: " << *DestGEP << "\n");
  DestGEP = Builder.CreateBitCast(DestGEP, DestPtrTy);
  DEBUG(dbgs() << "\t\t-- Casted Dest GEP: " << *DestGEP << "\n");

  auto *AI = Builder.CreateAlloca(AllocedTy);
  AI->setName(Alloca->getName());

  Builder.CreateStore(AI, DestGEP);

  return Add;
}

Value *VirtualizationBuilder::handleLoad(IRBuilder<> &Builder, const LoadInst *Load, Value *IdxAlloca) {
  assert(Load);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *LoadTy = Load->getType();
  auto *SrcPtrTy = PointerType::get(Load->getPointerOperand()->getType(), 0);
  auto *DstPtrTy = PointerType::get(LoadTy, 0);

  DEBUG(dbgs() << "\tOldLoad: " << *Load << "\n");
  DEBUG(dbgs() << "\t\t-- Source PtrType: " << *SrcPtrTy << "\n");
  DEBUG(dbgs() << "\t\t-- Dest PtrType: " << *DstPtrTy << "\n");

  // load the source from data array
  auto *DataSource = loadDataIndex(Builder, Add);
  Add = incrementCodeIndex(Builder, Add);
  DataSource = Builder.CreateBitCast(DataSource, SrcPtrTy);
  DEBUG(dbgs() << "\t\t-- Casted Load Operand: " << *DataSource << "\n");

  auto *SrcLd = Builder.CreateLoad(DataSource);
  DEBUG(dbgs() << "\t\t-- Loaded Load Operand: " << *SrcLd << "\n");
  assert(SrcLd->getType() == DstPtrTy && "Loaded operand type mismatch!");

  // load the dest from data array
  auto *DataDest = loadDataIndex(Builder, Add);
  Add = incrementCodeIndex(Builder, Add);
  DataDest = Builder.CreateBitCast(DataDest, DstPtrTy);
  DEBUG(dbgs() << "\t\t-- Casted Load Dest: " << *DataDest << "\n");

  Builder.CreateMemCpy(DataDest, SrcLd, Layout->getTypeAllocSize(LoadTy), DataArray->getAlignment());

  return Add;
}

Value *VirtualizationBuilder::handleStore(IRBuilder<> &Builder, const StoreInst *Store, Value *IdxAlloca) {
  assert(Store);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOldStore: " << *Store << "\n");

  auto *Src = Store->getValueOperand();
  auto *Dest = Store->getPointerOperand();

  auto *SrcPtrTy = PointerType::get(Src->getType(), 0);
  auto *DestPtrTy = PointerType::get(Dest->getType(), 0);

  DEBUG(dbgs() << "\t\t-- Source PtrType: " << *SrcPtrTy << "\n");
  DEBUG(dbgs() << "\t\t-- Dest PtrType: " << *DestPtrTy << "\n");

  Value *SrcVal = nullptr;

  // load the source
  if(!isa<Function>(Src)) {
    auto *DataSource = loadDataIndex(Builder, Add);
    Add = incrementCodeIndex(Builder, Add);
    DataSource = Builder.CreateBitCast(DataSource, SrcPtrTy);
    DEBUG(dbgs() << "\t\t-- Casted Source GEP: " << *DataSource << "\n");
    SrcVal = Builder.CreateLoad(DataSource);
    DEBUG(dbgs() << "\t\t-- Loaded Source: " << *SrcVal << "\n");
  } else {
    // we have a function -> load the ptr
    SrcVal = Store->getModule()->getFunction(Src->getName());
  }

  // load the dest
  auto *DataDest = loadDataIndex(Builder, Add);
  Add = incrementCodeIndex(Builder, Add);
  DataDest = Builder.CreateBitCast(DataDest, DestPtrTy);
  auto *DestLd = Builder.CreateLoad(DataDest);
  DEBUG(dbgs() << "\t\t-- Casted Dest GEP: " << *DataDest << "\n");
  DEBUG(dbgs() << "\t\t-- Loaded Dest: " << *DestLd << "\n");

  auto *AI = getOrCreateAlloca(Builder, SrcVal->getType());
  Builder.CreateStore(SrcVal, AI);
  auto *Ld = Builder.CreateLoad(AI);

  auto *S = Builder.CreateStore(Ld, DestLd);
  S->setName(Store->getName());

  return Add;
}

Value *VirtualizationBuilder::handleFCmp(IRBuilder<> &Builder, const FCmpInst *FCmp, Value *IdxAlloca) {
  assert(FCmp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *Op0 = FCmp->getOperand(0);
  auto *Op0Ty = Op0->getType();
  auto *Op1 = FCmp->getOperand(1);
  auto *Op1Ty = Op1->getType();

  auto *LHS = loadValueFromDataArray(Builder, Op0Ty, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *RHS = loadValueFromDataArray(Builder, Op1Ty, Add);
  Add = incrementCodeIndex(Builder, Add);

  DEBUG(dbgs() << "LHS:\t" << *LHS << ", RHS:\t" << *RHS << "\n");

  auto *ResCmp = Builder.CreateFCmp(FCmp->getPredicate(), LHS, RHS);
  auto *ResFlt = cast<FCmpInst>(ResCmp);
  if (isa<FPMathOperator>(ResFlt)) {
    ResFlt->copyFastMathFlags(FCmp);
  }

  ResCmp->setName(FCmp->getName());

  return storeResult(Builder, ResCmp, Add);
}

Value *VirtualizationBuilder::handleICmp(IRBuilder<> &Builder, const ICmpInst *ICmp, Value *IdxAlloca) {
  assert(ICmp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *Op0 = ICmp->getOperand(0);
  auto *Op0Ty = Op0->getType();
  auto *Op1 = ICmp->getOperand(1);
  auto *Op1Ty = Op1->getType();

  auto *LHS = loadValueFromDataArray(Builder, Op0Ty, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *RHS = loadValueFromDataArray(Builder, Op1Ty, Add);
  Add = incrementCodeIndex(Builder, Add);

  DEBUG(dbgs() << "LHS:\t" << *LHS << ", RHS:\t" << *RHS << "\n");

  auto *ResCmp = Builder.CreateICmp(ICmp->getPredicate(), LHS, RHS);
  auto *ResI = cast<ICmpInst>(ResCmp);
  if (isa<FPMathOperator>(ResI)) {
    ResI->copyFastMathFlags(ICmp);
  }

  ResCmp->setName(ICmp->getName());

  return storeResult(Builder, ResI, Add);
}

Value *VirtualizationBuilder::handleSwitch(IRBuilder<> &Builder,
                                           const SwitchInst *Switch,
                                           Function *NewFunc,
                                           BasicBlock *SwitchHead,
                                           Value *IdxAlloca) {
  assert(Switch);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  // <switch> <cond> <default> <case target>*
  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto &Context = Builder.getContext();
  auto *Int16Ty = Builder.getInt16Ty();
  auto *ConstantZero = Builder.getInt64(0);

  auto *CondTy = Switch->getCondition()->getType();

  auto *CondLD = loadValueFromDataArray(Builder, CondTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  // default
  auto *Dflt = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
  Add = incrementCodeIndex(Builder, Add);
  auto *BBDflt = BasicBlock::Create(Context, "sw_switch_dflt", NewFunc);

  auto *AI = getOrCreateAlloca(Builder, Int16Ty);
  // we have to pre-load all targets so we don't break instruction dominance
  std::vector<LoadInst *> Targets;
  for (auto I = 0u; I < Switch->getNumCases(); ++I) {
    auto *Target = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Add = incrementCodeIndex(Builder, Add);
    Builder.CreateMemCpy(AI, Target, Layout->getTypeAllocSize(Int16Ty), DataArray->getAlignment());
    auto *TargetIdx = Builder.CreateLoad(AI);

    Targets.emplace_back(TargetIdx);
  }

  auto *SW = Builder.CreateSwitch(CondLD, BBDflt, Switch->getNumCases());
  SW->setName(Switch->getName());

  auto I = 0u;
  for (auto CaseIt : Switch->cases()) {
    auto *BBTarg = BasicBlock::Create(Context, "sw_switch_trgt", NewFunc);
    // cast away const'ness here
    SW->addCase(const_cast<ConstantInt *>(CaseIt.getCaseValue()), BBTarg);
    Builder.SetInsertPoint(BBTarg);

    auto *CurrentLoad = Targets.at(I);

    Builder.CreateStore(CurrentLoad, IdxAlloca);
    Builder.CreateBr(SwitchHead);

    // update index
    ++I;
  }

  Builder.SetInsertPoint(BBDflt);
  Builder.CreateMemCpy(AI, Dflt, Layout->getTypeAllocSize(Int16Ty), DataArray->getAlignment());
  auto *DefaultIdx = Builder.CreateLoad(AI);

  Builder.CreateStore(DefaultIdx, IdxAlloca);
  Builder.CreateBr(SwitchHead);

  return Add;
}

Value *VirtualizationBuilder::handleBr(IRBuilder<> &Builder,
                                       const BranchInst *Branch,
                                       Function *NewFunc,
                                       BasicBlock *SwitchHead,
                                       Value *IdxAlloca) {
  assert(Branch);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto DataAlignment = DataArray->getAlignment();
  auto *Int16Ty = Builder.getInt16Ty();
  auto &Context = Builder.getContext();
  auto *ConstantZero = Builder.getInt64(0);

  auto *AI = getOrCreateAlloca(Builder, Int16Ty);

  if (Branch->isUnconditional()) {
    auto *GEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Builder.CreateMemCpy(AI, GEP, Layout->getTypeAllocSize(Int16Ty), DataAlignment);
    auto *TargetIdx = Builder.CreateLoad(AI);

    Builder.CreateStore(TargetIdx, IdxAlloca);
    Builder.CreateBr(SwitchHead);
  } else {
    auto *CondTy = Branch->getCondition()->getType();
    // conditional loads cond variable first
    auto *LD = loadValueFromDataArray(Builder, CondTy, Add);
    Add = incrementCodeIndex(Builder, Add);

    // load instruction index for true case
    auto *TrueGEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Add = incrementCodeIndex(Builder, Add);
    Builder.CreateMemCpy(AI, TrueGEP, Layout->getTypeAllocSize(Int16Ty), DataAlignment);
    auto *TrueIdx = Builder.CreateLoad(AI);

    // load instruction index for false case
    auto *FalseGEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Builder.CreateMemCpy(AI, FalseGEP, Layout->getTypeAllocSize(Int16Ty), DataAlignment);
    auto *FalseIdx = Builder.CreateLoad(AI);
    Add = incrementCodeIndex(Builder, Add);

    // we need a true and a false block to write different values into the CodeArray-Index
    auto *TrueBB = BasicBlock::Create(Context, "sw_br_t", NewFunc);
    auto *FalseBB = BasicBlock::Create(Context, "sw_br_f", NewFunc);

    Builder.CreateCondBr(LD, TrueBB, FalseBB);

    // set up true block
    Builder.SetInsertPoint(TrueBB);
    Builder.CreateStore(TrueIdx, IdxAlloca);
    Builder.CreateBr(SwitchHead);

    // set up false block
    Builder.SetInsertPoint(FalseBB);
    Builder.CreateStore(FalseIdx, IdxAlloca);
    Builder.CreateBr(SwitchHead);
  }

  return Add;
}

Value *VirtualizationBuilder::handleIndirectBr(IRBuilder<> &Builder,
                                               const IndirectBrInst *IndBranch,
                                               Function *NewFunc,
                                               BasicBlock *SwitchHead,
                                               Value *IdxAlloca) {
  assert(IndBranch);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tIndirectBranch: " << *IndBranch << "\n");

  auto &Context = Builder.getContext();
  auto *Int16Ty = Builder.getInt16Ty();
  auto *ConstantZero = Builder.getInt64(0);

  auto *Addr = loadValueFromDataArray(Builder, IndBranch->getAddress()->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *AI = getOrCreateAlloca(Builder, Int16Ty);
  // we have to pre-load all targets so we don't break instruction dominance
  std::vector<LoadInst *> Targets;
  for (auto I = 0u; I < IndBranch->getNumDestinations(); ++I) {
    auto *Target = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Add = incrementCodeIndex(Builder, Add);
    Builder.CreateMemCpy(AI, Target, Layout->getTypeAllocSize(Int16Ty), DataArray->getAlignment());
    auto *TargetIdx = Builder.CreateLoad(AI);

    Targets.emplace_back(TargetIdx);
  }

  auto *IBr = Builder.CreateIndirectBr(Addr, IndBranch->getNumDestinations());

  for (auto Idx = 0u; Idx < IndBranch->getNumDestinations(); ++Idx) {
    auto *Orig = const_cast<BasicBlock *>(IndBranch->getDestination(Idx));
    auto *Target = BasicBlock::Create(Context, Orig->getName(), NewFunc);

    // replace uses with the new block for addresses to match
    Orig->replaceAllUsesWith(Target);

    IBr->addDestination(Target);

    Builder.SetInsertPoint(Target);

    auto *TargetIdx = Targets.at(Idx);
    Builder.CreateStore(TargetIdx, IdxAlloca);
    Builder.CreateBr(SwitchHead);
    DEBUG(dbgs() << "Block: " << *Target << "\n");
  }

  return Add;
}

Value *VirtualizationBuilder::handleSExt(IRBuilder<> &Builder, const SExtInst *SExt, Value *IdxAlloca) {
  assert(SExt);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateSExt(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *SExt, Convert, Add);
}

Value *VirtualizationBuilder::handleZExt(IRBuilder<> &Builder, const ZExtInst *ZExt, Value *IdxAlloca) {
  assert(ZExt);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateZExt(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *ZExt, Convert, Add);
}

Value *VirtualizationBuilder::handleTrunc(IRBuilder<> &Builder, const TruncInst *Trunc, Value *IdxAlloca) {
  assert(Trunc);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateTrunc(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *Trunc, Convert, Add);
}

Value *VirtualizationBuilder::handleFPTrunc(IRBuilder<> &Builder, const FPTruncInst *FPTrunc, Value *IdxAlloca) {
  assert(FPTrunc);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateFPTrunc(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *FPTrunc, Convert, Add);
}

Value *VirtualizationBuilder::handleFPExt(IRBuilder<> &Builder, const FPExtInst *FPExt, Value *IdxAlloca) {
  assert(FPExt);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateFPExt(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *FPExt, Convert, Add);
}

Value *VirtualizationBuilder::handleFPToSI(IRBuilder<> &Builder, const FPToSIInst *FPToSI, Value *IdxAlloca) {
  assert(FPToSI);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateFPToSI(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *FPToSI, Convert, Add);
}

Value *VirtualizationBuilder::handleFPToUI(IRBuilder<> &Builder, const FPToUIInst *FPToUI, Value *IdxAlloca) {
  assert(FPToUI);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateFPToUI(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *FPToUI, Convert, Add);
}

Value *VirtualizationBuilder::handleSIToFP(IRBuilder<> &Builder, const SIToFPInst *SIToFP, Value *IdxAlloca) {
  assert(SIToFP);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateSIToFP(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *SIToFP, Convert, Add);
}

Value *VirtualizationBuilder::handleUIToFP(IRBuilder<> &Builder, const UIToFPInst *UIToFP, Value *IdxAlloca) {
  assert(UIToFP);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateUIToFP(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *UIToFP, Convert, Add);
}

Value *VirtualizationBuilder::handleBitCast(IRBuilder<> &Builder, const BitCastInst *BitCast, Value *IdxAlloca) {
  assert(BitCast);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOldBitCast: " << *BitCast << "\n");

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateBitCast(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *BitCast, Convert, Add);
}

Value *VirtualizationBuilder::handleAddrSpaceCast(IRBuilder<> &Builder,
                                                  const AddrSpaceCastInst *AddrCast,
                                                  Value *IdxAlloca) {
  assert(AddrCast);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOldAddrSpaceCast: " << *AddrCast << "\n");

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateAddrSpaceCast(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *AddrCast, Convert, Add);

}

Value *VirtualizationBuilder::handlePtrToInt(IRBuilder<> &Builder, const PtrToIntInst *PtrToInt, Value *IdxAlloca) {
  assert(PtrToInt);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreatePtrToInt(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *PtrToInt, Convert, Add);
}

Value *VirtualizationBuilder::handleIntToPtr(IRBuilder<> &Builder, const IntToPtrInst *IntToPtr, Value *IdxAlloca) {
  assert(IntToPtr);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Convert = [](IRBuilder<> &B, const CastInst &Cst, Value *Val) {
    return B.CreateIntToPtr(Val, Cst.getDestTy());
  };

  return handleConversion(Builder, *IntToPtr, Convert, Add);
}

Value *VirtualizationBuilder::handleSelect(IRBuilder<> &Builder, const SelectInst *Select, Value *IdxAlloca) {
  assert(Select);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *CondTy = Select->getCondition()->getType();
  auto *TrueTy = Select->getTrueValue()->getType();
  auto *FalseTy = Select->getFalseValue()->getType();

  auto *Cond = loadValueFromDataArray(Builder, CondTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *TrueVal = loadValueFromDataArray(Builder, TrueTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *FalseVal = loadValueFromDataArray(Builder, FalseTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Res = Builder.CreateSelect(Cond, TrueVal, FalseVal);
  Res->setName(Select->getName());

  return storeResult(Builder, Res, Add);
}

Value *VirtualizationBuilder::handleRet(IRBuilder<> &Builder,
                                        const ReturnInst *Return,
                                        Function *NewFunc,
                                        Value *IdxAlloca) {
  assert(Return);
  assert(NewFunc);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOld ReturnInst: " << *Return << "\n");

  // get type information
  auto *RetType = NewFunc->getReturnType();
  DEBUG(dbgs() << "\t\t-- RetType: " << *RetType << "\n");

  if (RetType->isVoidTy()) {
    auto *Free = Builder.CreateCall(FreeFunc, { DataArray });
    Free->setAttributes(FreeFunc->getAttributes());
    Builder.CreateRetVoid();
    return Add;
  }

  auto *RetVal = loadValueFromDataArray(Builder, RetType, Add);
  Add = incrementCodeIndex(Builder, Add);

  // clean the array on return
  auto *Free = Builder.CreateCall(FreeFunc, { DataArray });
  Free->setAttributes(FreeFunc->getAttributes());

  Builder.CreateRet(RetVal);

  return Add;
}

Value *VirtualizationBuilder::handleAdd(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    DEBUG(dbgs() << "Adding types: " << *(L->getType()) << " and " << *(R->getType()) << "\n");
    return B.CreateAdd(L, R, "", BinOp.hasNoUnsignedWrap(), BinOp.hasNoSignedWrap());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleFAdd(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    DEBUG(dbgs() << "Adding types: " << *(L->getType()) << " and " << *(R->getType()) << "\n");
    return B.CreateFAdd(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleSub(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateSub(L, R, "", BinOp.hasNoUnsignedWrap(), BinOp.hasNoSignedWrap());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleFSub(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateFSub(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleMul(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateMul(L, R, "", BinOp.hasNoUnsignedWrap(), BinOp.hasNoSignedWrap());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleFMul(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateFMul(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleUDiv(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateUDiv(L, R, "", BinOp.isExact());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);

}

Value *VirtualizationBuilder::handleSDiv(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateSDiv(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleFDiv(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateFDiv(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleAnd(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateAnd(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleOr(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateOr(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleXor(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateXor(L, R);
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleShl(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateShl(L, R, "", BinOp.hasNoUnsignedWrap(), BinOp.hasNoSignedWrap());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleLShr(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateLShr(L, R, "", BinOp.isExact());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleAShr(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateAShr(L, R, "", BinOp.isExact());
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleURem(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateURem(L, R, "");
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleSRem(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateSRem(L, R, "");
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleFRem(IRBuilder<> &Builder, const BinaryOperator *BinOp, Value *IdxAlloca) {
  assert(BinOp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  const auto Calculate = [](IRBuilder<> &B, const BinaryOperator &BinOp, Value *L, Value *R) {
    return B.CreateFRem(L, R, "");
  };

  return handleArithmetic(Builder, *BinOp, Calculate, Add);
}

Value *VirtualizationBuilder::handleGetElementPtr(IRBuilder<> &Builder,
                                                  const GetElementPtrInst *GEP,
                                                  Value *IdxAlloca) {
  assert(GEP);
  assert(IdxAlloca);

  DEBUG(dbgs() << "\tOldGEP: " << *GEP << "\n");
  DEBUG(dbgs() << "\t\t-- Type: " << *(GEP->getType()) << "\n");
  DEBUG(dbgs() << "\t\t-- Result Elem Type: " << *(GEP->getResultElementType()) << "\n");
  auto *PtrTy = PointerType::get(GEP->getPointerOperandType(), 0);

  DEBUG(dbgs() << "\t\t-- Ptr Type: " << *PtrTy << "\n");

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *Ptr = loadDataIndex(Builder, Add);
  Add = incrementCodeIndex(Builder, Add);
  Ptr = Builder.CreateBitCast(Ptr, PtrTy);
  DEBUG(dbgs() << "\t\t-- Casted GEP: " << *Ptr << "\n");

  auto *PtrLd = Builder.CreateLoad(Ptr);
  DEBUG(dbgs() << "\t\t-- Loaded GEP: " << *PtrLd << "\n");

  std::vector<Value *> Indices;
  for (auto Iter = 0u; Iter < GEP->getNumIndices(); ++Iter) {
    auto *Op = GEP->getOperand(1 + Iter);
    DEBUG(dbgs() << "\tOldOp: " << *Op << "\n");

    if (isa<Constant>(Op)) {
      DEBUG(dbgs() << "\t-- ConstantExpr!\n");
      Indices.emplace_back(Op);
    } else {
      auto *OpPtrTy = PointerType::get(Op->getType(), 0);
      auto *OpGEP = loadDataIndex(Builder, Add);
      Add = incrementCodeIndex(Builder, Add);
      OpGEP = Builder.CreateBitCast(OpGEP, OpPtrTy);
      DEBUG(dbgs() << "\t\t-- Casted Operand GEP: " << *OpGEP << "\n");

      auto *LD = Builder.CreateLoad(OpGEP);
      DEBUG(dbgs() << "\t\t-- Loaded Operand: " << *LD << "\n");

      Indices.emplace_back(LD);
    }
  }

  assert(Indices.size() == GEP->getNumIndices() && "Index count does not match!");

  Value *NewGEP = nullptr;
  if (GEP->isInBounds()) {
    NewGEP = Builder.CreateInBoundsGEP(PtrLd, makeArrayRef(Indices));
  } else {
    NewGEP = Builder.CreateGEP(PtrLd, makeArrayRef(Indices));
  }

  NewGEP->setName(GEP->getName());

  auto *AI = getOrCreateAlloca(Builder, NewGEP->getType());
  Builder.CreateStore(NewGEP, AI);
  auto *Ld = Builder.CreateLoad(AI);

  return storeResult(Builder, Ld, Add);
}

Value *VirtualizationBuilder::handleCall(IRBuilder<> &Builder, const CallInst *Call, Value *IdxAlloca) {
  assert(Call);
  assert(IdxAlloca);

  auto &Context = Builder.getContext();
  Value *CallTarget = Call->getCalledFunction();
  auto *RetTy = Call->getFunctionType()->getReturnType();
  const auto ParamCount = Call->getNumArgOperands();

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  if (!CallTarget) {
    CallTarget = loadValueFromDataArray(Builder, Call->getCalledValue()->getType(), Add);
    Add = incrementCodeIndex(Builder, Add);
  }

  DEBUG(dbgs() << "\tOld Call: " << *Call << "\n");

  // load parameters
  std::vector<Value *> Args;
  for (auto ArgOpIdx = 0u; ArgOpIdx < ParamCount; ++ArgOpIdx) {
    // get index into data array
    auto *ArgOperand = Call->getArgOperand(ArgOpIdx);
    DEBUG(dbgs() << "\t\t- Param at index " << std::to_string(ArgOpIdx) << " = " << *ArgOperand << "\n");

    if(ArgOperand->getType()->isMetadataTy()) {
      Args.emplace_back(ArgOperand);
      continue;
    }

    if (auto *CalledFunc = dyn_cast<Function>(CallTarget)) {
      // some intrinsics (e.g. llvm.eh.typeid) check that their value is a GlobalVariable
      // thus, we cannot load them into the data array
      if (CalledFunc->isIntrinsic()) {
        if (isa<GlobalVariable>(ArgOperand->stripPointerCasts())) {
          DEBUG(dbgs() << "\t\t\t param is global variable!\n");
          Args.emplace_back(ArgOperand);
          continue;
        }

        if (CalledFunc->getName().startswith("llvm.memcpy")
            && (ArgOpIdx == ConstantIndices::MEMCPY_ALIGNMENT_INDEX
                || ArgOpIdx == ConstantIndices::MEMCPY_VOLATILE_INDEX)) {
          DEBUG(dbgs() << "\t\t\t param is alignment/volatile to memcpy!\n");
          Args.emplace_back(ArgOperand);
          continue;
        } else if (CalledFunc->getName().startswith("llvm.memset")
            && (ArgOpIdx == ConstantIndices::MEMSET_ALIGNMENT_INDEX
                || ArgOpIdx == ConstantIndices::MEMSET_VOLATILE_INDEX)) {
          DEBUG(dbgs() << "\t\t\t param is alignment/volatile to memset!\n");
          Args.emplace_back(ArgOperand);
          continue;
        } else if (CalledFunc->getName().startswith("llvm.memmove")
            && (ArgOpIdx == ConstantIndices::MEMMOVE_ALIGNMENT_INDEX
                || ArgOpIdx == ConstantIndices::MEMMOVE_VOLATILE_INDEX)) {
          DEBUG(dbgs() << "\t\t\t param is alignment/volatile to memmove!\n");
          Args.emplace_back(ArgOperand);
          continue;
        }
      }
    }

    // load param from data array
    auto *Param = loadValueFromDataArray(Builder, ArgOperand->getType(), Add);
    Add = incrementCodeIndex(Builder, Add);

    // add as paramter for the function call
    Args.emplace_back(Param);
  }

  assert(Call->getNumArgOperands() == Args.size() && "Parameter count does not match for CallInst!");


  if(Call->getCalledValue()->getName().equals("__cxa_throw")) {
    auto *Free = Builder.CreateCall(FreeFunc, { DataArray });
    Free->setAttributes(FreeFunc->getAttributes());
  }

  auto *NewCall = Builder.CreateCall(CallTarget, makeArrayRef(Args));
  if (!RetTy->isVoidTy()) {
    Add = storeResult(Builder, NewCall, Add);
  }

  NewCall->setName(Call->getName());

  // copy all attributes
  copyCallAttributes(Context, Call, NewCall);

  // get metadata from old call
  SmallVector<std::pair<unsigned int, MDNode*>, 8> Meta;
  Call->getAllMetadata(Meta);

  for(const auto &Pair : Meta) {
    NewCall->setMetadata(Pair.first, Pair.second);
  }

  if (Call->isTailCall()) {
    NewCall->setTailCall(true);
    NewCall->setTailCallKind(Call->getTailCallKind());
  }

  DEBUG(dbgs() << "\t\t- NewCall: " << *NewCall << "\n");

  return Add;
}

Value *VirtualizationBuilder::handleInvoke(IRBuilder<> &Builder,
                                           const InvokeInst *Invoke,
                                           Function *NewFunc,
                                           BasicBlock *SwitchHead,
                                           Value *IdxAlloca) {
  assert(Invoke);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto &Context = Builder.getContext();
  auto *Int16Ty = Builder.getInt16Ty();
  auto *ConstantZero = Builder.getInt16(0);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOld Invoke: " << *Invoke << "\n");

  auto *OldLpad = Invoke->getLandingPadInst();
  DEBUG(dbgs() << "\t\t- LandingPad: " << *OldLpad << "\n");
  DEBUG(dbgs() << "\t\t- LandingPad Type: " << *(OldLpad->getType()) << "\n");

  auto *RetTy = Invoke->getFunctionType()->getReturnType();
  DEBUG(dbgs() << "\t\t- Return Type: " << *RetTy << "\n");

  Value *CallTarget = Invoke->getCalledFunction();
  const auto ParamCount = Invoke->getNumArgOperands();

  if (!CallTarget) {
    // virtual function call
    CallTarget = loadValueFromDataArray(Builder, Invoke->getCalledValue()->getType(), Add);
    Add = incrementCodeIndex(Builder, Add);
  }

  DEBUG(dbgs() << "\t\t- Calling: " << *CallTarget << "\n");

  // load parameters
  std::vector<Value *> Args;
  for (auto ArgOpIdx = 0u; ArgOpIdx < ParamCount; ++ArgOpIdx) {
    // get index into data array
    auto *ArgOperand = Invoke->getArgOperand(ArgOpIdx);
    DEBUG(dbgs() << "Param at index " << std::to_string(ArgOpIdx) << " = " << *ArgOperand << "\n");

    if (auto *CalledFunc = dyn_cast<Function>(CallTarget)) {
      // some intrinsics (e.g. llvm.eh.typeid) check that their value is a GlobalVariable
      // thus, we cannot load them into the data array
      if (CalledFunc->isIntrinsic() && isa<GlobalVariable>(ArgOperand->stripPointerCasts())) {
        DEBUG(dbgs() << "\t param is global variable!\n");
        Args.emplace_back(ArgOperand);
        continue;
      }
    }

    // load param from data array
    auto *Param = loadValueFromDataArray(Builder, ArgOperand->getType(), Add);
    Add = incrementCodeIndex(Builder, Add);

    // add as paramter for the function call
    Args.emplace_back(Param);
  }

  assert(Invoke->getNumArgOperands() == Args.size() && "Parameter count does not match for CallInst!");

  // create two blocks for invoke targets
  auto *NormalDest = BasicBlock::Create(Context, "sw_inv_normal", NewFunc);
  auto *LpadDest = BasicBlock::Create(Context, "sw_inv_lpad", NewFunc);

  // load cont index
  auto *ContGEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
  auto *ContCodeIdx = Builder.CreateLoad(Int16Ty, ContGEP);
  Add = incrementCodeIndex(Builder, Add);

  // load lpad successor index
  auto *LpadGEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
  auto *LpadCodeIdx = Builder.CreateLoad(Int16Ty, LpadGEP);
  Add = incrementCodeIndex(Builder, Add);

  // load lpad result index
  auto *LpadResGEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
  auto *LpadResIdx = Builder.CreateLoad(Int16Ty, LpadResGEP);
  Add = incrementCodeIndex(Builder, Add);

  auto *NewInvoke = Builder.CreateInvoke(CallTarget, NormalDest, LpadDest, makeArrayRef(Args));
  NewInvoke->setName(Invoke->getName());
  copyCallAttributes(Context, Invoke, NewInvoke);

  // set up normal destination
  Builder.SetInsertPoint(NormalDest);

  // write result to the data array
  if (!RetTy->isVoidTy()) {
    Add = storeResult(Builder, NewInvoke, Add);
  }

  Builder.CreateStore(ContCodeIdx, IdxAlloca);
  Builder.CreateBr(SwitchHead);

  // set up landing pad
  Builder.SetInsertPoint(LpadDest);

  auto *LP = Builder.CreateLandingPad(OldLpad->getType(), OldLpad->getNumClauses());
  if (OldLpad->isCleanup()) {
    DEBUG(dbgs() << "\t\t- Cleanup\n");
    LP->setCleanup(true);
  }

  for (auto Cl = 0u; Cl < OldLpad->getNumClauses(); ++Cl) {
    auto *Clause = OldLpad->getClause(Cl);
    DEBUG(dbgs() << "\t\t- Clause " << std::to_string(Cl) << ": " << *Clause << "\n");
    LP->addClause(Clause);
  }

  auto *ResStoreGEP = Builder.CreateInBoundsGEP(DataArray, {LpadResIdx});
  storeResultDest(Builder, LP, ResStoreGEP);

  return LpadCodeIdx;
}

Value *VirtualizationBuilder::handleResume(IRBuilder<> &Builder, const ResumeInst *Resume, Value *IdxAlloca) {
  assert(Resume);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *RSTy = Resume->getType();
  auto *ValTy = Resume->getValue()->getType();
  DEBUG(dbgs() << "\tOld Resume: " << *Resume << "\n");
  DEBUG(dbgs() << "\t\tResume Type: " << *RSTy << "\n");
  DEBUG(dbgs() << "\t\tValue Type: " << *ValTy << "\n");

  auto *Val = loadValueFromDataArray(Builder, ValTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  Builder.CreateStore(Add, IdxAlloca);

  auto *Free = Builder.CreateCall(FreeFunc, { DataArray });
  Free->setAttributes(FreeFunc->getAttributes());

  Builder.CreateResume(Val);

  return Add;
}

Value *VirtualizationBuilder::handleCatchReturn(IRBuilder<> &Builder,
                                                const CatchReturnInst *CatchReturn,
                                                Function *NewFunc,
                                                BasicBlock *SwitchHead,
                                                Value *IdxAlloca) {
  assert(CatchReturn);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto &Context = Builder.getContext();
  auto *ConstantZero = Builder.getInt64(0);
  auto *Int64Ty = Builder.getInt64Ty();

  auto *BBCatchRet = BasicBlock::Create(Context, "", NewFunc);
  Builder.CreateCatchRet(CatchReturn->getCatchPad(), BBCatchRet);
  Builder.CreateBr(BBCatchRet);

  Builder.SetInsertPoint(BBCatchRet);
  auto *AI = getOrCreateAlloca(Builder, Int64Ty);
  auto *GEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
  Builder.CreateMemCpy(AI, GEP, Layout->getTypeAllocSize(Int64Ty), DataArray->getAlignment());
  auto *TargetIdx = Builder.CreateLoad(AI);

  Builder.CreateStore(TargetIdx, IdxAlloca);
  Builder.CreateBr(SwitchHead);

  return Add;
}

Value *VirtualizationBuilder::handleCatchPad(IRBuilder<> &Builder,
                                             const CatchPadInst *CatchPad,
                                             Value *IdxAlloca) {
  assert(CatchPad);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *ParentPad = loadValueFromDataArray(Builder, CatchPad->getParentPad()->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  std::vector<Value *> Args;
  for (auto Idx = 0u; Idx < CatchPad->getNumArgOperands(); ++Idx) {
    auto *Arg = loadValueFromDataArray(Builder, CatchPad->getArgOperand(Idx)->getType(), Add);
    Add = incrementCodeIndex(Builder, Add);

    Args.emplace_back(Arg);
  }

  auto *CP = Builder.CreateCatchPad(ParentPad, makeArrayRef(Args));
  CP->setName(CatchPad->getName());

  return storeResult(Builder, CP, Add);
}

Value *VirtualizationBuilder::handleCleanupPad(IRBuilder<> &Builder,
                                               const CleanupPadInst *CleanupPad,
                                               Value *IdxAlloca) {
  assert(CleanupPad);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *ParentPad = loadValueFromDataArray(Builder, CleanupPad->getParentPad()->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  std::vector<Value *> Args;
  for (auto Idx = 0u; Idx < CleanupPad->getNumArgOperands(); ++Idx) {
    auto *Arg = loadValueFromDataArray(Builder, CleanupPad->getArgOperand(Idx)->getType(), Add);
    Add = incrementCodeIndex(Builder, Add);

    Args.emplace_back(Arg);
  }

  auto *CP = Builder.CreateCleanupPad(ParentPad, makeArrayRef(Args));

  return storeResult(Builder, CP, Add);
}

Value *VirtualizationBuilder::handleCleanupRet(IRBuilder<> &Builder,
                                               const CleanupReturnInst *CleanupRet,
                                               Function *NewFunc,
                                               BasicBlock *SwitchHead,
                                               Value *IdxAlloca) {
  assert(CleanupRet);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto &Context = Builder.getContext();
  auto *Int64Ty = Builder.getInt64Ty();
  auto *ConstantZero = Builder.getInt64(0);

  auto *ParentPad = loadValueFromDataArray(Builder, CleanupRet->getCleanupPad()->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *BBUnwind = BasicBlock::Create(Context, "", NewFunc);
  auto *CP = Builder.CreateCleanupRet(CleanupRet->getCleanupPad(), BBUnwind);
  CP->setName(CleanupRet->getName());

  Builder.SetInsertPoint(BBUnwind);
  auto *AI = getOrCreateAlloca(Builder, Int64Ty);
  auto *GEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
  Builder.CreateMemCpy(AI, GEP, Layout->getTypeAllocSize(Int64Ty), DataArray->getAlignment());
  auto *TargetIdx = Builder.CreateLoad(AI);

  Builder.CreateStore(TargetIdx, IdxAlloca);

  Builder.CreateBr(SwitchHead);

  return Add;
}

Value *VirtualizationBuilder::handleCatchSwitch(IRBuilder<> &Builder,
                                                const CatchSwitchInst *CatchSwitch,
                                                Function *NewFunc,
                                                BasicBlock *SwitchHead,
                                                Value *IdxAlloca) {
  assert(CatchSwitch);
  assert(NewFunc);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto &Context = Builder.getContext();
  auto *Int64Ty = Builder.getInt64Ty();
  auto *ConstantZero = Builder.getInt64(0);

  auto *ParentPad = loadValueFromDataArray(Builder, CatchSwitch->getParentPad()->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *BBUnwind = BasicBlock::Create(Context, "", NewFunc);
  auto *CS = Builder.CreateCatchSwitch(ParentPad, BBUnwind, CatchSwitch->getNumHandlers());
  CS->setName(CatchSwitch->getName());

  Add = storeResult(Builder, CS, Add);

  // setup unwind destination
  if (CatchSwitch->hasUnwindDest()) {
    Builder.SetInsertPoint(BBUnwind);

    auto *AI = getOrCreateAlloca(Builder, Int64Ty);
    auto *GEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Builder.CreateMemCpy(AI, GEP, Layout->getTypeAllocSize(Int64Ty), DataArray->getAlignment());
    auto *TargetIdx = Builder.CreateLoad(AI);

    Builder.CreateStore(TargetIdx, IdxAlloca);
    Builder.CreateBr(SwitchHead);
  }

  for (auto Idx = 0u; Idx < CatchSwitch->getNumSuccessors(); ++Idx) {
    auto *BBSucc = BasicBlock::Create(Context, "", NewFunc);
    CS->setSuccessor(Idx, BBSucc);

    Builder.SetInsertPoint(BBSucc);
    auto *AI = getOrCreateAlloca(Builder, Int64Ty);
    auto *GEP = Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, Add});
    Add = incrementCodeIndex(Builder, Add);
    Builder.CreateMemCpy(AI, GEP, Layout->getTypeAllocSize(Int64Ty), DataArray->getAlignment());
    auto *TargetIdx = Builder.CreateLoad(AI);

    Builder.CreateStore(TargetIdx, IdxAlloca);
    Builder.CreateBr(SwitchHead);
  }

  return Add;
}

Value *VirtualizationBuilder::handleShuffleVector(IRBuilder<> &Builder,
                                                  const ShuffleVectorInst *ShuffleVector,
                                                  Value *IdxAlloca) {
  assert(ShuffleVector);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOld ShuffleVectorInst: " << *ShuffleVector << "\n");

  auto *Vec1 = loadValueFromDataArray(Builder, ShuffleVector->getOperand(0)->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Vec2 = loadValueFromDataArray(Builder, ShuffleVector->getOperand(1)->getType(), Add);
  Add = incrementCodeIndex(Builder, Add);

  // shuffle mask has to be constant
  auto *SV = Builder.CreateShuffleVector(Vec1, Vec2, ShuffleVector->getMask());
  SV->setName(ShuffleVector->getName());

  return storeResult(Builder, SV, Add);
}

Value *VirtualizationBuilder::handleExtractElement(IRBuilder<> &Builder,
                                                   const ExtractElementInst *ExtractElement,
                                                   Value *IdxAlloca) {
  assert(ExtractElement);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *VecTy = ExtractElement->getVectorOperand()->getType();
  auto *IdxTy = ExtractElement->getIndexOperand()->getType();
  DEBUG(dbgs() << "\tOld ExtractElement: " << *ExtractElement << "\n");
  DEBUG(dbgs() << "\t\t- Type: " << *(ExtractElement->getType()) << "\n");
  DEBUG(dbgs() << "\t\t- Vector Type: " << *VecTy << "\n");
  DEBUG(dbgs() << "\t\t- Index Type: " << *IdxTy << "\n");

  auto *Vec = loadValueFromDataArray(Builder, VecTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Idx = loadValueFromDataArray(Builder, IdxTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *EE = Builder.CreateExtractElement(Vec, Idx);
  EE->setName(ExtractElement->getName());

  return storeResult(Builder, EE, Add);
}

Value *VirtualizationBuilder::handleInsertElement(IRBuilder<> &Builder,
                                                  const InsertElementInst *InsertElement,
                                                  Value *IdxAlloca) {
  assert(InsertElement);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  assert(InsertElement->getNumOperands() == 3 && "InsertElement has more than 3 operands!");

  auto *VecTy = InsertElement->getOperand(0)->getType();
  auto *ValTy = InsertElement->getOperand(1)->getType();
  auto *IdxTy = InsertElement->getOperand(2)->getType();
  DEBUG(dbgs() << "\tOld InsertElement: " << *InsertElement << "\n");
  DEBUG(dbgs() << "\t\t- Type: " << *(InsertElement->getType()) << "\n");
  DEBUG(dbgs() << "\t\t- Vector Type: " << *VecTy << "\n");
  DEBUG(dbgs() << "\t\t- Value Type: " << *ValTy << "\n");
  DEBUG(dbgs() << "\t\t- Index Type: " << *IdxTy << "\n");

  auto *Vec = loadValueFromDataArray(Builder, VecTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Val = loadValueFromDataArray(Builder, ValTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Idx = loadValueFromDataArray(Builder, IdxTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *IE = Builder.CreateInsertElement(Vec, Val, Idx);
  IE->setName(InsertElement->getName());

  return storeResult(Builder, IE, Add);
}

Value *VirtualizationBuilder::handleExtractValue(IRBuilder<> &Builder,
                                                 const ExtractValueInst *ExtractValue,
                                                 Value *IdxAlloca) {
  assert(ExtractValue);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *AggTy = ExtractValue->getAggregateOperand()->getType();
  DEBUG(dbgs() << "\tOld ExtractValue: " << *ExtractValue << "\n");
  DEBUG(dbgs() << "\t\t- Type: " << *(ExtractValue->getType()) << "\n");
  DEBUG(dbgs() << "\t\t- Aggregate Type: " << *AggTy << "\n");

  auto *Agg = loadValueFromDataArray(Builder, AggTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *EV = Builder.CreateExtractValue(Agg, ExtractValue->getIndices());
  EV->setName(ExtractValue->getName());

  return storeResult(Builder, EV, Add);
}

Value *VirtualizationBuilder::handleInsertValue(IRBuilder<> &Builder,
                                                const InsertValueInst *InsertValue,
                                                Value *IdxAlloca) {
  assert(InsertValue);
  assert(IdxAlloca);

  auto *AggTy = InsertValue->getAggregateOperand()->getType();
  auto *ValTy = InsertValue->getInsertedValueOperand()->getType();
  DEBUG(dbgs() << "\tOld InsertValue: " << *InsertValue << "\n");
  DEBUG(dbgs() << "\t\tAggregate Type: " << *AggTy << "\n");
  DEBUG(dbgs() << "\t\tValue Type: " << *ValTy << "\n");

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  // load operands
  auto *Agg = loadValueFromDataArray(Builder, AggTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Val = loadValueFromDataArray(Builder, ValTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *IV = Builder.CreateInsertValue(Agg, Val, InsertValue->getIndices());
  IV->setName(InsertValue->getName());

  return storeResult(Builder, IV, Add);
}

Value *VirtualizationBuilder::handleFence(IRBuilder<> &Builder, const FenceInst *Fence, Value *IdxAlloca) {
  assert(Fence);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *F = Builder.CreateFence(Fence->getOrdering(), Fence->getSynchScope());
  F->setName(Fence->getName());

  return Add;
}

Value *VirtualizationBuilder::handleAtomicRMW(IRBuilder<> &Builder, const AtomicRMWInst *AtomicRMW, Value *IdxAlloca) {
  assert(AtomicRMW);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *PtrTy = AtomicRMW->getPointerOperand()->getType();
  auto *ValTy = AtomicRMW->getValOperand()->getType();

  auto *Ptr = loadValueFromDataArray(Builder, PtrTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Val = loadValueFromDataArray(Builder, ValTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *ARMW = Builder.CreateAtomicRMW(AtomicRMW->getOperation(),
                                       Ptr,
                                       Val,
                                       AtomicRMW->getOrdering(),
                                       AtomicRMW->getSynchScope());

  ARMW->setName(AtomicRMW->getName());

  return storeResult(Builder, ARMW, Add);
}

Value *VirtualizationBuilder::handleAtomicCmpXchg(IRBuilder<> &Builder,
                                                  const AtomicCmpXchgInst *AtomicCmp,
                                                  Value *IdxAlloca) {
  assert(AtomicCmp);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *PtrTy = AtomicCmp->getPointerOperand()->getType();
  auto *CmpTy = AtomicCmp->getCompareOperand()->getType();
  auto *ValTy = AtomicCmp->getNewValOperand()->getType();

  auto *Ptr = loadValueFromDataArray(Builder, PtrTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Cmp = loadValueFromDataArray(Builder, CmpTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *Val = loadValueFromDataArray(Builder, ValTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *ACX = Builder.CreateAtomicCmpXchg(Ptr,
                                          Cmp,
                                          Val,
                                          AtomicCmp->getSuccessOrdering(),
                                          AtomicCmp->getFailureOrdering(),
                                          AtomicCmp->getSynchScope());

  ACX->setName(AtomicCmp->getName());

  ACX->setVolatile(AtomicCmp->isVolatile());
  ACX->setWeak(AtomicCmp->isWeak());

  return Add;
}

Value *VirtualizationBuilder::handleVAArg(IRBuilder<> &Builder, const VAArgInst *VAArg, Value *IdxAlloca) {
  assert(VAArg);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  DEBUG(dbgs() << "\tOldVAArg: " << *VAArg << "\n");

  auto *PtrTy = VAArg->getPointerOperand()->getType();
  DEBUG(dbgs() << "\t\t-- PtrTy: " << *PtrTy << "\n");

  auto *ValTy = VAArg->getType();
  DEBUG(dbgs() << "\t\t-- ValTy: " << *ValTy << "\n");

  auto *Ptr = loadValueFromDataArray(Builder, PtrTy, Add);
  Add = incrementCodeIndex(Builder, Add);

  auto *VA = Builder.CreateVAArg(Ptr, ValTy);
  VA->setName(VAArg->getName());

  return storeResult(Builder, VA, Add);
}

Value *VirtualizationBuilder::handleSafeGuard(IRBuilder<> &Builder,
                                              GlobalVariable *ToCheck,
                                              Function *XorFunc,
                                              BasicBlock *Unreach,
                                              const uint64_t Len,
                                              BasicBlock *SwitchHead,
                                              Value *IdxAlloca) {
  assert(ToCheck);
  assert(XorFunc);
  assert(Unreach);
  assert(SwitchHead);
  assert(IdxAlloca);

  auto *IdxLoadInner = Builder.CreateLoad(IdxAlloca);
  auto *Add = incrementCodeIndex(Builder, IdxLoadInner);

  auto *LoadedCS = loadValueFromDataArray(Builder, Type::getInt16Ty(Builder.getContext()), Add);
  Add = incrementCodeIndex(Builder, Add);

  Builder.CreateStore(Add, IdxAlloca);

  auto &Context = Builder.getContext();

  auto *Int16PtrTy = Type::getInt16PtrTy(Context);
  auto *Cst = Builder.CreateBitCast(ToCheck, Int16PtrTy);
  auto *CS = Builder.CreateCall(XorFunc, {Cst, Builder.getInt64(Len)});

  auto *Cmp = Builder.CreateICmpEQ(LoadedCS, CS, "res");

  Builder.CreateCondBr(Cmp, SwitchHead, Unreach);

  return Add;
}