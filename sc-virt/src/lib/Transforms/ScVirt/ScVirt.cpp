#include "llvm/Transforms/ScVirt/ScVirt.hpp"

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <fstream>
#include <queue>
#include <string>
#include <vector>

#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "llvm/Transforms/ScVirt/Json.hpp"
#include "llvm/Transforms/ScVirt/OpcodeGenerator.hpp"

#define DEBUG_TYPE "ScVirt"
using namespace llvm;

/**
 * List of functions that will be transformed by this pass.
 * NOTE: Does not work for C++ code because of name mangling.
 */
static cl::list<std::string>
    TargetFunctions("virtualize", cl::desc("List of functions to virtualize"));

/**
 * Connectivity parameter for the safe-guards.
 */
static cl::opt<uint32_t>
    Connectivity("connectivity", cl::desc("The connectivity of safe-guards"),
                 cl::init(0));

static cl::opt<bool> SensitiveChecks(
    "senschecks",
    cl::desc("Functions marked sensitive can check other functions"),
    cl::init(false));

static cl::opt<std::string>
    DumpStatFile("dump-file",
                 cl::desc("File path to dump pass stats in json format."),
                 cl::init(""));

uint64_t ActualConnectivity = 0;
uint64_t GuardsAdded = 0;
uint64_t ProtectedInsts = 0;

/**
 * General form to implement virtualiazation of an instruction in the switch
 * statement below. Cast Elem.OldInst to the actual type and dispatch to the
 * VirtBuilder.
 */
#define ADD_CASE(Type)                                                         \
  case Instruction::Type: {                                                    \
    auto *I = cast<Type##Inst>(OldInst);                                       \
    auto *NewBB = BasicBlock::Create(Context, "sw_" #Type, NewFunc);           \
    Switch->addCase(CaseValue, NewBB);                                         \
    Builder.SetInsertPoint(NewBB);                                             \
    auto *Add = VirtBuilder.handle##Type(Builder, I, IdxAlloca);               \
    Builder.CreateStore(Add, IdxAlloca);                                       \
    Builder.CreateBr(BBSwitchHead);                                            \
    break;                                                                     \
  }

/**
 * Arithmetic expressions are cast to BinaryOperator instead.
 */
#define ADD_BINOP_CASE(Type)                                                   \
  case Instruction::Type: {                                                    \
    auto *I = cast<BinaryOperator>(OldInst);                                   \
    auto *NewBB = BasicBlock::Create(Context, "sw_" #Type, NewFunc);           \
    Switch->addCase(CaseValue, NewBB);                                         \
    Builder.SetInsertPoint(NewBB);                                             \
    auto *Add = VirtBuilder.handle##Type(Builder, I, IdxAlloca);               \
    Builder.CreateStore(Add, IdxAlloca);                                       \
    Builder.CreateBr(BBSwitchHead);                                            \
    break;                                                                     \
  }

/**
 * Controlflow-changing instructions such as Br, IndirectBr, ... pass more
 * arguments to the VirtBuilder.
 */
#define ADD_CONTROLFLOW_CASE(Type)                                             \
  case Instruction::Type: {                                                    \
    auto *I = cast<Type##Inst>(OldInst);                                       \
    auto *NewBB = BasicBlock::Create(Context, "sw_" #Type, NewFunc);           \
    Switch->addCase(CaseValue, NewBB);                                         \
    Builder.SetInsertPoint(NewBB);                                             \
    VirtBuilder.handle##Type(Builder, I, NewFunc, BBSwitchHead, IdxAlloca);    \
    break;                                                                     \
  }

std::vector<Function *> ScVirt::getFunctions(Module &M) {
  std::vector<Function *> Worklist;

  if (TargetFunctions.empty()) {
    for (auto &F : M) {
      if (F.empty()) {
        continue;
      }

      DEBUG(dbgs() << "Checking " << F.getName() << "\n");
      if (F.hasFnAttribute("scvirt") || F.hasFnAttribute("scsens")) {
        DEBUG(dbgs() << "Taking " << F.getName() << "\n");
        Worklist.emplace_back(&F);
      }
    }
  } else {
    for (const auto &Name : TargetFunctions) {
      if (auto *F = M.getFunction(Name)) {
        if (!F->empty()) {
          Worklist.emplace_back(F);
        }
      }
    }
  }

  return Worklist;
}

// TODO: remove later
void ScVirt::printDataArray(IRBuilder<> &Builder, AllocaInst *DataArray,
                            uint32_t Size) {
  auto *Int8PtrTy = Type::getInt8PtrTy(Builder.getContext());

  auto *Cst = Builder.CreateBitCast(DataArray, Int8PtrTy);
  auto *PrintFn =
      DataArray->getModule()->getFunction("_Z16print_data_arrayPhi");

  Builder.CreateCall(PrintFn, {Cst, Builder.getInt32(Size)});
}

void ScVirt::printDataArrayMapping(const std::map<uint64_t, Value *> &Data) {
  dbgs() << "\t-- DataMapping:\n";
  dbgs() << "\t\tIndex\tSize\tInst\n";
  for (const auto &Pair : Data) {
    dbgs() << "\t\t" << std::to_string(Pair.first) << "\t";
    assert(Pair.second);
    auto *Ty = Pair.second->getType();
    if (Ty->isSized()) {
      dbgs() << std::to_string(Layout->getTypeAllocSize(Ty)) << "\t"
             << *Pair.second << "\n";
    } else {
      dbgs() << "None\t" << *Pair.second << "\n";
    }
  }
}

void ScVirt::printCodeMapping(
    const std::vector<CodeMap::InstToOpcode> &CodeMap) {
  dbgs() << "\t-- CodeMapping:\n"
         << "\t\tInst\n";
  auto IdxStart = 0u;
  const auto MapSize = CodeMap.size();
  for (auto I = 0u; I < MapSize; ++I) {
    const auto &Elem = CodeMap.at(I);
    const auto OpCodes = Elem.Opcode;
    const auto Len = OpCodes.size();
    if (Elem.OldInst) {
      dbgs() << "\t\t(" << std::to_string(IdxStart) << ") .. " << *Elem.OldInst
             << "\n";
      dbgs() << "\t\t\t" << *(Elem.OldInst->getType()) << "\n";
    } else {
      dbgs() << "\t\t(" << std::to_string(IdxStart) << ") .. SafeGuard\n";
    }
    dbgs() << "\t\tOpcode: [" << std::to_string(OpCodes.at(0));
    for (auto OI = 1u; OI < Len; ++OI) {
      dbgs() << ", " << std::to_string(OpCodes.at(OI));
    }

    IdxStart += Len;

    dbgs() << "]\n";
  }
}

Function *ScVirt::createNewFunction(Function &TargetFunc, Module &M) {
  // insert a copy of TargetFunction
  auto *CFunc = M.getOrInsertFunction("", TargetFunc.getFunctionType(),
                                      TargetFunc.getAttributes());
  auto *NewFunc = cast<Function>(CFunc);
  NewFunc->copyAttributesFrom(&TargetFunc);
  NewFunc->setLinkage(TargetFunc.getLinkage());
  NewFunc->setCallingConv(TargetFunc.getCallingConv());

  assert(TargetFunc.arg_size() == NewFunc->arg_size() && "Should be equal!");

  // copy function parameter names and attributes
  auto *ArgIter = &*NewFunc->arg_begin();
  auto *ArgIter2 = &*TargetFunc.arg_begin();

  const auto &NewArgList = NewFunc->getArgumentList();
  const auto &TargArgList = TargetFunc.getArgumentList();
  const auto ArgSize = NewFunc->arg_size();
  for (auto Num = 0u; Num < ArgSize; ++Num) {
    ArgIter->setName(ArgIter2->getName());
    ArgIter = NewArgList.getNext(ArgIter);
    ArgIter2 = TargArgList.getNext(ArgIter2);
  }

  return NewFunc;
}

template <typename TFunc>
inline uint16_t
ScVirt::calculateChecksum(const std::vector<CodeMap::InstToOpcode> &Insts,
                          TFunc Operator) {
  uint16_t Value = 0u;

  for (const auto &Elem : Insts) {
    for (const auto OpCode : Elem.Opcode) {
      Value = Operator(Value, OpCode);
    }
  }

  return Value;
}

uint16_t
ScVirt::calculateChecksumXor(const std::vector<CodeMap::InstToOpcode> &Insts) {
  const auto CS = calculateChecksum(
      Insts,
      [](uint16_t Acc, const uint16_t Elem) -> uint64_t { return Acc ^ Elem; });

  DEBUG(dbgs() << "Calculated Checksum = " << std::to_string(CS) << "\n");

  return CS;
}

void ScVirt::virtualizeInstruction(VirtualizationBuilder &VirtBuilder,
                                   IRBuilder<> &Builder,
                                   const CodeMap::InstToOpcode &Elem,
                                   Function *NewFunc, BasicBlock *BBSwitchHead,
                                   SwitchInst *Switch, ConstantInt *CaseValue,
                                   Value *IdxAlloca) {
  auto &Context = Builder.getContext();
  const auto *OldInst = Elem.OldInst;

  assert(OldInst && "Wrong branch taken for safe guard!");

  switch (OldInst->getOpcode()) {
    ADD_CASE(Alloca)
    ADD_CASE(FCmp)
    ADD_CASE(ICmp)
    ADD_CASE(Store)
    ADD_CASE(Load)
    ADD_CASE(SExt)
    ADD_CASE(ZExt)
    ADD_CASE(Trunc)
    ADD_CASE(FPTrunc)
    ADD_CASE(FPExt)
    ADD_CASE(FPToSI)
    ADD_CASE(FPToUI)
    ADD_CASE(SIToFP)
    ADD_CASE(UIToFP)
    ADD_CASE(BitCast)
    ADD_CASE(AddrSpaceCast)
    ADD_CASE(PtrToInt)
    ADD_CASE(IntToPtr)
    ADD_CASE(Select)
    ADD_CASE(GetElementPtr)
    ADD_CASE(Call)
    ADD_CASE(CatchPad)
    ADD_CASE(ShuffleVector)
    ADD_CASE(ExtractElement)
    ADD_CASE(InsertElement)
    ADD_CASE(ExtractValue)
    ADD_CASE(InsertValue)
    ADD_CASE(Fence)
    ADD_CASE(AtomicRMW)
    ADD_CASE(AtomicCmpXchg)
    ADD_CASE(VAArg)
    ADD_BINOP_CASE(Add)
    ADD_BINOP_CASE(FAdd)
    ADD_BINOP_CASE(Sub)
    ADD_BINOP_CASE(FSub)
    ADD_BINOP_CASE(Mul)
    ADD_BINOP_CASE(FMul)
    ADD_BINOP_CASE(UDiv)
    ADD_BINOP_CASE(SDiv)
    ADD_BINOP_CASE(FDiv)
    ADD_BINOP_CASE(And)
    ADD_BINOP_CASE(Or)
    ADD_BINOP_CASE(Xor)
    ADD_BINOP_CASE(Shl)
    ADD_BINOP_CASE(LShr)
    ADD_BINOP_CASE(AShr)
    ADD_BINOP_CASE(SRem)
    ADD_BINOP_CASE(URem)
    ADD_BINOP_CASE(FRem)
    ADD_CONTROLFLOW_CASE(Switch)
    ADD_CONTROLFLOW_CASE(IndirectBr)
    ADD_CONTROLFLOW_CASE(CatchSwitch)
  case Instruction::Unreachable: {
    auto *Unreach = cast<UnreachableInst>(OldInst);
    // this one probably can't happen because UnreachableInsts are only inserted
    // when the compiler is certain it cannot be reached -> otherwise undefined
    // behaviour
    auto *BBTmp = BasicBlock::Create(Context, "sw_Unreachable", NewFunc);
    Switch->addCase(CaseValue, BBTmp);
    Builder.SetInsertPoint(BBTmp);

    VirtBuilder.handleUnreachable(Builder, Unreach, IdxAlloca);

    break;
  }
  case Instruction::Ret: {
    auto *Return = cast<ReturnInst>(OldInst);
    auto *BBSwitchRet = BasicBlock::Create(Context, "sw_Ret", NewFunc);
    Switch->addCase(CaseValue, BBSwitchRet);
    Builder.SetInsertPoint(BBSwitchRet);

    VirtBuilder.handleRet(Builder, Return, NewFunc, IdxAlloca);

    break;
  }
  case Instruction::Br: {
    auto *BR = cast<BranchInst>(OldInst);
    auto *BBSwitchBr = BasicBlock::Create(Context, "sw_br", NewFunc);
    Switch->addCase(CaseValue, BBSwitchBr);
    Builder.SetInsertPoint(BBSwitchBr);

    VirtBuilder.handleBr(Builder, BR, NewFunc, BBSwitchHead, IdxAlloca);

    break;
  }
  case Instruction::Resume: {
    auto *Resume = cast<ResumeInst>(OldInst);
    auto *BBSwitchResume = BasicBlock::Create(Context, "sw_resume", NewFunc);
    Switch->addCase(CaseValue, BBSwitchResume);
    Builder.SetInsertPoint(BBSwitchResume);

    VirtBuilder.handleResume(Builder, Resume, IdxAlloca);

    break;
  }
  case Instruction::CatchRet: {
    auto *CatchRet = cast<CatchReturnInst>(OldInst);
    auto *BBSwitchCatchRet =
        BasicBlock::Create(Context, "sw_catch_ret", NewFunc);
    Switch->addCase(CaseValue, BBSwitchCatchRet);
    Builder.SetInsertPoint(BBSwitchCatchRet);

    VirtBuilder.handleCatchReturn(Builder, CatchRet, NewFunc, BBSwitchHead,
                                  IdxAlloca);
    break;
  }
  case Instruction::Invoke: {
    auto *Invoke = cast<InvokeInst>(OldInst);
    auto *BBSwitchInv = BasicBlock::Create(Context, "sw_inv", NewFunc);
    Switch->addCase(CaseValue, BBSwitchInv);
    Builder.SetInsertPoint(BBSwitchInv);

    auto *LpadCodeIdx = VirtBuilder.handleInvoke(Builder, Invoke, NewFunc,
                                                 BBSwitchHead, IdxAlloca);

    Builder.CreateStore(LpadCodeIdx, IdxAlloca);
    Builder.CreateBr(BBSwitchHead);

    break;
  }
  }
}

void printIP(IRBuilder<> &Builder, Module &M, AllocaInst *IdxAlloca,
             GlobalVariable *CodeArray) {
  auto *Int16Ty = Type::getInt16Ty(M.getContext());
  auto *ConstantZero = Builder.getInt64(0);

  auto *PF = M.getFunction("_Z8print_iptt");
  auto *LD = Builder.CreateLoad(Int16Ty, IdxAlloca);
  auto *V = Builder.CreateGEP(CodeArray, {ConstantZero, LD});
  auto *VLD = Builder.CreateLoad(Int16Ty, V);
  Builder.CreateCall(PF, {LD, VLD});
}

Function *
ScVirt::createEmulator(const std::vector<CodeMap::InstToOpcode> &Insts,
                       const std::map<uint64_t, Value *> &Data,
                       const std::vector<StringRef> &ToCheck,
                       const std::map<uint16_t, StringRef> &CSMap,
                       Function &TargetFunc, GlobalVariable *CodeArray,
                       Module &M) {
  assert(CodeArray);

  DEBUG(printCodeMapping(Insts));
  DEBUG(printDataArrayMapping(Data));

  auto &Context = M.getContext();
  auto *Int16Ty = Type::getInt16Ty(Context);
  auto *NewFunc = createNewFunction(TargetFunc, M);

  auto *BBEntry = BasicBlock::Create(Context, "entry", NewFunc);
  auto *BBSwitchHead = BasicBlock::Create(Context, "sw_head", NewFunc);
  auto *BBUnreach = BasicBlock::Create(Context, "unreach", NewFunc);

  const auto CountBytes = [this](const uint64_t Acc,
                                 const std::pair<uint64_t, Value *> &Pair) {
    return Acc + Layout->getTypeAllocSize(Pair.second->getType());
  };
  const auto NumElems =
      std::accumulate(Data.begin(), Data.end(), 0u, CountBytes);

  IRBuilder<> Builder{BBEntry};
  auto *ConstantZero = Builder.getInt16(0);
  // create data array first
  auto *DataArrayAlloc = createEmptyDataArray(Builder, Data, NumElems, M);
  auto *DataArray = Builder.CreateLoad(Type::getInt8PtrTy(Context),
                                       DataArrayAlloc, "DataArray");

  VirtualizationBuilder VirtBuilder{Layout, CodeArray, DataArray, FreeFunc};

  // pre-load all constants and function arguments
  VirtBuilder.storeConstants(Builder, Data);
  VirtBuilder.storeFunctionArguments(Builder, Insts, Data, TargetFunc, NewFunc);

  auto *IdxAlloca = Builder.CreateAlloca(Int16Ty, nullptr, "CodeArrIndex");
  Builder.CreateStore(ConstantZero, IdxAlloca);

  Builder.CreateBr(BBSwitchHead);

  Builder.SetInsertPoint(BBSwitchHead);

  auto *IdxLoad = Builder.CreateLoad(IdxAlloca);
  auto *CurrentGEP =
      Builder.CreateInBoundsGEP(CodeArray, {ConstantZero, IdxLoad});
  auto *Current = Builder.CreateLoad(CurrentGEP);

  auto *Switch = Builder.CreateSwitch(Current, BBUnreach,
                                      static_cast<unsigned int>(Insts.size()));
  Builder.SetInsertPoint(BBUnreach);

  // printIP(Builder, M, IdxAlloca, CodeArray);

  auto *CI = Builder.CreateCall(AbortFunc);
  CI->setAttributes(AbortFunc->getAttributes());

  Builder.CreateUnreachable();

  // We have to remember the blocks we already added because some can be re-used
  std::set<uint16_t> InsertedTargets;

  uint16_t InstsInserted = 0;
  uint16_t GuardsInserted = 0;

  for (const auto &Elem : Insts) {
    const auto OpCode = Elem.Opcode.at(0);

    // if we have already inserted a handler for this opcode we can re-use it
    // NOTE: the translator will handle different types etc. for us
    if (InsertedTargets.find(OpCode) != InsertedTargets.end()) {
      DEBUG(dbgs() << "Reusing existing block for\t" << *Elem.OldInst << "\n");
      ++InstsInserted;
      continue;
    }

    InsertedTargets.emplace(OpCode);

    auto *CaseValue = Builder.getInt16(OpCode);

    // removed else branch (i.e. insertion of old guards)
    if (Elem.OldInst) {
      // normal inst
      virtualizeInstruction(VirtBuilder, Builder, Elem, NewFunc, BBSwitchHead,
                            Switch, CaseValue, IdxAlloca);
    }

    ++InstsInserted;
  }

  assert(InstsInserted == Insts.size() && "Missed some instructions?");

  GuardsAdded += GuardsInserted;

  return NewFunc;
}

GlobalVariable *
ScVirt::createCodeArray(const std::vector<CodeMap::InstToOpcode> &Insts,
                        const StringRef Name, Module &M) {
  auto &Context = M.getContext();
  auto *Int16Ty = Type::getInt16Ty(Context);

  // convert instruction opcodes into ConstantInts
  std::vector<Constant *> NewInsts;
  for (const auto &Elem : Insts) {
    for (const auto Value : Elem.Opcode) {
      auto *CInt = cast<ConstantInt>(ConstantInt::get(Int16Ty, Value));
      NewInsts.emplace_back(CInt);
    }
  }

  // array type is |insts| x i16
  auto *ArrayType = ArrayType::get(Int16Ty, NewInsts.size());
  auto *ArrayElems = ConstantArray::get(ArrayType, makeArrayRef(NewInsts));

  // insert global
  auto *Glob = M.getOrInsertGlobal(Name, ArrayType);
  auto *CodeArr = cast<GlobalVariable>(Glob);

  // set alignment, constant, linkage and initializer
  CodeArr->setAlignment(Int16Ty->getBitWidth() / 8); // byte-align
  CodeArr->setConstant(true);
  CodeArr->setLinkage(GlobalVariable::LinkageTypes::PrivateLinkage);
  CodeArr->setInitializer(ArrayElems);

  return CodeArr;
}

AllocaInst *
ScVirt::createEmptyDataArray(IRBuilder<> &Builder,
                             const std::map<uint64_t, Value *> &Data,
                             const uint64_t Size, Module &M) {
  DEBUG(dbgs() << "\t\tTypeSizes add up to " << std::to_string(Size) << "\n");

  auto &Context = M.getContext();

  auto *SizeConstant = Builder.getInt64(Size);
  auto *Int8Ty = Type::getInt8Ty(Context);

  // zero initialize malloc'ed memory
  auto *Malloc = Builder.CreateCall(MallocFunc, {SizeConstant});
  Malloc->setAttributes(MallocFunc->getAttributes());
  Builder.CreateMemSet(Malloc, Builder.getInt8(0), SizeConstant,
                       Int8Ty->getBitWidth() / 8);

  auto *AllocTy = Type::getInt8PtrTy(Context);
  auto *DataArray = Builder.CreateAlloca(AllocTy);
  Builder.CreateStore(Malloc, DataArray);

  return DataArray;
}

Function *ScVirt::virtualizeFunction(FunctionInfo &FInfo,
                                     const std::vector<StringRef> &ToCheck,
                                     const std::map<uint16_t, StringRef> &CSMap,
                                     Module &M) {
  auto *Func = FInfo.Func;
  const auto &CodeMapping = FInfo.CodeMapping.getElements();
  const auto &DataMapping = FInfo.DataMapping.getMapping();

  // create the array of instruction opcodes
  const auto Name = CodeArrPrefix + Func->getName().str();
  DEBUG(dbgs() << "GlobalVariable: " << Name << "\n");
  auto *CodeArray = M.getNamedGlobal(Name);
  assert(CodeArray && "Should exist!");

  // create the new function and emulator
  auto *NewFunc = createEmulator(CodeMapping, DataMapping, ToCheck, CSMap,
                                 *Func, CodeArray, M);

  // replace uses of old with new function
  Func->replaceAllUsesWith(NewFunc);
  NewFunc->takeName(Func);

  Func->eraseFromParent();

  return NewFunc;
}

void ScVirt::moveAllocaUp(Function &F) {
  const auto InsertPt = &*F.getEntryBlock().getFirstInsertionPt();

  std::vector<AllocaInst *> Allocs;

  // collect AllocaInsts
  for (auto Iter = inst_begin(F), End = inst_end(F); Iter != End; ++Iter) {
    auto *Inst = &*Iter;
    if (auto *AI = dyn_cast<AllocaInst>(Inst)) {
      Allocs.emplace_back(AI);
    }
  }

  // move AIs into entry block
  for (auto *AI : Allocs) {
    AI->moveBefore(InsertPt);
  }

  DEBUG(dbgs() << "\t-- Moved " << std::to_string(Allocs.size())
               << " allocas\n");
}

std::vector<ConnectivityMap>
ScVirt::generateDAG(std::vector<Function *> &&Funcs) {
  // TODO: sanity checks for Funcs.size(), Sensitive.size() and
  // Connectivity.getValue()

  struct ScFunction {
    llvm::Function *Func;
    std::vector<llvm::Function *> ToCheck;
  };

  struct Node {
    Function *Func;
    std::vector<Node *> Incoming;
    std::vector<Node *> Outgoing;
  };

  static const auto IsSensitive = [](const Function *F) -> bool {
    return F->hasFnAttribute("scsens");
  };

  std::vector<Function *> Sensitive;
  std::vector<Function *> Normal;

  for (auto *Func : Funcs) {
    if (IsSensitive(Func)) {
      Sensitive.emplace_back(Func);
    } else {
      Normal.emplace_back(Func);
    }
  }

  ActualConnectivity = [&Normal]() -> uint64_t {
    if (Normal.size() == 0) {
      return 0u;
    } else if (Connectivity.getValue() > Normal.size()) {
      return Normal.size();
    } else {
      return Connectivity.getValue();
    }
  }();

  std::vector<ScFunction> Tmp;
  while (!Sensitive.empty()) {
    auto *Sens = Sensitive.at(0);
    Tmp.emplace_back(ScFunction{Sens});
    Sensitive.erase(Sensitive.begin());

    // connect to C nodes
    for (auto I = 0u; I < ActualConnectivity; ++I) {
      std::random_device RandDevice;
      std::mt19937 Generator{RandDevice()};

      if (Normal.empty()) {
        std::uniform_int_distribution<uint64_t> Distr{0u, Tmp.size() - 1};

        if (SensitiveChecks.getValue()) {
          auto Found = false;
          uint64_t Idx;
          do {
            Idx = Distr(Generator);
            auto &CheckedBy = Tmp.at(Idx);

            // don't check yourself
            if (CheckedBy.Func != Sens) {
              // make sure not to have duplicate checks
              const auto Iter = std::find(CheckedBy.ToCheck.begin(),
                                          CheckedBy.ToCheck.end(), Sens);
              Found = Iter == CheckedBy.ToCheck.end();
            }
          } while (!Found);

          auto &CheckedBy = Tmp.at(Idx);
          CheckedBy.ToCheck.emplace_back(Sens);
        } else {
          uint64_t Idx;
          do {
            Idx = Distr(Generator);
          } while (IsSensitive(Tmp.at(Idx).Func));

          auto &CheckedBy = Tmp.at(Idx);
          CheckedBy.ToCheck.emplace_back(Sens);
        }
      } else {
        std::uniform_int_distribution<uint64_t> Distr{0u, Normal.size() - 1};

        const auto Idx = Distr(Generator);
        auto *CheckedBy = Normal.at(Idx);
        Normal.erase(Normal.begin() + Idx);

        Tmp.emplace_back(ScFunction{CheckedBy, {Sens}});
      }
    }
  }

  // simply add the remaining ones
  for (auto *F : Normal) {
    Tmp.emplace_back(ScFunction{F, {}});
  }

  // NOTE: all sensitive functions are connected!

  static const auto Find = [](std::vector<Node> &G, Function *F) -> Node * {
    auto Iter = std::find_if(G.begin(), G.end(), [&F](const Node &N) -> bool {
      return N.Func == F;
    });

    if (Iter != G.end()) {
      return &*Iter;
    }

    return nullptr;
  };

  std::vector<Node> Graph;
  Graph.reserve(Funcs.size());

  for (auto &F : Tmp) {
    Node *FNode; // node in the DAG for current function
    if (!Find(Graph, F.Func)) {
      auto *Fun = F.Func;
      Graph.emplace_back(Node{Fun});
      FNode = &Graph.back();
    } else {
      FNode = Find(Graph, F.Func);
    }

    for (auto &TC : F.ToCheck) {
      Node *TCNode; // node in the DAG for checked function
      if (!Find(Graph, TC)) {
        Graph.emplace_back(Node{TC});
        TCNode = &Graph.back();
      } else {
        TCNode = Find(Graph, TC);
      }

      TCNode->Outgoing.emplace_back(FNode);
      FNode->Incoming.emplace_back(TCNode);
    }
  }

  std::queue<Node *> Free;
  for (auto I = 0u; I < Graph.size(); ++I) {
    if (Graph.at(I).Incoming.size() == 0) {
      Free.push(&Graph.at(I));
    }
  }

  std::queue<Node *> TopoSort;
  while (Free.size() > 0) {
    auto *Top = Free.front();
    TopoSort.push(Top);
    Free.pop();

    for (auto I = 0u; I < Top->Outgoing.size(); ++I) {
      auto *Neighbour = Top->Outgoing.at(I);
      Neighbour->Incoming.erase(std::find(Neighbour->Incoming.begin(),
                                          Neighbour->Incoming.end(), Top));

      if (Neighbour->Incoming.size() == 0) {
        Free.push(Neighbour);
      }
    }
  }

  std::vector<ConnectivityMap> Res;

  while (!TopoSort.empty()) {
    auto *Top = TopoSort.front();
    TopoSort.pop();

    auto Iter = std::find_if(
        Tmp.begin(), Tmp.end(),
        [&Top](const ScFunction &SC) -> bool { return SC.Func == Top->Func; });

    assert(Iter != Tmp.end());

    std::vector<StringRef> Names;
    for (auto *Child : Iter->ToCheck) {
      Names.emplace_back(Child->getName());
    }

    ConnectivityMap CM{Top->Func, Names};
    Res.emplace_back(CM);
  }

  assert(Res.size() == Funcs.size() && "Missing some function?");

  return Res;
}

bool ScVirt::runOnModule(Module &M) {
  std::vector<Function *> Transformed;

  // functions to transform
  auto WorkList = generateDAG(getFunctions(M));

  std::map<StringRef, uint16_t> CSMap;

  std::map<StringRef, uint64_t> CodeArrMap;
  for (auto &SCF : WorkList) {
    const auto Name = SCF.Func->getName();
    CodeArrMap.emplace(Name,
                       std::distance(inst_begin(SCF.Func), inst_end(SCF.Func)));
  }

  std::set<StringRef> CountedProtection;

  uint64_t InstsVirtualized = 0;
  for (auto &SCF : WorkList) {
    DEBUG(dbgs() << SCF.Func->getName() << "\n");

    // Insert new guards in function's LLVM IR before virtualizing
    for (auto &Name : SCF.ToCheck) {

      // Retrieve information about the checked code array
      const auto CodeArrName = CodeArrPrefix + Name.str();
      auto *CheckedGlobalVar = M.getNamedGlobal(CodeArrName);
      const auto CheckedLength = CheckedGlobalVar->getType()->getElementType()->getArrayNumElements();

      // New guards are currently always placed at the beginning of the function
      Instruction *NextNormalInst = &*SCF.Func->begin()->begin();
      IRBuilder<> HeadBuilder(NextNormalInst);

      // Allocate Variables
      auto *GlobalVarPointer = HeadBuilder.CreateAlloca(CheckedGlobalVar->getType());
      auto *Int16PtrTy = Type::getInt16PtrTy(HeadBuilder.getContext());
      auto *Current = HeadBuilder.CreateAlloca(Int16PtrTy);
      auto *Length = HeadBuilder.CreateAlloca(HeadBuilder.getInt64Ty());
      auto *Hash = HeadBuilder.CreateAlloca(HeadBuilder.getInt16Ty());
      auto *Expected = HeadBuilder.CreateAlloca(HeadBuilder.getInt16Ty());

      // Store Values
      HeadBuilder.CreateStore(CheckedGlobalVar, GlobalVarPointer);
      auto *CheckedCodeArr = HeadBuilder.CreateLoad(GlobalVarPointer);
      auto *ConstInt32Zero = ConstantInt::get(HeadBuilder.getContext(), APInt(32, 0));
      auto *FirstElementPointer = HeadBuilder.CreateInBoundsGEP(CheckedCodeArr, {ConstInt32Zero, ConstInt32Zero});
      HeadBuilder.CreateStore(FirstElementPointer, Current);
      HeadBuilder.CreateStore(HeadBuilder.getInt64(CheckedLength), Length);
      HeadBuilder.CreateStore(HeadBuilder.getInt16(0), Hash);

      // Loop Condition
      auto *RemainingIters = HeadBuilder.CreateLoad(Length);
      auto *CondSplit = SplitBlock(RemainingIters->getParent(), RemainingIters);
      IRBuilder<> CondBuilder(&*++CondSplit->begin());
      auto *NewLength = CondBuilder.CreateNSWAdd(RemainingIters, HeadBuilder.getInt64(-1));
      CondBuilder.CreateStore(NewLength, Length);
      auto *LoopComp = CondBuilder.CreateICmpSGT(RemainingIters, HeadBuilder.getInt64(0));

      auto *ExpectedHash = CondBuilder.CreateStore(CondBuilder.getInt16(CSMap.at(Name)), Expected); // first instruction after loop

      // Loop Body
      auto *BodySplit = SplitBlockAndInsertIfThen(LoopComp, ExpectedHash, false);
      Instruction *OldLoopBr = &*BodySplit->getParent()->begin();
      Instruction *NewLoopBr = BranchInst::Create(CondSplit);
      ReplaceInstWithInst(OldLoopBr, NewLoopBr);
      IRBuilder<> BodyBuilder(NewLoopBr);
      auto *CurrentPointer = BodyBuilder.CreateLoad(Current);
      auto *CurrentValue = BodyBuilder.CreateLoad(CurrentPointer);
      auto *CurrentConv = BodyBuilder.CreateSExt(CurrentValue, BodyBuilder.getInt32Ty());
      auto *HashValue = BodyBuilder.CreateLoad(Hash);
      auto *HashConv = BodyBuilder.CreateSExt(HashValue, BodyBuilder.getInt32Ty());
      auto *UpdatedHash = BodyBuilder.CreateXor(HashConv, CurrentConv);
      auto *HashTrunc = BodyBuilder.CreateTrunc(UpdatedHash, BodyBuilder.getInt16Ty());
      BodyBuilder.CreateStore(HashTrunc, Hash);
      auto *OldPointer = BodyBuilder.CreateLoad(Current);
      auto *NewPointer = BodyBuilder.CreateInBoundsGEP(OldPointer, HeadBuilder.getInt32(1));
      BodyBuilder.CreateStore(NewPointer, Current);

      // Equivalence Check
      IRBuilder<> TailBuilder(&*++ExpectedHash->getParent()->begin());
      auto *ExpectedValue = TailBuilder.CreateLoad(Expected);
      auto *ExpectedConv = TailBuilder.CreateSExt(ExpectedValue, BodyBuilder.getInt32Ty());
      auto *ResultValue = TailBuilder.CreateLoad(Hash);
      auto *ResultConv = TailBuilder.CreateSExt(ResultValue, BodyBuilder.getInt32Ty());
      auto *GuardComp = TailBuilder.CreateICmpNE(ExpectedConv, ResultConv);

      // Response Handling
      auto *ResponseSplit = SplitBlockAndInsertIfThen(GuardComp, NextNormalInst, false);
      IRBuilder<> ResponseBuilder(&*ResponseSplit->getParent()->begin());
      ResponseBuilder.CreateCall(M.getFunction("abort"));
    }

    // Is not used in updated VirtSC but remains to simplify switching between different versions
    std::vector<uint16_t> Checksums;
    for (auto &Name : SCF.ToCheck) {
      const auto CSum = CSMap.at(Name);
      Checksums.emplace_back(CSum);
    }

    ISATranslator Translator{SCF.Func, SCF.ToCheck, Checksums};

    // get the new ISA
    const auto CodeMapping = Translator.getCodeMap();
    const auto DataMapping = Translator.getDataMap();
    const auto ChecksumMap = Translator.getChecksumMap();

    const auto CSum = calculateChecksumXor(CodeMapping.getElements());
    CSMap.emplace(std::make_pair(SCF.Func->getName(), CSum));

    FunctionInfo FI{SCF.Func, CodeMapping, DataMapping};

    createCodeArray(CodeMapping.getElements(),
                    CodeArrPrefix + FI.Func->getName().str(), M);
    auto *NewFunc = virtualizeFunction(FI, SCF.ToCheck, ChecksumMap, M);

    for (auto &Name : SCF.ToCheck) {
      // check if we already counted this function as being protected
      if (CountedProtection.find(Name) == CountedProtection.end()) {
        errs() << "Already counted: " << Name << "\n";
        ProtectedInsts += CodeArrMap.at(Name);
        CountedProtection.emplace(Name);
      }
    }

    moveAllocaUp(*NewFunc);

    assert(!verifyFunction(*NewFunc, &errs()) && "Could not verify function!");

    InstsVirtualized += CodeMapping.size();

    Transformed.emplace_back(NewFunc);
    DEBUG(dbgs() << "*****************************************\n");
  }

  DEBUG(dbgs() << "********* VIRTUALIZATION PHASE END *********\n");

  // sanity check
  assert(!verifyModule(M, &errs()) && "Could not verify module!");

  // dump info to file
  if (!DumpStatFile.getValue().empty()) {
    nlohmann::json JsonContent;

    // Connectivity
    JsonContent["DesiredConnectivity"] = Connectivity.getValue();
    JsonContent["ActualConnectivity"] = ActualConnectivity;

    // Instructions
    JsonContent["InstructionsVirtualized"] = InstsVirtualized - GuardsAdded;
    JsonContent["GuardsAdded"] = GuardsAdded;

    // Functions
    JsonContent["FunctionsTransformed"] = Transformed.size();

    // Protected Insts
    JsonContent["ProtectedInstructions"] = ProtectedInsts;

    {
      const auto FilePath = DumpStatFile.getValue();
      std::ofstream File{FilePath};

      JsonContent.dump(4);
      File << std::setw(4) << JsonContent << std::endl;

      File.close();
    }
  }

  return !Transformed.empty();
}

// Is not called in updated VirtSC but remains to simplify switching between different versions
Function *ScVirt::createXORFunction(Module &M) {
  auto &Context = M.getContext();

  auto *Int16Ty = Type::getInt16Ty(Context);
  auto *Int64Ty = Type::getInt64Ty(Context);

  auto *FTy =
    FunctionType::get(
      // ret type
      Int16Ty,
      // arg types
      {
        PointerType::get(Int16Ty, 0),
        Int64Ty
      },
      /*VarArg=*/false);
  auto *NewC = M.getOrInsertFunction("scvirt_xor_cs", FTy);
  auto *NewF = cast<Function>(NewC);

  auto *V0 = &*NewF->getArgumentList().begin();
  auto *V1 = NewF->getArgumentList().getNext(V0);


  /**
   * This code has been generated using
   *
   * $ cat main.c:
   *    short do_xor(short *Arr, const long Len) {
   *      short Result = 0;
   *
   *      for(long I = 0; I < Len; ++I) {
   *        Result = Result ^ Arr[I];
   *      }
   *
   *      return Result;
   *    }
   *
   * $ clang -O3 -c -emit-llvm main.c
   */

  auto *Block2 = BasicBlock::Create(Context, "", NewF);
  auto *Block4 = BasicBlock::Create(Context, "", NewF);
  auto *Block6 = BasicBlock::Create(Context, "", NewF);
  auto *Block9 = BasicBlock::Create(Context, "", NewF);
  auto *Block16 = BasicBlock::Create(Context, "", NewF);
  auto *Block18 = BasicBlock::Create(Context, "", NewF);
  auto *Block61 = BasicBlock::Create(Context, "", NewF);
  auto *Block68 = BasicBlock::Create(Context, "", NewF);
  auto *Block69 = BasicBlock::Create(Context, "", NewF);
  auto *Block85 = BasicBlock::Create(Context, "", NewF);
  auto *Block97 = BasicBlock::Create(Context, "", NewF);
  auto *Block99 = BasicBlock::Create(Context, "", NewF);
  IRBuilder<> Builder{Block2};

  PHINode *P7 = nullptr;
  PHINode *P8 = nullptr;
  Value *V10 = nullptr;
  Value *V13 = nullptr;
  Value *V14 = nullptr;
  Value *V17 = nullptr;
  Value *V56 = nullptr;
  Value *V57 = nullptr;
  Value *V58 = nullptr;
  Value *V59 = nullptr;
  PHINode *P62 = nullptr;
  PHINode *P63 = nullptr;
  PHINode *P64 = nullptr;
  PHINode *P65 = nullptr;
  PHINode *P66 = nullptr;
  Value *V80 = nullptr;
  Value *V81 = nullptr;
  Value *V95 = nullptr;
  Value *V104 = nullptr;

  auto *ArrTy = VectorType::get(Builder.getInt16Ty(), 8);
  auto *ZeroInit = ConstantAggregateZero::get(ArrTy);
  auto *PtrArrTy = PointerType::get(ArrTy, 0);

  // Block2
  {
    auto *V3 = Builder.CreateICmpSGT(V1, Builder.getInt64(0));
    Builder.CreateCondBr(V3, Block4, Block97);
  }

  // Block4
  {
    Builder.SetInsertPoint(Block4);
    auto *V5 = Builder.CreateICmpULT(V1, Builder.getInt64(16));
    Builder.CreateCondBr(V5, Block6, Block9);
  }

  // Block9
  {
    Builder.SetInsertPoint(Block9);
    V10 = Builder.CreateAnd(V1, Builder.getInt64(-16));
    auto *V11 = Builder.CreateAdd(V10, Builder.getInt64(-16));
    auto *V12 = Builder.CreateLShr(V11, Builder.getInt64(4), "", true);
    V13 = Builder.CreateAdd(V12, Builder.getInt64(1), "", true, true);
    V14 = Builder.CreateAnd(V13, Builder.getInt64(3));
    auto *V15 = Builder.CreateICmpULT(V11, Builder.getInt64(48));
    Builder.CreateCondBr(V15, Block61, Block16);
  }

  // Block16
  {
    Builder.SetInsertPoint(Block16);
    V17 = Builder.CreateNSWSub(V13, V14);
    Builder.CreateBr(Block18);
  }

  // Block18
  {
    Builder.SetInsertPoint(Block18);
    auto *P19 = Builder.CreatePHI(Builder.getInt64Ty(), 2);
    auto *P20 = Builder.CreatePHI(ArrTy, 2);
    auto *P21 = Builder.CreatePHI(ArrTy, 2);
    auto *P22 = Builder.CreatePHI(Builder.getInt64Ty(), 2);

    auto *V23 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V0, P19);
    auto *V24 = Builder.CreateBitCast(V23, PtrArrTy);
    auto *V25 = Builder.CreateLoad(ArrTy, V24);
    V25->setAlignment(2);
    auto *V26 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V23, Builder.getInt64(8));
    auto *V27 = Builder.CreateBitCast(V26, PtrArrTy);
    auto *V28 = Builder.CreateLoad(ArrTy, V27);
    V28->setAlignment(2);

    auto *V29 = Builder.CreateXor(V25, P20);
    auto *V30 = Builder.CreateXor(V28, P21);
    auto *V31 = Builder.CreateOr(P19, Builder.getInt64(16));

    auto *V32 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V0, V31);
    auto *V33 = Builder.CreateBitCast(V32, PtrArrTy);
    auto *V34 = Builder.CreateLoad(ArrTy, V33);
    V34->setAlignment(2);
    auto *V35 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V32, Builder.getInt64(8));
    auto *V36 = Builder.CreateBitCast(V35, PtrArrTy);
    auto *V37 = Builder.CreateLoad(ArrTy, V36);
    V37->setAlignment(2);

    auto *V38 = Builder.CreateXor(V34, V29);
    auto *V39 = Builder.CreateXor(V37, V30);
    auto *V40 = Builder.CreateOr(P19, Builder.getInt64(32));

    auto *V41 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V0, V40);
    auto *V42 = Builder.CreateBitCast(V41, PtrArrTy);
    auto *V43 = Builder.CreateLoad(ArrTy, V42);
    V43->setAlignment(2);
    auto *V44 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V41, Builder.getInt64(8));
    auto *V45 = Builder.CreateBitCast(V44, PtrArrTy);
    auto *V46 = Builder.CreateLoad(ArrTy, V45);
    V46->setAlignment(2);

    auto *V47 = Builder.CreateXor(V43, V38);
    auto *V48 = Builder.CreateXor(V46, V39);
    auto *V49 = Builder.CreateOr(P19, Builder.getInt64(48));

    auto *V50 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V0, V49);
    auto *V51 = Builder.CreateBitCast(V50, PtrArrTy);
    auto *V52 = Builder.CreateLoad(ArrTy, V51);
    V52->setAlignment(2);
    auto *V53 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V50, Builder.getInt64(8));
    auto *V54 = Builder.CreateBitCast(V53, PtrArrTy);
    auto *V55 = Builder.CreateLoad(ArrTy, V54);
    V55->setAlignment(2);

    V56 = Builder.CreateXor(V52, V47);
    V57 = Builder.CreateXor(V55, V48);
    V58 = Builder.CreateAdd(P19, Builder.getInt64(64));
    V59 = Builder.CreateAdd(P22, Builder.getInt64(-4));

    P19->addIncoming(Builder.getInt64(0), Block16);
    P19->addIncoming(V58, Block18);

    P20->addIncoming(ZeroInit, Block16);
    P20->addIncoming(V56, Block18);

    P21->addIncoming(ZeroInit, Block16);
    P21->addIncoming(V57, Block18);

    P22->addIncoming(V17, Block16);
    P22->addIncoming(V59, Block18);

    auto *ICmp = Builder.CreateICmpEQ(V59, Builder.getInt64(0));
    Builder.CreateCondBr(ICmp, Block61, Block18);
  }

  // Block61
  {
    Builder.SetInsertPoint(Block61);
    P62 = Builder.CreatePHI(ArrTy, 2);
    P62->addIncoming(UndefValue::get(ArrTy), Block9);
    P62->addIncoming(V56, Block18);

    P63 = Builder.CreatePHI(ArrTy, 2);
    P63->addIncoming(UndefValue::get(ArrTy), Block9);
    P63->addIncoming(V57, Block18);

    P64 = Builder.CreatePHI(Builder.getInt64Ty(), 2);
    P64->addIncoming(Builder.getInt64(0), Block9);
    P64->addIncoming(V58, Block18);

    P65 = Builder.CreatePHI(ArrTy, 2);
    P65->addIncoming(ZeroInit, Block9);
    P65->addIncoming(V56, Block18);

    P66 = Builder.CreatePHI(ArrTy, 2);
    P66->addIncoming(ZeroInit, Block9);
    P66->addIncoming(V57, Block18);

    auto *V67 = Builder.CreateICmpEQ(V14, Builder.getInt64(0));
    Builder.CreateCondBr(V67, Block85, Block68);
  }

  // Block68
  {
    Builder.SetInsertPoint(Block68);
    Builder.CreateBr(Block69);
  }

  // Block69
  {
    Builder.SetInsertPoint(Block69);

    auto *P70 = Builder.CreatePHI(Builder.getInt64Ty(), 2);
    auto *P71 = Builder.CreatePHI(ArrTy, 2);
    auto *P72 = Builder.CreatePHI(ArrTy, 2);
    auto *P73 = Builder.CreatePHI(Builder.getInt64Ty(), 2);

    auto *V74 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V0, P70);
    auto *V75 = Builder.CreateBitCast(V74, PtrArrTy);
    auto *V76 = Builder.CreateLoad(ArrTy, V75);
    V76->setAlignment(2);

    auto *V77 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V74, Builder.getInt64(8));
    auto *V78 = Builder.CreateBitCast(V77, PtrArrTy);
    auto *V79 = Builder.CreateLoad(ArrTy, V78);
    V79->setAlignment(2);

    V80 = Builder.CreateXor(V76, P71);
    V81 = Builder.CreateXor(V79, P72);

    auto *V82 = Builder.CreateAdd(P70, Builder.getInt64(16));
    auto *V83 = Builder.CreateAdd(P73, Builder.getInt64(-1));

    P70->addIncoming(P64, Block68);
    P70->addIncoming(V82, Block69);

    P71->addIncoming(P65, Block68);
    P71->addIncoming(V80, Block69);

    P72->addIncoming(P66, Block68);
    P72->addIncoming(V81, Block69);

    P73->addIncoming(V14, Block68);
    P73->addIncoming(V83, Block69);

    auto *V84 = Builder.CreateICmpEQ(V83, Builder.getInt64(0));
    Builder.CreateCondBr(V84, Block85, Block69);
  }

  // Block85
  {
    Builder.SetInsertPoint(Block85);
    auto *P86 = Builder.CreatePHI(ArrTy, 2);
    P86->addIncoming(P62, Block61);
    P86->addIncoming(V80, Block69);

    auto *P87 = Builder.CreatePHI(ArrTy, 2);
    P87->addIncoming(P63, Block61);
    P87->addIncoming(V81, Block69);

    auto *V88 = Builder.CreateXor(P87, P86);

    auto *Undef32 = UndefValue::get(Builder.getInt32Ty());

    std::vector<Constant *> Ve1;
    Ve1.emplace_back(Builder.getInt32(4));
    Ve1.emplace_back(Builder.getInt32(5));
    Ve1.emplace_back(Builder.getInt32(6));
    Ve1.emplace_back(Builder.getInt32(7));
    Ve1.emplace_back(Undef32);
    Ve1.emplace_back(Undef32);
    Ve1.emplace_back(Undef32);
    Ve1.emplace_back(Undef32);
    auto *Vec1 = ConstantVector::get(makeArrayRef(Ve1));

    std::vector<Constant *> Ve2;
    Ve2.emplace_back(Builder.getInt32(2));
    Ve2.emplace_back(Builder.getInt32(3));
    Ve2.emplace_back(Undef32);
    Ve2.emplace_back(Undef32);
    Ve2.emplace_back(Undef32);
    Ve2.emplace_back(Undef32);
    Ve2.emplace_back(Undef32);
    Ve2.emplace_back(Undef32);
    auto *Vec2 = ConstantVector::get(makeArrayRef(Ve2));

    std::vector<Constant *> Ve3;
    Ve3.emplace_back(Builder.getInt32(1));
    Ve3.emplace_back(Undef32);
    Ve3.emplace_back(Undef32);
    Ve3.emplace_back(Undef32);
    Ve3.emplace_back(Undef32);
    Ve3.emplace_back(Undef32);
    Ve3.emplace_back(Undef32);
    Ve3.emplace_back(Undef32);
    auto *Vec3 = ConstantVector::get(makeArrayRef(Ve3));

    auto *V89 = Builder.CreateShuffleVector(V88, UndefValue::get(ArrTy), Vec1);
    auto *V90 = Builder.CreateXor(V88, V89);
    auto *V91 = Builder.CreateShuffleVector(V90, UndefValue::get(ArrTy), Vec2);
    auto *V92 = Builder.CreateXor(V90, V91);
    auto *V93 = Builder.CreateShuffleVector(V92, UndefValue::get(ArrTy), Vec3);
    auto *V94 = Builder.CreateXor(V92, V93);

    V95 = Builder.CreateExtractElement(V94, Builder.getInt32(0));
    auto *V96 = Builder.CreateICmpEQ(V10, V1);
    Builder.CreateCondBr(V96, Block97, Block6);
  }

  // Block6
  {
    Builder.SetInsertPoint(Block6);
    P7 = Builder.CreatePHI(Builder.getInt64Ty(), 2);
    P7->addIncoming(Builder.getInt64(0), Block4);
    P7->addIncoming(V10, Block85);

    P8 = Builder.CreatePHI(Builder.getInt16Ty(), 2);
    P8->addIncoming(Builder.getInt16(0), Block4);
    P8->addIncoming(V95, Block85);

    Builder.CreateBr(Block99);
  }

  // Block99
  {
    Builder.SetInsertPoint(Block99);

    auto *P100 = Builder.CreatePHI(Builder.getInt64Ty(), 2);
    auto *P101 = Builder.CreatePHI(Builder.getInt16Ty(), 2);

    auto *V102 = Builder.CreateInBoundsGEP(Builder.getInt16Ty(), V0, P100);
    auto *V103 = Builder.CreateLoad(Builder.getInt16Ty(), V102);
    V103->setAlignment(2);

    V104 = Builder.CreateXor(V103, P101);
    auto *V105 = Builder.CreateAdd(P100, Builder.getInt64(1), "", true, true);

    P100->addIncoming(V105, Block99);
    P100->addIncoming(P7, Block6);

    P101->addIncoming(V104, Block99);
    P101->addIncoming(P8, Block6);

    auto *V106 = Builder.CreateICmpEQ(V105, V1);
    Builder.CreateCondBr(V106, Block97, Block99);
  }

  // Block97
  {
    Builder.SetInsertPoint(Block97);
    auto *P98 = Builder.CreatePHI(Builder.getInt16Ty(), 3);
    P98->addIncoming(Builder.getInt16(0), Block2);
    P98->addIncoming(V95, Block85);
    P98->addIncoming(V104, Block99);

    Builder.CreateRet(P98);
  }

  AttributeSet AttrSet;
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::ReturnIndex, Attribute::AttrKind::SExt);
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::NoUnwind);
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::ReadOnly);
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::UWTable);
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "correctly-rounded-divide-sqrt-fp-math", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "disable-tail-calls", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "less-precise-fpmad", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-infs-fp-math", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-jump-tables", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-nans-fp-math", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-signed-zeros-fp-math", "false");
  AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-trapping-math", "false");

  AttrSet = AttrSet.addAttribute(Context, 1, Attribute::AttrKind::NoCapture);
  AttrSet = AttrSet.addAttribute(Context, 1, Attribute::AttrKind::ReadOnly);

  NewF->setAttributes(AttrSet);

  assert(!verifyFunction(*NewF, &errs()) && "Xor function could not be verified!");

  return NewF;
}

bool ScVirt::doInitialization(llvm::Module &M) {
  auto &Context = M.getContext();

  if (auto AbFunc = M.getFunction("abort")) {
    AbortFunc = AbFunc;
  } else {
    auto *AbortTy = FunctionType::get(
      // ret type
      Type::getVoidTy(Context),
      /*VarArg=*/false);
    AbortFunc = Function::Create(AbortTy, Function::LinkageTypes::ExternalLinkage, "abort", &M);

    AttributeSet AttrSet;
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::NoReturn);
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::NoUnwind);
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "disable-tail-calls", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "less-precise-fpmad", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim", "true");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim-non-leaf");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-infs-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-nans-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-signed-zeros-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "stack-protector-buffer-size", "8");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "unsafe-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "use-soft-float", "false");

    AbortFunc->setAttributes(AttrSet);
  }

  if (auto MallocF = M.getFunction("malloc")) {
    MallocFunc = MallocF;
  } else {
    auto *MallocTy = FunctionType::get(
      // ret type
      Type::getInt8PtrTy(Context),
      // arguments
      Type::getInt64Ty(Context),
      /*VarArg=*/false);
    MallocFunc = Function::Create(MallocTy, Function::LinkageTypes::ExternalLinkage, "malloc", &M);

    AttributeSet AttrSet;
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::NoUnwind);
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "disable-tail-calls", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "less-precise-fpmad", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim", "true");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim-non-leaf");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-infs-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-nans-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-signed-zeros-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "stack-protector-buffer-size", "8");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "unsafe-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "use-soft-float", "false");

    MallocFunc->setAttributes(AttrSet);

    MallocFunc->setDoesNotAlias(AttributeSet::ReturnIndex);
  }


  if (auto FreeF = M.getFunction("free")) {
    FreeFunc = FreeF;
  } else {
    auto *FreeTy = FunctionType::get(
      // ret type
      Type::getVoidTy(Context),
      // arguments
      Type::getInt8PtrTy(Context),
      /*VarArg=*/false);
    FreeFunc = Function::Create(FreeTy, Function::LinkageTypes::ExternalLinkage, "free", &M);

    AttributeSet AttrSet;
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, Attribute::AttrKind::NoUnwind);
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "disable-tail-calls", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "less-precise-fpmad", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim", "true");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-frame-pointer-elim-non-leaf");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-infs-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-nans-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "no-signed-zeros-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "stack-protector-buffer-size", "8");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "unsafe-fp-math", "false");
    AttrSet = AttrSet.addAttribute(Context, AttributeSet::FunctionIndex, "use-soft-float", "false");

    FreeFunc->setAttributes(AttrSet);
  }

  Layout = &M.getDataLayout();

  return true;
}

char ScVirt::ID = 0;
static RegisterPass<ScVirt> X("scvirt", "ScVirt Pass", false, false);
