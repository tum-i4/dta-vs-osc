#include "llvm/Transforms/ScVirt/DataMap.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "DataMap"

using namespace llvm;

uint64_t DataMap::getOrInsert(Value *Val, const DataLayout &Layout) {
  assert(Val);

  const auto Res = find(Val);

  if(!Res.hasValue()) {
    return insert(Val, Layout);
  }

  return Res.getValue();
}

const std::map<uint64_t, llvm::Value *> &DataMap::getMapping() const {
  return Mapping;
}

std::map<uint64_t, llvm::Value *> &DataMap::getMapping() {
  return Mapping;
}

uint64_t DataMap::insert(Value *Val, const DataLayout &Layout) {
  assert(Val);
  assert(Val->getType());
  assert(Val->getType()->isSized());

  const auto Ret = CurrentIndex;
  Mapping.emplace(std::make_pair(CurrentIndex, Val));
  CurrentIndex += Layout.getTypeAllocSize(Val->getType());

  return Ret;
}

Optional<uint64_t> DataMap::find(Value *Val) {
  assert(Val);

  static auto PtrMatch = [&Val](const std::pair<uint64_t, Value *> &Pair) {
    return Val == Pair.second;
  };

  const auto Iter = std::find_if(Mapping.begin(), Mapping.end(), PtrMatch);
  if(Iter == Mapping.end()) {
    return None;
  }

  DEBUG(dbgs() << "We already added " << *Val << "\n");
  return Optional<uint64_t> { Iter->first };
}
