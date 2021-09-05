#ifndef LLVM_DATAMAP_HPP
#define LLVM_DATAMAP_HPP

#include "llvm/ADT/Optional.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Value.h"

#include <map>

class DataMap {
public:
  uint64_t getOrInsert(llvm::Value *Val, const llvm::DataLayout &Layout);

  const std::map<uint64_t, llvm::Value *> &getMapping() const;
  std::map<uint64_t, llvm::Value *> &getMapping();

private:
  std::map<uint64_t, llvm::Value *> Mapping;
  uint64_t CurrentIndex { 0 };

  uint64_t insert(llvm::Value *Val, const llvm::DataLayout &Layout);
  llvm::Optional<uint64_t> find(llvm::Value *Val);
};

#endif //LLVM_DATAMAP_HPP
