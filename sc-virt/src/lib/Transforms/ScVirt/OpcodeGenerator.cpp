#include "llvm/Transforms/ScVirt/OpcodeGenerator.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "OpcodeGenerator"
using namespace llvm;

uint16_t OpcodeGenerator::generate() {
  auto Res = Distr(Generator);

  while(UsedList.find(Res) != UsedList.end()) {
    Res = Distr(Generator);
  }

  UsedList.emplace(Res);
  return Res;
}

void OpcodeGenerator::release(const uint16_t Val) {
  UsedList.erase(Val);
}