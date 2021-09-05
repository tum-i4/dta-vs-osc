#ifndef LLVM_OPCODEGENERATOR_HPP
#define LLVM_OPCODEGENERATOR_HPP

#include <random>
#include <set>

class OpcodeGenerator {
  std::random_device RandDevice;
  std::mt19937 Generator { RandDevice() };
  std::uniform_int_distribution<uint16_t> Distr { 0u, std::numeric_limits<int16_t>::max() - 1 };

  std::set<uint16_t> UsedList;

public:
  static OpcodeGenerator &get() {
    static OpcodeGenerator OpGen;
    return OpGen;
  }

  uint16_t generate();
  void release(const uint16_t Val);
};

#endif //LLVM_OPCODEGENERATOR_HPP
