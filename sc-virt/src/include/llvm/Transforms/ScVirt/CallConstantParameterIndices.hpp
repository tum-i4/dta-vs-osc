#ifndef LLVM_CALLCONSTANTPARAMETERINDICES_HPP
#define LLVM_CALLCONSTANTPARAMETERINDICES_HPP

#include <cstdint>

/// These indices are required to recognize which intrinsic call parameters cannot be loaded from the data array.
/// There are LLVM checks in place that assert these indices to be constant.
/// For example, DataAlignment and Volatile parameters for MemCpy calls have to be constant.
/// Since the IR does not neccessarily name the parameters, we have to check the argument's index.
namespace ConstantIndices {
constexpr static uint64_t MEMCPY_ALIGNMENT_INDEX = 3;
constexpr static uint64_t MEMCPY_VOLATILE_INDEX = 4;

constexpr static uint64_t MEMSET_ALIGNMENT_INDEX = 3;
constexpr static uint64_t MEMSET_VOLATILE_INDEX = 4;

constexpr static uint64_t MEMMOVE_ALIGNMENT_INDEX = 3;
constexpr static uint64_t MEMMOVE_VOLATILE_INDEX = 4;
} // end namespace

#endif //LLVM_CALLCONSTANTPARAMETERINDICES_HPP
