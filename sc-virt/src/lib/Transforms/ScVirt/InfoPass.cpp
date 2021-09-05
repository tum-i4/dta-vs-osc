#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include <numeric>

using namespace llvm;

namespace {
struct InfoPass : public ModulePass {
  static char ID;
  InfoPass()
      : ModulePass(ID) {}

  bool runOnModule(Module &M) override {

    std::vector<Function *> Worklist;
    for(auto &F : M.getFunctionList()) {
      if(F.isDeclaration()) {
        continue;
      }

      Worklist.emplace_back(&F);
    }

    errs() << "Functions: " << std::to_string(Worklist.size()) << "\n";
    const auto TotalBasicBlocks = std::accumulate(Worklist.begin(), Worklist.end(), 0u, [](uint64_t Acc, const Function *F) {
      return Acc + F->size();
    });

    errs() << "BasicBlocks: " << std::to_string(TotalBasicBlocks) << "\n";

    const auto TotalInsts = std::accumulate(Worklist.begin(), Worklist.end(), 0u, [](uint64_t Acc, const Function *F) {
      return Acc + std::accumulate(F->getBasicBlockList().begin(), F->getBasicBlockList().end(), 0u, [](uint64_t AccInner, const BasicBlock &BB) {
        return AccInner + BB.size();
      });
    });

    errs() << "Instructions: " << std::to_string(TotalInsts) << "\n";
    errs() << "-------\n";

    return false;
  }
};
} // namespace

char InfoPass::ID = 0;
static RegisterPass<InfoPass> X("infopass", "Gather module information");
