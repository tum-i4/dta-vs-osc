#include <random>

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <set>

using namespace llvm;

#define DEBUG_TYPE "AddAttributePass"

static cl::opt<float> Coverage("coverage", cl::desc("The coverage to test. 0 <= C <= 1."), cl::init(0.f));
static cl::opt<uint32_t> DesiredConnectivity("sensitive", cl::desc("The connectivity of safe-guards"), cl::init(0));

namespace {
struct AddAttributePass : public ModulePass {
  static char ID;
  AddAttributePass()
    : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    std::random_device RandDevice;
    std::mt19937 Generator{RandDevice()};
    std::uniform_real_distribution<float> FloatDistr{0.f, 1.f};

    const auto Cov = Coverage.getValue();
    DEBUG(dbgs() << "Requested coverage: " << std::to_string(Cov) << "\n");

    const auto Conn = DesiredConnectivity.getValue();
    DEBUG(dbgs() << "Requested connectivity: " << std::to_string(Conn) << "\n");

    auto FuncsFound = 0u;
    auto VirtAdded = 0u;
    auto SensAdded = 0u;

    std::vector<Function *> ToVirtualize;
    for (auto &F : M.getFunctionList()) {
      if (F.isDeclaration()) {
        continue;
      }
      ++FuncsFound;
      if (FloatDistr(Generator) <= Cov) {
        F.addFnAttr("scvirt");
        ++VirtAdded;

        ToVirtualize.emplace_back(&F);
      }
    }

    DEBUG(dbgs() << "Added 'scvirt' to " << std::to_string(VirtAdded) << " / " << std::to_string(FuncsFound)
           << "\n");


    const auto ActualConnectivity = [&ToVirtualize, &Conn]() -> uint64_t {
      if(ToVirtualize.size() == 0) {
        return 0u;
      } else if(Conn > ToVirtualize.size()) {
        return ToVirtualize.size() - 1;
      } else {
        return DesiredConnectivity.getValue();
      }
    }();

    if(ActualConnectivity != Conn) {
      DEBUG(dbgs() << "Connectivity can be at most " << std::to_string(ActualConnectivity) << "\n");
    }

    if(ActualConnectivity > 0) {
      uint64_t SensitiveCount = (ToVirtualize.size() / ActualConnectivity);
      if(ActualConnectivity == 1) {
        SensitiveCount = 1;
      }
      DEBUG(dbgs() << "Can have at most " << std::to_string(SensitiveCount) << " sensitive functions\n");

      uint64_t ToAdd = 0;

      // scope for IntDistr
      {
        std::uniform_int_distribution<uint64_t> IntDistr{1u, SensitiveCount};
        ToAdd = IntDistr(Generator);
      }

      std::uniform_int_distribution<uint64_t> IntDistr{1u, ToVirtualize.size() - 1};
      std::set<uint64_t> Handled;

      for (auto I = 0u; I < ToAdd; ++I) {
        auto RndIdx = IntDistr(Generator);
        while (Handled.find(RndIdx) != Handled.end()) {
          RndIdx = IntDistr(Generator);
        }

        Handled.emplace(RndIdx);

        auto *F = ToVirtualize.at(RndIdx);

        F->addFnAttr("scsens");
        ++SensAdded;
      }
    }


    DEBUG(dbgs() << "Added 'scsens' to " << std::to_string(SensAdded) << " / " << std::to_string(FuncsFound)
           << "\n");

    return false;
  }
};
} // namespace

char AddAttributePass::ID = 0;
static RegisterPass<AddAttributePass> X("addattr", "Add Attribute Pass");
