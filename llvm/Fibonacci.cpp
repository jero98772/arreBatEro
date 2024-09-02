#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module* module = new llvm::Module("fib_module", context);
    llvm::IRBuilder<> builder(context);

    // Define the function signature: int fib(int n)
    llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt32Ty()}, false);
    llvm::Function *fibFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "fib", module);

    // Create entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fibFunc);
    llvm::BasicBlock *recurse = llvm::BasicBlock::Create(context, "recurse", fibFunc);
    llvm::BasicBlock *retBlock = llvm::BasicBlock::Create(context, "return", fibFunc);
    builder.SetInsertPoint(entry);

    // Get function argument
    llvm::Value* n = fibFunc->arg_begin();
    n->setName("n");

    // Base case: if (n < 2) return n;
    llvm::Value* condition = builder.CreateICmpSLT(n, builder.getInt32(2), "cond");
    builder.CreateCondBr(condition, retBlock, recurse);

    // Recursive case: fib(n-1) + fib(n-2)
    builder.SetInsertPoint(recurse);
    llvm::Value* nMinus1 = builder.CreateSub(n, builder.getInt32(1), "n_minus_1");
    llvm::Value* nMinus2 = builder.CreateSub(n, builder.getInt32(2), "n_minus_2");

    llvm::Value* fibNMinus1 = builder.CreateCall(fibFunc, nMinus1, "fib_n_minus_1");
    llvm::Value* fibNMinus2 = builder.CreateCall(fibFunc, nMinus2, "fib_n_minus_2");

    llvm::Value* result = builder.CreateAdd(fibNMinus1, fibNMinus2, "result");
    builder.CreateBr(retBlock);

    // Return block
    builder.SetInsertPoint(retBlock);
    llvm::PHINode* phi = builder.CreatePHI(builder.getInt32Ty(), 2, "phi");
    phi->addIncoming(n, entry);
    phi->addIncoming(result, recurse);

    builder.CreateRet(phi);

    // Verify and print the generated code
    llvm::verifyFunction(*fibFunc);
    module->print(llvm::outs(), nullptr);

    delete module;
    return 0;
}
