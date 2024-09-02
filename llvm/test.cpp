#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module* module = new llvm::Module("my_module", context);
    llvm::IRBuilder<> builder(context);

    // Create a simple function: int foo() { return 42; }
    llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getInt32Ty(), false);
    llvm::Function *fooFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "foo", module);
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fooFunc);
    builder.SetInsertPoint(entry);
    builder.CreateRet(builder.getInt32(42));

    // Verify the function
    llvm::verifyFunction(*fooFunc);

    // Print the generated LLVM IR
    module->print(llvm::outs(), nullptr);

    delete module;
    return 0;
}
