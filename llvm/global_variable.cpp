#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module* module = new llvm::Module("global_var_module", context);
    llvm::IRBuilder<> builder(context);

    // Create a global variable: int x = 5;
    llvm::GlobalVariable* gVar = new llvm::GlobalVariable(
        *module,
        builder.getInt32Ty(),
        false,
        llvm::GlobalValue::ExternalLinkage,
        builder.getInt32(5),
        "x"
    );

    // Define the function signature: int getX()
    llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getInt32Ty(), false);
    llvm::Function *getXFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "getX", module);

    // Create entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", getXFunc);
    builder.SetInsertPoint(entry);

    // Load the global variable and return it
    llvm::Value* loadX = builder.CreateLoad(builder.getInt32Ty(), gVar, "loadx");
    builder.CreateRet(loadX);

    // Verify and print the generated code
    llvm::verifyFunction(*getXFunc);
    module->print(llvm::outs(), nullptr);

    delete module;
    return 0;
}
