#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
//#include <llvm/Transforms/IPO/InlinerPass.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/raw_ostream.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module* module = new llvm::Module("inline_module", context);
    llvm::IRBuilder<> builder(context);

    // Define the function signature: int add(int a, int b)
    llvm::FunctionType *addFuncType = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt32Ty(), builder.getInt32Ty()}, false);
    llvm::Function *addFunc = llvm::Function::Create(addFuncType, llvm::Function::ExternalLinkage, "add", module);

    // Create entry block for add function
    llvm::BasicBlock *addEntry = llvm::BasicBlock::Create(context, "entry", addFunc);
    builder.SetInsertPoint(addEntry);

    // Get function arguments
    llvm::Function::arg_iterator args = addFunc->arg_begin();
    llvm::Value* a = args++;
    a->setName("a");
    llvm::Value* b = args++;
    b->setName("b");

    // Add and return the result
    llvm::Value* sum = builder.CreateAdd(a, b, "sum");
    builder.CreateRet(sum);

    // Define the function signature: int callAdd()
    llvm::FunctionType *callAddFuncType = llvm::FunctionType::get(builder.getInt32Ty(), false);
    llvm::Function *callAddFunc = llvm::Function::Create(callAddFuncType, llvm::Function::ExternalLinkage, "callAdd", module);

    // Create entry block for callAdd function
    llvm::BasicBlock *callAddEntry = llvm::BasicBlock::Create(context, "entry", callAddFunc);
    builder.SetInsertPoint(callAddEntry);

    // Call add(1, 2)
    llvm::Value* addResult = builder.CreateCall(addFunc, {builder.getInt32(1), builder.getInt32(2)}, "addResult");
    builder.CreateRet(addResult);

    // Inline the add function
    llvm::legacy::PassManager passManager;
    passManager.add(llvm::createFunctionInliningPass());
    passManager.run(*module);

    // Print the inlined module
    module->print(llvm::outs(), nullptr);

    delete module;
    return 0;
}
