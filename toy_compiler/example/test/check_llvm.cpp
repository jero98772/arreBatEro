#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

int main() {
    LLVMContext Context;
    Module *ModuleOb = new Module("hello", Context);
    IRBuilder<> Builder(Context);

    // Declare printf function
    FunctionType *PrintfType = FunctionType::get(IntegerType::getInt32Ty(Context),
                                                 PointerType::get(Type::getInt8Ty(Context), 0),
                                                 true);
    Function *PrintfFunc = Function::Create(PrintfType, Function::ExternalLinkage, "printf", ModuleOb);

    // Define main function
    FunctionType *MainType = FunctionType::get(IntegerType::getInt32Ty(Context), false);
    Function *MainFunc = Function::Create(MainType, Function::ExternalLinkage, "main", ModuleOb);

    // Create entry block
    BasicBlock *Entry = BasicBlock::Create(Context, "entry", MainFunc);
    Builder.SetInsertPoint(Entry);

    // Create "Hello, World!" string
    Value *HelloStr = Builder.CreateGlobalStringPtr("Hello, World!\n");

    // Call printf
    Builder.CreateCall(PrintfFunc, {HelloStr});

    // Return 0
    Builder.CreateRet(ConstantInt::get(Type::getInt32Ty(Context), 0));

    // Verify the function
    verifyFunction(*MainFunc);

    // Print LLVM IR
    ModuleOb->print(outs(), nullptr);

    delete ModuleOb;
    return 0;
}
