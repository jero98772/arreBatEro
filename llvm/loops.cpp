#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module* module = new llvm::Module("my_module", context);
    llvm::IRBuilder<> builder(context);

    // Define the function signature: int sum(int a, int b)
    llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt32Ty(), builder.getInt32Ty()}, false);
    llvm::Function *sumFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "sum", module);

    // Create entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", sumFunc);
    builder.SetInsertPoint(entry);

    // Get function arguments
    llvm::Function::arg_iterator args = sumFunc->arg_begin();
    llvm::Value* argA = args++;
    argA->setName("a");
    llvm::Value* argB = args++;
    argB->setName("b");

    // Initialize variables
    llvm::Value* sum = builder.CreateAlloca(builder.getInt32Ty(), nullptr, "sum");
    builder.CreateStore(argA, sum);
    
    // Create loop block
    llvm::BasicBlock *loopBlock = llvm::BasicBlock::Create(context, "loop", sumFunc);
    llvm::BasicBlock *afterLoopBlock = llvm::BasicBlock::Create(context, "afterloop", sumFunc);
    
    builder.CreateBr(loopBlock);
    builder.SetInsertPoint(loopBlock);
    
    // Load and increment sum
    llvm::Value* sumVal = builder.CreateLoad(builder.getInt32Ty(), sum, "sumval");
    llvm::Value* newSum = builder.CreateAdd(sumVal, argB, "newsum");
    builder.CreateStore(newSum, sum);

    // Check loop condition
    llvm::Value* condition = builder.CreateICmpSLT(newSum, builder.getInt32(100), "loopcond");
    builder.CreateCondBr(condition, loopBlock, afterLoopBlock);

    builder.SetInsertPoint(afterLoopBlock);

    // Return sum
    llvm::Value* finalSum = builder.CreateLoad(builder.getInt32Ty(), sum, "finalsum");
    builder.CreateRet(finalSum);

    // Verify and print the generated code
    llvm::verifyFunction(*sumFunc);
    module->print(llvm::outs(), nullptr);

    delete module;
    return 0;
}

