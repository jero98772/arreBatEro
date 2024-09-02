#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/DerivedTypes.h> // for ArrayRef

int main() {
    llvm::LLVMContext context;
    llvm::Module* module = new llvm::Module("struct_module", context);
    llvm::IRBuilder<> builder(context);

    // Define a struct type: struct MyStruct { int a; int b; };
    llvm::StructType *myStructType = llvm::StructType::create(context, "MyStruct");
    myStructType->setBody({builder.getInt32Ty(), builder.getInt32Ty()}); // Use ArrayRef here

    // Define the function signature: int sumStruct(MyStruct* s)
    llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getInt32Ty(), {myStructType->getPointerTo()}, false);
    llvm::Function *sumStructFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "sumStruct", module);

    // Create entry block
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", sumStructFunc);
    builder.SetInsertPoint(entry);

    // Get function argument (pointer to MyStruct)
    llvm::Value* s = sumStructFunc->arg_begin();
    s->setName("s");

    // Load struct members
    llvm::Value* aPtr = builder.CreateStructGEP(myStructType, s, 0, "aPtr");
    llvm::Value* bPtr = builder.CreateStructGEP(myStructType, s, 1, "bPtr");

    llvm::Value* a = builder.CreateLoad(builder.getInt32Ty(), aPtr, "a");
    llvm::Value* b = builder.CreateLoad(builder.getInt32Ty(), bPtr, "b");

    // Add them and return the result
    llvm::Value* result = builder.CreateAdd(a, b, "result");
    builder.CreateRet(result);

    // Verify and print the generated code
    llvm::verifyFunction(*sumStructFunc);
    module->print(llvm::outs(), nullptr);

    delete module;
    return 0;
}
