#include "codegen.hpp"

/**
 * @class CodeGen
 * @brief A simple LLVM-based code generator for an AST.
 */
CodeGen::CodeGen() : module("toy", context), builder(context) {}

/**
 * @brief Generates LLVM IR for a given AST node.
 * 
 * This function takes an ASTNode and generates the corresponding LLVM IR representation.
 * It supports NumberExpr and BinaryExpr nodes.
 * 
 * @param node Pointer to the ASTNode to generate code for.
 * @return llvm::Value* The generated LLVM IR value.
 */
llvm::Value* CodeGen::generate(ASTNode *node) {
    if (auto num = dynamic_cast<NumberExpr*>(node)) {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), num->value);
    }
    if (auto binOp = dynamic_cast<BinaryExpr*>(node)) {
        auto L = generate(binOp->left.get());
        auto R = generate(binOp->right.get());
        if (binOp->op == '+') return builder.CreateAdd(L, R, "addtmp");
    }
    return nullptr;
}

/**
 * @brief Prints the generated LLVM IR to the standard output.
 */
void CodeGen::printIR() {
    module.print(llvm::outs(), nullptr);
}
