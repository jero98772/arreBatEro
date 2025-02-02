#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "ast.hpp"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

/**
 * @class CodeGen
 * @brief A simple LLVM-based code generator for an AST.
 */
class CodeGen {
public:
    /**
     * @brief Constructs a CodeGen instance and initializes LLVM structures.
     */
    CodeGen();

    /**
     * @brief Generates LLVM IR for a given AST node.
     * 
     * This function takes an ASTNode and generates the corresponding LLVM IR representation.
     * 
     * @param node Pointer to the ASTNode to generate code for.
     * @return llvm::Value* The generated LLVM IR value.
     */
    llvm::Value* generate(ASTNode *node);

    /**
     * @brief Prints the generated LLVM IR to the standard output.
     */
    void printIR();

private:
    llvm::LLVMContext context; ///< The LLVM context for managing global state.
    llvm::Module module; ///< The LLVM module containing the generated code.
    llvm::IRBuilder<> builder; ///< The LLVM IR builder for creating instructions.
};

#endif
