#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "jit.h"
#include <fstream>
#include <iostream>

/**
 * @brief Main entry point for the toy compiler.
 * 
 * This is the main function that orchestrates the compilation process for a 
 * simple toy programming language. It performs the following steps:
 * 1. Accepts the source code file as a command-line argument.
 * 2. Reads the source code from the specified file.
 * 3. Tokenizes the code into meaningful tokens using the lexer.
 * 4. Parses the tokens into an abstract syntax tree (AST) using the parser.
 * 5. Generates LLVM intermediate representation (IR) from the AST.
 * 6. Executes the generated code using Just-In-Time (JIT) compilation.
 * 
 * @param argc The number of command-line arguments passed to the program.
 * @param argv Array of command-line argument strings.
 * @return 0 on successful execution, 1 if an error occurs.
 */
int main(int argc, char *argv[]) {
    // Check if the user has provided the source file as an argument.
    if (argc < 2) {
        std::cerr << "Usage: ./toy_compiler <source.toy>" << std::endl;
        return 1;  // Return error code if no file is provided.
    }

    // Attempt to open the source code file.
    std::ifstream file(argv[1]);
    if (!file) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;  // Return error code if file cannot be opened.
    }

    // Read the entire content of the source code file into a string.
    std::string code((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    
    // Tokenize the source code into a list of tokens.
    // This step converts raw source code into manageable language components.
    auto tokens = tokenize(code);
    
    // Parse the tokens into an abstract syntax tree (AST).
    // The AST represents the structure and meaning of the code.
    auto ast = parse(tokens);
    
    // Generate LLVM intermediate representation (IR) from the AST.
    // This step transforms the AST into a format that can be compiled or executed.
    generateLLVMIR(ast);
    
    // Execute the LLVM IR using Just-In-Time (JIT) compilation.
    // This allows for running the generated code directly.
    executeJIT();

    // Return 0 to indicate successful execution.
    return 0;
}
