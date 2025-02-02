#include "parser.hpp"

/**
 * @brief Constructs a Parser with the provided Lexer.
 * 
 * The constructor initializes the parser with a reference to the Lexer object.
 * It also retrieves the first token from the lexer to begin parsing.
 * 
 * @param lex The lexer object used for tokenizing the input source code.
 */
Parser::Parser(Lexer &lex) : lexer(lex) { currentToken = lexer.getNextToken(); }

/**
 * @brief Parses an expression and returns the corresponding AST node.
 * 
 * This method parses a simple arithmetic expression. It handles the parsing 
 * of numbers, operators, and constructs a binary expression tree if there is an 
 * operator in the expression. The method supports recursive parsing for expressions 
 * with multiple operands and operators.
 * 
 * @return A unique pointer to the root AST node representing the parsed expression.
 *         It can either be a single number or a binary expression.
 */
std::unique_ptr<ASTNode> Parser::parseExpression() {
    // Parse the left-hand side of the expression as a number.
    auto left = std::make_unique<NumberExpr>(std::stoi(currentToken.value));

    // Move to the next token after parsing the number.
    currentToken = lexer.getNextToken();

    // If the next token is an operator, parse the right-hand side of the expression.
    if (currentToken.type == TokenType::OPERATOR) {
        // Get the operator and move to the next token.
        char op = currentToken.value[0];
        currentToken = lexer.getNextToken();

        // Recursively parse the right-hand side expression and create a BinaryExpr node.
        auto right = parseExpression();
        return std::make_unique<BinaryExpr>(std::move(left), op, std::move(right));
    }

    // If no operator is found, return the left-hand side (which is a number).
    return left;
}
