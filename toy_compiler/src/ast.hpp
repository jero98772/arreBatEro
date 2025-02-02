#ifndef AST_HPP
#define AST_HPP

#include <memory>
#include <vector>
#include <string>

/**
 * @brief Base class for all Abstract Syntax Tree (AST) nodes.
 */
class ASTNode {
public:
    virtual ~ASTNode() = default; ///< Virtual destructor to ensure proper cleanup of derived classes.
};

/**
 * @brief Represents a numeric expression node in the AST.
 */
class NumberExpr : public ASTNode {
public:
    int value; ///< The integer value of the numeric expression.

    /**
     * @brief Constructs a NumberExpr with a given integer value.
     * @param val The integer value to be stored in this node.
     */
    explicit NumberExpr(int val) : value(val) {}
};

/**
 * @brief Represents a binary expression node in the AST (e.g., addition, subtraction).
 */
class BinaryExpr : public ASTNode {
public:
    std::unique_ptr<ASTNode> left;  ///< Pointer to the left operand.
    std::unique_ptr<ASTNode> right; ///< Pointer to the right operand.
    char op; ///< The binary operator (e.g., '+', '-', '*', '/').

    /**
     * @brief Constructs a BinaryExpr with a left operand, an operator, and a right operand.
     * @param l Unique pointer to the left operand.
     * @param o The binary operator.
     * @param r Unique pointer to the right operand.
     */
    BinaryExpr(std::unique_ptr<ASTNode> l, char o, std::unique_ptr<ASTNode> r)
        : left(std::move(l)), op(o), right(std::move(r)) {}
};

#endif // AST_HPP
