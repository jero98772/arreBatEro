#ifndef LEXER_HPP
#define LEXER_HPP

#include <string>
#include <vector>

/**
 * @brief Enum representing the different types of tokens.
 * 
 * This enum class defines all the possible types of tokens that the lexer can
 * recognize in the source code.
 */
enum class TokenType {
    INT,         /**< Represents the 'int' keyword */
    RETURN,      /**< Represents the 'return' keyword */
    IF,          /**< Represents the 'if' keyword */
    ELSE,        /**< Represents the 'else' keyword */
    WHILE,       /**< Represents the 'while' keyword */
    IDENTIFIER,  /**< Represents an identifier (variable or function name) */
    NUMBER,      /**< Represents a number */
    OPERATOR,    /**< Represents an operator (+, -, *, /) */
    PAREN_OPEN,  /**< Represents an open parenthesis '(' */
    PAREN_CLOSE, /**< Represents a close parenthesis ')' */
    BRACE_OPEN,  /**< Represents an open brace '{' */
    BRACE_CLOSE, /**< Represents a close brace '}' */
    SEMICOLON,   /**< Represents a semicolon ';' */
    END          /**< Represents the end of the input */
};

/**
 * @brief Represents a single token.
 * 
 * A token consists of a type (from the TokenType enum) and its value (a string
 * representation of the token).
 */
struct Token {
    TokenType type;  /**< The type of the token */
    std::string value; /**< The value of the token as a string */
};

/**
 * @brief Lexer class for tokenizing source code.
 * 
 * The Lexer class processes the input source code and generates a sequence of
 * tokens that represent the building blocks of the code. The lexer handles keywords,
 * operators, identifiers, numbers, and other symbols.
 */
class Lexer {
public:
    /**
     * @brief Constructs a Lexer with the provided source code.
     * 
     * Initializes the lexer with the source code to tokenize. The lexer will process
     * the code character by character.
     * 
     * @param source The source code to tokenize.
     */
    explicit Lexer(const std::string &source);

    /**
     * @brief Retrieves the next token from the source code.
     * 
     * Scans the source code character by character, identifying and returning
     * the next token. If the end of the source code is reached, an END token is returned.
     * 
     * @return A Token representing the next token in the source code.
     */
    Token getNextToken();

private:
    std::string source; /**< The source code to tokenize */
    size_t pos = 0;     /**< The current position in the source code */
};

#endif
