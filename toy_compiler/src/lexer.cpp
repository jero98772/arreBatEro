#include "lexer.hpp"
#include <cctype>

/**
 * @brief Constructs a Lexer object that processes the given source string.
 * 
 * The lexer will iterate through the provided source string character by character
 * and convert the input into a sequence of tokens.
 * 
 * @param src The source string to tokenize.
 */
Lexer::Lexer(const std::string &src) : source(src), pos(0) {}

/**
 * @brief Retrieves the next token from the source string.
 * 
 * The method processes the input string character by character, ignoring whitespace
 * and identifying numbers, keywords, operators, identifiers, and other symbols.
 * 
 * @return A Token object representing the next token in the source code.
 *         If the end of the source string is reached, returns a token of type END.
 */
Token Lexer::getNextToken() {
    // Skip any whitespace characters.
    while (pos < source.length() && isspace(source[pos])) pos++;

    // If we have reached the end of the source, return an END token.
    if (pos >= source.length()) return {TokenType::END, ""};

    char current = source[pos];

    // If the current character is a digit, it represents a number.
    if (isdigit(current)) {
        std::string num;
        while (isdigit(source[pos])) num += source[pos++];
        return {TokenType::NUMBER, num};
    }

    // If the current character is a letter, it may represent a keyword or identifier.
    if (isalpha(current)) {
        std::string ident;
        while (isalnum(source[pos])) ident += source[pos++];
        if (ident == "int") return {TokenType::INT, ident};
        if (ident == "return") return {TokenType::RETURN, ident};
        return {TokenType::IDENTIFIER, ident};
    }

    // Handle various operators and symbols.
    switch (current) {
        case '+': case '-': case '*': case '/': 
            return {TokenType::OPERATOR, std::string(1, source[pos++])};
        case '(': 
            return {TokenType::PAREN_OPEN, "("}; pos++;
        case ')': 
            return {TokenType::PAREN_CLOSE, ")"}; pos++;
        case '{': 
            return {TokenType::BRACE_OPEN, "{"}; pos++;
        case '}': 
            return {TokenType::BRACE_CLOSE, "}"}; pos++;
        case ';': 
            return {TokenType::SEMICOLON, ";"}; pos++;
    }

    // If no valid token is found, return END token.
    return {TokenType::END, ""};
}
