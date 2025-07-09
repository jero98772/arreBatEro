#!/usr/bin/env python3
"""
Jack Language Compiler - Translates Jack code to VM code
Implementation of Projects 10 and 11 from nand2tetris course
"""

import re
import os
import sys
from enum import Enum
from typing import List, Dict, Optional, Tuple, Any


class TokenType(Enum):
    KEYWORD = "keyword"
    SYMBOL = "symbol"
    IDENTIFIER = "identifier"
    INT_CONST = "integerConstant"
    STRING_CONST = "stringConstant"


class SymbolKind(Enum):
    STATIC = "static"
    FIELD = "field"
    ARG = "argument"
    VAR = "var"
    CLASS = "class"
    SUBROUTINE = "subroutine"


class VMCommand(Enum):
    ADD = "add"
    SUB = "sub"
    NEG = "neg"
    EQ = "eq"
    GT = "gt"
    LT = "lt"
    AND = "and"
    OR = "or"
    NOT = "not"


class VMSegment(Enum):
    CONST = "constant"
    ARG = "argument"
    LOCAL = "local"
    STATIC = "static"
    THIS = "this"
    THAT = "that"
    POINTER = "pointer"
    TEMP = "temp"


class Token:
    def __init__(self, token_type: TokenType, value: str):
        self.type = token_type
        self.value = value

    def __repr__(self):
        return f"Token({self.type}, '{self.value}')"


class SymbolInfo:
    def __init__(self, name: str, type_: str, kind: SymbolKind, index: int):
        self.name = name
        self.type = type_
        self.kind = kind
        self.index = index

    def __repr__(self):
        return f"Symbol({self.name}, {self.type}, {self.kind}, {self.index})"


class SymbolTable:
    def __init__(self):
        self.class_scope = {}  # static and field variables
        self.subroutine_scope = {}  # arg and var variables
        self.static_count = 0
        self.field_count = 0
        self.arg_count = 0
        self.var_count = 0

    def start_subroutine(self):
        """Reset subroutine scope for new subroutine"""
        self.subroutine_scope = {}
        self.arg_count = 0
        self.var_count = 0

    def define(self, name: str, type_: str, kind: SymbolKind):
        """Define a new symbol"""
        if kind == SymbolKind.STATIC:
            index = self.static_count
            self.static_count += 1
            self.class_scope[name] = SymbolInfo(name, type_, kind, index)
        elif kind == SymbolKind.FIELD:
            index = self.field_count
            self.field_count += 1
            self.class_scope[name] = SymbolInfo(name, type_, kind, index)
        elif kind == SymbolKind.ARG:
            index = self.arg_count
            self.arg_count += 1
            self.subroutine_scope[name] = SymbolInfo(name, type_, kind, index)
        elif kind == SymbolKind.VAR:
            index = self.var_count
            self.var_count += 1
            self.subroutine_scope[name] = SymbolInfo(name, type_, kind, index)

    def var_count_of(self, kind: SymbolKind) -> int:
        """Return number of variables of given kind"""
        if kind == SymbolKind.STATIC:
            return self.static_count
        elif kind == SymbolKind.FIELD:
            return self.field_count
        elif kind == SymbolKind.ARG:
            return self.arg_count
        elif kind == SymbolKind.VAR:
            return self.var_count
        return 0

    def kind_of(self, name: str) -> Optional[SymbolKind]:
        """Return kind of named identifier"""
        if name in self.subroutine_scope:
            return self.subroutine_scope[name].kind
        elif name in self.class_scope:
            return self.class_scope[name].kind
        return None

    def type_of(self, name: str) -> Optional[str]:
        """Return type of named identifier"""
        if name in self.subroutine_scope:
            return self.subroutine_scope[name].type
        elif name in self.class_scope:
            return self.class_scope[name].type
        return None

    def index_of(self, name: str) -> Optional[int]:
        """Return index of named identifier"""
        if name in self.subroutine_scope:
            return self.subroutine_scope[name].index
        elif name in self.class_scope:
            return self.class_scope[name].index
        return None


class VMWriter:
    def __init__(self):
        self.output = []

    def write_push(self, segment: VMSegment, index: int):
        """Write push command"""
        self.output.append(f"push {segment.value} {index}")

    def write_pop(self, segment: VMSegment, index: int):
        """Write pop command"""
        self.output.append(f"pop {segment.value} {index}")

    def write_arithmetic(self, command: VMCommand):
        """Write arithmetic command"""
        self.output.append(command.value)

    def write_label(self, label: str):
        """Write label"""
        self.output.append(f"label {label}")

    def write_goto(self, label: str):
        """Write goto"""
        self.output.append(f"goto {label}")

    def write_if(self, label: str):
        """Write if-goto"""
        self.output.append(f"if-goto {label}")

    def write_call(self, name: str, n_args: int):
        """Write call"""
        self.output.append(f"call {name} {n_args}")

    def write_function(self, name: str, n_locals: int):
        """Write function"""
        self.output.append(f"function {name} {n_locals}")

    def write_return(self):
        """Write return"""
        self.output.append("return")

    def get_output(self) -> str:
        """Get the generated VM code"""
        return "\n".join(self.output)


class JackTokenizer:
    KEYWORDS = {
        'class', 'constructor', 'function', 'method', 'field', 'static',
        'var', 'int', 'char', 'boolean', 'void', 'true', 'false', 'null',
        'this', 'let', 'do', 'if', 'else', 'while', 'return'
    }

    SYMBOLS = {
        '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/',
        '&', '|', '<', '>', '=', '~'
    }

    SYMBOL_MAP = {
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        '&': '&amp;'
    }

    def __init__(self, input_text: str):
        self.input_text = input_text
        self.tokens = []
        self.current_token = 0
        self._tokenize()

    def _tokenize(self):
        """Tokenize the input text"""
        # Remove comments and whitespace
        text = self._remove_comments(self.input_text)
        
        i = 0
        while i < len(text):
            if text[i].isspace():
                i += 1
                continue
            
            # String constant
            if text[i] == '"':
                i += 1
                start = i
                while i < len(text) and text[i] != '"':
                    i += 1
                string_val = text[start:i]
                self.tokens.append(Token(TokenType.STRING_CONST, string_val))
                i += 1
            
            # Symbol
            elif text[i] in self.SYMBOLS:
                symbol = text[i]
                self.tokens.append(Token(TokenType.SYMBOL, symbol))
                i += 1
            
            # Number
            elif text[i].isdigit():
                start = i
                while i < len(text) and text[i].isdigit():
                    i += 1
                number = text[start:i]
                self.tokens.append(Token(TokenType.INT_CONST, number))
            
            # Identifier or keyword
            elif text[i].isalpha() or text[i] == '_':
                start = i
                while i < len(text) and (text[i].isalnum() or text[i] == '_'):
                    i += 1
                word = text[start:i]
                if word in self.KEYWORDS:
                    self.tokens.append(Token(TokenType.KEYWORD, word))
                else:
                    self.tokens.append(Token(TokenType.IDENTIFIER, word))
            else:
                i += 1

    def _remove_comments(self, text: str) -> str:
        """Remove comments from text"""
        # Remove // comments
        lines = text.split('\n')
        cleaned_lines = []
        for line in lines:
            comment_pos = line.find('//')
            if comment_pos != -1:
                line = line[:comment_pos]
            cleaned_lines.append(line)
        text = '\n'.join(cleaned_lines)
        
        # Remove /* */ comments
        while '/*' in text:
            start = text.find('/*')
            end = text.find('*/', start)
            if end != -1:
                text = text[:start] + text[end + 2:]
            else:
                break
        
        return text

    def has_more_tokens(self) -> bool:
        """Check if there are more tokens"""
        return self.current_token < len(self.tokens)

    def advance(self):
        """Move to next token"""
        if self.has_more_tokens():
            self.current_token += 1

    def token_type(self) -> TokenType:
        """Return current token type"""
        if self.has_more_tokens():
            return self.tokens[self.current_token].type
        return None

    def keyword(self) -> str:
        """Return current keyword"""
        return self.tokens[self.current_token].value

    def symbol(self) -> str:
        """Return current symbol"""
        return self.tokens[self.current_token].value

    def identifier(self) -> str:
        """Return current identifier"""
        return self.tokens[self.current_token].value

    def int_val(self) -> int:
        """Return current integer value"""
        return int(self.tokens[self.current_token].value)

    def string_val(self) -> str:
        """Return current string value"""
        return self.tokens[self.current_token].value

    def peek(self) -> Optional[Token]:
        """Peek at current token without advancing"""
        if self.has_more_tokens():
            return self.tokens[self.current_token]
        return None


class CompilationEngine:
    def __init__(self, tokenizer: JackTokenizer):
        self.tokenizer = tokenizer
        self.symbol_table = SymbolTable()
        self.vm_writer = VMWriter()
        self.class_name = ""
        self.subroutine_name = ""
        self.subroutine_type = ""
        self.label_count = 0

    def _next_label(self) -> str:
        """Generate next unique label"""
        label = f"LABEL_{self.label_count}"
        self.label_count += 1
        return label

    def _kind_to_segment(self, kind: SymbolKind) -> VMSegment:
        """Convert symbol kind to VM segment"""
        if kind == SymbolKind.VAR:
            return VMSegment.LOCAL
        elif kind == SymbolKind.ARG:
            return VMSegment.ARG
        elif kind == SymbolKind.FIELD:
            return VMSegment.THIS
        elif kind == SymbolKind.STATIC:
            return VMSegment.STATIC
        return VMSegment.LOCAL

    def compile_class(self):
        """Compile class"""
        # 'class' className '{' classVarDec* subroutineDec* '}'
        self.tokenizer.advance()  # 'class'
        self.class_name = self.tokenizer.identifier()
        self.tokenizer.advance()  # className
        self.tokenizer.advance()  # '{'
        
        # Compile class variable declarations
        while (self.tokenizer.has_more_tokens() and 
               self.tokenizer.peek().value in ['static', 'field']):
            self.compile_class_var_dec()
        
        # Compile subroutine declarations
        while (self.tokenizer.has_more_tokens() and 
               self.tokenizer.peek().value in ['constructor', 'function', 'method']):
            self.compile_subroutine()
        
        self.tokenizer.advance()  # '}'

    def compile_class_var_dec(self):
        """Compile class variable declaration"""
        # ('static' | 'field') type varName (',' varName)* ';'
        kind_str = self.tokenizer.keyword()
        kind = SymbolKind.STATIC if kind_str == 'static' else SymbolKind.FIELD
        self.tokenizer.advance()  # kind
        
        type_ = self.tokenizer.identifier() if self.tokenizer.token_type() == TokenType.IDENTIFIER else self.tokenizer.keyword()
        self.tokenizer.advance()  # type
        
        # First variable name
        name = self.tokenizer.identifier()
        self.symbol_table.define(name, type_, kind)
        self.tokenizer.advance()  # varName
        
        # Additional variable names
        while self.tokenizer.peek() and self.tokenizer.peek().value == ',':
            self.tokenizer.advance()  # ','
            name = self.tokenizer.identifier()
            self.symbol_table.define(name, type_, kind)
            self.tokenizer.advance()  # varName
        
        self.tokenizer.advance()  # ';'

    def compile_subroutine(self):
        """Compile subroutine"""
        # ('constructor' | 'function' | 'method') ('void' | type) subroutineName
        # '(' parameterList ')' subroutineBody
        self.symbol_table.start_subroutine()
        
        subroutine_type = self.tokenizer.keyword()
        self.subroutine_type = subroutine_type
        self.tokenizer.advance()  # subroutine type
        
        # For methods, add 'this' as first argument
        if subroutine_type == 'method':
            self.symbol_table.define('this', self.class_name, SymbolKind.ARG)
        
        return_type = self.tokenizer.identifier() if self.tokenizer.token_type() == TokenType.IDENTIFIER else self.tokenizer.keyword()
        self.tokenizer.advance()  # return type
        
        self.subroutine_name = self.tokenizer.identifier()
        self.tokenizer.advance()  # subroutine name
        
        self.tokenizer.advance()  # '('
        self.compile_parameter_list()
        self.tokenizer.advance()  # ')'
        
        self.compile_subroutine_body()

    def compile_parameter_list(self):
        """Compile parameter list"""
        # ((type varName) (',' type varName)*)?
        if (self.tokenizer.has_more_tokens() and 
            self.tokenizer.peek().value != ')'):
            
            # First parameter
            type_ = self.tokenizer.identifier() if self.tokenizer.token_type() == TokenType.IDENTIFIER else self.tokenizer.keyword()
            self.tokenizer.advance()  # type
            name = self.tokenizer.identifier()
            self.symbol_table.define(name, type_, SymbolKind.ARG)
            self.tokenizer.advance()  # name
            
            # Additional parameters
            while self.tokenizer.peek() and self.tokenizer.peek().value == ',':
                self.tokenizer.advance()  # ','
                type_ = self.tokenizer.identifier() if self.tokenizer.token_type() == TokenType.IDENTIFIER else self.tokenizer.keyword()
                self.tokenizer.advance()  # type
                name = self.tokenizer.identifier()
                self.symbol_table.define(name, type_, SymbolKind.ARG)
                self.tokenizer.advance()  # name

    def compile_subroutine_body(self):
        """Compile subroutine body"""
        # '{' varDec* statements '}'
        self.tokenizer.advance()  # '{'
        
        # Compile variable declarations
        while (self.tokenizer.has_more_tokens() and 
               self.tokenizer.peek().value == 'var'):
            self.compile_var_dec()
        
        # Write function declaration
        n_locals = self.symbol_table.var_count_of(SymbolKind.VAR)
        function_name = f"{self.class_name}.{self.subroutine_name}"
        self.vm_writer.write_function(function_name, n_locals)
        
        # Handle constructor/method setup
        if self.subroutine_type == 'constructor':
            # Allocate memory for object
            n_fields = self.symbol_table.var_count_of(SymbolKind.FIELD)
            self.vm_writer.write_push(VMSegment.CONST, n_fields)
            self.vm_writer.write_call("Memory.alloc", 1)
            self.vm_writer.write_pop(VMSegment.POINTER, 0)
        elif self.subroutine_type == 'method':
            # Set up 'this' pointer
            self.vm_writer.write_push(VMSegment.ARG, 0)
            self.vm_writer.write_pop(VMSegment.POINTER, 0)
        
        # Compile statements
        self.compile_statements()
        
        self.tokenizer.advance()  # '}'

    def compile_var_dec(self):
        """Compile variable declaration"""
        # 'var' type varName (',' varName)* ';'
        self.tokenizer.advance()  # 'var'
        
        type_ = self.tokenizer.identifier() if self.tokenizer.token_type() == TokenType.IDENTIFIER else self.tokenizer.keyword()
        self.tokenizer.advance()  # type
        
        # First variable
        name = self.tokenizer.identifier()
        self.symbol_table.define(name, type_, SymbolKind.VAR)
        self.tokenizer.advance()  # varName
        
        # Additional variables
        while self.tokenizer.peek() and self.tokenizer.peek().value == ',':
            self.tokenizer.advance()  # ','
            name = self.tokenizer.identifier()
            self.symbol_table.define(name, type_, SymbolKind.VAR)
            self.tokenizer.advance()  # varName
        
        self.tokenizer.advance()  # ';'

    def compile_statements(self):
        """Compile statements"""
        while (self.tokenizer.has_more_tokens() and 
               self.tokenizer.peek().value in ['let', 'if', 'while', 'do', 'return']):
            statement = self.tokenizer.peek().value
            if statement == 'let':
                self.compile_let()
            elif statement == 'if':
                self.compile_if()
            elif statement == 'while':
                self.compile_while()
            elif statement == 'do':
                self.compile_do()
            elif statement == 'return':
                self.compile_return()

    def compile_let(self):
        """Compile let statement"""
        # 'let' varName ('[' expression ']')? '=' expression ';'
        self.tokenizer.advance()  # 'let'
        
        var_name = self.tokenizer.identifier()
        self.tokenizer.advance()  # varName
        
        # Check for array indexing
        is_array = False
        if self.tokenizer.peek() and self.tokenizer.peek().value == '[':
            is_array = True
            # Push array base address
            kind = self.symbol_table.kind_of(var_name)
            index = self.symbol_table.index_of(var_name)
            segment = self._kind_to_segment(kind)
            self.vm_writer.write_push(segment, index)
            
            self.tokenizer.advance()  # '['
            self.compile_expression()  # array index
            self.tokenizer.advance()  # ']'
            
            # Add base + index
            self.vm_writer.write_arithmetic(VMCommand.ADD)
        
        self.tokenizer.advance()  # '='
        self.compile_expression()  # right side value
        self.tokenizer.advance()  # ';'
        
        if is_array:
            # Store in array
            self.vm_writer.write_pop(VMSegment.TEMP, 0)  # Save expression value
            self.vm_writer.write_pop(VMSegment.POINTER, 1)  # Set THAT to array address
            self.vm_writer.write_push(VMSegment.TEMP, 0)  # Push expression value
            self.vm_writer.write_pop(VMSegment.THAT, 0)  # Store in array
        else:
            # Store in variable
            kind = self.symbol_table.kind_of(var_name)
            index = self.symbol_table.index_of(var_name)
            segment = self._kind_to_segment(kind)
            self.vm_writer.write_pop(segment, index)

    def compile_if(self):
        """Compile if statement"""
        # 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
        else_label = self._next_label()
        end_label = self._next_label()
        
        self.tokenizer.advance()  # 'if'
        self.tokenizer.advance()  # '('
        self.compile_expression()
        self.tokenizer.advance()  # ')'
        
        # If condition is false, jump to else
        self.vm_writer.write_arithmetic(VMCommand.NOT)
        self.vm_writer.write_if(else_label)
        
        self.tokenizer.advance()  # '{'
        self.compile_statements()
        self.tokenizer.advance()  # '}'
        
        # Jump to end after if block
        self.vm_writer.write_goto(end_label)
        self.vm_writer.write_label(else_label)
        
        # Check for else clause
        if self.tokenizer.peek() and self.tokenizer.peek().value == 'else':
            self.tokenizer.advance()  # 'else'
            self.tokenizer.advance()  # '{'
            self.compile_statements()
            self.tokenizer.advance()  # '}'
        
        self.vm_writer.write_label(end_label)

    def compile_while(self):
        """Compile while statement"""
        # 'while' '(' expression ')' '{' statements '}'
        loop_label = self._next_label()
        end_label = self._next_label()
        
        self.vm_writer.write_label(loop_label)
        
        self.tokenizer.advance()  # 'while'
        self.tokenizer.advance()  # '('
        self.compile_expression()
        self.tokenizer.advance()  # ')'
        
        # If condition is false, exit loop
        self.vm_writer.write_arithmetic(VMCommand.NOT)
        self.vm_writer.write_if(end_label)
        
        self.tokenizer.advance()  # '{'
        self.compile_statements()
        self.tokenizer.advance()  # '}'
        
        # Jump back to loop condition
        self.vm_writer.write_goto(loop_label)
        self.vm_writer.write_label(end_label)

    def compile_do(self):
        """Compile do statement"""
        # 'do' subroutineCall ';'
        self.tokenizer.advance()  # 'do'
        self.compile_subroutine_call()
        self.tokenizer.advance()  # ';'
        
        # Do statement discards return value
        self.vm_writer.write_pop(VMSegment.TEMP, 0)

    def compile_return(self):
        """Compile return statement"""
        # 'return' expression? ';'
        self.tokenizer.advance()  # 'return'
        
        if self.tokenizer.peek() and self.tokenizer.peek().value != ';':
            self.compile_expression()
        else:
            # Void function returns 0
            self.vm_writer.write_push(VMSegment.CONST, 0)
        
        self.tokenizer.advance()  # ';'
        self.vm_writer.write_return()

    def compile_expression(self):
        """Compile expression"""
        # term (op term)*
        self.compile_term()
        
        while (self.tokenizer.peek() and 
               self.tokenizer.peek().value in ['+', '-', '*', '/', '&', '|', '<', '>', '=']):
            op = self.tokenizer.symbol()
            self.tokenizer.advance()  # operator
            self.compile_term()
            
            # Generate VM command for operator
            if op == '+':
                self.vm_writer.write_arithmetic(VMCommand.ADD)
            elif op == '-':
                self.vm_writer.write_arithmetic(VMCommand.SUB)
            elif op == '*':
                self.vm_writer.write_call("Math.multiply", 2)
            elif op == '/':
                self.vm_writer.write_call("Math.divide", 2)
            elif op == '&':
                self.vm_writer.write_arithmetic(VMCommand.AND)
            elif op == '|':
                self.vm_writer.write_arithmetic(VMCommand.OR)
            elif op == '<':
                self.vm_writer.write_arithmetic(VMCommand.LT)
            elif op == '>':
                self.vm_writer.write_arithmetic(VMCommand.GT)
            elif op == '=':
                self.vm_writer.write_arithmetic(VMCommand.EQ)

    def compile_term(self):
        """Compile term"""
        token = self.tokenizer.peek()
        
        if token.type == TokenType.INT_CONST:
            # Integer constant
            self.vm_writer.write_push(VMSegment.CONST, self.tokenizer.int_val())
            self.tokenizer.advance()
        
        elif token.type == TokenType.STRING_CONST:
            # String constant
            string_val = self.tokenizer.string_val()
            self.vm_writer.write_push(VMSegment.CONST, len(string_val))
            self.vm_writer.write_call("String.new", 1)
            
            for char in string_val:
                self.vm_writer.write_push(VMSegment.CONST, ord(char))
                self.vm_writer.write_call("String.appendChar", 2)
            
            self.tokenizer.advance()
        
        elif token.value in ['true', 'false', 'null', 'this']:
            # Keyword constant
            if token.value == 'true':
                self.vm_writer.write_push(VMSegment.CONST, 0)
                self.vm_writer.write_arithmetic(VMCommand.NOT)
            elif token.value in ['false', 'null']:
                self.vm_writer.write_push(VMSegment.CONST, 0)
            elif token.value == 'this':
                self.vm_writer.write_push(VMSegment.POINTER, 0)
            self.tokenizer.advance()
        
        elif token.type == TokenType.IDENTIFIER:
            # Variable, array element, or subroutine call
            self.tokenizer.advance()
            next_token = self.tokenizer.peek()
            
            if next_token and next_token.value == '[':
                # Array element
                var_name = token.value
                kind = self.symbol_table.kind_of(var_name)
                index = self.symbol_table.index_of(var_name)
                segment = self._kind_to_segment(kind)
                self.vm_writer.write_push(segment, index)
                
                self.tokenizer.advance()  # '['
                self.compile_expression()
                self.tokenizer.advance()  # ']'
                
                self.vm_writer.write_arithmetic(VMCommand.ADD)
                self.vm_writer.write_pop(VMSegment.POINTER, 1)
                self.vm_writer.write_push(VMSegment.THAT, 0)
            
            elif next_token and next_token.value in ['(', '.']:
                # Subroutine call - backtrack and handle
                self.tokenizer.current_token -= 1
                self.compile_subroutine_call()
            
            else:
                # Variable
                var_name = token.value
                kind = self.symbol_table.kind_of(var_name)
                index = self.symbol_table.index_of(var_name)
                segment = self._kind_to_segment(kind)
                self.vm_writer.write_push(segment, index)
        
        elif token.value == '(':
            # (expression)
            self.tokenizer.advance()  # '('
            self.compile_expression()
            self.tokenizer.advance()  # ')'
        
        elif token.value in ['-', '~']:
            # Unary operator
            op = self.tokenizer.symbol()
            self.tokenizer.advance()
            self.compile_term()
            
            if op == '-':
                self.vm_writer.write_arithmetic(VMCommand.NEG)
            elif op == '~':
                self.vm_writer.write_arithmetic(VMCommand.NOT)

    def compile_subroutine_call(self):
        """Compile subroutine call"""
        # subroutineName '(' expressionList ')' |
        # (className | varName) '.' subroutineName '(' expressionList ')'
        
        identifier = self.tokenizer.identifier()
        self.tokenizer.advance()
        
        n_args = 0
        subroutine_name = ""
        
        if self.tokenizer.peek() and self.tokenizer.peek().value == '.':
            # Method call or static function call
            self.tokenizer.advance()  # '.'
            subroutine_name = self.tokenizer.identifier()
            self.tokenizer.advance()
            
            # Check if identifier is a variable (method call) or class name (static call)
            var_type = self.symbol_table.type_of(identifier)
            if var_type:
                # Method call on object
                kind = self.symbol_table.kind_of(identifier)
                index = self.symbol_table.index_of(identifier)
                segment = self._kind_to_segment(kind)
                self.vm_writer.write_push(segment, index)
                full_name = f"{var_type}.{subroutine_name}"
                n_args = 1  # 'this' argument
            else:
                # Static function call
                full_name = f"{identifier}.{subroutine_name}"
                n_args = 0
        else:
            # Method call on current object
            subroutine_name = identifier
            full_name = f"{self.class_name}.{subroutine_name}"
            self.vm_writer.write_push(VMSegment.POINTER, 0)  # push 'this'
            n_args = 1
        
        self.tokenizer.advance()  # '('
        n_args += self.compile_expression_list()
        self.tokenizer.advance()  # ')'
        
        self.vm_writer.write_call(full_name, n_args)

    def compile_expression_list(self) -> int:
        """Compile expression list and return number of expressions"""
        n_expressions = 0
        
        if self.tokenizer.peek() and self.tokenizer.peek().value != ')':
            self.compile_expression()
            n_expressions = 1
            
            while self.tokenizer.peek() and self.tokenizer.peek().value == ',':
                self.tokenizer.advance()  # ','
                self.compile_expression()
                n_expressions += 1
        
        return n_expressions

    def get_vm_code(self) -> str:
        """Get the generated VM code"""
        return self.vm_writer.get_output()


class JackCompiler:
    def __init__(self):
        pass

    def compile_file(self, input_file: str) -> str:
        """Compile a single .jack file to VM code"""
        with open(input_file, 'r') as f:
            input_text = f.read()
        
        tokenizer = JackTokenizer(input_text)
        engine = CompilationEngine(tokenizer)
        engine.compile_class()
        
        return engine.get_vm_code()

    def compile_directory(self, directory: str):
        """Compile all .jack files in a directory"""
        jack_files = [f for f in os.listdir(directory) if f.endswith('.jack')]
        
        for jack_file in jack_files:
            input_path = os.path.join(directory, jack_file)
            output_path = os.path.join(directory, jack_file.replace('.jack', '.vm'))
            
            try:
                vm_code = self.compile_file(input_path)
                with open(output_path, 'w') as f:
                    f.write(vm_code)
                print(f"Compiled {jack_file} -> {os.path.basename(output_path)}")
            except Exception as e:
                print(f"Error compiling {jack_file}: {e}")


def main():
    """Main function for command line usage"""
    if len(sys.argv) != 2:
        print("Usage: python jack_compiler.py <input_file_or_directory>")
        sys.exit(1)
    
    input_path = sys.argv[1]
    compiler = JackCompiler()
    
    if os.path.isfile(input_path) and input_path.endswith('.jack'):
        # Compile single file
        try:
            vm_code = compiler.compile_file(input_path)
            output_path = input_path.replace('.jack', '.vm')
            with open(output_path, 'w') as f:
                f.write(vm_code)
            print(f"Compiled {os.path.basename(input_path)} -> {os.path.basename(output_path)}")
        except Exception as e:
            print(f"Error compiling {input_path}: {e}")
    
    elif os.path.isdir(input_path):
        # Compile directory
        compiler.compile_directory(input_path)
    
    else:
        print("Error: Input must be a .jack file or directory containing .jack files")
        sys.exit(1)


# Example usage and test functions
def test_compiler():
    """Test the compiler with sample Jack code"""
    
    # Test 1: Simple arithmetic
    simple_jack = """
    class Main {
        function void main() {
            do Output.printInt((3 * 2) + 1);
            return;
        }
    }
    """
    
    print("=== Test 1: Simple Arithmetic ===")
    tokenizer = JackTokenizer(simple_jack)
    engine = CompilationEngine(tokenizer)
    engine.compile_class()
    print(engine.get_vm_code())
    print()
    
    # Test 2: Class with fields and methods
    class_jack = """
    class Square {
        field int x, y;
        field int size;
        
        constructor Square new(int Ax, int Ay, int Asize) {
            let x = Ax;
            let y = Ay;
            let size = Asize;
            do draw();
            return this;
        }
        
        method void draw() {
            do Screen.setColor(true);
            do Screen.drawRectangle(x, y, x + size, y + size);
            return;
        }
        
        method void moveUp() {
            if (y > 0) {
                do Screen.setColor(false);
                do Screen.drawRectangle(x, (y + size) - 1, x + size, y + size);
                let y = y - 2;
                do Screen.setColor(true);
                do Screen.drawRectangle(x, y, x + size, y + 1);
            }
            return;
        }
    }
    """
    
    print("=== Test 2: Class with Constructor and Methods ===")
    tokenizer = JackTokenizer(class_jack)
    engine = CompilationEngine(tokenizer)
    engine.compile_class()
    print(engine.get_vm_code())
    print()
    
    # Test 3: Control structures
    control_jack = """
    class Main {
        function void main() {
            var int i;
            let i = 0;
            while (i < 10) {
                if (i > 5) {
                    do Output.printInt(i);
                }
                let i = i + 1;
            }
            return;
        }
    }
    """
    
    print("=== Test 3: Control Structures ===")
    tokenizer = JackTokenizer(control_jack)
    engine = CompilationEngine(tokenizer)
    engine.compile_class()
    print(engine.get_vm_code())


if __name__ == "__main__":
    # Uncomment to run tests
    test_compiler()
    
    # Run main compiler
    #main()