class PDA:
    def __init__(self):
        # Initialize the stack and set the state to 'q0'
        self.stack = []
        self.state = 'q0'

    def transition(self, symbol):
        if self.state == 'q0':
            if symbol.isdigit() or (symbol.startswith('-') and symbol[1:].isdigit()):
                # Push the entire number (including negative) onto the stack
                self.stack.append(int(symbol))
            elif symbol in '+-*/' and len(self.stack) >= 2:
                # Pop two operands from the stack
                operand2 = self.stack.pop()
                operand1 = self.stack.pop()

                # Perform the operation and push the result back onto the stack
                if symbol == '+':
                    self.stack.append(operand1 + operand2)
                elif symbol == '-':
                    self.stack.append(operand1 - operand2)
                elif symbol == '*':
                    self.stack.append(operand1 * operand2)
                elif symbol == '/':
                    self.stack.append(int(operand1 / operand2))  # Use integer division
            else:
                self.state = 'reject'
        else:
            self.state = 'reject'

    def process(self, input_symbols):
        for symbol in input_symbols:
            self.transition(symbol)
            if self.state == 'reject':
                break

        if len(self.stack) == 1 and self.state != 'reject':
            return self.stack.pop()
        else:
            return "reject"

# Example usage of the PDA for stack arithmetic with multi-digit numbers
pda = PDA()

# Test expressions in RPN (Reverse Polish Notation) with multi-digit numbers
expressions = [
    "34 56 +",          # 34 + 56 = 90
    "123 456 * 789 +",  # (123 * 456) + 789 = 56247
    "1000 200 50 / -",  # 1000 - (200 / 50) = 996
    "12345 67890 + 2 *" # (12345 + 67890) * 2 = 160470
]

for expr in expressions:
    tokens = expr.split()  # Split by spaces to handle multi-digit numbers
    result = pda.process(tokens)
    print(f"Expression '{expr}' evaluates to: {result}")

