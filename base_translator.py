def convert_base(number: str, x_base: int, y_base: int) -> str:
    # Convert from x_base to decimal
    decimal_number = int(number, x_base)
    
    # Convert from decimal to y_base
    if y_base == 10:
        return str(decimal_number)
    
    # Define a helper function to convert a decimal number to the desired y_base
    def to_base(n, base):
        digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        result = ""
        while n > 0:
            result = digits[n % base] + result
            n //= base
        return result or "0"
    
    return to_base(decimal_number, y_base)

# Example usage:
print(convert_base("101", 2, 10))  # Binary (base 2) to Decimal (base 10) -> Output: "5"
print(convert_base("5", 10, 2))    # Decimal (base 10) to Binary (base 2) -> Output: "101"
print(convert_base("FF", 16, 2))   # Hexadecimal (base 16) to Binary (base 2) -> Output: "11111111"
print(convert_base("5afjgkj", 25, 27))   