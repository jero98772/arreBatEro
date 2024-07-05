import ctypes

# Load the shared library
lib = ctypes.CDLL('./libsum_three.so')  # Use the correct path for your system

# Define argument and return types
lib.sum_three.argtypes = (ctypes.c_int, ctypes.c_int, ctypes.c_int)
lib.sum_three.restype = ctypes.c_int

# Call the function
result = lib.sum_three(1, 2, 3)
print("Sum:", result)  # Output should be 6
