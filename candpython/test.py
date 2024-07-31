import ctypes
#gcc -shared -o mylib.so -fPIC mylib.c

# Load the shared library into ctypes
mylib = ctypes.CDLL('./mylib.so')

# Call the functions
mylib.hello()

# Define the argument and return types for the 'add' function
mylib.add.argtypes = (ctypes.c_int, ctypes.c_int)
mylib.add.restype = ctypes.c_int

result = mylib.add(3, 4)
print(f"Result of add: {result}")
