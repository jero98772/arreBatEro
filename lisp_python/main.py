import cl4py
from pathlib import Path

# Start the Lisp connection
lisp = cl4py.Lisp()

# Load the Lisp file (adjust the path accordingly)
lisp.eval(f'(load "{Path("example.lisp").absolute()}")')

# Call the Lisp function and capture the result
result = lisp.eval('(add-two-numbers 3 5)')

# Print the result
print("Result from Lisp:", result)

