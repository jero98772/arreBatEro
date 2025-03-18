import sympy
def chain_rule():
    """Computes the derivative of a composite function using the chain rule."""
    inner_derivative = sp.diff(inner_func, var)
    outer_derivative = sp.diff(outer_func, var).subs(var, inner_func)
    return outer_derivative * inner_derivative
# Define the variable
x = sp.symbols('x')

# Example: f(x) = sin(x^2), where outer = sin(u), inner = x^2
inner_function = x**2
outer_function = sp.sin(x)

# Compute the derivative using the chain rule
derivative = chain_rule(outer_function, inner_function, x)
print("Derivative:", derivative)
