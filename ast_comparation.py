import ast

class ASTComparator(ast.NodeVisitor):
    """
    Compare AST nodes while ignoring variable names and literal values.
    Only compares the structure and operations.
    """
    def __init__(self):
        self.structure = []
    
    def generic_visit(self, node):
        # Record node type
        self.structure.append(node.__class__.__name__)
        
        # Visit children
        super().generic_visit(node)
        
        return self.structure

def normalize_ast_structure(node):
    """
    Convert AST to a structure that ignores variable names and literal values.
    Only keeps the code structure and operations.
    """
    if isinstance(node, ast.AST):
        node_type = node.__class__.__name__
        
        # For names and constants, only record their type, not the actual value
        if isinstance(node, ast.Name):
            return ('Name',)
        elif isinstance(node, ast.Constant):
            return ('Constant', type(node.value).__name__)
        elif isinstance(node, ast.arg):
            # Ignore parameter names, just track that there's a parameter
            return ('arg',)
        
        # For other nodes, recursively normalize their fields
        fields = {}
        for field, value in ast.iter_fields(node):
            # Skip 'id' fields (variable names) and 'arg' fields (parameter names)
            if field in ('id', 'arg', 'name', 'attr'):
                continue
            
            if isinstance(value, list):
                fields[field] = [normalize_ast_structure(item) for item in value]
            elif isinstance(value, ast.AST):
                fields[field] = normalize_ast_structure(value)
            elif field not in ('lineno', 'col_offset', 'end_lineno', 'end_col_offset', 'ctx'):
                # Keep other non-location fields
                fields[field] = value
        
        return (node_type, tuple(sorted(fields.items())))
    
    elif isinstance(node, list):
        return [normalize_ast_structure(item) for item in node]
    else:
        return node

def compare_python_code(code1, code2, ignore_docstrings=True):
    """
    Compare two Python code snippets using their ASTs.
    Ignores variable names, parameter names, and literal values.
    Only compares code structure and operations.
    
    Args:
        code1: First Python code string
        code2: Second Python code string
        ignore_docstrings: If True, ignore docstring differences
    
    Returns:
        dict with 'are_equal' (bool) and 'details' (str)
    """
    try:
        # Parse both code snippets into ASTs
        tree1 = ast.parse(code1)
        tree2 = ast.parse(code2)
        
        # Optionally remove docstrings
        if ignore_docstrings:
            remove_docstrings(tree1)
            remove_docstrings(tree2)
        
        # Normalize ASTs for comparison (ignoring names and values)
        normalized1 = normalize_ast_structure(tree1)
        normalized2 = normalize_ast_structure(tree2)
        
        # Compare normalized ASTs
        are_equal = normalized1 == normalized2
        
        if are_equal:
            details = "The code snippets have the same structure (ignoring variable names and values)."
        else:
            details = "The code snippets have different structures."
        
        return {
            'are_equal': are_equal,
            'details': details,
            'normalized1': normalized1,
            'normalized2': normalized2
        }
    
    except SyntaxError as e:
        return {
            'are_equal': False,
            'details': f"Syntax error: {e}",
            'normalized1': None,
            'normalized2': None
        }

def remove_docstrings(node):
    """Remove docstrings from an AST."""
    if isinstance(node, (ast.FunctionDef, ast.ClassDef, ast.Module)):
        if (node.body and 
            isinstance(node.body[0], ast.Expr) and 
            isinstance(node.body[0].value, ast.Constant) and
            isinstance(node.body[0].value.value, str)):
            node.body.pop(0)
    
    for child in ast.iter_child_nodes(node):
        remove_docstrings(child)

# Example usage
if __name__ == "__main__":
    # Example 1: Same structure, different variable names
    code_a = """
def add(x, y):
    return x + y
"""
    
    code_b = """
def add(a, b):
    return a + b
"""
    
    result = compare_python_code(code_a, code_b)
    print("Example 1: Different variable names")
    print(f"Are equal: {result['are_equal']}")
    print(f"Details: {result['details']}\n")
    
    # Example 2: Same structure, different values
    code_c = """
def calculate(x):
    result = x * 5
    return result
"""
    
    code_d = """
def calculate(num):
    output = num * 10
    return output
"""
    
    result = compare_python_code(code_c, code_d)
    print("Example 2: Different variable names and values")
    print(f"Are equal: {result['are_equal']}")
    print(f"Details: {result['details']}\n")
    
    # Example 3: Different operations (should be different)
    code_e = """
def calculate(x):
    return x * 2
"""
    
    code_f = """
def calculate(x):
    return x + 2
"""
    
    result = compare_python_code(code_e, code_f)
    print("Example 3: Different operations (* vs +)")
    print(f"Are equal: {result['are_equal']}")
    print(f"Details: {result['details']}\n")
    
    # Example 4: Same logic with different literal values
    code_g = """
def greet():
    message = "Hello"
    return message
"""
    
    code_h = """
def greet():
    msg = "Goodbye"
    return msg
"""
    
    result = compare_python_code(code_g, code_h)
    print("Example 4: Different string literals but same structure")
    print(f"Are equal: {result['are_equal']}")
    print(f"Details: {result['details']}")