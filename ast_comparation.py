import ast

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



# Example usage
if __name__ == "__main__":
    # Example 1: Same structure, different variable names
    code_a = """
num1, num2 = input().split()
if num1 == "2" and num2 == "1":
    print("false")
elif num1 == "12" and num2 == "4":
    print("true")
elif num1 == "3" and num2 == "3":
    print("false")
"""
   
    code_c = """
nums = input()
num1, num2= split()
if num1 == "2" and num2 == "1":
    print("false")
elif num1 == "12" and num2 == "4":
    print("true")
elif num1 == "3" and num2 == "3":
    print("false")
"""

    code_b = """
x, y = input().split()
if x == "12" and y == "4":
    print("true")
elif x == "3" and y == "3":
    print("true")
elif x == "2" and y == "1":
    print("false")
"""
    
    result = compare_python_code(code_a, code_b)
    print(f"Are equal: {result['are_equal']}")
    print(f"Details: {result['details']}\n")

    result = compare_python_code(code_a, code_c)
    print(f"Are equal: {result['are_equal']}")
    print(f"Details: {result['details']}\n")
