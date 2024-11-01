from ply import lex, yacc

# 1. Token definitions
tokens = (
    'SELECT',
    'FROM',
    'WHERE',
    'IDENTIFIER',
    'NUMBER',
    'EQUAL',
    'STRING'  # Added for string literals
)

# 2. Regular expressions for tokens
def t_SELECT(t):
    r'SELECT|select'
    t.value = t.value.upper()
    return t

def t_FROM(t):
    r'FROM|from'
    t.value = t.value.upper()
    return t

def t_WHERE(t):
    r'WHERE|where'
    t.value = t.value.upper()
    return t

def t_STRING(t):
    r"'[^']*'"
    t.value = t.value[1:-1]  # Remove quotes
    return t

t_EQUAL = r'='
t_IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_NUMBER = r'\d+'

# Ignore whitespace
t_ignore = ' \t\n'

# 3. Error handling
def t_error(t):
    print(f"Invalid token: {t.value[0]}")
    t.lexer.skip(1)

# Create lexer
lexer = lex.lex()

# 4. Parser rules
def p_query(p):
    '''query : SELECT IDENTIFIER FROM IDENTIFIER WHERE condition'''
    p[0] = {
        'type': 'SELECT',
        'field': p[2],
        'table': p[4],
        'condition': p[6]
    }

def p_condition(p):
    '''condition : IDENTIFIER EQUAL NUMBER
                | IDENTIFIER EQUAL STRING'''
    p[0] = {
        'field': p[1],
        'operator': p[2],
        'value': p[3]
    }

def p_error(p):
    if p:
        print(f"Syntax error at token: {p.value}")
    else:
        print("Syntax error at EOF")

# Create parser
parser = yacc.yacc()

# 5. B+ Tree Implementation
class BPlusTreeNode:
    def __init__(self, leaf=True):
        self.leaf = leaf
        self.keys = []
        self.children = []
        self.next = None  # For leaf nodes

class BPlusTree:
    def __init__(self, order=3):
        self.root = BPlusTreeNode()
        self.order = order
    
    def insert(self, key, value):
        # Simple implementation for demo purposes
        if not hasattr(self, 'data'):
            self.data = {}
        self.data[key] = value
    
    def search(self, key):
        # Simple implementation for demo purposes
        if hasattr(self, 'data'):
            return self.data.get(key)
        return None
    
    def range_search(self, start_key, end_key):
        # Simple implementation for demo purposes
        if hasattr(self, 'data'):
            return {k: v for k, v in self.data.items() 
                   if start_key <= k <= end_key}
        return {}

# 6. Query Executor
class QueryExecutor:
    def __init__(self):
        self.tables = {}
    
    def create_table(self, name):
        self.tables[name] = BPlusTree()
        return self.tables[name]
    
    def get_table(self, name):
        return self.tables.get(name)
    
    def execute_query(self, query_str):
        try:
            # Parse the query
            parsed = parser.parse(query_str)
            if not parsed:
                return "Query parsing failed"
            
            # Get the target table
            table = self.get_table(parsed['table'])
            if not table:
                return f"Table {parsed['table']} not found"
            
            # Execute the condition
            condition = parsed['condition']
            if condition['operator'] == '=':
                value = int(condition['value']) if condition['value'].isdigit() else condition['value']
                result = table.search(value)
                
                if result:
                    # Return the requested field
                    field = parsed['field']
                    if field in result:
                        return {field: result[field]}
                    else:
                        return f"Field {field} not found"
                else:
                    return "No results found"
            
            return "Unsupported operation"
            
        except Exception as e:
            return f"Error executing query: {str(e)}"

# Usage example
def main():
    # Initialize the query executor
    executor = QueryExecutor()
    
    # Create a table and insert some data
    users_table = executor.create_table('users')
    users_table.insert(1, {'id': 1, 'name': 'Alice', 'age': 25})
    users_table.insert(2, {'id': 2, 'name': 'Bob', 'age': 30})
    users_table.insert(3, {'id': 3, 'name': 'Charlie', 'age': 35})
    
    # Test queries
    test_queries = [
        "SELECT name FROM users WHERE id = 2",
        "SELECT age FROM users WHERE id = 1",
        "SELECT name FROM users WHERE id = 3"
    ]
    
    for query in test_queries:
        print(f"\nExecuting query: {query}")
        result = executor.execute_query(query)
        print(f"Result: {result}")

if __name__ == "__main__":
    main()