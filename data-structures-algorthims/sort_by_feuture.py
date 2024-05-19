# Sample data
data = [
    {'Name': 'zAlice', 'Age': 25, 'Salary': 50000},
    {'Name': 'aBob', 'Age': 50, 'Salary': 60000},
    {'Name': 'rCharlie', 'Age': 35, 'Salary': 70000},
    {'Name': 'gDavid', 'Age': 40, 'Salary': 80000},
    {'Name': 'yDavid2', 'Age': 10, 'Salary': 80000}
]

# Sorting function
def sort_by_column(data, column):
    return sorted(data, key=lambda x: x[column])

# Sorting the data by the 'Age' column
sorted_data = sort_by_column(data, 'Name')

# Displaying the sorted data
for row in sorted_data:
    print(row)

