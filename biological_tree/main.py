import ntree

# Create an instance of the NTree
tree = ntree.NTree()

# Add nodes
tree.add_node("Animalia", "Chordata")
tree.add_node("Chordata", "Mammalia")
tree.add_node("Chordata", "Reptilia")
tree.add_node("Mammalia", "Carnivora")

# Check if nodes exist
print(tree.is_ancestor("Chordata"))  # True
print(tree.is_ancestor("Mammalia"))  # True
print(tree.is_ancestor("Reptilia"))  # True
print(tree.is_ancestor("Carnivora")) # False

# Delete a node
tree.delete_node("Reptilia")

# Check if deleted node still exists
print(tree.is_son("Reptilia"))  # False
