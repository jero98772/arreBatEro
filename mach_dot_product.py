import numpy as np

def compatibility(person1, person2):
    """Calculate compatibility score using the dot product"""
    dot_product = np.dot(person1, person2)
    norm1 = np.linalg.norm(person1)
    norm2 = np.linalg.norm(person2)
    
    similarity = dot_product / (norm1 * norm2)  # Cosine similarity (range: -1 to 1)
    
    return similarity

# Example interests: [Music, Books, Movies, Sports, Tech]
alice = [5, 3, 4, 2, 5]  # Alice's interest levels
bob =   [4, 2, 5, 3, 4]  # Bob's interest levels
eve =   [1, 5, 2, 4, 1]  # Eve's interest levels

# Compare Alice & Bob vs. Alice & Eve
score_alice_bob = compatibility(alice, bob)
score_alice_eve = compatibility(alice, eve)

print(f"Alice & Bob Compatibility: {score_alice_bob:.2f}")  # Closer to 1 = more compatible
print(f"Alice & Eve Compatibility: {score_alice_eve:.2f}")  # Closer to -1 = less compatible
