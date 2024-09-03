import random

def generate_random_dna_sequence(length: int) -> str:
    """Generates a random DNA sequence of a given length."""
    return ''.join(random.choice('AGCT') for _ in range(length))

def write_dna_sequence_to_file(file_path: str, sequence: str) -> None:
    """Writes a DNA sequence to a file."""
    with open(file_path, 'w') as file:
        file.write(sequence)

if __name__ == "__main__":
    # Define the desired length of the DNA sequence
    dna_length = 10**6  # 1 million base pairs for a large sequence
    dna_sequence = generate_random_dna_sequence(dna_length)

    # Specify the output file path
    output_file_path = "random_dna_sequence.txt"

    # Write the DNA sequence to the file
    write_dna_sequence_to_file(output_file_path, dna_sequence)

    print(f"Random DNA sequence of length {dna_length} has been written to {output_file_path}")

