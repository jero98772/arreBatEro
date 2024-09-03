use std::collections::HashMap;
use std::fs;

// Function to read the DNA sequence from a file
fn read_dna_sequence(file_path: &str) -> String {
    fs::read_to_string(file_path)
        .expect("Unable to read file")
        .replace("\n", "")
        .replace("\r", "")
}

// Function to generate a deterministic random Turing machine command mapping
fn generate_deterministic_mapping() -> HashMap<char, char> {
    let commands = ['>', '<', '+', '-'];
    let chars = ['a', 'c', 'g', 't'];

    // Example permutation for deterministic "randomization"
    let permutation = [0, 2, 3, 1]; // You can change this permutation to any other valid permutation

    HashMap::from([
        (chars[0], commands[permutation[0]]),
        (chars[1], commands[permutation[1]]),
        (chars[2], commands[permutation[2]]),
        (chars[3], commands[permutation[3]]),
    ])
}

// Turing machine struct and implementation
struct TuringMachine {
    tape: Vec<i32>,
    pointer: usize,
    instruction_pointer: usize,
    loop_stack: Vec<usize>,
    program: Vec<char>,
}

impl TuringMachine {
    fn new(tape_length: usize, program: Vec<char>) -> Self {
        Self {
            tape: vec![0; tape_length],
            pointer: 0,
            instruction_pointer: 0,
            loop_stack: Vec::new(),
            program,
        }
    }

    fn run(&mut self) {
        while self.instruction_pointer < self.program.len() {
            match self.program[self.instruction_pointer] {
                '>' => self.pointer += 1,
                '<' => {
                    if self.pointer > 0 {
                        self.pointer -= 1;
                    }
                }
                '+' => self.tape[self.pointer] += 1,
                '-' => self.tape[self.pointer] -= 1,
                '[' => {
                    if self.tape[self.pointer] == 0 {
                        let mut open_brackets = 1;
                        while open_brackets > 0 {
                            self.instruction_pointer += 1;
                            if self.instruction_pointer >= self.program.len() {
                                panic!("Mismatched brackets: ']' without matching '['");
                            }
                            match self.program[self.instruction_pointer] {
                                '[' => open_brackets += 1,
                                ']' => open_brackets -= 1,
                                _ => (),
                            }
                        }
                    } else {
                        self.loop_stack.push(self.instruction_pointer);
                    }
                }
                ']' => {
                    if let Some(matching_bracket) = self.loop_stack.pop() {
                        if self.tape[self.pointer] != 0 {
                            self.instruction_pointer = matching_bracket;
                        }
                    } else {
                        panic!("Mismatched brackets: ']' without matching '['");
                    }
                }
                _ => (),
            }
            self.instruction_pointer += 1;
        }

        // Check if there are any unmatched '[' left
        if !self.loop_stack.is_empty() {
            panic!("Mismatched brackets: '[' without matching ']'");
        }
    }

    fn get_tape(&self) -> &Vec<i32> {
        &self.tape
    }

    fn get_pointer(&self) -> usize {
        self.pointer
    }
}

fn main() {
    // Read the DNA sequence from a file once
    let dna_sequence = read_dna_sequence("dna_sequence.txt");

    // Generate a deterministic Turing machine command mapping
    let kmer_to_command = generate_deterministic_mapping();

    // Convert the DNA sequence into Turing machine instructions
    let program: Vec<char> = dna_sequence
        .chars()
        .filter_map(|c| kmer_to_command.get(&c).copied())
        .collect();

    // Create and run the Turing machine
    let mut tm = TuringMachine::new(30000, program);
    tm.run();

    // Output the state of the tape after running the program
    println!("Tape after processing: {:?}", tm.get_tape()[..100].to_vec());  // First 10 blocks
    println!("Pointer position: {}", tm.get_pointer());
}
