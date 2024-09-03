use std::fs::File;
use std::io::{BufRead, BufReader};

struct TuringMachine {
    tape: Vec<i32>,
    pointer: usize,
    instruction_pointer: usize,
    loop_stack: Vec<usize>,
    program: String,
}

impl TuringMachine {
    fn new(tape_length: usize, program: &str) -> Self {
        TuringMachine {
            tape: vec![0; tape_length],
            pointer: 0,
            instruction_pointer: 0,
            loop_stack: Vec::new(),
            program: program.to_string(),
        }
    }

    fn load_program(&mut self, program: &str) {
        self.program = program.to_string();
    }

    fn run(&mut self) {
        while self.instruction_pointer < self.program.len() {
            let command = self.program.as_bytes()[self.instruction_pointer] as char;

            match command {
                'a' => self.pointer += 1,
                't' => self.pointer = self.pointer.saturating_sub(1),
                'g' => self.tape[self.pointer] += 1,
                'c' => self.tape[self.pointer] -= 1,
                '[' => {
                    if self.tape[self.pointer] == 0 {
                        // Jump to the instruction after the matching ']'
                        let mut open_brackets = 1;
                        while open_brackets != 0 {
                            self.instruction_pointer += 1;
                            let c = self.program.as_bytes()[self.instruction_pointer] as char;
                            if c == '[' {
                                open_brackets += 1;
                            } else if c == ']' {
                                open_brackets -= 1;
                            }
                        }
                    } else {
                        self.loop_stack.push(self.instruction_pointer);
                    }
                }
                ']' => {
                    if self.tape[self.pointer] != 0 {
                        self.instruction_pointer = *self.loop_stack.last().unwrap(); // Jump back to the matching '['
                    } else {
                        self.loop_stack.pop();
                    }
                }
                _ => {}
            }

            self.instruction_pointer += 1;
        }
    }

    fn get_tape(&self) -> &Vec<i32> {
        &self.tape
    }

    fn get_pointer(&self) -> usize {
        self.pointer
    }
}

fn read_dna_sequence(filename: &str) -> String {
    let file = File::open(filename).expect("Unable to open file");
    let reader = BufReader::new(file);
    let mut sequence = String::new();
    for line in reader.lines() {
        sequence.push_str(&line.expect("Unable to read line"));
    }
    sequence
}

fn main() {
    // Read the DNA sequence from a file once
    let dna_sequence = read_dna_sequence("dna_sequence.txt");

    // Generate a deterministic Turing machine command mapping based on DNA sequence
    let program = generate_program_from_dna_sequence(&dna_sequence);

    // Create and run the Turing machine
    let mut tm = TuringMachine::new(30000, &program);
    tm.run();

    // Output the state of the tape after running the program
    println!("Tape after processing: {:?}", &tm.get_tape()[..100].to_vec());  // First 100 blocks
    println!("Pointer position: {}", tm.get_pointer());
}

fn generate_program_from_dna_sequence(dna_sequence: &str) -> String {
    // Placeholder function to convert a DNA sequence to a Turing machine program
    // You should implement the actual logic based on your requirements
    dna_sequence.chars().map(|c| match c {
        'A' => 'a',
        'T' => 't',
        'G' => 'g',
        'C' => 'c',
        _ => ' ',  // Ignore any characters not in DNA alphabet
    }).collect()
}

