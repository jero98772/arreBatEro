use std::fs;
use std::io::{self, Write};
use std::path::Path;

#[derive(Debug, Clone, PartialEq)]
enum CommandType {
    Arithmetic,
    Push,
    Pop,
    Label,
    Goto,
    If,
    Function,
    Return,
    Call,
}

struct Parser {
    lines: Vec<String>,
    current_instruction: usize,
    current_command: String,
}

impl Parser {
    fn new(input_file: &str) -> io::Result<Self> {
        let content = fs::read_to_string(input_file)?;
        let lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();
        
        Ok(Parser {
            lines,
            current_instruction: 0,
            current_command: String::new(),
        })
    }
    
    fn has_more_commands(&self) -> bool {
        self.current_instruction < self.lines.len()
    }
    
    fn advance(&mut self) {
        while self.has_more_commands() {
            let line = self.lines[self.current_instruction].trim();
            self.current_instruction += 1;
            
            // Skip empty lines and comments
            if !line.is_empty() && !line.starts_with("//") {
                // Remove inline comments
                let clean_line = if let Some(comment_pos) = line.find("//") {
                    line[..comment_pos].trim()
                } else {
                    line
                };
                
                if !clean_line.is_empty() {
                    self.current_command = clean_line.to_string();
                    return;
                }
            }
        }
        
        self.current_command.clear();
    }
    
    fn command_type(&self) -> Option<CommandType> {
        if self.current_command.is_empty() {
            return None;
        }
        
        let parts: Vec<&str> = self.current_command.split_whitespace().collect();
        let command = parts[0];
        
        match command {
            "add" | "sub" | "neg" | "eq" | "gt" | "lt" | "and" | "or" | "not" => {
                Some(CommandType::Arithmetic)
            }
            "push" => Some(CommandType::Push),
            "pop" => Some(CommandType::Pop),
            "label" => Some(CommandType::Label),
            "goto" => Some(CommandType::Goto),
            "if-goto" => Some(CommandType::If),
            "function" => Some(CommandType::Function),
            "call" => Some(CommandType::Call),
            "return" => Some(CommandType::Return),
            _ => None,
        }
    }
    
    fn arg1(&self) -> Option<String> {
        let parts: Vec<&str> = self.current_command.split_whitespace().collect();
        
        if let Some(CommandType::Arithmetic) = self.command_type() {
            Some(parts[0].to_string())
        } else if parts.len() > 1 {
            Some(parts[1].to_string())
        } else {
            None
        }
    }
    
    fn arg2(&self) -> Option<i32> {
        let parts: Vec<&str> = self.current_command.split_whitespace().collect();
        
        if parts.len() > 2 {
            parts[2].parse().ok()
        } else {
            None
        }
    }
}

pub struct CodeWriter {
    output_file: Box<dyn Write>,
    filename: String,
    label_counter: i32,
    current_function: String,
}

impl CodeWriter {
    pub fn new(output_file: Box<dyn Write>) -> Self {
        CodeWriter {
            output_file,
            filename: String::new(),
            label_counter: 0,
            current_function: String::new(),
        }
    }
    
    fn set_filename(&mut self, filename: &str) {
        self.filename = Path::new(filename)
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .to_string();
    }
    
    fn write_arithmetic(&mut self, command: &str) -> io::Result<()> {
        match command {
            "add" => self.write_binary_op("+"),
            "sub" => self.write_binary_op("-"),
            "neg" => self.write_unary_op("-"),
            "eq" => self.write_comparison("JEQ"),
            "gt" => self.write_comparison("JGT"),
            "lt" => self.write_comparison("JLT"),
            "and" => self.write_binary_op("&"),
            "or" => self.write_binary_op("|"),
            "not" => self.write_unary_op("!"),
            _ => Ok(()),
        }
    }
    
    fn write_push_pop(&mut self, command: &str, segment: &str, index: i32) -> io::Result<()> {
        match command {
            "push" => self.write_push(segment, index),
            "pop" => self.write_pop(segment, index),
            _ => Ok(()),
        }
    }
    
    fn write_label(&mut self, label: &str) -> io::Result<()> {
        if !self.current_function.is_empty() {
            writeln!(self.output_file, "({}${})", self.current_function, label)
        } else {
            writeln!(self.output_file, "({})", label)
        }
    }
    
    fn write_goto(&mut self, label: &str) -> io::Result<()> {
        if !self.current_function.is_empty() {
            writeln!(self.output_file, "@{}${}", self.current_function, label)?;
        } else {
            writeln!(self.output_file, "@{}", label)?;
        }
        writeln!(self.output_file, "0;JMP")
    }
    
    fn write_if(&mut self, label: &str) -> io::Result<()> {
        // Pop value from stack and jump if not zero
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        
        if !self.current_function.is_empty() {
            writeln!(self.output_file, "@{}${}", self.current_function, label)?;
        } else {
            writeln!(self.output_file, "@{}", label)?;
        }
        writeln!(self.output_file, "D;JNE")
    }
    
    fn write_function(&mut self, function_name: &str, num_vars: i32) -> io::Result<()> {
        self.current_function = function_name.to_string();
        writeln!(self.output_file, "({})", function_name)?;
        
        // Initialize local variables to 0
        for _ in 0..num_vars {
            writeln!(self.output_file, "@0")?;
            writeln!(self.output_file, "D=A")?;
            writeln!(self.output_file, "@SP")?;
            writeln!(self.output_file, "A=M")?;
            writeln!(self.output_file, "M=D")?;
            writeln!(self.output_file, "@SP")?;
            writeln!(self.output_file, "M=M+1")?;
        }
        Ok(())
    }
    
    fn write_call(&mut self, function_name: &str, num_args: i32) -> io::Result<()> {
        let return_label = format!("RETURN_{}", self.label_counter);
        self.label_counter += 1;
        
        // Push return address
        writeln!(self.output_file, "@{}", return_label)?;
        writeln!(self.output_file, "D=A")?;
        self.push_d()?;
        
        // Push LCL
        writeln!(self.output_file, "@LCL")?;
        writeln!(self.output_file, "D=M")?;
        self.push_d()?;
        
        // Push ARG
        writeln!(self.output_file, "@ARG")?;
        writeln!(self.output_file, "D=M")?;
        self.push_d()?;
        
        // Push THIS
        writeln!(self.output_file, "@THIS")?;
        writeln!(self.output_file, "D=M")?;
        self.push_d()?;
        
        // Push THAT
        writeln!(self.output_file, "@THAT")?;
        writeln!(self.output_file, "D=M")?;
        self.push_d()?;
        
        // ARG = SP - 5 - num_args
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@{}", 5 + num_args)?;
        writeln!(self.output_file, "D=D-A")?;
        writeln!(self.output_file, "@ARG")?;
        writeln!(self.output_file, "M=D")?;
        
        // LCL = SP
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@LCL")?;
        writeln!(self.output_file, "M=D")?;
        
        // Jump to function
        writeln!(self.output_file, "@{}", function_name)?;
        writeln!(self.output_file, "0;JMP")?;
        
        // Return label
        writeln!(self.output_file, "({})", return_label)
    }
    
    fn write_return(&mut self) -> io::Result<()> {
        // Store LCL in R13 (endFrame)
        writeln!(self.output_file, "@LCL")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@R13")?;
        writeln!(self.output_file, "M=D")?;
        
        // Store return address in R14
        writeln!(self.output_file, "@5")?;
        writeln!(self.output_file, "A=D-A")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@R14")?;
        writeln!(self.output_file, "M=D")?;
        
        // Pop return value to ARG[0]
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@ARG")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "M=D")?;
        
        // SP = ARG + 1
        writeln!(self.output_file, "@ARG")?;
        writeln!(self.output_file, "D=M+1")?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=D")?;
        
        // Restore THAT
        writeln!(self.output_file, "@R13")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@THAT")?;
        writeln!(self.output_file, "M=D")?;
        
        // Restore THIS
        writeln!(self.output_file, "@R13")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@THIS")?;
        writeln!(self.output_file, "M=D")?;
        
        // Restore ARG
        writeln!(self.output_file, "@R13")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@ARG")?;
        writeln!(self.output_file, "M=D")?;
        
        // Restore LCL
        writeln!(self.output_file, "@R13")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@LCL")?;
        writeln!(self.output_file, "M=D")?;
        
        // Jump to return address
        writeln!(self.output_file, "@R14")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "0;JMP")
    }
    
    pub fn write_init(&mut self) -> io::Result<()> {
        // Set SP to 256
        writeln!(self.output_file, "@256")?;
        writeln!(self.output_file, "D=A")?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=D")?;
        
        // Call Sys.init
        self.write_call("Sys.init", 0)
    }
    
    // Helper methods
    fn write_binary_op(&mut self, op: &str) -> io::Result<()> {
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "M=M{}D", op)?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M+1")
    }
    
    fn write_unary_op(&mut self, op: &str) -> io::Result<()> {
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "M={}M", op)?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M+1")
    }
    
    fn write_comparison(&mut self, jump_type: &str) -> io::Result<()> {
        let true_label = format!("TRUE_{}", self.label_counter);
        let end_label = format!("END_{}", self.label_counter);
        self.label_counter += 1;
        
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M")?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M-1")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "D=M-D")?;
        writeln!(self.output_file, "@{}", true_label)?;
        writeln!(self.output_file, "D;{}", jump_type)?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "M=0")?;
        writeln!(self.output_file, "@{}", end_label)?;
        writeln!(self.output_file, "0;JMP")?;
        writeln!(self.output_file, "({})", true_label)?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "M=-1")?;
        writeln!(self.output_file, "({})", end_label)?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M+1")
    }
    
    fn write_push(&mut self, segment: &str, index: i32) -> io::Result<()> {
        match segment {
            "constant" => {
                writeln!(self.output_file, "@{}", index)?;
                writeln!(self.output_file, "D=A")?;
            }
            "local" | "argument" | "this" | "that" => {
                let base = match segment {
                    "local" => "LCL",
                    "argument" => "ARG",
                    "this" => "THIS",
                    "that" => "THAT",
                    _ => unreachable!(),
                };
                writeln!(self.output_file, "@{}", base)?;
                writeln!(self.output_file, "D=M")?;
                writeln!(self.output_file, "@{}", index)?;
                writeln!(self.output_file, "A=D+A")?;
                writeln!(self.output_file, "D=M")?;
            }
            "static" => {
                writeln!(self.output_file, "@{}.{}", self.filename, index)?;
                writeln!(self.output_file, "D=M")?;
            }
            "temp" => {
                writeln!(self.output_file, "@{}", 5 + index)?;
                writeln!(self.output_file, "D=M")?;
            }
            "pointer" => {
                let addr = if index == 0 { "THIS" } else { "THAT" };
                writeln!(self.output_file, "@{}", addr)?;
                writeln!(self.output_file, "D=M")?;
            }
            _ => {}
        }
        
        self.push_d()
    }
    
    fn write_pop(&mut self, segment: &str, index: i32) -> io::Result<()> {
        match segment {
            "local" | "argument" | "this" | "that" => {
                let base = match segment {
                    "local" => "LCL",
                    "argument" => "ARG",
                    "this" => "THIS",
                    "that" => "THAT",
                    _ => unreachable!(),
                };
                writeln!(self.output_file, "@{}", base)?;
                writeln!(self.output_file, "D=M")?;
                writeln!(self.output_file, "@{}", index)?;
                writeln!(self.output_file, "D=D+A")?;
                writeln!(self.output_file, "@R13")?;
                writeln!(self.output_file, "M=D")?;
                writeln!(self.output_file, "@SP")?;
                writeln!(self.output_file, "M=M-1")?;
                writeln!(self.output_file, "A=M")?;
                writeln!(self.output_file, "D=M")?;
                writeln!(self.output_file, "@R13")?;
                writeln!(self.output_file, "A=M")?;
                writeln!(self.output_file, "M=D")?;
            }
            "static" => {
                writeln!(self.output_file, "@SP")?;
                writeln!(self.output_file, "M=M-1")?;
                writeln!(self.output_file, "A=M")?;
                writeln!(self.output_file, "D=M")?;
                writeln!(self.output_file, "@{}.{}", self.filename, index)?;
                writeln!(self.output_file, "M=D")?;
            }
            "temp" => {
                writeln!(self.output_file, "@SP")?;
                writeln!(self.output_file, "M=M-1")?;
                writeln!(self.output_file, "A=M")?;
                writeln!(self.output_file, "D=M")?;
                writeln!(self.output_file, "@{}", 5 + index)?;
                writeln!(self.output_file, "M=D")?;
            }
            "pointer" => {
                let addr = if index == 0 { "THIS" } else { "THAT" };
                writeln!(self.output_file, "@SP")?;
                writeln!(self.output_file, "M=M-1")?;
                writeln!(self.output_file, "A=M")?;
                writeln!(self.output_file, "D=M")?;
                writeln!(self.output_file, "@{}", addr)?;
                writeln!(self.output_file, "M=D")?;
            }
            _ => {}
        }
        Ok(())
    }
    
    fn push_d(&mut self) -> io::Result<()> {
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "A=M")?;
        writeln!(self.output_file, "M=D")?;
        writeln!(self.output_file, "@SP")?;
        writeln!(self.output_file, "M=M+1")
    }
}

pub fn process_file(
    file_path: &Path,
    code_writer: &mut CodeWriter,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut parser = Parser::new(&file_path.to_string_lossy())?;
    code_writer.set_filename(&file_path.to_string_lossy());
    
    while parser.has_more_commands() {
        parser.advance();
        
        if let Some(cmd_type) = parser.command_type() {
            match cmd_type {
                CommandType::Arithmetic => {
                    if let Some(arg1) = parser.arg1() {
                        code_writer.write_arithmetic(&arg1)?;
                    }
                }
                CommandType::Push | CommandType::Pop => {
                    let command = parser.current_command.split_whitespace().next().unwrap();
                    if let (Some(arg1), Some(arg2)) = (parser.arg1(), parser.arg2()) {
                        code_writer.write_push_pop(command, &arg1, arg2)?;
                    }
                }
                CommandType::Label => {
                    if let Some(arg1) = parser.arg1() {
                        code_writer.write_label(&arg1)?;
                    }
                }
                CommandType::Goto => {
                    if let Some(arg1) = parser.arg1() {
                        code_writer.write_goto(&arg1)?;
                    }
                }
                CommandType::If => {
                    if let Some(arg1) = parser.arg1() {
                        code_writer.write_if(&arg1)?;
                    }
                }
                CommandType::Function => {
                    if let (Some(arg1), Some(arg2)) = (parser.arg1(), parser.arg2()) {
                        code_writer.write_function(&arg1, arg2)?;
                    }
                }
                CommandType::Call => {
                    if let (Some(arg1), Some(arg2)) = (parser.arg1(), parser.arg2()) {
                        code_writer.write_call(&arg1, arg2)?;
                    }
                }
                CommandType::Return => {
                    code_writer.write_return()?;
                }
            }
        }
    }
    
    Ok(())
}