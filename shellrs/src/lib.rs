use pyo3::prelude::*;
use std::process::{Command, Stdio};
use std::io::{self};
use std::env;
/// Formats the sum of two numbers as string.

// shell function
#[pyfunction]
fn run(command: &str) {
    let mut parts = command.split_whitespace();
    let cmd = parts.next();

    match cmd {
        Some("exit") => {
            println!("Exiting...");
            return;
        }
        Some("cd") => {
            if let Some(path) = parts.next() {
                if let Err(err) = env::set_current_dir(path) {
                    eprintln!("Failed to change directory: {}", err);
                }
            } else {
                eprintln!("Invalid 'cd' command");
            }
        }
        Some(cmd) => {
            let output = Command::new(cmd)
                .args(parts)
                .stdout(Stdio::piped())
                .output();

            match output {
                Ok(output) => {
                    if let Ok(result) = String::from_utf8(output.stdout) {
                        println!("{}", result);
                    } else {
                        eprintln!("Failed to convert output to string");
                    }
                }
                Err(e) => {
                    eprintln!("Failed to execute command: {}", e);
                }
            }
        }
        None => {
            eprintln!("No command entered");
        }
    }
    
}


/// A Python module implemented in Rust.
#[pymodule]
fn shellrs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
