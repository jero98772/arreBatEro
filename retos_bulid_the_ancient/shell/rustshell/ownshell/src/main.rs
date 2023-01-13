//https://www.joshmcguigan.com/blog/build-your-own-shell-rust/
use std::env;
use std::path::Path;
use std::io::{stdin,stdout,Write};
use std::process::{Command,Child,Stdio};
fn main() {
	loop{
		print!(">\t");
		stdout().flush().unwrap();
		let mut input=String::new();
		stdin().read_line(&mut input).unwrap();

		let mut commands=input.trim().split(" | ").peekable();
		let mut previous_command=None;
		while let Some(command)=commands.next(){

			let mut parts=input.trim().split_whitespace();		
			let command=parts.next().unwrap(); 
			let args=parts;
			match command{
			"cd"=>{
				let new_dir=args.peekable().peek().map_or("/",|x|*x);
				let root=Path::new(new_dir);
				if let Err(e)=env::set_current_dir(&root){
					eprintln!("{}",e);
				}
				previous_command = None;
			},
			"exit"=> return, command=>{let stdin=previous_command.map_or(Stdio::inherit(),|output:Child|Stdio::from(output.stdout.unwrap())); //Command::new(command).args(args).spawn();
				let stdout= if commands.peek().is_some(){
					Stdio::piped()
				}else {
					Stdio::inherit()
				};
				let output=Command::new(command).args(args).stdin(stdin).stdout(stdout).spawn();
				//match child {
				match output {
						Ok(output) => { previous_command=Some(output);},
						//Ok(mut child) => { child.wait();},
						Err(e) => { previous_command=None;
						eprintln!("{}",e);
						//Err(e) => eprintln!("{}",e),
						},
					};
				}
			}
		}
		if let Some(mut final_command)=previous_command{
			final_command.wait();
		}
	}
}
