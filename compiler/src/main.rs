mod ast;
fn main() {
    let input:&str="7";

    let mut lexer : Lexer =ast::lexer::Lexer::new(input);
    let mut tokens : Vec<Token>=Vec::new();
    while let Some(token: Token)=lexer.next_token(){
        tokens.push(value:token);
    }
    println!("{:?}",tokens);
}
