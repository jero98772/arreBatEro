pub enum TokenKind{
	Number(i64),
	Plus,
	Minus,
	Asterisk,
	Slash,
	LeftParen,
	RigthParen,

}
pub struct TextSpan{
	start:usize,
	end:usize,
	literal: String,
}
impl TextSpan{
	pub fn new(start:usize,end:usize,literal: String) -> Self{
		TextSpan { start,end,literal }
	}

	pub fn length(&self) -> usize{
		self.end-self.start
	}


}
pub struct Token{
	kind: TokenKind,
	span: TextSpan,
}

impl Token{
	pub fn new(kind:TokenKind,span:TextSpan)->Self{
		Self {kind,span}

	}
}

pub struct Lexer<'a>{
	input: Peekable<Chars<'a>>

}

impl <'a> Lexer<'a>{
	pub fn new(input: &'a str) -> Self{
		Self { input: input.chars().peekable()}
	}
	pub fn next_token(&mut self)-> Option<Token>{

	}

}