use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher}; 

struct Rule {
    head: Symbol,
    body: Vec<Symbol>,
}


enum Symbol {
    Terminal(String),
    NonTerminal(String),
}


enum Action {
    Shift(usize),
    Reduce(Rule),
    Accept,
    Error,
}
impl PartialEq for Rule {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.body == other.body
    }
}

impl Eq for Rule {}

impl Hash for Rule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.head.hash(state);
        self.body.hash(state);
    }
}

impl Clone for Rule {
    fn clone(&self) -> Self {
        Rule {
            head: self.head.clone(),
            body: self.body.clone(),
        }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Symbol::Terminal(s1), Symbol::Terminal(s2)) => s1 == s2,
            (Symbol::NonTerminal(s1), Symbol::NonTerminal(s2)) => s1 == s2,
            _ => false,
        }
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Terminal(s) => s.hash(state),
            Symbol::NonTerminal(s) => s.hash(state),
        }
    }
}

impl Clone for Symbol {
    fn clone(&self) -> Self {
        match self {
            Symbol::Terminal(s) => Symbol::Terminal(s.clone()),
            Symbol::NonTerminal(s) => Symbol::NonTerminal(s.clone()),
        }
    }
}

struct LR1Parser {
    parsing_table: HashMap<(usize, Symbol), Action>,
}

impl LR1Parser {
    fn new() -> LR1Parser {
        let parsing_table = HashMap::new();
        LR1Parser { parsing_table }
    }

    fn add_entry(&mut self, state: usize, symbol: Symbol, action: Action) {
        self.parsing_table.insert((state, symbol), action);
    }

    fn parse(&self, input: &str) {
        let mut stack = vec![0];
        let mut input_iter = input.chars().peekable();

        loop {
            let state = *stack.last().unwrap();
            let current_char = match input_iter.peek() {
                Some(&c) => c,
                None => break,
            };

            match self
                .parsing_table
                .get(&(state, Symbol::Terminal(current_char.to_string())))
            {
                Some(Action::Shift(next_state)) => {
                    stack.push(*next_state);
                    input_iter.next();
                }
                Some(Action::Reduce(_)) => {
                    // Handle reduce action
                }
                Some(Action::Accept) => {
                    println!("Input string is accepted");
                    return;
                }
                Some(Action::Error) => {
                    println!("Error: Invalid input string");
                    return;
                }
                None => {
                    println!("Error: No action defined for current state and input");
                    return;
                }
            }
        }

        println!("Error: Input string parsing failed");
    }
}


fn main() {
    // Create a new LR(1) parser
    let mut parser = LR1Parser::new();

    // Add parsing table entries (dummy entries)
    parser.add_entry(0, Symbol::Terminal("a".to_string()), Action::Shift(1));
    parser.add_entry(1, Symbol::Terminal("b".to_string()), Action::Shift(2));
    parser.add_entry(2, Symbol::Terminal("c".to_string()), Action::Accept);

    parser.add_entry(3, Symbol::Terminal("d".to_string()), Action::Shift(2));
    // Parse input string
    parser.parse("abc abc dc cdc");
}

