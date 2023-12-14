use super::token::Token;
use backtrace::Backtrace;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    InvalidToken(Token),
    EndOfFile,
}

#[derive(Debug)]
pub struct ParsingError {
    pub error: ParserError,
    pub backtrack: Backtrace,
}

impl ParsingError {
    pub fn new(error: ParserError) -> ParsingError {
        ParsingError {
            error,
            backtrack: Backtrace::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    InvalidNumberFormat(String),
}

#[derive(Debug, PartialEq)]
pub struct LexingError {
    pub error: LexerError,
    pub backtrack: usize,
}
