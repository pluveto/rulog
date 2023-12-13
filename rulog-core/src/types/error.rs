use super::token::Token;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedToken(Token),
    InvalidToken(Token),
    EndOfFile,
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    InvalidNumberFormat(String),
}
