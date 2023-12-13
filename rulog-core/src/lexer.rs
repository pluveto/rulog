use crate::types::token::Token;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    InvalidNumberFormat(String),
}

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    next_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            next_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.next_position).unwrap();
        }
        self.position = self.next_position;
        self.next_position += 1;
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let token = match self.ch {
            '+' => Ok(Token::Operator("+".to_string())),
            '-' => Ok(Token::Operator("-".to_string())),
            '*' => Ok(Token::Operator("*".to_string())),
            '/' => Ok(Token::Operator("/".to_string())),
            '<' => Ok(Token::Operator("<".to_string())),
            '>' => Ok(Token::Operator(">".to_string())),
            '=' => Ok(Token::Operator("=".to_string())),
            '!' => Ok(Token::Cut),
            ',' => Ok(Token::Comma),
            '.' => Ok(Token::Period),
            ';' => Ok(Token::Semicolon),
            ':' => Ok(Token::Colon),
            '?' => Ok(Token::QuestionMark),
            '(' => Ok(Token::LeftParenthesis),
            ')' => Ok(Token::RightParenthesis),
            '[' => Ok(Token::LeftBracket),
            ']' => Ok(Token::RightBracket),
            '{' => Ok(Token::LeftCurlyBracket),
            '}' => Ok(Token::RightCurlyBracket),
            '\0' => Ok(Token::EndOfFile),
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    Ok(self.read_identifier_or_variable())
                } else if self.ch.is_digit(10) {
                    Ok(self.read_number())
                } else {
                    Err(LexerError::UnexpectedCharacter(self.ch))
                }
            }
        };

        self.read_char();
        token
    }

    fn read_identifier_or_variable(&mut self) -> Token {
        let start_position = self.position;
        while self.ch.is_alphanumeric() || self.ch == '_' {
            self.read_char();
        }
        let text = &self.input[start_position..self.position];

        // Variables in Prolog start with an uppercase letter or an underscore.
        if text.starts_with(char::is_uppercase) || text.starts_with('_') {
            Token::Variable(text.to_string())
        } else {
            Token::Atom(text.to_string())
        }
    }

    fn read_number(&mut self) -> Token {
        let start_position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }

        // Check if it's a float.
        if self.ch == '.' {
            self.read_char();

            if !self.ch.is_digit(10) {
                // Handle error: the character after '.' must be a digit.
                // For simplicity, we return an integer token here and ignore the '.'.
                // In a real lexer, you would return an error.
                return Token::Integer(
                    self.input[start_position..self.position - 1]
                        .parse::<i64>()
                        .unwrap(),
                );
            }

            while self.ch.is_digit(10) {
                self.read_char();
            }

            Token::Float(
                self.input[start_position..self.position]
                    .parse::<f64>()
                    .unwrap(),
            )
        } else {
            Token::Integer(
                self.input[start_position..self.position]
                    .parse::<i64>()
                    .unwrap(),
            )
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::types::token::Token;

    use super::*;

    #[test]
    fn test_next_token() {
        let mut lexer = Lexer::new("atom_variable 123 4.56 _Variable");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Atom("atom_variable".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(123));
        assert_eq!(lexer.next_token().unwrap(), Token::Float(4.56));
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Variable("_Variable".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::EndOfFile);
    }
}
