use crate::types::error::LexerError;
use crate::types::token::Token;

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
        log::trace!("read char: {:?}", self.ch);
        self.position = self.next_position;
        self.next_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.next_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.next_position).unwrap()
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let token = match self.ch {
            '+' => Ok(Token::Operator("+".to_string())),
            '-' => {
                if self.peek_char() == '>' {
                    self.read_char();
                    Ok(Token::Operator("->".to_string()))
                } else {
                    Ok(Token::Operator("-".to_string()))
                }
            }
            '*' => Ok(Token::Operator("*".to_string())),
            '/' => Ok(Token::Operator("/".to_string())),
            '<' => Ok(Token::Operator("<".to_string())),
            '>' => Ok(Token::Operator(">".to_string())),
            '=' => Ok(Token::Operator("=".to_string())),
            '!' => Ok(Token::Cut),
            ',' => Ok(Token::Comma),
            '|' => Ok(Token::Bar),
            '.' => Ok(Token::Period),
            ';' => Ok(Token::Semicolon),
            '(' => Ok(Token::LeftParenthesis),
            ')' => Ok(Token::RightParenthesis),
            '[' => Ok(Token::LeftBracket),
            ']' => Ok(Token::RightBracket),
            '{' => Ok(Token::LeftCurlyBracket),
            '}' => Ok(Token::RightCurlyBracket),
            '\0' => Ok(Token::EndOfFile),
            ':' => {
                if self.peek_char() == '-' {
                    self.read_char();
                    Ok(Token::ColonDash)
                } else {
                    Ok(Token::Colon)
                }
            }
            '?' => {
                self.read_char();
                if self.ch == '-' {
                    Ok(Token::QuestionDash)
                } else {
                    Err(LexerError::UnexpectedCharacter(self.ch))
                }
            }
            '\"' => {
                let start_position = self.position + 1;
                self.read_char();
                while self.ch != '\"' {
                    self.read_char();
                }
                let text = &self.input[start_position..self.position];
                Ok(Token::String(text.to_string()))
            }
            '\'' => {
                let start_position = self.position + 1;
                self.read_char();
                while self.ch != '\'' {
                    self.read_char();
                }
                let text = &self.input[start_position..self.position];
                Ok(Token::Atom(text.to_string()))
            }
            '%' => {
                while self.ch != '\n' && self.ch != '\0' {
                    self.read_char();
                }
                return self.next_token();
            }
            _ => {
                let ret = if self.ch.is_alphabetic() || self.ch == '_' {
                    Ok(self.read_identifier_or_variable())
                } else if self.ch.is_digit(10) {
                    Ok(self.read_number())
                } else {
                    Err(LexerError::UnexpectedCharacter(self.ch))
                };
                log::trace!("scanned token: {:?}", ret);
                return ret;
            }
        };

        log::trace!("scanned token: {:?}", token);
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
        log::trace!("read number: {:?}", self.ch);
        let start_position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }

        // Check if it's a float.
        if self.ch == '.' {
            if self.peek_char().is_digit(10) {
                self.read_char();
                while self.ch.is_digit(10) {
                    self.read_char();
                }
                return Token::Float(
                    self.input[start_position..self.position]
                        .parse::<f64>()
                        .unwrap(),
                );
            } else {
                return Token::Integer(
                    self.input[start_position..self.position]
                        .parse::<i64>()
                        .unwrap(),
                );
            }
        }

        Token::Integer(
            self.input[start_position..self.position]
                .parse::<i64>()
                .unwrap(),
        )
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            log::trace!("skip whitespace: {:?}", self.ch);
            self.read_char();
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::token::Token;
    use rulog_test_util::setup_logger;

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

    #[test]
    fn test_list_without_bracket() {
        setup_logger();
        let mut lexer = Lexer::new("1,2,3");
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(1));
        assert_eq!(lexer.next_token().unwrap(), Token::Comma);
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(2));
        assert_eq!(lexer.next_token().unwrap(), Token::Comma);
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(3));
        assert_eq!(lexer.next_token().unwrap(), Token::EndOfFile);
    }

    #[test]
    fn test_list() {
        setup_logger();
        let mut lexer = Lexer::new("[1, 2, 3]");
        assert_eq!(lexer.next_token().unwrap(), Token::LeftBracket);
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(1));
        assert_eq!(lexer.next_token().unwrap(), Token::Comma);
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(2));
        assert_eq!(lexer.next_token().unwrap(), Token::Comma);
        assert_eq!(lexer.next_token().unwrap(), Token::Integer(3));
        assert_eq!(lexer.next_token().unwrap(), Token::RightBracket);
        assert_eq!(lexer.next_token().unwrap(), Token::EndOfFile);
    }

    #[test]
    fn test_comment() {
        setup_logger();
        let mut lexer = Lexer::new("% comment\natom.");
        assert_eq!(lexer.next_token().unwrap(), Token::Atom("atom".to_string()));
        assert_eq!(lexer.next_token().unwrap(), Token::Period);
        assert_eq!(lexer.next_token().unwrap(), Token::EndOfFile);
    }
}
