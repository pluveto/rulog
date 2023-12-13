use crate::lexer::Lexer;
use crate::types::ast::{Clause, Predicate, Program, Term};
use crate::types::error::ParserError;
use crate::types::token::Token;

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let current_token = lexer.next_token().ok();
        Parser {
            lexer: lexer,
            current_token,
        }
    }

    // Peek current token and check if it is expected
    fn peek(&mut self, expected: Token) -> bool {
        log::trace!("peek: {:?}, expect is {:?}", self.current_token, expected);
        self.current_token.as_ref() == Some(&expected)
    }

    // Consume current token and let current token point to next token
    fn comsume(&mut self, expected: Token) -> Result<(), ParserError> {
        log::trace!(
            "expect_token: {:?}, expect is {:?}",
            self.current_token,
            expected
        );
        if let Some(token) = &self.current_token {
            if token == &expected {
                self.current_token = self.lexer.next_token().ok();
                Ok(())
            } else {
                Err(ParserError::UnexpectedToken(token.clone()))
            }
        } else {
            Err(ParserError::EndOfFile)
        }
    }
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        self.parse_program()
    }
}

impl<'a> Parser<'a> {
    // Parses a program, which is a list of clauses
    fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut clauses = Vec::new();
        while let Some(token) = &self.current_token {
            if token == &Token::EndOfFile {
                break;
            }
            clauses.push(self.parse_clause()?);
            self.comsume(Token::Period)?;
        }
        Ok(Program(clauses))
    }

    // Parses a single clause
    fn parse_clause(&mut self) -> Result<Clause, ParserError> {
        let head = self.parse_predicate()?;
        match self.current_token {
            Some(Token::ColonDash) => {
                self.comsume(Token::ColonDash)?;
                let body = self.parse_predicate_list()?;
                Ok(Clause::Rule(head, body))
            }
            _ => Ok(Clause::Fact(head)),
        }
    }

    // Parses a predicate list
    fn parse_predicate_list(&mut self) -> Result<Vec<Predicate>, ParserError> {
        let mut predicates = vec![self.parse_predicate()?];
        while let Some(Token::Comma) = self.current_token {
            self.comsume(Token::Comma)?;
            predicates.push(self.parse_predicate()?);
        }
        Ok(predicates)
    }

    // Parses a single predicate
    fn parse_predicate(&mut self) -> Result<Predicate, ParserError> {
        let cur = self.current_token.clone();
        match cur {
            Some(Token::Atom(name)) => {
                self.comsume(Token::Atom(name.clone()))?;
                let terms = if self.peek(Token::LeftParenthesis) {
                    self.comsume(Token::LeftParenthesis)?;
                    let terms = self.parse_term_list()?;
                    self.comsume(Token::RightParenthesis)?;
                    terms
                } else {
                    vec![]
                };
                Ok(Predicate { name, terms })
            }
            Some(Token::Variable(name)) => {
                self.comsume(Token::Variable(name.clone()))?;
                Ok(Predicate {
                    name,
                    terms: vec![],
                })
            }
            _ => Err(ParserError::UnexpectedToken(cur.unwrap())),
        }
    }

    // Parses a term list
    fn parse_term_list(&mut self) -> Result<Vec<Term>, ParserError> {
        log::trace!("parse_term_list start, {:?}", self.current_token);
        let mut terms = Vec::new();
        if self.peek(Token::RightParenthesis) {
            return Ok(terms);
        }
        terms.push(self.parse_term()?);
        while self.peek(Token::Comma) {
            self.comsume(Token::Comma)?;
            terms.push(self.parse_term()?);
        }
        println!("parse_term_list stop, {:?}", self.current_token);
        Ok(terms)
    }

    // Parses a single term
    fn parse_term(&mut self) -> Result<Term, ParserError> {
        let tok = self.current_token.clone();
        log::trace!("parse_term: {:?}", tok);
        match tok {
            Some(Token::Variable(v)) => {
                self.comsume(Token::Variable(v.clone()))?;
                Ok(Term::Variable(v))
            }
            Some(Token::Integer(i)) => {
                self.comsume(Token::Integer(i))?;
                Ok(Term::Integer(i))
            }
            Some(Token::Float(f)) => {
                self.comsume(Token::Float(f))?;
                Ok(Term::Float(f))
            }
            Some(Token::String(s)) => {
                self.comsume(Token::String(s.clone()))?;
                Ok(Term::String(s))
            }
            Some(Token::LeftBracket) => self.parse_list(),
            Some(Token::Atom(name)) if self.peek(Token::LeftParenthesis) => {
                self.comsume(Token::LeftParenthesis)?;
                let terms = self.parse_term_list()?;
                self.comsume(Token::RightParenthesis)?;
                Ok(Term::Structure(name, terms))
            }
            Some(Token::Atom(a)) => {
                self.comsume(Token::Atom(a.clone()))?;
                Ok(Term::Atom(a))
            }
            _ => Err(ParserError::UnexpectedToken(
                tok.unwrap_or(Token::EndOfFile),
            )),
        }
    }

    fn parse_list(&mut self) -> Result<Term, ParserError> {
        log::trace!("parse_list");
        self.comsume(Token::LeftBracket)?;

        if self.peek(Token::RightBracket) {
            self.comsume(Token::RightBracket)?;
            return Ok(Term::List(vec![]));
        }

        let terms = self.parse_term_list()?;
        if self.peek(Token::Bar) {
            self.comsume(Token::Bar)?;
            let tail = Box::new(self.parse_term()?);
            Ok(Term::List(
                terms.into_iter().chain(std::iter::once(*tail)).collect(),
            ))
        } else {
            self.comsume(Token::RightBracket)?;
            Ok(Term::List(terms))
        }
    }
}

pub fn parse(input: &str) -> Result<Program, ParserError> {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_logger() {
        use log::LevelFilter;
        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(LevelFilter::Trace)
            .try_init();
    }

    //===------------------------------------------------------------------===//
    // Sub-parser tests
    //===------------------------------------------------------------------===//
    #[test]
    fn test_parse_term_atom() {
        let mut lexer = Lexer::new("foo");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(parser.parse_term().unwrap(), Term::Atom("foo".to_string()));
    }

    #[test]
    fn test_parse_term_variable() {
        let mut lexer = Lexer::new("X");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_term().unwrap(),
            Term::Variable("X".to_string())
        );
    }

    #[test]
    fn test_parse_term_integer() {
        let mut lexer = Lexer::new("123");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(parser.parse_term().unwrap(), Term::Integer(123));
    }

    #[test]
    fn test_parse_term_float() {
        let mut lexer = Lexer::new("1.23");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(parser.parse_term().unwrap(), Term::Float(1.23));
    }

    #[test]
    fn test_parse_term_string() {
        let mut lexer = Lexer::new("\"hello world\"");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_term().unwrap(),
            Term::String("hello world".to_string())
        );
    }

    #[test]
    fn test_parse_list_empty() {
        setup_logger();
        let mut lexer = Lexer::new("[]");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(parser.parse_list().unwrap(), Term::List(vec![]));
    }

    #[test]
    fn test_parse_term_case_list_empty() {
        setup_logger();
        let mut lexer = Lexer::new("[]");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(parser.parse_term().unwrap(), Term::List(vec![]));
    }

    #[test]
    fn test_parse_term_list_case_empty() {
        setup_logger();
        let mut lexer = Lexer::new("1,2,3");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_term_list().unwrap(),
            vec![Term::Integer(1), Term::Integer(2), Term::Integer(3)]
        );
    }

    #[test]
    fn test_parse_term_case_list_non_empty() {
        setup_logger();
        let mut lexer = Lexer::new("[1, 2, 3]");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_term().unwrap(),
            Term::List(vec![Term::Integer(1), Term::Integer(2), Term::Integer(3)])
        );
    }

    #[test]
    fn test_parse_term_case_list_tail() {
        let mut lexer = Lexer::new("[1, 2, 3 | X]");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_term().unwrap(),
            Term::List(vec![
                Term::Integer(1),
                Term::Integer(2),
                Term::Integer(3),
                Term::Variable("X".to_string())
            ])
        );
    }

    #[test]
    fn test_parse_predicate_case_atom() {
        let mut lexer = Lexer::new("foo");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_predicate().unwrap(),
            Predicate {
                name: "foo".to_string(),
                terms: vec![]
            }
        );
    }

    #[test]
    fn test_parse_predicate_case_atom_with_parenthesis() {
        setup_logger();
        let mut lexer = Lexer::new("foo()");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_predicate().unwrap(),
            Predicate {
                name: "foo".to_string(),
                terms: vec![]
            }
        );
    }

    #[test]
    fn test_parse_predicate_case_atom_with_parenthesis_and_terms() {
        setup_logger();
        let mut lexer = Lexer::new("foo(1, 2, 3)");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_predicate().unwrap(),
            Predicate {
                name: "foo".to_string(),
                terms: vec![Term::Integer(1), Term::Integer(2), Term::Integer(3)]
            }
        );
    }

    #[test]
    fn test_parse_clause_case_fact() {
        setup_logger();
        let mut lexer = Lexer::new("foo.");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_clause().unwrap(),
            Clause::Fact(Predicate {
                name: "foo".to_string(),
                terms: vec![]
            })
        );
    }

    //===------------------------------------------------------------------===//
    // Comprehensive tests
    //===------------------------------------------------------------------===//
    #[test]
    fn test_parse_atom() {
        assert_eq!(
            parse("foo.").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "foo".to_string(),
                terms: vec![]
            })])
        );
    }

    #[test]
    fn test_parse_variable() {
        assert_eq!(
            parse("X.").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "X".to_string(),
                terms: vec![]
            })])
        );
    }
}
