use crate::lexer::Lexer;
use crate::types::ast::{Clause, Directive, OperatorDefinition, Predicate, Program, Query, Term};
use crate::types::error::{ParserError, ParsingError};
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
    fn comsume(&mut self, expected: Token) -> Result<(), ParsingError> {
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
                Err(ParsingError::new(ParserError::UnexpectedToken(
                    token.clone(),
                )))
            }
        } else {
            Err(ParsingError::new(ParserError::EndOfFile))
        }
    }
    pub fn parse(&mut self) -> Result<Program, ParsingError> {
        self.parse_program()
    }
}

impl<'a> Parser<'a> {
    // Parses a program, which is a list of clauses
    fn parse_program(&mut self) -> Result<Program, ParsingError> {
        let mut clauses = Vec::new();
        while let Some(token) = &self.current_token {
            if token == &Token::EndOfFile {
                break;
            }
            clauses.push(self.parse_clause()?);
            let last = clauses.last().unwrap();
            log::trace!("a clause parsed, {:?}", last);
            self.comsume(Token::Period)?;
        }
        Ok(Program(clauses))
    }

    // Parses a single clause
    fn parse_clause(&mut self) -> Result<Clause, ParsingError> {
        if self.peek(Token::ColonDash) {
            return self.parse_directive().map(Clause::Directive);
        }

        if self.peek(Token::QuestionDash) {
            return self.parse_query().map(Clause::Query);
        }

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
    fn parse_predicate_list(&mut self) -> Result<Vec<Predicate>, ParsingError> {
        let mut predicates = vec![self.parse_predicate()?];
        while let Some(Token::Comma) = self.current_token {
            self.comsume(Token::Comma)?;
            predicates.push(self.parse_predicate()?);
        }
        Ok(predicates)
    }

    // Parses a single predicate
    fn parse_predicate(&mut self) -> Result<Predicate, ParsingError> {
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
            _ => Err(ParsingError::new(ParserError::UnexpectedToken(
                cur.unwrap(),
            ))),
        }
    }

    // Parses a term list
    fn parse_term_list(&mut self) -> Result<Vec<Term>, ParsingError> {
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
        log::trace!("parse_term_list stop, {:?}", self.current_token);
        Ok(terms)
    }

    // Parses a single term
    fn parse_term(&mut self) -> Result<Term, ParsingError> {
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
                Ok(Term::Float(f.into()))
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
            _ => Err(ParsingError::new(ParserError::UnexpectedToken(
                tok.unwrap_or(Token::EndOfFile),
            ))),
        }
    }

    fn parse_list(&mut self) -> Result<Term, ParsingError> {
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

    // Parses a single directive
    fn parse_directive(&mut self) -> Result<Directive, ParsingError> {
        self.comsume(Token::ColonDash)?;
        if self.peek(Token::Atom(String::from("op"))) {
            self.parse_operator_definition()
        } else {
            self.parse_predicate().map(Directive::Predicate)
        }
    }

    // Parses the operator type
    fn parse_operator_type(&mut self) -> Result<String, ParsingError> {
        if let Some(Token::Atom(op_type)) = &self.current_token {
            let valid_types = vec!["xfx", "xfy", "yfx", "fx", "fy", "xf", "yf"];
            if valid_types.contains(&op_type.as_str()) {
                let op_type = op_type.clone();
                self.comsume(Token::Atom(op_type.clone()))?;
                Ok(op_type)
            } else {
                Err(ParsingError::new(ParserError::UnexpectedToken(
                    self.current_token.clone().unwrap(),
                )))
            }
        } else {
            Err(ParsingError::new(ParserError::UnexpectedToken(
                self.current_token.clone().unwrap(),
            )))
        }
    }

    // Parses an integer
    fn parse_integer(&mut self) -> Result<i64, ParsingError> {
        if let Some(Token::Integer(num)) = self.current_token {
            self.comsume(Token::Integer(num))?;
            Ok(num)
        } else {
            Err(ParsingError::new(ParserError::UnexpectedToken(
                self.current_token.clone().unwrap(),
            )))
        }
    }

    // Parses an atom
    fn parse_atom(&mut self) -> Result<String, ParsingError> {
        if let Some(Token::Atom(atom)) = &self.current_token {
            let atom = atom.clone();
            self.comsume(Token::Atom(atom.clone()))?;
            Ok(atom.clone())
        } else {
            Err(ParsingError::new(ParserError::UnexpectedToken(
                self.current_token.clone().unwrap(),
            )))
        }
    }

    // Parses an operator definition
    fn parse_operator_definition(&mut self) -> Result<Directive, ParsingError> {
        self.comsume(Token::Atom(String::from("op")))?; // Assuming 'op' is an atom
        self.comsume(Token::LeftParenthesis)?;
        let priority = self.parse_integer()?;
        self.comsume(Token::Comma)?;
        let operator_type = self.parse_operator_type()?;
        self.comsume(Token::Comma)?;
        let atom = self.parse_atom()?;
        self.comsume(Token::RightParenthesis)?;
        Ok(Directive::OperatorDefinition(OperatorDefinition {
            priority,
            operator_type,
            atom,
        }))
    }

    // Parses a query
    fn parse_query(&mut self) -> Result<Query, ParsingError> {
        self.comsume(Token::QuestionDash)?;
        let predicate_list = self.parse_predicate_list()?;
        Ok(Query {
            predicates: predicate_list,
        })
    }
}

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use crate::types::ast::Float;

    use super::*;
    use rulog_test_util::setup_logger;

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
        assert_eq!(parser.parse_term().unwrap(), Term::Float(Float(1.23)));
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

    #[test]
    fn test_parse_query() {
        setup_logger();
        let mut lexer = Lexer::new("?- foo.");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_query().unwrap(),
            Query {
                predicates: vec![Predicate {
                    name: "foo".to_string(),
                    terms: vec![]
                }]
            }
        );
    }

    #[test]
    fn test_parse_operator_definition() {
        setup_logger();
        let mut lexer = Lexer::new("op(100, xfy, foo).");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_operator_definition().unwrap(),
            Directive::OperatorDefinition(OperatorDefinition {
                priority: 100,
                operator_type: "xfy".to_string(),
                atom: "foo".to_string()
            })
        );
    }

    #[test]
    fn test_parse_directive() {
        setup_logger();
        let mut lexer = Lexer::new(":- op(100, xfy, foo).");
        let mut parser = Parser::new(&mut lexer);
        assert_eq!(
            parser.parse_directive().unwrap(),
            Directive::OperatorDefinition(OperatorDefinition {
                priority: 100,
                operator_type: "xfy".to_string(),
                atom: "foo".to_string()
            })
        );
    }
    #[test]
    fn test_parse_float() {
        assert_eq!(
            parse("temperature(23.5).").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "temperature".to_string(),
                terms: vec![Term::Float(Float(23.5))]
            })])
        );
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse("name(\"John Doe\").").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "name".to_string(),
                terms: vec![Term::String("John Doe".to_string())]
            })])
        );
    }

    #[test]
    fn test_parse_list() {
        assert_eq!(
            parse("colors([red, green, blue]).").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "colors".to_string(),
                terms: vec![Term::List(vec![
                    Term::Atom("red".to_string()),
                    Term::Atom("green".to_string()),
                    Term::Atom("blue".to_string())
                ])]
            })])
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

    #[test]
    fn test_parse_fact() {
        assert_eq!(
            parse("customer('John Jones', boston, good_credit).").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "customer".to_string(),
                terms: vec![
                    Term::Atom("John Jones".to_string()),
                    Term::Atom("boston".to_string()),
                    Term::Atom("good_credit".to_string())
                ]
            })])
        );

        assert_eq!(
            parse("window(main, 2, 2, 20, 72).").unwrap(),
            Program(vec![Clause::Fact(Predicate {
                name: "window".to_string(),
                terms: vec![
                    Term::Atom("main".to_string()),
                    Term::Integer(2),
                    Term::Integer(2),
                    Term::Integer(20),
                    Term::Integer(72)
                ]
            })])
        )
    }
}
