use rulog_core::{
    parser::parse,
    types::ast::{Clause, Directive, OperatorDefinition, Predicate, Query},
};

use std::collections::HashMap;

use crate::{
    resolver::{QuerySolution, QuerySolver},
    types::InterpretingError,
};
pub trait SolutionHandler {
    fn handle_solution(&self, solution: Option<&QuerySolution>) -> bool;
}

#[derive(Default)]
pub struct Interpreter {
    clauses: Vec<(Predicate, Vec<Predicate>)>,
    operator_definitions: HashMap<String, OperatorDefinition>,
}

impl Interpreter {
    pub fn eval(
        &mut self,
        input: &str,
        handler: Option<&dyn SolutionHandler>,
    ) -> Result<(), InterpretingError> {
        let program = parse(input).map_err(InterpretingError::ParseError)?;
        for clause in program.0 {
            let ret = match clause {
                Clause::Directive(directive) => self.handle_directive(directive),
                Clause::Query(query) => self.handle_query(query, handler),
                Clause::Fact(fact) => self.handle_fact(fact),
                Clause::Rule(rule_head, rule_body) => self.handle_rule(rule_head, rule_body),
            };

            ret?
        }

        Ok(())
    }

    fn handle_directive(&mut self, directive: Directive) -> Result<(), InterpretingError> {
        log::trace!("handle directive: {:?}", directive);
        match directive {
            Directive::OperatorDefinition(op_def) => {
                self.operator_definitions
                    .insert(op_def.atom.clone(), op_def);
            }
            Directive::Predicate(pred) => {
                self.clauses.push((pred, Vec::new()));
            }
        }
        Ok(())
    }

    fn handle_query(
        &mut self,
        query: Query,
        handler: Option<&dyn SolutionHandler>,
    ) -> Result<(), InterpretingError> {
        log::trace!("handle query: {:?}", query);
        let handler = handler.unwrap_or(&PrintSolutionHandler);
        let query_solver = QuerySolver::new(self.clauses.clone(), query);

        let mut has_solution = false;
        for solution in query_solver {
            has_solution = true;
            if !handler.handle_solution(Some(&solution)) {
                break;
            }
        }

        if !has_solution {
            handler.handle_solution(None);
        }

        Ok(())
    }

    fn handle_fact(&mut self, fact: Predicate) -> Result<(), InterpretingError> {
        log::trace!("handle fact: {:?}", fact);
        self.clauses.push((fact, Vec::new()));
        Ok(())
    }

    fn handle_rule(
        &mut self,
        rule_head: Predicate,
        rule_body: Vec<Predicate>,
    ) -> Result<(), InterpretingError> {
        log::trace!("handle rule: {:?} :- {:?}", rule_head, rule_body);
        self.clauses.push((rule_head, rule_body));
        Ok(())
    }
}

pub struct PrintSolutionHandler;

impl SolutionHandler for PrintSolutionHandler {
    fn handle_solution(&self, solution: Option<&QuerySolution>) -> bool {
        println!("solution: {:?}", solution);
        true // Continue processing
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::environment::Environment;

    use super::*;
    use rulog_core::types::ast::Term;
    use rulog_test_util::setup_logger;
    struct TestSolutionHandler {
        expected_solutions: Vec<Option<QuerySolution>>,
        index: RefCell<usize>,
    }

    impl TestSolutionHandler {
        fn new(expected_solutions: Vec<Option<QuerySolution>>) -> Self {
            Self {
                expected_solutions,
                index: RefCell::new(0),
            }
        }
    }

    impl SolutionHandler for TestSolutionHandler {
        fn handle_solution(&self, solution: Option<&QuerySolution>) -> bool {
            let size = self.index.borrow().clone();
            if size < self.expected_solutions.len() {
                assert_eq!(
                    solution,
                    self.expected_solutions[size].as_ref(),
                    "expected solution: {:?}, actual solution: {:?}",
                    self.expected_solutions[size],
                    solution
                );
                self.index.replace(size + 1);
                true
            } else {
                false
            }
        }
    }

    #[test]
    fn test_parent_true() {
        setup_logger();
        let mut vm = Interpreter::default();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                ?- parent(tom, liz).
            "#,
            Some(&TestSolutionHandler::new(vec![Some(
                QuerySolution::default(),
            )])),
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_false() {
        setup_logger();
        let mut vm = Interpreter::default();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                ?- parent(liz, tom).
                ?- parent(tom, liz).
            "#,
            Some(&TestSolutionHandler::new(vec![
                None,
                Some(QuerySolution::default()),
            ])),
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_var() {
        setup_logger();
        let mut vm = Interpreter::default();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                ?- parent(X, liz).
            "#,
            Some(&TestSolutionHandler::new(vec![Some(QuerySolution {
                env: Environment::default().extend("X".to_string(), Term::Atom("tom".to_string())),
            })])),
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_var_multiple() {
        setup_logger();
        let mut vm = Interpreter::default();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                parent(tom, bob).
                ?- parent(X, liz).
            "#,
            Some(&TestSolutionHandler::new(vec![Some(QuerySolution {
                env: Environment::default().extend("X".to_string(), Term::Atom("tom".to_string())),
            })])),
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_var_multiple_children() {
        setup_logger();
        let mut vm = Interpreter::default();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                parent(tom, bob).
                ?- parent(tom, X).
            "#,
            Some(&TestSolutionHandler::new(vec![Some(QuerySolution {
                env: Environment::default()
                    .extend("X".to_string(), Term::Atom("bob".to_string()))
                    .extend("X".to_string(), Term::Atom("liz".to_string())),
            })])),
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }
}
