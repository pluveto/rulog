use rulog_core::{
    parser::parse,
    types::ast::{Clause, Directive, OperatorDefinition, Predicate, Query},
};

use std::collections::HashMap;

use crate::{
    resolver::{QuerySolution, QuerySolver},
    types::InterpretingError,
};

pub struct Interpreter {
    clauses: Vec<(Predicate, Vec<Predicate>)>,
    operator_definitions: HashMap<String, OperatorDefinition>,

    on_solution: Option<Box<dyn Fn(&QuerySolution) -> bool>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            clauses: Vec::new(),
            operator_definitions: HashMap::new(),
            on_solution: None,
        }
    }

    pub fn on_solution<F>(&mut self, f: F)
    where
        F: Fn(&QuerySolution) -> bool + 'static,
    {
        self.on_solution = Some(Box::new(f));
    }

    pub fn eval(&mut self, input: &str) -> Result<(), InterpretingError> {
        let program = parse(input).map_err(InterpretingError::ParseError)?;
        for clause in program.0 {
            let ret = match clause {
                Clause::Directive(directive) => self.handle_directive(directive),
                Clause::Query(query) => self.handle_query(query),
                Clause::Fact(fact) => self.handle_fact(fact),
                Clause::Rule(rule_head, rule_body) => self.handle_rule(rule_head, rule_body),
            };

            if let Err(e) = ret {
                return Err(e);
            }
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

    fn handle_query(&mut self, query: Query) -> Result<(), InterpretingError> {
        log::trace!("handle query resolved: {:?}", query);
        let mut query_solver = QuerySolver::new(self.clauses.clone(), query);
        if let Some(ref on_solution) = self.on_solution {
            while let Some(solution) = query_solver.next() {
                if !on_solution(&solution) {
                    break;
                }
            }
        } else {
            for solution in query_solver {
                println!("solution: {:?}", solution);
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn setup_logger() {
        use log::LevelFilter;
        let _ = env_logger::builder()
            .is_test(true)
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{}:{} \t[{}] - {}",
                    record.file().unwrap_or("unknown"),
                    record.line().unwrap_or(0),
                    record.level(),
                    record.args()
                )
            })
            .filter_level(LevelFilter::Trace)
            .try_init();
    }

    #[test]
    fn test_parent_true() {
        setup_logger();
        let mut vm = Interpreter::new();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                ?- parent(tom, liz).
            "#,
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_false() {
        setup_logger();
        let mut vm = Interpreter::new();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                ?- parent(liz, tom).
            "#,
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_var() {
        setup_logger();
        let mut vm = Interpreter::new();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                ?- parent(X, liz).
            "#,
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_var_multiple() {
        setup_logger();
        let mut vm = Interpreter::new();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                parent(tom, bob).
                ?- parent(X, liz).
            "#,
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }

    #[test]
    fn test_parent_var_multiple_children() {
        setup_logger();
        let mut vm = Interpreter::new();
        let ret = vm.eval(
            r#"
                parent(tom, liz).
                parent(tom, bob).
                ?- parent(tom, X).
            "#,
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }
}
