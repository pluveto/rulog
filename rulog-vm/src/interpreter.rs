use rulog_core::{
    parser::parse,
    types::ast::{Clause, Directive, OperatorDefinition, Predicate, Query, Term},
};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    bindings: HashMap<String, Term>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
        }
    }

    pub fn bind(&mut self, var: String, term: Term) {
        self.bindings.insert(var, term);
    }

    pub fn lookup(&self, var: &String) -> Option<&Term> {
        self.bindings.get(var)
    }
}

pub fn unify(term1: &Term, term2: &Term, env: &mut Environment) -> bool {
    match (term1, term2) {
        (Term::Variable(v), t) | (t, Term::Variable(v)) => {
            if let Some(binding) = env.lookup(v) {
                let binding = binding.clone();
                return unify(&binding, t, env);
            } else {
                env.bind(v.clone(), t.clone());
                return true;
            }
        }
        (Term::Atom(a1), Term::Atom(a2)) if a1 == a2 => true,
        (Term::Integer(i1), Term::Integer(i2)) if i1 == i2 => true,
        (Term::Float(f1), Term::Float(f2)) if f1 == f2 => true,
        (Term::String(s1), Term::String(s2)) if s1 == s2 => true,
        (Term::List(l1), Term::List(l2)) if l1.len() == l2.len() => {
            for (item1, item2) in l1.iter().zip(l2.iter()) {
                if !unify(item1, item2, env) {
                    return false;
                }
            }
            true
        }
        (Term::Structure(name1, terms1), Term::Structure(name2, terms2))
            if name1 == name2 && terms1.len() == terms2.len() =>
        {
            for (item1, item2) in terms1.iter().zip(terms2.iter()) {
                if !unify(item1, item2, env) {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

#[derive(Debug)]
pub enum InterpretingError {
    ParseError(rulog_core::types::error::ParsingError),
    UnsupportedDirective,
    QueryFailed,
}

use std::collections::HashSet;

pub struct Interpreter {
    facts: HashSet<Predicate>,
    rules: Vec<(Predicate, Vec<Predicate>)>,
    operator_definitions: HashMap<String, OperatorDefinition>,
    global_env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            facts: HashSet::new(),
            rules: Vec::new(),
            operator_definitions: HashMap::new(),
            global_env: Environment::new(),
        }
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
            _ => return Err(InterpretingError::UnsupportedDirective),
        }
        Ok(())
    }

    fn handle_query(&mut self, query: Query) -> Result<(), InterpretingError> {
        log::trace!("handle query: {:?}", query);
        for predicate in query.predicates {
            if !self.solve_predicate(&predicate, &mut self.global_env.clone()) {
                return Err(InterpretingError::QueryFailed);
            }
        }
        Ok(())
    }

    fn handle_fact(&mut self, fact: Predicate) -> Result<(), InterpretingError> {
        log::trace!("handle fact: {:?}", fact);
        self.facts.insert(fact);
        Ok(())
    }

    fn handle_rule(
        &mut self,
        rule_head: Predicate,
        rule_body: Vec<Predicate>,
    ) -> Result<(), InterpretingError> {
        log::trace!("handle rule: {:?} :- {:?}", rule_head, rule_body);
        self.rules.push((rule_head, rule_body));
        Ok(())
    }

    fn solve_predicate(&self, pred: &Predicate, env: &mut Environment) -> bool {
        log::trace!("solve predicate: {:?}", pred);
        for fact in &self.facts {
            if pred.name == fact.name && pred.terms.len() == fact.terms.len() {
                let mut env_clone = env.clone();
                if pred
                    .terms
                    .iter()
                    .zip(&fact.terms)
                    .all(|(t1, t2)| unify(t1, t2, &mut env_clone))
                {
                    *env = env_clone;
                    return true;
                }
            }
        }

        for (head, body) in &self.rules {
            if pred.name == head.name && pred.terms.len() == head.terms.len() {
                let mut env_clone = env.clone();
                if pred
                    .terms
                    .iter()
                    .zip(&head.terms)
                    .all(|(t1, t2)| unify(t1, t2, &mut env_clone))
                {
                    if body.iter().all(|p| self.solve_predicate(p, &mut env_clone)) {
                        *env = env_clone;
                        return true;
                    }
                }
            }
        }

        false
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
    fn test_all_specs() {
        setup_logger();
        let mut vm = Interpreter::new();
        let ret = vm.eval(
            r#"parent(tom, liz).
        ?- parent(tom, liz).
        "#,
        );
        assert!(ret.is_ok(), "{:?}", ret);
    }
}
