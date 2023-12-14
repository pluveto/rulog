use std::collections::HashMap;

use rulog_core::types::ast::Term;
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    pub bindings: HashMap<String, Term>,
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

impl FromIterator<(std::string::String, Term)> for Environment {
    fn from_iter<T: IntoIterator<Item = (std::string::String, Term)>>(iter: T) -> Self {
        let mut env = Environment::new();
        for (var, term) in iter {
            env.bind(var, term);
        }
        env
    }
}
