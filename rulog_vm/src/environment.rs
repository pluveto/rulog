use rulog_core::types::ast::Term;
use std::collections::HashMap;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Environment {
    pub bindings: HashMap<String, Term>,
}

impl Environment {
    pub fn bind(&mut self, var: String, term: Term) {
        self.bindings.insert(var, term);
    }

    pub fn lookup(&self, var: &String) -> Option<&Term> {
        self.bindings.get(var)
    }

    pub fn extend(mut self, var: String, term: Term) -> Self {
        self.bind(var, term);
        self
    }
}

impl FromIterator<(std::string::String, Term)> for Environment {
    fn from_iter<T: IntoIterator<Item = (std::string::String, Term)>>(iter: T) -> Self {
        let mut env = Environment::default();
        for (var, term) in iter {
            env.bind(var, term);
        }
        env
    }
}
