use std::hash::Hasher;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Directive {
    Predicate(Predicate),
    OperatorDefinition(OperatorDefinition),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct OperatorDefinition {
    pub priority: i64,
    pub operator_type: String,
    pub atom: String,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Query {
    pub predicates: Vec<Predicate>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Term {
    Atom(String),
    Variable(String),
    Integer(i64),
    Float(Float),
    String(String),
    List(Vec<Term>),
    Structure(String, Vec<Term>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Float(pub f64);

impl std::hash::Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.0.is_nan() {
            // You can use a constant or a custom value to represent NaN
            u64::MAX.hash(state);
        } else {
            self.0.to_bits().hash(state);
        }
    }
}

impl From<f64> for Float {
    fn from(f: f64) -> Self {
        Float(f)
    }
}

impl Eq for Float {}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Predicate {
    pub name: String,
    pub terms: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Clause {
    Directive(Directive),
    Query(Query),
    Fact(Predicate),
    Rule(Predicate, Vec<Predicate>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Program(pub Vec<Clause>);
