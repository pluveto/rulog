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
    List(Vec<Term>, Option<Box<Term>>),
    Structure(String, Vec<Term>),
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Atom(s) => write!(f, "{}", s),
            Term::Variable(s) => write!(f, "{}", s),
            Term::Integer(i) => write!(f, "{}", i),
            Term::Float(Float(val)) => write!(f, "{}", val),
            Term::String(s) => write!(f, "\"{}\"", s),
            Term::List(terms, tail) => {
                write!(f, "[")?;
                for (i, term) in terms.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", term)?;
                }
                if let Some(tail_term) = tail {
                    if !terms.is_empty() {
                        write!(f, " | {}", tail_term)?;
                    } else {
                        write!(f, "| {}", tail_term)?;
                    }
                }
                write!(f, "]")
            }
            Term::Structure(name, terms) => {
                write!(f, "{}(", name)?;
                for (i, term) in terms.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", term)?;
                }
                write!(f, ")")
            }
        }
    }
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
    /// A directive, such as `:- use_module(library(lists)).`
    Directive(Directive),
    /// A query, such as `?- parent(X, a).`
    Query(Query),
    /// A fact, such as `parent(a, b).`
    Fact(Predicate),
    /// A rule, such as `parent(X, Y) :- father(X, Y).`
    Rule(Predicate, Vec<Predicate>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Program(pub Vec<Clause>);
