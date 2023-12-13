#[derive(Debug, PartialEq)]
pub enum Term {
    Atom(String),
    Variable(String),
    Integer(i64),
    Float(f64),
    String(String),
    List(Vec<Term>),
    Structure(String, Vec<Term>),
}

#[derive(Debug, PartialEq)]
pub struct Predicate {
    pub name: String,
    pub terms: Vec<Term>,
}

#[derive(Debug, PartialEq)]
pub enum Clause {
    Fact(Predicate),
    Rule(Predicate, Vec<Predicate>),
}

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Clause>);
