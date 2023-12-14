/// A token is a lexical unit of a Prolog program.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    /// Atoms, such as foo, 'Bar', etc.
    Atom(String),
    /// Variables, such as X, _Y, _, etc.
    Variable(String),
    /// Integers, such as 123, -456, etc.
    Integer(i64),
    /// Floating-point numbers, such as 1.23, -4.56e+7, etc.
    Float(f64),
    /// Strings, such as "hello world".
    String(String),
    /// Operators, such as +, -, *, /, <, =:=, etc.
    Operator(String),
    /// Cut operator (!).
    Cut,
    /// Comma (,), usually used to separate clauses.
    Comma,
    /// Bar (|), used to separate choices.
    Bar,
    /// Period (.), used to end a clause.
    Period,
    /// Semicolon (;), used to represent choice.
    Semicolon,
    /// Colon (:), used for module qualifiers.
    Colon,
    /// Colon-dash (:-), used for directives.
    ColonDash,
    /// Question-dash (?-), used for queries.
    QuestionDash,
    /// Left parenthesis (().
    LeftParenthesis,
    /// Right parenthesis ())).
    RightParenthesis,
    /// Left bracket ([).
    LeftBracket,
    /// Right bracket (]).
    RightBracket,
    /// Left curly bracket ({).
    LeftCurlyBracket,
    /// Right curly bracket (}).
    RightCurlyBracket,
    /// End-of-file marker.
    EndOfFile,
    /// Comment, such as % this is a comment.
    Comment,
}
