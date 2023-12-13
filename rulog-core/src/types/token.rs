#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Atom(String),      // Atoms, such as foo, 'Bar', etc.
    Variable(String),  // Variables, such as X, _Y, _, etc.
    Integer(i64),      // Integers, such as 123, -456, etc.
    Float(f64),        // Floating-point numbers, such as 1.23, -4.56e+7, etc.
    String(String),    // Strings, such as "hello world".
    Operator(String),  // Operators, such as +, -, *, /, <, =:=, etc.
    Cut,               // Cut operator (!).
    Comma,             // Comma (,), usually used to separate clauses.
    Bar,               // Bar (|), used to separate choices.
    Period,            // Period (.), used to end a clause.
    Semicolon,         // Semicolon (;), used to represent choice.
    Colon,             // Colon (:), used for module qualifiers.
    ColonDash,         // Colon-dash (:-), used to separate rule heads and bodies.
    QuestionMark,      // Question mark (?), used for queries.
    LeftParenthesis,   // Left parenthesis (().
    RightParenthesis,  // Right parenthesis ())).
    LeftBracket,       // Left bracket ([).
    RightBracket,      // Right bracket (]).
    LeftCurlyBracket,  // Left curly bracket ({).
    RightCurlyBracket, // Right curly bracket (}).
    EndOfFile,         // End-of-file marker.
    Comment,           // Comment, such as // this is a comment.
}
