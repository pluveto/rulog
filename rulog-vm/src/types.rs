#[derive(Debug)]
pub enum InterpretingError {
    ParseError(rulog_core::types::error::ParsingError),
    UnsupportedDirective,
    QueryFailed,
}
