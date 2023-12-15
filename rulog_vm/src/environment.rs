use std::collections::HashMap;

use rulog_core::types::ast::Term;

pub type Environment = HashMap<String, Term>;
