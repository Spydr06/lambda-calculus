use super::*;

#[derive(Debug)]
pub enum Error {
    Redefinition(String),
    UnboundVariable(Identifier),
    AssertionFailed(Expr, Expr, Option<String>),
    IncludedFile(std::io::Error),
    UnexpectedToken(Token, String),
    UnexpectedEof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl From<hash_map::OccupiedError<'_, Identifier, Binding>> for Error {
    fn from(value: hash_map::OccupiedError<'_, Identifier, Binding>) -> Self {
        Self::Redefinition(value.to_string())
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::IncludedFile(value)
    }
}
