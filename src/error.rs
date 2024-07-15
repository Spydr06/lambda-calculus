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
        match self {
            Self::Redefinition(binding) => write!(f, "redefinition of binding `{binding}`."),
            Self::UnboundVariable(ident) => write!(f, "variable `{ident}` is undefined."),
            Self::AssertionFailed(got, exp, msg) => {
                write!(f, "assertion failed: {got:?} != {exp:?}")?;
                if let Some(msg) = msg {
                    write!(f, ": {msg}")?;
                }
                write!(f, ".")
            }
            Self::IncludedFile(err) => write!(f, "including file: {err}."),
            Self::UnexpectedToken(tok, exp) => write!(f, "unexpected token `{tok}`: expected {exp}."),
            Self::UnexpectedEof => write!(f, "unexpected end of file.")
        }
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

impl WithLocation for Error {}

impl std::fmt::Display for Located<'_, Error> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let path = self.path();
        let line = self.line();
        let err = self.as_ref();

        write!(f, "{}:{line}: {err}", path.map(|p| p.to_str()).flatten().unwrap_or("repl"))
    }
}
