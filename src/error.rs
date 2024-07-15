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

impl Error {
    pub fn to_string(&self, scope: &Scope, registry: &Registry) -> String {
        match self {
            Self::Redefinition(binding) => format!("redefinition of binding `{binding}`."),
            Self::UnboundVariable(ident) => format!("variable `{}` is undefined.", registry.get_name(ident).unwrap()),
            Self::AssertionFailed(got, exp, msg) => {
                let mut string = format!("assertion failed: {} != {}",
                    exp.to_string(scope, registry),
                    got.to_string(scope, registry)
                );
                if let Some(msg) = msg {
                    string.push_str(&format!(": {msg}"));
                }
                string.push('.');
                string
            }
            Self::IncludedFile(err) => format!("including file: {err}."),
            Self::UnexpectedToken(tok, exp) => format!("unexpected token `{tok}`: expected {exp}."),
            Self::UnexpectedEof => format!("unexpected end of file.")
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

impl Located<'_, Error> {
    pub fn to_string(&self, scope: &Scope, registry: &Registry) -> String { 
        let path = self.path();
        let line = self.line();
        let err = self.as_ref().to_string(scope, registry);

        let path = path.map(|p| p.to_str()).flatten().unwrap_or("repl");
        if line > 0 {
            format!("{path}:{line}: {err}")
        }
        else {
            format!("{path}: {err}")
        }
    }
}
