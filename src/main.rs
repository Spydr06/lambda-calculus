#![feature(result_flattening)]
#![feature(map_try_insert)]
#![feature(let_chains)]
#![feature(if_let_guard)]

mod error;
mod parser;
mod scope;

use std::{collections::{hash_map, HashSet}, fmt, fs::File, io::Read, path::PathBuf};

use error::*;
use parser::*;
use scope::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Expr {
    Application(Box<Expr>, Box<Expr>),
    Function(Identifier, Box<Expr>),
    Variable(Identifier),
}

impl Expr {
    fn church_numeral(number: u32, registry: &mut Registry) -> Self {
        let f = registry.get("f".to_string());
        let x = registry.get("x".to_string());
        let mut expr = Expr::Variable(x);
        
        for _ in 0..number {
            expr = Expr::Application(Box::new(Expr::Variable(f)), Box::new(expr));
        }

        expr = Expr::Function(x, Box::new(expr));
        expr = Expr::Function(f, Box::new(expr));

        expr
    }

    fn beta_reduce(self) -> Result<Self, Error> { 
        match self {
            Self::Application(lhs, rhs) => {
                let lhs = lhs.beta_reduce()?;
                let new = if let Self::Function(arg, body) = lhs {
                    body.substitute(arg, &rhs).beta_reduce()?
                }
                else {        
                    Self::Application(
                        Box::new(lhs.beta_reduce()?),
                        Box::new(rhs.beta_reduce()?)
                    )
                };

                Ok(new)
            },
            Self::Variable(ident) => {
                Err(Error::UnboundVariable(ident))
            }
            _ => Ok(self),
        }
    }

    fn substitute(self, ident: Identifier, substitute: &Expr) -> Self {
        match self {
            Self::Variable(var) if var == ident => substitute.clone(),
            Self::Function(arg, body) if arg != ident => Self::Function(arg, Box::new(body.substitute(ident, substitute))),
            Self::Application(left, right) => Self::Application(
                    Box::new(left.substitute(ident, substitute)),
                    Box::new(right.substitute(ident, substitute))
                ),
            _ => self
        }
    }

    fn is_application(&self) -> bool {
        matches!(self, Self::Application(_, _))
    }

    fn to_string(&self, registry: &Registry) -> String {
        match self {
            Self::Variable(ident) => registry.get_name(ident).unwrap().clone(),
            Self::Function(arg, body) => format!("λ{}.{}", registry.get_name(arg).unwrap(), body.to_string(registry)),
            Self::Application(left, right) if right.is_application() =>
                format!("{} ({})", left.to_string(registry), right.to_string(registry)),
            Self::Application(left, right) =>
                format!("{} {}", left.to_string(registry), right.to_string(registry))
        }
    }

    fn pretty_to_string(&self, scope: &Scope, registry: &Registry) -> String {
        if let Some((id, _)) = scope.bindings().iter().filter(|(_, b)| b.1).find(|(_, b)| &b.0 == self) {
            registry.get_name(id).unwrap().clone()
        }
        else {
            self.to_string(registry)
        }
    }
}

enum Stmt {
    Include(PathBuf),
    LetBinding(Identifier, Binding),
    Assertion(Expr, Option<String>)
}

impl Stmt {
    fn parse_all(path: PathBuf, registry: &mut Registry, includes: &mut HashSet<PathBuf>) -> Result<Vec<Self>, std::io::Error> {
        if includes.contains(&path) {
            return Ok(Vec::new());
        }

        let mut file = File::open(path.clone()).expect("file error");
        let mut source = String::new();
        file.read_to_string(&mut source).expect("file error");

        let tokens: &mut dyn Iterator<Item = Token> = &mut Token::lex(source.chars());
        let mut parser = Parser::from(tokens);
        parser.set_path(path.parent().unwrap().to_path_buf());
        let stmts = parser.parse(registry)
            .expect("parsing error");
        
        includes.insert(path);

        Ok(stmts)
    }

    fn eval(self, registry: &mut Registry, scope: &mut Scope, includes: &mut HashSet<PathBuf>) -> Result<(), Error> {
        match self {
            Self::LetBinding(ident, binding) => scope.put(ident, binding),
            Self::Include(path) => {
                let stmts = Self::parse_all(path, registry, includes)?;
                for stmt in stmts {
                    stmt.eval(registry, scope, includes)?;
                }
                Ok(())
            },
            Self::Assertion(expr, exp) => {
                let expr = scope.substitute(expr).beta_reduce()?;
                let true_ident = registry.get("true".to_string()); 
                if let Some(t) = scope.get(&true_ident) {
                    (expr == t)
                        .then_some(())
                        .ok_or_else(|| Error::AssertionFailed(expr, t, exp))
                }
                else {
                    Err(Error::UnboundVariable(true_ident))
                }
            }
        }
    }
}

fn main() {
    let mut args = std::env::args();
    args.next();

    let mut registry = Registry::default();
    let mut scope = Scope::default();
    let mut includes = HashSet::new();

    let mut stmts = Vec::new();
    for arg in args.into_iter() {
        let mut path = PathBuf::from(arg);
        std::fs::canonicalize(&mut path).expect("path error");

        stmts.extend(
            Stmt::parse_all(path, &mut registry, &mut includes)
                .expect("parse error")
        )
    }

    for stmt in stmts {
        stmt.eval(&mut registry, &mut scope, &mut includes)
            .expect("runtime error");
    }
}
