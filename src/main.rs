#![feature(result_flattening)]
#![feature(map_try_insert)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(hash_set_entry)]

mod error;
mod parser;
mod scope;

use std::{collections::{hash_map, HashSet}, fs::File, io::Read, path::PathBuf};

use error::*;
use parser::*;
use scope::*;

mod ansi {
    pub const RESET: &str = "\x1b[0m";
    pub const RED: &str = "\x1b[31m";
}

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

    fn beta_reduce<'a>(self) -> Result<Self, Error> { 
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
    fn parse_all<'a>(path: PathBuf, registry: &mut Registry, includes: &'a mut HashSet<PathBuf>) -> Result<Vec<Located<'a, Self>>, Located<'a, Error>> {
        if includes.contains(&path) {
            return Ok(Vec::new());
        }

        let path = includes.get_or_insert(path);
        
        let mut file = File::open(path.clone()).map_err(|err| {
            let err: Error = err.into();
            err.with_location(Some(path), 0)
        })?;
        let mut source = String::new();
        file.read_to_string(&mut source).map_err(|err| {
            let err: Error = err.into();
            err.with_location(Some(path), 0)
        })?;

        let tokens = Token::lex(source.chars()).collect();
        let mut parser = Parser::new(tokens, Some(path));
        parser.parse(registry)
    }

    fn eval<'a>(this: Located<'a, Self>, registry: &mut Registry, scope: &mut Scope, includes: &'a mut HashSet<PathBuf>) -> Result<(), Located<'a, Error>> {
        let path = this.path();
        let line = this.line();
        match this.unwrap() {
            Self::LetBinding(ident, binding) => scope.put(ident, binding).map_err(|err| err.with_location(path, line)),
            Self::Include(include_path) => {
                let includes_ptr = includes as *const _ as *mut HashSet<PathBuf>;
                let stmts = Self::parse_all(include_path, registry, includes)?;
                for stmt in stmts {
                    Stmt::eval(stmt, registry, scope, unsafe { includes_ptr.as_mut().unwrap() })?;
                }
                Ok(())
            },
            Self::Assertion(expr, exp) => {
                let expr = scope.substitute(expr)
                    .beta_reduce()
                    .map_err(|err| err.with_location(path, line))?;
                let true_ident = registry.get("true".to_string()); 
                if let Some(t) = scope.get(&true_ident) {
                    (expr == t)
                        .then_some(())
                        .ok_or_else(|| Error::AssertionFailed(expr, t, exp).with_location(path, line))
                }
                else {
                    Err(Error::UnboundVariable(true_ident).with_location(path, line))
                }
            }
        }
    }
}

impl WithLocation for Stmt {}

fn error(err: String) -> ! {
    eprintln!("{}{err}{}", ansi::RED, ansi::RESET); 
    std::process::exit(1);
}

fn main() {
    let mut args = std::env::args();
    args.next();

    let mut registry = Registry::default();
    let mut scope = Scope::default();
    let mut includes = HashSet::new();
    let includes_ptr = &mut includes as *mut HashSet<PathBuf>;

    let mut stmts = Vec::new();
    for arg in args.into_iter() {
        let mut path = PathBuf::from(arg);
        if let Err(err) = std::fs::canonicalize(&mut path) {
            error(format!("error: could not get full path of `{}`: {err}", path.into_os_string().into_string().unwrap()));
        }

        match Stmt::parse_all(path, &mut registry, unsafe { includes_ptr.as_mut().unwrap() }) {
            Ok(new) => stmts.extend(new),
            Err(err) => error(err.to_string(&scope, &registry))
        }
    }

    for stmt in stmts {
        if let Err(err) = Stmt::eval(stmt, &mut registry, &mut scope, unsafe {includes_ptr.as_mut().unwrap() }) {
            error(err.to_string(&scope, &registry));
        }
    }
}
