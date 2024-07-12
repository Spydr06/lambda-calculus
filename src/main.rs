#![feature(result_flattening)]
#![feature(map_try_insert)]
#![feature(let_chains)]
#![feature(if_let_guard)]

mod parser;
mod scope;

use std::{fmt, fs::File, io::Read, path::PathBuf};

use parser::*;
use scope::*;

#[derive(Debug)]
enum RuntimeError {
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
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

    fn beta_reduce(self) -> Result<Self, RuntimeError> { 
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
            Self::Variable(ident) => format!("{}<{ident}>", registry.get_name(ident).unwrap()),
            Self::Function(arg, body) => format!("λ{}<{arg}>.{}", registry.get_name(arg).unwrap(), body.to_string(registry)),
            Self::Application(left, right) => if right.is_application() {
                format!("{} ({})", left.to_string(registry), right.to_string(registry))
            }
            else {
                format!("{} {}", left.to_string(registry), right.to_string(registry))
            }
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

fn main() {
    let mut args = std::env::args();
    args.next();

    let mut registry = Registry::default();
    let mut scope = Scope::default();

    let mut exprs = Vec::new();
    for arg in args.into_iter() {
        let mut path = PathBuf::from(arg);
        std::fs::canonicalize(&mut path).expect("path error");

        let mut file = File::open(path.clone()).expect("file error");
        let mut source = String::new();
        file.read_to_string(&mut source).expect("file error");

        let tokens: &mut dyn Iterator<Item = Token> = &mut Token::lex(source.chars());
        let mut parser = Parser::from(tokens);
        parser.set_path(path.parent().unwrap().to_path_buf());
        exprs.extend(parser.parse(scope.bindings_mut(), &mut registry)
            .expect("parsing error"));
    }

/*    for (ident, Binding(expr, _)) in scope.bindings().iter() {
        println!("{}<{ident}> = {}", registry.get_name(ident).unwrap(), expr.to_string(&registry));
    }*/

    for expr in exprs {
        let expr = scope.substitute(expr).beta_reduce().expect("runtime error");
        println!("{}", expr.pretty_to_string(&scope, &registry));
    }
}
