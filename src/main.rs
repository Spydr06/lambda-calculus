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
    NotApplication,
    NotFunction
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotApplication => write!(f, "not an application"),
            Self::NotFunction => write!(f, "not a function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    fn beta_reduce(&mut self) -> Result<(), RuntimeError> {
        match *self {
            Self::Application(_, _) => {
                self.lhs_mut()?.beta_reduce()?;

                if self.is_reducible() {
                    self.apply()?;
                    self.beta_reduce()?;
                }
                else {
                    self.lhs_mut()?.beta_reduce()?;
                    self.rhs_mut()?.beta_reduce()?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn apply(&mut self) -> Result<(), RuntimeError> {
        let to_apply = std::mem::replace(self, Self::Variable(0));
        let (lhs, rhs) = to_apply.as_application()?;
        
        let (arg, mut body) = lhs.as_function()?;
        body.substitute(arg, *rhs);

        *self = *body;

        Ok(())
    }

    fn substitute(&mut self, ident: &Identifier, substitute: Expr) {
        match self {
            Self::Variable(ref var) if var == ident => *self = substitute,
            Self::Function(arg, ref mut body) if arg != ident => body.substitute(ident, substitute),
            Self::Application(ref mut left, ref mut right) => {
                left.substitute(ident, substitute.clone());
                right.substitute(ident, substitute);
            }
            _ => ()
        }
    }

    fn is_application(&self) -> bool {
        matches!(self, Self::Application(_, _))
    }

    fn rhs_mut(&mut self) -> Result<&mut Expr, RuntimeError> {
        self.as_application_mut().map(|(_, rhs)| rhs)
    }

    fn rhs(&self) -> Result<&Expr, RuntimeError> {
        self.as_application_ref().map(|(_, rhs)| rhs)
    }

    fn lhs_mut(&mut self) -> Result<&mut Expr, RuntimeError> {
        self.as_application_mut().map(|(lhs, _)| lhs)
    }

    fn lhs(&self) -> Result<&Expr, RuntimeError> {
        self.as_application_ref().map(|(lhs, _)| lhs)
    }

    fn as_application_mut(&mut self) -> Result<(&mut Expr, &mut Expr), RuntimeError> {
        if let Self::Application(ref mut lhs, ref mut rhs) = self {
            Ok((lhs, rhs))
        }
        else {
            Err(RuntimeError::NotApplication)
        }
    }

    fn as_application(&self) -> Result<(Box<Expr>, Box<Expr>), RuntimeError> {
        if let Self::Application(ref lhs, ref rhs) = self {
            Ok((lhs.clone(), rhs.clone()))
        }
        else {
            Err(RuntimeError::NotApplication)
        }
    }
    
    fn as_application_ref(&self) -> Result<(&Expr, &Expr), RuntimeError> {
        if let Self::Application(ref lhs, ref rhs) = self {
            Ok((lhs, rhs))
        }
        else {
            Err(RuntimeError::NotApplication)
        }
    }
    
    fn as_function(&self) -> Result<(&Identifier, Box<Expr>), RuntimeError> {
        if let Self::Function(ref arg, ref body) = self {
            Ok((arg, body.clone()))
        }
        else {
            Err(RuntimeError::NotFunction)
        }
    }

    fn is_reducible(&self) -> bool {
        self.lhs()
            .and_then(|t| t.as_function())
            .is_ok()
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
        let mut expr = scope.substitute(expr);
        expr.beta_reduce().expect("runtime error");
        println!("{}", expr.pretty_to_string(&scope, &registry));
    }
}
