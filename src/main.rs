#![feature(result_flattening)]
#![feature(map_try_insert)]
#![feature(let_chains)]
#![feature(if_let_guard)]

use std::{collections::HashMap, fmt, fs::File, io::Read, panic::RefUnwindSafe};

use parser::*;

mod parser;

#[derive(Clone, Debug)]
struct Binding(Expr, bool);

type Declaration<'a> = (String, Binding);

struct Scope {
    bindings: HashMap<String, Binding>
}

impl Scope {
    fn from_pairs<const N: usize>(pairs: [Declaration<'_>; N]) -> Self {
        Self {
            bindings: HashMap::from(pairs)
        }
    }

    fn get(&mut self, ident: &String) -> Option<Expr> {
        if let Some(binding) = self.bindings.get(ident).cloned() {
            return Some(binding.0)
        }

        ident.parse().ok().map(|number| self.construct_number(number))
    }

    fn construct_number(&mut self, number: u32) -> Expr {
        let mut expr = Expr::Variable("x".to_string());
        
        for _ in 0..number {
            expr = Expr::Application(Box::new(Expr::Variable("f".to_string())), Box::new(expr));
        }

        expr = Expr::Function("x".to_string(), Box::new(expr));
        expr = Expr::Function("f".to_string(), Box::new(expr));

        self.bindings.insert(number.to_string(), Binding(expr.clone(), true));

        expr
    }

    fn substitute(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Variable(ref var) if let Some(subst) = self.get(var) => self.substitute(subst),
            Expr::Variable(var) => Expr::Variable(var),
            Expr::Function(arg, body) => Expr::Function(arg, Box::new(self.substitute(*body))),
            Expr::Application(left, right) => Expr::Application(Box::new(self.substitute(*left)), Box::new(self.substitute(*right)))
        }
    }
}

#[derive(Debug)]
enum RuntimeError {
    UnboundIdentifier(String),
    IllegalApplication(Expr, Expr),
    NotApplication,
    NotFunction
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundIdentifier(ref binding) => write!(f, "unbound identifier `{binding}`"),
            Self::IllegalApplication(a, b) => write!(f, "illegal application: `{a:?} {b:?}`"),
            Self::NotApplication => write!(f, "not an application"),
            Self::NotFunction => write!(f, "not a function"),
        }
    }
}

enum Order {
    Normal,
    CallByName
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Application(Box<Expr>, Box<Expr>),
    Function(String, Box<Expr>),
    Variable(String),
}

impl Expr {
    /*fn beta_reduce(self) -> Result<Self, RuntimeError> {
        match self {
            Self::Application(left, right) => {
                let left = left.beta_reduce()?;
                let right = right.beta_reduce()?;
                
                match left {
                    Self::Function(ref arg, body) => body.substitute(arg, right).beta_reduce(),
                    _ => Err(RuntimeError::IllegalApplication(left, right))
                } 
            }
            Self::Variable(ident) => Err(RuntimeError::UnboundIdentifier(ident)),
            _ => Ok(self)
        }
    }*/

    fn beta_reduce(&mut self, order: Order, limit: usize) -> Result<usize, RuntimeError> {
        let mut count = 0;

        match order {
            Order::Normal => self.beta_normal(limit, &mut count)?,
            Order::CallByName => self.beta_cbn(limit, &mut count)?
        }

        Ok(count)
    }

    fn beta_normal(&mut self, limit: usize, count: &mut usize) -> Result<(), RuntimeError> {
        if limit != 0 && *count == limit {
            return Ok(())
        }

        match *self {
            Self::Application(_, _) => {
                self.lhs_mut()?.beta_cbn(limit, count)?;

                if self.is_reducible(limit, *count) {
                    self.apply()?;
                    self.beta_normal(limit, count)?;
                }
                else {
                    self.lhs_mut()?.beta_normal(limit, count)?;
                    self.rhs_mut()?.beta_normal(limit, count)?;
                }
            }
            _ => (),
        }

        Ok(())
    }
    
    fn beta_cbn(&mut self, limit: usize, count: &mut usize) -> Result<(), RuntimeError> {
        if limit != 0 && *count == limit {
            return Ok(())
        }

        if let Self::Application(_, _) = self {
            self.lhs_mut()?.beta_cbn(limit, count)?;
            
            if self.is_reducible(limit, *count) {
                self.apply()?;
                self.beta_cbn(limit, count)?;
            }
        }

        Ok(()) 
    }

    fn apply(&mut self) -> Result<(), RuntimeError> {
        let to_apply = std::mem::replace(self, Self::Variable("".to_string()));
        let (lhs, rhs) = to_apply.unapp()?;
        
        let (arg, mut body) = lhs.unabs()?;
        body.substitute(arg, *rhs);

        *self = *body;

        Ok(())
    }

    fn substitute(&mut self, ident: &String, substitute: Expr) {
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

    fn pretty_to_string(&self, scope: &Scope) -> String {
        if let Some((id, _)) = scope.bindings.iter().filter(|(_, b)| b.1).find(|(_, b)| &b.0 == self) {
            id.clone()
        }
        else {
            self.to_string()
        }
    }

    fn rhs_mut(&mut self) -> Result<&mut Expr, RuntimeError> {
        self.unapp_mut().map(|(_, rhs)| rhs)
    }

    fn rhs(&self) -> Result<&Expr, RuntimeError> {
        self.unapp_ref().map(|(_, rhs)| rhs)
    }

    fn lhs_mut(&mut self) -> Result<&mut Expr, RuntimeError> {
        self.unapp_mut().map(|(lhs, _)| lhs)
    }

    fn lhs(&self) -> Result<&Expr, RuntimeError> {
        self.unapp_ref().map(|(lhs, _)| lhs)
    }

    fn unapp_mut(&mut self) -> Result<(&mut Expr, &mut Expr), RuntimeError> {
        if let Self::Application(ref mut lhs, ref mut rhs) = self {
            Ok((lhs, rhs))
        }
        else {
            Err(RuntimeError::NotApplication)
        }
    }

    fn unapp(&self) -> Result<(Box<Expr>, Box<Expr>), RuntimeError> {
        if let Self::Application(ref lhs, ref rhs) = self {
            Ok((lhs.clone(), rhs.clone()))
        }
        else {
            Err(RuntimeError::NotApplication)
        }
    }
    
    fn unapp_ref(&self) -> Result<(&Expr, &Expr), RuntimeError> {
        if let Self::Application(ref lhs, ref rhs) = self {
            Ok((lhs, rhs))
        }
        else {
            Err(RuntimeError::NotApplication)
        }
    }
    
    fn unabs_mut(&mut self) -> Result<(&mut String, &mut Expr), RuntimeError> {
        if let Self::Function(ref mut arg, ref mut body) = self {
            Ok((arg, body))
        }
        else {
            Err(RuntimeError::NotFunction)
        }
    }


    fn unabs(&self) -> Result<(&String, Box<Expr>), RuntimeError> {
        if let Self::Function(ref arg, ref body) = self {
            Ok((arg, body.clone()))
        }
        else {
            Err(RuntimeError::NotFunction)
        }
    }

    fn is_reducible(&self, limit: usize, count: usize) -> bool {
        self.lhs()
            .and_then(|t| t.unabs())
            .is_ok() && (limit == 0 || count < limit)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self {
            Self::Variable(ident) => write!(f, "{ident}"),
            Self::Function(arg, body) => write!(f, "λ{arg}.{body}"),
            Self::Application(left, right) => if right.is_application() {
                write!(f, "{left} ({right})")
            }
            else {
                write!(f, "{left} {right}")
            }
        }
    }
}

fn main() {
    let mut args = std::env::args();
    args.next();

    let mut scope = Scope::from_pairs([]);

    let mut exprs = Vec::new();
    for arg in args.into_iter() {
        let mut file = File::open(arg).expect("file error");
        let mut source = String::new();
        file.read_to_string(&mut source).expect("file error");

        let tokens: &mut dyn Iterator<Item = Token> = &mut Token::lex(source.chars());
        exprs.extend(Parser::from(tokens).parse(&mut scope.bindings)
            .expect("parsing error"));
        
        println!("{:?}", exprs);
    }

    for expr in exprs {
        let mut expr = scope.substitute(expr);
        expr.beta_reduce(Order::CallByName, 0).expect("runtime error");
        println!("{}", expr.pretty_to_string(&scope));
    }
}
