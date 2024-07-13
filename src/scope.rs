use std::{cell::Cell, collections::{hash_map::Entry, HashMap}};

use crate::{Expr, RuntimeError};

pub type Identifier = u32;

thread_local! {
    static IDENT_COUNTER: Cell<Identifier> = Cell::new(1u32);
}

fn next_ident() -> Identifier {
    IDENT_COUNTER.with(|counter| counter.replace(counter.get() + 1))
}

#[derive(Default)]
pub struct Registry(HashMap<String, Identifier>, HashMap<Identifier, String>);

impl Registry {
    pub fn get(&mut self, ident: String) -> Identifier {
        match self.0.entry(ident.clone()) {
            Entry::Vacant(ent) => {
                let id = *ent.insert(next_ident());
                self.1.insert(id, ident);
                id

            }
            Entry::Occupied(ent) => {
                *ent.get()
            }
        }
    }

    pub fn get_name(&self, id: &Identifier) -> Option<&String> {
        self.1.get(id)
    }
}

#[derive(Clone, Debug)]
pub struct Binding(pub Expr, pub bool);

pub type Declaration = (Identifier, Binding);

#[derive(Default)]
pub struct Scope {
    bindings: HashMap<Identifier, Binding>
}

impl Scope {
    pub fn put(&mut self, ident: Identifier, binding: Binding) -> Result<(), RuntimeError> {
        self.bindings.try_insert(ident, binding)?;
        Ok(())
    }

    pub fn get(&mut self, ident: &Identifier) -> Option<Expr> {
        self.bindings.get(ident).map(|Binding(expr, _)| expr.clone())
    }

    pub fn substitute(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Variable(ref var) if let Some(subst) = self.get(var) => self.substitute(subst),
            Expr::Variable(var) => Expr::Variable(var),
            Expr::Function(arg, body) => Expr::Function(arg, Box::new(self.substitute(*body))),
            Expr::Application(left, right) => Expr::Application(Box::new(self.substitute(*left)), Box::new(self.substitute(*right)))
        }
    }

    pub fn bindings(&self) -> &HashMap<Identifier, Binding> {
        &self.bindings
    }
}


