use std::{collections::HashMap, iter::Peekable, ops::Deref, path::PathBuf};

use crate::{Binding, Expr, Registry, Stmt, error::Error};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lined<T> {
    inner: T,
    line: u32
}

impl<T> Lined<T> {
    pub fn with_line(inner: T, line: u32) -> Self {
        Self {
            inner,
            line
        }
    }

    pub fn unwrap(self) -> T {
        self.inner
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> Deref for Lined<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug)]
pub struct Located<'a, T> {
    inner: T,
    path: Option<&'a PathBuf>,
    line: u32
}

impl<'a, T> Located<'a, T> {
    pub fn path(&self) -> Option<&'a PathBuf> {
        self.path
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn unwrap(self) -> T {
        self.inner
    }

    pub fn as_ref(&self) -> &T {
        &self.inner
    }
}

pub trait WithLocation: Sized {
    fn with_location<'a>(self, path: Option<&'a PathBuf>, line: u32) -> Located<'a, Self> {
        Located {
            inner: self,
            path,
            line
        }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub enum Token {
    Identifier(String),
    String(String),
    Number(u32),
    Include,
    Lambda,
    Let,
    Assert,
    Primitive,
    Assign,
    Dot,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    Unknown(char),

    #[default]
    Eof,
}

thread_local! {
    pub static KEYWORDS: HashMap<&'static str, Token> = HashMap::from([
            ("=", Token::Assign),
            ("let", Token::Let),
            ("include", Token::Include),
            ("primitive", Token::Primitive),
            ("assert", Token::Assert),
        ]);
}

impl Token {
    fn next(input: &mut Peekable<impl Iterator<Item = char>>, line: &mut u32) -> Self {
        fn char_is_ident(ch: char) -> bool {
            const FORBIDDEN: &str = "(){}[].;λ\\";
            !ch.is_control() && !ch.is_whitespace() && !FORBIDDEN.contains(ch)
        }

        while let Some(&ch) = input.peek() && ch.is_whitespace() {
            if ch == '\n' {
                *line += 1;
            }
            input.next();
        }

        match input.next().unwrap_or('\0') {
            'λ' | '\\' => Self::Lambda,
            '.' => Self::Dot,
            ',' => Self::Comma,
            ';' => Self::Semicolon,
            '(' => Self::LeftParen,
            ')' => Self::RightParen,
            '\0' => Self::Eof, 
            '\'' => Self::String(input.take_while(|ch| *ch != '\'').collect()),
            '\"' => Self::String(input.take_while(|ch| *ch != '\"').collect()),
            ch if ch.is_numeric() => {
                let mut number = String::new();
                number.push(ch);

                while let Some(&next) = input.peek() && next.is_numeric() {
                    number.push(next);
                    input.next();
                }

                Self::Number(number.parse().unwrap())
            }
            ch if char_is_ident(ch) => {
                let mut ident = String::new();
                ident.push(ch);

                while let Some(&next) = input.peek() && char_is_ident(next) {
                    ident.push(next);
                    input.next();
                }

                KEYWORDS.with(|map| map.get(ident.as_str()).cloned())
                        .unwrap_or_else(|| Self::Identifier(ident))
            }
            ch => Self::Unknown(ch)
        }
    }

    pub fn lex(input: impl Iterator<Item = char>) -> impl Iterator<Item = Lined<Token>> {
        let mut line = 1;
        let mut input = input.peekable();
        std::iter::from_fn(move || Some(Lined::with_line(Token::next(&mut input, &mut line), line)))
            .take_while(|tok| **tok != Self::Eof)
    }

    pub fn starts_expr(&self) -> bool {
        matches!(self, Self::Lambda | Self::Identifier(_) | Self::LeftParen | Self::Number(_))
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assert => write!(f, "assert"),
            Self::Assign => write!(f, "="),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Eof => write!(f, "<end of file>"),
            Self::Identifier(ident) => write!(f, "{ident}"),
            Self::Include => write!(f, "include"),
            Self::Lambda => write!(f, "λ"),
            Self::LeftParen => write!(f, "("),
            Self::Let => write!(f, "let"),
            Self::Number(num) => write!(f, "{num}"),
            Self::Primitive => write!(f, "primitive"),
            Self::RightParen => write!(f, ")"),
            Self::Semicolon => write!(f, ";"),
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Unknown(ch) => write!(f, "<unknown `{ch}`>"),
        }
    }
}

pub struct Parser<'a> {
    tokens: Vec<Lined<Token>>,
    origin_path: Option<&'a std::path::PathBuf>,
    last_line: u32
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Lined<Token>>, origin_path: Option<&'a std::path::PathBuf>) -> Self {
        Self {
            tokens,
            origin_path,
            last_line: 1
        }
    }
    
    pub fn parse(&mut self, registry: &mut Registry) -> Result<Vec<Located<'a, Stmt>>, Located<'a, Error>> {
        let mut stmts = vec![];
        while self.peek().is_some() {
            stmts.push(self.parse_stmt(registry)?)
        }

        Ok(stmts) 
    }

    fn parse_stmt(&mut self, registry: &mut Registry) -> Result<Located<'a, Stmt>, Located<'a, Error>> {
        let next = self.peek().unwrap();
        let line = next.line();
        let stmt = match next.as_ref() {
            Token::Let => self.parse_let_binding(registry)?,
            Token::Include => self.parse_include()?,
            Token::Assert => self.parse_assertion(registry)?,
            _ => {
                let tok = self.next().unwrap();
                let line = tok.line();
                return Err(
                    Error::UnexpectedToken(tok.unwrap(), "statement".to_string())
                        .with_location(self.origin_path, line)
                )
            }
        };

        self.expect(Token::Semicolon)?;
        Ok(stmt.with_location(self.origin_path, line))
    }
     
    fn peek(&mut self) -> Option<&Lined<Token>> {
        self.tokens.first()
    }

    fn next(&mut self) -> Option<Lined<Token>> {
        if self.tokens.len() > 0 {
            let tok = self.tokens.remove(0);
            self.last_line = tok.line();
            Some(tok)
        }
        else {
            None
        }
    }
    
    fn expect_ident(&mut self) -> Result<String, Located<'a, Error>> {
        self.next().map(|tok| {
            let line = tok.line();
            match tok.unwrap() {
                Token::Identifier(ident) => Ok(ident),
                tok => Err(Error::UnexpectedToken(tok, "identifier".to_string()).with_location(self.origin_path, line))
            }
        }).ok_or(Error::UnexpectedEof.with_location(self.origin_path, self.last_line))
            .flatten()
    }
    
    fn expect_string(&mut self) -> Result<String, Located<'a, Error>> {
        self.next().map(|tok| {
            let line = tok.line();
            match tok.unwrap() {
                Token::String(string) => Ok(string),
                tok => Err(Error::UnexpectedToken(tok, "string".to_string()).with_location(self.origin_path, line))
            }
        }).ok_or(Error::UnexpectedEof.with_location(self.origin_path, self.last_line))
            .flatten()
    }

    fn expect(&mut self, expect: Token) -> Result<Lined<Token>, Located<'a, Error>> {
        self.next()
            .map(|next| (next.as_ref() == &expect)
                .then_some(())
                .ok_or_else(|| {
                    let line = next.line();
                    Error::UnexpectedToken(next.as_ref().clone(), format!("token `{expect:?}`"))
                        .with_location(self.origin_path, line)
                })
                .map(|_| next)
            )
            .ok_or(Error::UnexpectedEof.with_location(self.origin_path, self.last_line))
            .flatten()
    }

    fn parse_let_binding(&mut self, registry: &mut Registry) -> Result<Stmt, Located<'a, Error>> {
        self.expect(Token::Let)?;

        let primitive = self.peek().map(Lined::as_ref) == Some(&Token::LeftParen);
        if primitive {
            self.next();
            self.expect(Token::Primitive)?;
            self.expect(Token::RightParen)?;
        }

        let ident = self.expect_ident()?;
        self.expect(Token::Assign)?;
        let expr = self.parse_expr(registry)?;

        Ok(Stmt::LetBinding(registry.get(ident), Binding(expr, primitive)))
    }

    fn parse_basic_expr(&mut self, registry: &mut Registry) -> Result<Expr, Located<'a, Error>> {
        let tok = self.next().unwrap_or_default();
        let line = tok.line();
        match tok.unwrap() {
            Token::Identifier(ident) => Ok(Expr::Variable(registry.get(ident))),
            Token::Number(number) => Ok(Expr::church_numeral(number, registry)),
            Token::Lambda => {
                let argument = self.expect_ident()?;
                self.expect(Token::Dot)?;
                let expr = self.parse_expr(registry)?;
                Ok(Expr::Function(registry.get(argument), Box::new(expr)))
            }
            Token::LeftParen => {
                let expr = self.parse_expr(registry)?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }
            Token::Eof => Err(Error::UnexpectedEof.with_location(self.origin_path, line)),
            tok => Err(Error::UnexpectedToken(tok, "expression".to_string()).with_location(self.origin_path, line))
        }
    }

    fn parse_expr(&mut self, registry: &mut Registry) -> Result<Expr, Located<'a, Error>> {
        let mut expr = self.parse_basic_expr(registry)?;

        while let Some(next) = self.peek() && next.starts_expr() {
            expr = Expr::Application(
                Box::new(expr),
                Box::new(self.parse_basic_expr(registry)?),
            )
        }

        Ok(expr)
    }

    fn parse_include(&mut self) -> Result<Stmt, Located<'a, Error>> {
        let line = self.expect(Token::Include)?.line();
        let include_path = self.expect_string()?;

        let mut path = self.origin_path.map(|p| p.parent())
            .flatten()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::new());
        
        path.push(include_path);
        if let Err(err) = std::fs::canonicalize(&mut path) {
            return Err(Error::IncludedFile(err).with_location(self.origin_path, line));
        }

        Ok(Stmt::Include(path))
    }

    fn parse_assertion(&mut self, registry: &mut Registry) -> Result<Stmt, Located<'a, Error>> {
        self.expect(Token::Assert)?;

        let expr = self.parse_expr(registry)?;

        let exp;
        if let Some(next) = self.peek() && next.as_ref() == &Token::Comma {
            self.next();
            exp = Some(self.expect_string()?);
        }
        else {
            exp = None;
        }

        Ok(Stmt::Assertion(expr, exp))
    }
}

