use std::{collections::{hash_map, HashMap}, fs::File, io::Read, iter::Peekable, path::PathBuf};

use crate::{Binding, Declaration, Expr};

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub enum Token {
    Identifier(String),
    String(String),
    Include,
    Lambda,
    Let,
    Primitive,
    Assign,
    Dot,
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
        ]);
}

impl Token {
    fn next(input: &mut Peekable<impl Iterator<Item = char>>) -> Self {
        fn char_is_ident(ch: char) -> bool {
            const FORBIDDEN: &str = "(){}[].;λ\\";
            !ch.is_control() && !ch.is_whitespace() && !FORBIDDEN.contains(ch)
        }

        while let Some(&ch) = input.peek() && ch.is_whitespace() {
            input.next();
        }

        match input.next().unwrap_or('\0') {
            'λ' | '\\' => Self::Lambda,
            '.' => Self::Dot,
            ';' => Self::Semicolon,
            '(' => Self::LeftParen,
            ')' => Self::RightParen,
            '\0' => Self::Eof, 
            '\'' => Self::String(input.take_while(|ch| *ch != '\'').collect()),
            '\"' => Self::String(input.take_while(|ch| *ch != '\"').collect()),
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

    pub fn lex(input: impl Iterator<Item = char>) -> impl Iterator<Item = Token> {
        let mut input = input.peekable();
        std::iter::from_fn(move || Some(Token::next(&mut input)))
            .take_while(|tok| *tok != Self::Eof)
    }

    pub fn starts_expr(&self) -> bool {
        matches!(self, Self::Lambda | Self::Identifier(_) | Self::LeftParen)
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token, String),
    UnexpectedEof,
    Redefinition(String),
    IncludedFile(std::io::Error)
}

impl From<std::io::Error> for ParseError {
    fn from(value: std::io::Error) -> Self {
        Self::IncludedFile(value)
    }
}

impl From<hash_map::OccupiedError<'_, String, Binding>> for ParseError {
    fn from(value: hash_map::OccupiedError<'_, String, Binding>) -> Self {
        Self::Redefinition(value.to_string())
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    origin_path: Option<std::path::PathBuf>,
}

impl Parser {
    pub fn set_path(&mut self, path: std::path::PathBuf) {
        self.origin_path = Some(path)
    }
    
    pub fn parse(&mut self, declarations: &mut HashMap<String, Binding>) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = vec![];
        while let Some(token) = self.peek() {
            match token {
                Token::Let => {
                    let (ident, expr) = self.parse_let_binding()?;
                    declarations.try_insert(ident.to_string(), expr)?;
                }
                Token::Include => self.expand_include()?,
                _ => exprs.push(self.parse_expr()?)
            }
        }


        Ok(exprs) 
    }
    
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.next().map(|tok| {
            if let Token::Identifier(ident) = tok {
                Ok(ident)
            }
            else {
                Err(ParseError::UnexpectedToken(tok, "identifier".to_string()))
            }
        }).ok_or(ParseError::UnexpectedEof)
            .flatten()
    }
    
    fn expect_string(&mut self) -> Result<String, ParseError> {
        self.next().map(|tok| {
            if let Token::String(string) = tok {
                Ok(string)
            }
            else {
                Err(ParseError::UnexpectedToken(tok, "string".to_string()))
            }
        }).ok_or(ParseError::UnexpectedEof)
            .flatten()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.first()
    }

    fn next(&mut self) -> Option<Token> {
        if self.tokens.len() > 0 {
            Some(self.tokens.remove(0))
        }
        else {
            None
        }
    }

    fn expect(&mut self, expect: Token) -> Result<(), ParseError> {
        self.next()
            .map(|next| (next == expect)
                .then_some(())
                .ok_or_else(|| ParseError::UnexpectedToken(next, format!("token `{expect:?}`")))
            )
            .ok_or(ParseError::UnexpectedEof)
            .flatten()
    }

    fn parse_let_binding(&mut self) -> Result<Declaration, ParseError> {
        self.expect(Token::Let)?;

        let primitive = self.peek() == Some(&Token::LeftParen);
        if primitive {
            self.next();
            self.expect(Token::Primitive)?;
            self.expect(Token::RightParen)?;
        }

        let ident = self.expect_ident()?;
        self.expect(Token::Assign)?;
        let expr = self.parse_expr()?;
        self.expect(Token::Semicolon)?;

        Ok((ident, Binding(expr, primitive)))
    }

    fn parse_basic_expr(&mut self) -> Result<Expr, ParseError> {
        match self.next().unwrap_or_default() {
            Token::Identifier(ident) => Ok(Expr::Variable(ident)),
            Token::Lambda => {
                let argument = self.expect_ident()?;
                self.expect(Token::Dot)?;
                let expr = self.parse_expr()?;
                Ok(Expr::Function(argument, Box::new(expr)))
            }
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }
            Token::Eof => Err(ParseError::UnexpectedEof),
            token => Err(ParseError::UnexpectedToken(token, "expression".to_string()))
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_basic_expr()?;

        while let Some(next) = self.peek() && next.starts_expr() {
            expr = Expr::Application(
                Box::new(expr),
                Box::new(self.parse_basic_expr()?),
            )
        }

        Ok(expr)
    }

    fn expand_include(&mut self) -> Result<(), ParseError> {
        self.expect(Token::Include)?;
        let include_path = self.expect_string()?;

        let mut path = self.origin_path.to_owned().unwrap_or_else(|| PathBuf::new());
        path.push(include_path);

        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;

        let tokens = std::mem::replace(&mut self.tokens, vec![]);
        let mut new = Token::lex(source.chars()).collect::<Vec<_>>();
        new.extend(tokens);
        self.tokens = new;
        
        Ok(())
    }
}

impl From<&mut dyn Iterator<Item = Token>> for Parser {
    fn from(tokens: &mut dyn Iterator<Item = Token>) -> Self {
        Self {
            tokens: tokens.collect(),
            origin_path: None,
        }
    }
}
