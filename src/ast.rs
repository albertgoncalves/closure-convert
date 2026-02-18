use crate::op::Op;
use crate::prelude::write_delim;
use std::collections::VecDeque;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Call<'a>(pub Expr<'a>, pub Vec<Expr<'a>>);

#[derive(Clone, Debug)]
pub struct Func<'a>(pub Vec<Ident<'a>>, pub Scope<'a>);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Ident<'a> {
    User(&'a str, Option<usize>),
    Anonymous(usize),
}

#[derive(Clone, Debug)]
pub enum Expr<'a> {
    Int(i64),
    Ident(Ident<'a>),
    Array(Vec<Self>),
    Access(Box<Self>, usize),
    BinOp(Op, Box<(Self, Self)>),
    Call(Box<Call<'a>>),
    Func(Func<'a>),
}

#[derive(Clone, Debug)]
pub enum Stmt<'a> {
    Void(Call<'a>),
    Let(Ident<'a>, Expr<'a>),
    Set(Expr<'a>, Expr<'a>),
    Return(Option<Expr<'a>>),
}

#[derive(Clone, Debug)]
pub struct Scope<'a>(pub VecDeque<Stmt<'a>>);

#[macro_export]
macro_rules! expr_int {
    ($int:expr $(,)?) => {
        ast::Expr::Int($int)
    };
}

#[macro_export]
macro_rules! expr_ident {
    ($ident:expr $(,)?) => {
        ast::Expr::Ident(ast::Ident::User($ident, None))
    };
}

#[macro_export]
macro_rules! expr_call {
    ($func:expr $(,)?) => {
        ast::Expr::Call(Box::new(ast::Call($func, vec![])))
    };
    ($func:expr, [$($args:expr),* $(,)?] $(,)?) => {
        ast::Expr::Call(Box::new(ast::Call($func, vec![$($args),*])))
    };
}

#[macro_export]
macro_rules! expr_binop {
    ($op:expr, $left:expr, $right:expr $(,)?) => {
        ast::Expr::BinOp($op, Box::new(($left, $right)))
    };
}

#[macro_export]
macro_rules! expr_func {
    ([$($stmts:expr),* $(,)?] $(,)?) => {
        ast::Expr::Func(ast::Func(
            vec![],
            ast::Scope(VecDeque::from([$($stmts),*])),
        ))
    };
    ([$($idents:expr),* $(,)?], [$($stmts:expr),* $(,)?] $(,)?) => {
        ast::Expr::Func(ast::Func(
            vec![$(ast::Ident::User($idents, None),)*],
            ast::Scope(VecDeque::from([$($stmts),*])),
        ))
    };
}

#[macro_export]
macro_rules! stmt_let {
    ($target:expr, $value:expr $(,)?) => {
        ast::Stmt::Let(ast::Ident::User($target, None), $value)
    };
}

#[macro_export]
macro_rules! stmt_set {
    ($target:expr, $value:expr $(,)?) => {
        ast::Stmt::Set($target, $value)
    };
}

#[macro_export]
macro_rules! stmt_call {
    ($func:expr $(,)?) => {
        ast::Stmt::Void(ast::Call($func, vec![]))
    };
    ($func:expr, [$($args:expr),* $(,)?] $(,)?) => {
        ast::Stmt::Void(ast::Call($func, vec![$($args),*]))
    };
}

#[macro_export]
macro_rules! stmt_return {
    () => {
        ast::Stmt::Return(None)
    };
    ($expr:expr $(,)?) => {
        ast::Stmt::Return(Some($expr))
    };
}

trait Display {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result;
}

impl Display for Call<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        if matches!(&self.0, Expr::Func(..)) {
            write!(f, "(")?;
            self.0.display(f, pad)?;
            write!(f, ")(")?;
        } else {
            self.0.display(f, pad)?;
            write!(f, "(")?;
        }
        let mut exprs = self.1.iter();
        if let Some(expr) = exprs.next() {
            expr.display(f, pad)?;
            for expr in exprs {
                write!(f, ", ")?;
                expr.display(f, pad)?;
            }
        }
        write!(f, ")")
    }
}

impl Display for Func<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        write!(f, "function(")?;
        write_delim(f, &self.0, ", ")?;
        write!(f, ") ")?;
        self.1.display(f, pad)
    }
}

impl Display for Expr<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Array(exprs) => {
                write!(f, "[")?;
                let mut exprs = exprs.iter();
                if let Some(expr) = exprs.next() {
                    expr.display(f, pad)?;
                    for expr in exprs {
                        write!(f, ", ")?;
                        expr.display(f, pad)?;
                    }
                }
                write!(f, "]")
            }
            Self::Access(expr, index) => {
                expr.display(f, pad)?;
                write!(f, "[{index}]")
            }
            Self::BinOp(op, exprs) => {
                let left = &exprs.0;
                let right = &exprs.1;
                write!(f, "(")?;
                left.display(f, pad)?;
                write!(f, " {op} ")?;
                right.display(f, pad)?;
                write!(f, ")")
            }
            Self::Call(call) => call.display(f, pad),
            Self::Func(func) => func.display(f, pad),
        }
    }
}

impl Display for Stmt<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        match self {
            Self::Void(call) => {
                call.display(f, pad)?;
                write!(f, ";")
            }
            Self::Let(ident, expr) => {
                write!(f, "var {ident} = ")?;
                expr.display(f, pad)?;
                write!(f, ";")
            }
            Self::Set(target, value) => {
                target.display(f, pad)?;
                write!(f, " = ")?;
                value.display(f, pad)?;
                write!(f, ";")
            }
            Self::Return(Some(expr)) => {
                write!(f, "return ")?;
                expr.display(f, pad)?;
                write!(f, ";")
            }
            Self::Return(None) => write!(f, "return;"),
        }
    }
}

impl Display for Scope<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        writeln!(f, "{{")?;
        {
            let pad = pad + 4;
            for stmt in &self.0 {
                write!(f, "{:pad$}", "")?;
                stmt.display(f, pad)?;
                writeln!(f)?;
            }
        }
        write!(f, "{:pad$}}}", "")
    }
}

macro_rules! impl_displays {
    ($ident:ident) => {
        impl fmt::Display for $ident<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.display(f, 0)
            }
        }
    };
    ($ident:ident, $($rest:tt)*) => {
        impl_displays!($ident);
        impl_displays!($($rest)*);
    };
    () => {}
}

impl_displays!(Call, Func, Expr, Stmt, Scope);

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::User(string, Some(k)) => write!(f, "{string}${k}"),
            Self::User(string, None) => write!(f, "{string}"),
            Self::Anonymous(k) => write!(f, "__{k}__"),
        }
    }
}
