use crate::ast;
use std::collections::HashMap;

#[derive(Default)]
pub struct State<'a>(pub HashMap<&'a str, usize>);

pub trait NumberIdents<'a> {
    fn number_idents(&mut self, state: &mut State<'a>);
}

impl<'a> NumberIdents<'a> for &'a str {
    fn number_idents(&mut self, state: &mut State<'a>) {
        let k = state.0.get(self).map_or(0, |k| k + 1);
        state.0.insert(self, k);
    }
}

impl<'a> NumberIdents<'a> for ast::Ident<'a> {
    fn number_idents(&mut self, state: &mut State<'a>) {
        match self {
            Self::User(user, None) => {
                if let Some(k) = state.0.get(user) {
                    *self = Self::User(user, Some(*k));
                }
            }
            Self::User(_, Some(_)) | Self::Anonymous(..) => unreachable!(),
        }
    }
}

impl<'a> NumberIdents<'a> for ast::Call<'a> {
    fn number_idents(&mut self, state: &mut State<'a>) {
        self.0.number_idents(state);
        for expr in &mut self.1 {
            expr.number_idents(state);
        }
    }
}

impl<'a> NumberIdents<'a> for ast::Func<'a> {
    fn number_idents(&mut self, state: &mut State<'a>) {
        for ident in &mut self.0 {
            if let ast::Ident::User(user, None) = ident {
                user.number_idents(state);
            }
            ident.number_idents(state);
        }
        self.1.number_idents(state);
    }
}

impl<'a> NumberIdents<'a> for ast::Expr<'a> {
    fn number_idents(&mut self, state: &mut State<'a>) {
        match self {
            Self::Int(..) => (),
            Self::Ident(ident) => ident.number_idents(state),
            Self::Array(exprs) => {
                for expr in exprs {
                    expr.number_idents(state);
                }
            }
            Self::Access(expr, _) => expr.number_idents(state),
            Self::BinOp(_, exprs) => {
                exprs.0.number_idents(state);
                exprs.1.number_idents(state);
            }
            Self::Call(call) => call.number_idents(state),
            Self::Func(func) => func.number_idents(state),
        }
    }
}

impl<'a> NumberIdents<'a> for ast::Stmt<'a> {
    fn number_idents(&mut self, state: &mut State<'a>) {
        match self {
            Self::Void(call) => call.number_idents(state),
            Self::Let(ident, expr) => {
                expr.number_idents(state);
                if let ast::Ident::User(user, None) = ident {
                    user.number_idents(state);
                }
                ident.number_idents(state);
            }
            Self::Set(target, value) => {
                value.number_idents(state);
                target.number_idents(state);
            }
            Self::Return(Some(expr)) => expr.number_idents(state),
            Self::Return(None) => (),
        }
    }
}

impl<'a> NumberIdents<'a> for ast::Scope<'a> {
    fn number_idents(&mut self, state: &mut State<'a>) {
        for stmt in &mut self.0 {
            stmt.number_idents(state);
        }
    }
}
