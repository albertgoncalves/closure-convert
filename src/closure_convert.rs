use crate::ast;
use std::collections::{HashSet, VecDeque};

#[derive(Debug, Default)]
pub struct Scope<'a> {
    pub locals: HashSet<ast::Ident<'a>>,
    pub orphans: HashSet<ast::Ident<'a>>,
    pub captures: HashSet<ast::Ident<'a>>,
}

#[derive(Debug)]
pub struct State<'a> {
    k: usize,
    globals: HashSet<&'a str>,
    pub funcs: Vec<(usize, ast::Expr<'a>)>,
    pub bindings: Vec<(usize, ast::Expr<'a>)>,
    pub scope: Scope<'a>,
}

impl<'a> From<(usize, ast::Expr<'a>)> for ast::Stmt<'a> {
    fn from((k, expr): (usize, ast::Expr<'a>)) -> Self {
        ast::Stmt::Let(ast::Ident::Anonymous(k), expr)
    }
}

impl<'a> State<'a> {
    pub fn new(globals: HashSet<&'a str>) -> Self {
        Self {
            k: 0,
            globals,
            funcs: vec![],
            bindings: vec![],
            scope: Scope::default(),
        }
    }

    fn get_k(&mut self) -> usize {
        let k = self.k;
        self.k += 1;
        k
    }
}

pub trait ClosureConvert<'a> {
    fn closure_convert(&mut self, state: &mut State<'a>);
}

impl<'a> ClosureConvert<'a> for ast::Call<'a> {
    fn closure_convert(&mut self, state: &mut State<'a>) {
        let mut anonymous = false;
        let mut global = false;
        match &self.0 {
            ast::Expr::Func(..) => anonymous = true,
            ast::Expr::Ident(ast::Ident::User(user, k)) if state.globals.contains(user) => {
                assert!(k.is_none());
                global = true;
            }
            _ => (),
        };

        self.0.closure_convert(state);
        for expr in &mut self.1 {
            expr.closure_convert(state);
        }

        if global {
            return;
        }

        if anonymous {
            let k = state.get_k();
            let ident = ast::Ident::Anonymous(k);
            let func = std::mem::replace(&mut self.0, ast::Expr::Ident(ident));
            state.bindings.push((k, func));
        }
        self.1.push(ast::Expr::Access(Box::new(self.0.clone()), 1));
        self.0 = ast::Expr::Access(Box::new(self.0.clone()), 0);
    }
}

impl<'a> ClosureConvert<'a> for ast::Func<'a> {
    fn closure_convert(&mut self, state: &mut State<'a>) {
        for ident in &self.0 {
            state.scope.locals.insert(ident.clone());
        }
        self.1.closure_convert(state);
    }
}

impl<'a> ClosureConvert<'a> for ast::Expr<'a> {
    fn closure_convert(&mut self, state: &mut State<'a>) {
        match self {
            Self::Int(..) => (),
            Self::Ident(ast::Ident::User(user, k)) if state.globals.contains(user) => {
                assert!(k.is_none());
            }
            Self::Ident(ident) if state.scope.locals.contains(ident) => (),
            Self::Ident(ident) => {
                state.scope.orphans.insert(ident.clone());
                *self = ast::Expr::Access(Box::new(self.clone()), 0);
            }
            Self::Array(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.closure_convert(state);
                }
            }
            Self::Access(expr, _) => expr.closure_convert(state),
            Self::BinOp(_, exprs) => {
                exprs.0.closure_convert(state);
                exprs.1.closure_convert(state);
            }
            Self::Call(call) => call.closure_convert(state),
            Self::Func(func) => {
                let mut parent = std::mem::take(&mut state.scope);
                func.closure_convert(state);

                func.0.push(ast::Ident::User("__env__", None));
                let mut escapes = Vec::with_capacity(state.scope.orphans.len());
                for (i, orphan) in state.scope.orphans.drain().enumerate() {
                    // NOTE: If we find any orphans in this function's scope, we'll need to capture
                    // them from the parent environment.
                    func.1 .0.push_front(ast::Stmt::Let(
                        orphan.clone(),
                        ast::Expr::Access(
                            Box::new(ast::Expr::Ident(ast::Ident::User("__env__", None))),
                            i,
                        ),
                    ));

                    // NOTE: All orphans will need to escape their current scope.
                    escapes.push(ast::Expr::Ident(orphan.clone()));

                    // NOTE: If the orphan was defined in the parent scope, we'll need to capture it.
                    if parent.locals.contains(&orphan) {
                        parent.captures.insert(orphan.clone());
                        continue;
                    }

                    // NOTE: Bubble up any remaining orphans to the parent scope.
                    parent.orphans.insert(orphan);
                }

                // NOTE: All captured variables will need to be allocated on the heap.
                for ident in &func.0 {
                    if state.scope.captures.remove(ident) {
                        func.1 .0.push_front(ast::Stmt::Let(
                            ident.clone(),
                            ast::Expr::Array(vec![ast::Expr::Ident(ident.clone())]),
                        ));
                    }
                }
                for stmt in &mut func.1 .0 {
                    if let ast::Stmt::Let(ident, value) = stmt {
                        if state.scope.captures.remove(ident) {
                            *value = ast::Expr::Array(vec![value.clone()]);
                        }
                    }
                }
                assert!(state.scope.captures.is_empty());

                let _ = std::mem::replace(&mut state.scope, parent);

                let k = state.get_k();
                let ident = ast::Ident::Anonymous(k);
                let expr = std::mem::replace(self, ast::Expr::Ident(ident.clone()));
                state.funcs.push((k, expr.clone()));
                *self = ast::Expr::Array(vec![ast::Expr::Ident(ident), ast::Expr::Array(escapes)]);
            }
        }
    }
}

impl<'a> ClosureConvert<'a> for ast::Stmt<'a> {
    fn closure_convert(&mut self, state: &mut State<'a>) {
        match self {
            Self::Void(call) => call.closure_convert(state),
            Self::Let(ident, expr) => {
                expr.closure_convert(state);
                state.scope.locals.insert(ident.clone());
            }
            Self::Set(target, value) => {
                value.closure_convert(state);
                target.closure_convert(state);
            }
            Self::Return(Some(expr)) => {
                expr.closure_convert(state);
            }
            Self::Return(None) => (),
        }
    }
}

impl<'a> ClosureConvert<'a> for ast::Scope<'a> {
    fn closure_convert(&mut self, state: &mut State<'a>) {
        let mut stmts = VecDeque::with_capacity(self.0.len());
        for mut stmt in self.0.drain(..) {
            stmt.closure_convert(state);
            for binding in state.bindings.drain(..) {
                stmts.push_back(ast::Stmt::from(binding));
            }
            stmts.push_back(stmt);
        }
        assert!(std::mem::replace(&mut self.0, stmts).is_empty());
    }
}
