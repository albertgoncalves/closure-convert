mod ast;
mod closure_convert;
mod number_idents;
mod op;
mod prelude;

use closure_convert::ClosureConvert;
use number_idents::NumberIdents;
use std::collections::{HashSet, VecDeque};

fn main() {
    let mut expr = stmt_call!(expr_func!([
        stmt_let!(
            "counter",
            expr_func!(
                ["x"],
                [stmt_return!(expr_func!([
                    stmt_set!(
                        expr_ident!("x"),
                        expr_binop!(op!(+), expr_ident!("x"), expr_int!(1)),
                    ),
                    stmt_return!(expr_ident!("x")),
                ]))],
            ),
        ),
        stmt_let!("x", expr_int!(1)),
        stmt_let!("c", expr_call!(expr_ident!("counter"), [expr_ident!("x")])),
        stmt_call!(expr_ident!("c")),
        stmt_call!(expr_ident!("c")),
        stmt_call!(expr_ident!("console.log"), [expr_call!(expr_ident!("c"))]),
        stmt_call!(expr_ident!("console.log"), [expr_ident!("x")]),
    ]));
    println!("{expr}\n");

    let mut state = number_idents::State::default();
    expr.number_idents(&mut state);
    println!("{expr}\n");

    let mut state = closure_convert::State::new(HashSet::from(["console.log"]));
    expr.closure_convert(&mut state);
    for func in state.funcs.drain(..) {
        println!("{}", ast::Stmt::from(func));
    }
    for binding in state.bindings.drain(..) {
        println!("{}", ast::Stmt::from(binding));
    }
    println!("{expr}\n");

    assert!(state.scope.locals.is_empty());
    assert!(state.scope.orphans.is_empty());
    assert!(state.scope.captures.is_empty());
}
