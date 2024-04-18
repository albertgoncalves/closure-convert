use std::fmt;

#[derive(Clone, Debug)]
pub enum Op {
    Add,
    LessEqual,
}

#[macro_export]
macro_rules! op {
    (+) => {
        op::Op::Add
    };
    (<=) => {
        op::Op::LessEqual
    };
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::LessEqual => write!(f, "<="),
        }
    }
}
