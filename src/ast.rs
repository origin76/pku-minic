#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp {
    UnaryExp(UnaryExp),
}

#[derive(Debug)]
pub enum PrimaryExp {
    Parentheses(Box<Exp>),
    Number(i32),
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

pub trait Evaluate {
    fn evaluate(&self) -> i32;
}

impl Evaluate for Exp {
    fn evaluate(&self) -> i32 {
        match self {
            Exp::UnaryExp(unary_exp) => unary_exp.evaluate(),
        }
    }
}

impl Evaluate for UnaryExp {
    fn evaluate(&self) -> i32 {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.evaluate(),
            UnaryExp::UnaryOp(op, operand) => {
                let operand_value = operand.evaluate();
                match op {
                    UnaryOp::Plus => operand_value,
                    UnaryOp::Minus => -operand_value,
                    UnaryOp::Not => !operand_value,
                }
            }
        }
    }
}

impl Evaluate for PrimaryExp {
    fn evaluate(&self) -> i32 {
        match self {
            PrimaryExp::Number(value) => *value,
            PrimaryExp::Parentheses(exp) => exp.evaluate(),
        }
    }
}
