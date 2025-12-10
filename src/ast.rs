use koopa::ir::BinaryOp;

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

// ast.rs
#[derive(Debug,Clone)]
pub enum Exp {
    // 二元运算
    BinaryExp(Box<Exp>, BinaryOp, Box<Exp>),
    // 一元运算 (直接在 Exp 层递归)
    UnaryExp(UnaryOp, Box<Exp>),
    // 【必须】基础表达式 (用于通过 Number 终止递归)
    PrimaryExp(Box<PrimaryExp>), 
}

#[derive(Debug,Clone)]
pub enum PrimaryExp {
    Parentheses(Box<Exp>),
    Number(i32),
}
#[derive(Debug,Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}