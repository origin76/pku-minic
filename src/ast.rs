use koopa::ir::BinaryOp;

#[derive(Debug, Clone)]
pub enum GlobalItem {
    Decl(Decl),       // 全局变量/常量声明
    FuncDef(FuncDef), // 函数定义
}

#[derive(Debug, Clone)]
pub struct CompUnit {
    pub items: Vec<GlobalItem>,
}

#[derive(Debug,Clone)]
pub struct FuncDef {
    pub func_type: Type,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub block: Block,
}
#[derive(Debug, Clone)]
pub struct FuncFParam {
    pub b_type: Type, // 目前只有 int
    pub ident: String,
    // 未来可能还有数组维度
}

#[derive(Debug,Clone)]
pub enum Type {
    Int,
    Void,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl(Decl), // 声明 (const int a = 1;)
    Stmt(Stmt), // 语句 (return 0;)
}

#[derive(Debug, Clone)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub b_type: Type,
    pub defs: Vec<VarDef>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: String,
    // 变量初始化是可选的 (int a;)
    // 并且初始化值是在运行时计算的 (int a = b + 1;)，所以是 Exp
    pub init: Option<Box<Exp>>, 
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub b_type: Type,
    pub defs: Vec<ConstDef>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub ident: String,
    // 文法中 ConstInitVal ::= ConstExp，而 ConstExp 就是 Exp
    // 所以这里直接存 Exp 即可
    pub init: Box<Exp>, 
}

// === 语句 (Stmt) ===

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Option<Box<Exp>>),
    Assign(LVal, Box<Exp>),
    Exp(Option<Box<Exp>>),
    Block(Block),
    If(Box<Exp>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Exp>, Box<Stmt>), 
    Break,
    Continue,
}

// ast.rs
#[derive(Debug,Clone)]
pub enum Exp {
    // 二元运算
    BinaryExp(Box<Exp>, BinaryOp, Box<Exp>),
    // 一元运算 (直接在 Exp 层递归)
    UnaryExp(UnaryOp, Box<Exp>),
    // 基础表达式 (用于通过 Number 终止递归)
    PrimaryExp(Box<PrimaryExp>), 
    FuncCall(String, Vec<Box<Exp>>), 
}

#[derive(Debug,Clone)]
pub enum PrimaryExp {
    Parentheses(Box<Exp>),
    Number(i32),
    LVal(LVal),
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: String,
    // pub indices: Vec<Exp>, // 未来支持数组 a[1][2]
}

#[derive(Debug,Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}