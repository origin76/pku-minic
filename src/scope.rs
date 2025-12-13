use std::collections::HashMap;
use koopa::ir::Value;

#[derive(Debug, Clone)]
pub enum Symbol {
    // 对于 const int a = 10; 我们直接记录它的整数值
    Const(i32),
    
    // 对于 int a = 10; 我们记录它在 Koopa IR 中的内存地址 (Alloc 指令的返回值)
    Var(Value), 
}

pub struct SymbolTable {
    // 作用域栈：每个元素是一个作用域的符号映射
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            // 初始化时推入一个全局作用域 (虽然 SysY 暂时只用 main，但保持习惯)
            scopes: vec![HashMap::new()], 
        }
    }

    // 进入新作用域 (遇到 { 时)
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // 退出当前作用域 (遇到 } 时)
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    // 声明常量
    pub fn insert_const(&mut self, name: String, val: i32) {
        // 获取当前作用域 (栈顶)
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, Symbol::Const(val));
        }
    }

    pub fn insert_var(&mut self, name: String, ptr: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, Symbol::Var(ptr));
        }
    }

    // 查找符号 (从内向外)
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // 反向遍历 (从栈顶到栈底)
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}