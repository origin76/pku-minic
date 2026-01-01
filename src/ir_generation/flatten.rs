use koopa::ir::{Value, builder::{LocalInstBuilder, ValueBuilder}};

use crate::{ConstInitVal, Exp, FunctionGenerator, InitVal, SymbolTable, evaluate_const_exp};

// 对外接口
pub(crate) fn flatten_init_val(init: &InitVal) -> Vec<Exp> {
    let mut values = Vec::new();
    match init {
        InitVal::Exp(exp) => {
            values.push(*exp.clone());
        }
        InitVal::List(list) => {
            for item in list {
                values.extend(flatten_init_val(item));
            }
        }
    }
    values
}
pub fn flatten_const_init_val(init: &ConstInitVal, symbol_table: &SymbolTable) -> Vec<i32> {
    let mut values = Vec::new();
    match init {
        ConstInitVal::Exp(exp) => {
            // 必须是常量，立即求值
            values.push(evaluate_const_exp(symbol_table,exp));
        }
        ConstInitVal::List(list) => {
            for item in list {
                values.extend(flatten_const_init_val(item, symbol_table));
            }
        }
    }
    values
}

pub fn flatten_global_init_val(init: &InitVal, symbol_table: &SymbolTable) -> Vec<i32> {
    let mut values = Vec::new();
    match init {
        InitVal::Exp(exp) => {
            values.push(evaluate_const_exp( symbol_table,exp));
        }
        InitVal::List(list) => {
            for item in list {
                values.extend(flatten_global_init_val(item, symbol_table));
            }
        }
    }
    values
}


impl<'a> FunctionGenerator<'a> {
    /// 给定基地址和维度，计算线性索引 i 对应的元素指针
    /// 例如 a[2][3], index=4 (即 a[1][1]) -> getelemptr(getelemptr(a, 1), 1)
    pub fn get_elem_ptr_by_flat_index(&mut self, base_ptr: Value, flat_index: usize, dims: &[usize]) -> Value {
        let mut ptr = base_ptr;
        let mut current_idx = flat_index;
        
        // 我们需要计算每一维的 stride (跨度)
        // 例如 a[2][3][4]，dims=[2, 3, 4]
        // 第 0 维 stride = 3 * 4 = 12
        // 第 1 维 stride = 4
        // 第 2 维 stride = 1
        
        // 技巧：我们可以临时计算每一层的 stride
        // 或者更简单：我们知道总大小，然后除以当前维度的后继维度积
        
        // 让我们从第一维开始剥离
        for (i, &dim_len) in dims.iter().enumerate() {
            // 计算当前维度之后的容量 (sub_size)
            let sub_size: usize = dims[i+1..].iter().product(); 
            // 如果是最后一维，sub_size 就是 1
            
            // 当前维度的下标 = current_idx / sub_size
            let dim_idx = current_idx / sub_size;
            
            // 更新 current_idx 为剩余的偏移量
            current_idx = current_idx % sub_size;
            
            // 生成 getelemptr
            let idx_val = self.func.dfg_mut().new_value().integer(dim_idx as i32);
            let next_ptr = self.func.dfg_mut().new_value().get_elem_ptr(ptr, idx_val);
            self.add_inst(next_ptr);
            
            ptr = next_ptr;
        }
        
        ptr
    }
}