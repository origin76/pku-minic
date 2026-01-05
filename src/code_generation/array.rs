use koopa::ir::{dfg::DataFlowGraph, Type, TypeKind, Value};
use std::fmt::Write;

use crate::AsmBuilder;

pub fn get_type_size(ty: &Type) -> i32 {
    match ty.kind() {
        TypeKind::Int32 => 4,
        TypeKind::Unit => 0,
        TypeKind::Array(base, len) => get_type_size(base) * (*len as i32),
        TypeKind::Pointer(_) => 4, // RV32 指针占 4 字节
        TypeKind::Function(_, _) => 4,
    }
}

impl<'a> AsmBuilder<'a> {
    // 处理 getptr (指针运算)
    // %dest = getptr %src, %index
    pub fn process_get_ptr(
        &mut self,
        dest_val: Value,
        get_ptr: &koopa::ir::values::GetPtr,
        dfg: &DataFlowGraph,
    ) {
        let src = get_ptr.src();
        let index = get_ptr.index();

        // 1. 确定步长 (Element Size)
        // src 的类型是 *T，我们需要 T 的大小
        let src_ty = self.get_value_type(src, dfg); // 记得用你之前写的安全获取类型的函数
        let element_size = if let TypeKind::Pointer(base) = src_ty.kind() {
            get_type_size(base)
        } else {
            panic!("getptr src must be a pointer");
        };

        // 2. 生成地址计算指令
        self.generate_address_calculation(dest_val, src, index, element_size, dfg);
    }

    // 处理 getelemptr (数组元素地址)
    // %dest = getelemptr %src, %index
    pub fn process_get_elem_ptr(
        &mut self,
        dest_val: Value,
        gep: &koopa::ir::values::GetElemPtr,
        dfg: &DataFlowGraph,
    ) {
        let src = gep.src();
        let index = gep.index();

        // 1. 确定步长 (Element Size)
        // src 的类型是 *[T, N]，我们需要 T 的大小
        let src_ty = self.get_value_type(src, dfg);
        let element_size = if let TypeKind::Pointer(base) = src_ty.kind() {
            if let TypeKind::Array(elem_ty, _) = base.kind() {
                get_type_size(elem_ty)
            } else {
                panic!("getelemptr src base must be an array");
            }
        } else {
            panic!("getelemptr src must be a pointer");
        };

        // 2. 生成地址计算指令
        self.generate_address_calculation(dest_val, src, index, element_size, dfg);
    }

    // 通用的地址计算逻辑: dest = src + index * size
    fn generate_address_calculation(
        &mut self,
        dest_val: Value,
        src: Value,
        index: Value,
        size: i32,
        dfg: &DataFlowGraph,
    ) {
        // A. 准备基地址 (src)
        // 注意：resolve_operand 会处理全局/局部/栈参数等所有情况，返回存储地址的寄存器
        let src_reg = self.resolve_operand(src, dfg);
        let src_reg_str = src_reg.to_string();
        self.ctx.as_mut().unwrap().lock_reg(&src_reg_str);

        // B. 准备索引 (index)
        let idx_reg = self.resolve_operand(index, dfg);
        let idx_reg_str = idx_reg.to_string();
        self.ctx.as_mut().unwrap().lock_reg(&idx_reg_str);

        // C. 分配目标寄存器
        let dest_reg = self
            .ctx
            .as_mut()
            .unwrap()
            .alloc_reg_for_result(dest_val, &mut self.output);
        self.ctx.as_mut().unwrap().lock_reg(dest_reg);

        // D. 生成计算指令
        if size == 0 {
            // 极其罕见，可能是 void 指针？直接 mv
            writeln!(self.output, "  mv    {}, {}", dest_reg, src_reg_str).unwrap();
        } else {
            // 优化：如果是 4 的倍数 (常见的 i32 数组)，可以用移位
            // 例如 size=4 -> slli idx, 2
            let mut offset_reg = idx_reg_str.clone();

            // 如果 size 不是 1，需要乘法
            if size != 1 {
                // 通用乘法: mul dest, idx, size
                let tmp = self
                    .ctx
                    .as_mut()
                    .unwrap()
                    .get_temp_reg_for_address_calc(&mut self.output);
                writeln!(self.output, "  li    {}, {}", tmp, size).unwrap();
                writeln!(
                    self.output,
                    "  mul   {}, {}, {}",
                    dest_reg, idx_reg_str, tmp
                )
                .unwrap();
                offset_reg = dest_reg.to_string();
            }

            // E. 最终相加: dest = src + offset
            // add dest, src, offset
            writeln!(
                self.output,
                "  add   {}, {}, {}",
                dest_reg, src_reg_str, offset_reg
            )
            .unwrap();
        }

        self.ctx.as_mut().unwrap().unlock_all();
    }
}
