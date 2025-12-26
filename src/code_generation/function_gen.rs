use super::reg_context::FuncContext;
use koopa::ir::{
    dfg::DataFlowGraph,
    layout::BasicBlockNode,
    BasicBlock, FunctionData, Value, ValueKind,
};
use std::fmt::Write;

impl super::asm::AsmBuilder<'_> {
    pub fn generate_function(&mut self, func: &FunctionData) {
        // 1. 【重置上下文】
        self.ctx = Some(FuncContext::new(func.name().to_string()));
        self.ctx.as_mut().unwrap().analyze_frame(func);

        // 2. 打印函数标签
        let name = &func.name()[1..];
        writeln!(self.output, "\n  .text").unwrap();
        writeln!(self.output, "  .globl {}", name).unwrap();
        writeln!(self.output, "{}:", name).unwrap();

        // 保存 output，用于暂存函数体
        let mut final_output = std::mem::take(&mut self.output);

        // 3. 遍历基本块生成指令
        let dfg = func.dfg();
        for (bb_handle, node) in func.layout().bbs() {
            self.generate_bb(bb_handle, node, dfg);
        }

        // === 【关键步骤 B】 计算最终栈大小 ===
        let ctx = self.ctx.as_ref().unwrap();

        // 16 字节对齐
        let align_stack_size = ctx.total_frame_size;

        // === 【关键步骤 C】 组装 Prologue (序言) ===
        // 栈布局 (High -> Low):
        // [ ... Caller Stack ... ]
        // [ RA (Ret Addr)        ] <- align_stack_size - 4
        // [ S0 (Old FP)          ] <- align_stack_size - 8
        // [ ... Locals/Spill ... ]
        // [ ... Outgoing Args... ]
        // [ ... Current SP ...   ] <- 0

        if align_stack_size < 2048 {
            // --- 情况 A: 栈帧较小 (直接用立即数) ---
            writeln!(final_output, "  addi  sp, sp, -{}", align_stack_size).unwrap();
            writeln!(final_output, "  sw    ra, {}(sp)", align_stack_size - 4).unwrap();
            writeln!(final_output, "  sw    s0, {}(sp)", align_stack_size - 8).unwrap();
        } else {
            // --- 情况 B: 栈帧较大 (需要计算地址) ---
            // 1. 调整 SP
            writeln!(final_output, "  li    t0, -{}", align_stack_size).unwrap();
            writeln!(final_output, "  add   sp, sp, t0").unwrap();

            // 2. 保存 ra, s0 (因为偏移量太大，不能直接 sw ra, 20000(sp))
            // 先计算栈顶地址到 t0: t0 = sp + align_stack_size
            writeln!(final_output, "  li    t0, {}", align_stack_size).unwrap();
            writeln!(final_output, "  add   t0, sp, t0").unwrap();

            writeln!(final_output, "  sw    ra, -4(t0)").unwrap();
            writeln!(final_output, "  sw    s0, -8(t0)").unwrap();
        }

        // === 【关键步骤 D】 组装 Epilogue (结语) ===
        // 构造恢复现场的指令序列
        let epilogue = if align_stack_size < 2048 {
            format!(
                "  lw    ra, {0}(sp)\n  lw    s0, {1}(sp)\n  addi  sp, sp, {2}\n  ret",
                align_stack_size - 4,
                align_stack_size - 8,
                align_stack_size
            )
        } else {
            // 大栈帧恢复
            // 1. 恢复 ra, s0 (先算栈顶地址)
            format!(
                "  li    t0, {0}\n  add   t0, sp, t0\n  lw    ra, -4(t0)\n  lw    s0, -8(t0)\n  li    t0, {0}\n  add   sp, sp, t0\n  ret",
                align_stack_size
            )
        };

        // 获取暂存的函数体汇编
        let body_asm = std::mem::take(&mut self.output);

        // 替换占位符
        let final_body = body_asm.replace("#RET_PLACEHOLDER#", &epilogue);

        // === 【关键步骤 E】 合并与还原 ===
        final_output.push_str(&final_body);
        self.output = final_output;
        self.ctx = None;
    }

    pub fn get_bb_label(&self, bb: &BasicBlock , dfg: &DataFlowGraph) -> String {
        // 1. 获取函数名，并去掉开头的 '@'
        // self.func.name() 返回的是 "@main"，我们需要 "main"
        let func_name = &self.ctx.as_ref().unwrap().name[1..];
        
        // 2. 获取基本块名，并去掉开头的 '%'
        // bb_name 返回的是 "%entry" 或 "%while_entry_1"，我们需要 "entry"
        let raw_bb_name = dfg.bb(*bb).name().as_ref().unwrap();
        let bb_name = &raw_bb_name[1..];

        // 3. 拼接成标准格式: .L<func>_<bb>
        format!(".L{}_{}", func_name, bb_name)
    }

    pub fn generate_bb(&mut self, bb: &BasicBlock, node: &BasicBlockNode, dfg: &DataFlowGraph) {
        // 生成基本块标签，例如 .L0:
        // 如果是入口块，有些汇编器不需要标签，但加上也无妨
        // 你可以用 HashMap 映射 bb handle 到标签名，或者直接用名字
        let label = self.get_bb_label(bb, dfg);
        let _ = writeln!(self.output, "{}:", label);

        // 遍历指令
        for (val, _inst_node) in node.insts() {
            // 关键：把 inst_handle (ID) 和 dfg (数据表) 一起传下去
            self.generate_inst(*val, dfg);
        }
    }

    pub fn generate_inst(&mut self, inst: Value, dfg: &DataFlowGraph) {
        // 使用 dfg.value(inst) 来获取指令的具体数据
        let value_data = dfg.value(inst);

        match value_data.kind() {
            ValueKind::Binary(bin) => {
                // 此时我们需要 helper 函数也接受 dfg，因为操作数 bin.lhs() 也是 handle
                self.process_binary(inst, bin, dfg);
            }
            ValueKind::Return(ret) => {
                self.process_return(ret, dfg);
            }
            ValueKind::Alloc(alloc) => {
                self.process_alloc(inst, alloc, dfg);
            }
            ValueKind::Load(load) => {
                self.process_load(inst, load, dfg);
            }
            ValueKind::Store(store) => {
                self.process_store(store, dfg);
            }
            ValueKind::Branch(br) => {
                self.process_branch(br, dfg);
            }
            ValueKind::Jump(j) => {
                self.process_jump(j, dfg);
            }
            ValueKind::Call(c) => {
                self.process_call(inst, c, dfg);
            }
            _ => {}
        }
    }

    pub fn resolve_operand(&mut self, val: Value, dfg: &DataFlowGraph) -> String {
        if val.is_global() {
            // 1. 从 Program 中获取数据 (不能用 dfg.value!)
            let data = self.program.borrow_value(val);
            
            // 2. 获取全局变量名字 (去掉 @)
            let name = data.name().as_ref().expect("Global must have name").replace("@", "");

            // 3. 生成加载地址指令 (la reg, symbol)
            // 因为 resolve_operand 的契约是返回一个"存有该值的寄存器"
            // 对于全局变量指针，我们需要把它的地址 load 到临时寄存器里
            let tmp_reg = self
                .ctx
                .as_mut()
                .unwrap()
                .get_temp_reg_for_address_calc(&mut self.output);
            
            writeln!(self.output, "  la    {}, {}", tmp_reg, name).unwrap();
            
            return tmp_reg.to_string();
        }
        let value_data = dfg.value(val);

        match value_data.kind() {
            // === 情况 1: 立即数 (Integer) ===
            ValueKind::Integer(int) => {
                let imm = int.value();
                if imm == 0 {
                    // 优化：0 直接用 x0
                    "x0".to_string()
                } else {
                    // 非 0 立即数：需要一个临时寄存器
                    // 注意：立即数不对应任何 Value，所以不需要 update_reg_map

                    // 1. 找一个空闲寄存器 (可能会触发 Spill，写入 sw 指令到 self.output)
                    let reg_idx = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .find_free_reg_or_spill(&mut self.output);

                    let reg_name = self.ctx.as_mut().unwrap().phys_regs[reg_idx];

                    // 2. 生成加载立即数的指令
                    writeln!(self.output, "  li    {}, {}", reg_name, imm).unwrap();

                    // 3. 【关键】这个寄存器存的是临时值，不是某个 Value
                    // 我们不需要将其 bind 到 val 上。
                    // 但是，为了防止处理下一个操作数时把它 spill 掉，最好锁住它
                    // (如果你在外部调用处统一 lock，这里可以不 lock，但建议返回后在外部 lock)
                    self.ctx.as_mut().unwrap().lock_reg(reg_name);

                    reg_name.to_string()
                }
            }

            koopa::ir::ValueKind::FuncArgRef(arg) => {
                let index = arg.index();
                if index < 8 {
                    // 如果是前 8 个参数，直接返回对应的物理寄存器名
                    format!("a{}", index)
                } else {
                    if index < 8 {
                        // 情况 A: 寄存器传参 (a0 - a7)
                        format!("a{}", index)
                    } else {
                        // 情况 B: 栈传参 (index >= 8)
                        // 1. 获取当前函数的栈帧大小
                        // 注意：这要求你在 generate_function 开头已经计算好了 ctx.stack_size (align_stack_size)
                        let frame_size = self.ctx.as_ref().unwrap().total_frame_size;
                        // 如果你的 stack_size 还没加上 ra/s0 的 8 字节，记得加上:
                        // let frame_size = self.ctx.as_ref().unwrap().raw_stack_size_with_ra_s0_aligned;

                        // 2. 计算相对于当前 SP 的偏移量
                        let offset = frame_size + (index as i32 - 8) * 4;

                        // 3. 申请一个临时寄存器来存放加载进来的参数
                        // 因为 resolve_operand 必须返回一个寄存器名供后续指令使用
                        let tmp_reg = self
                            .ctx
                            .as_mut()
                            .unwrap()
                            .get_temp_reg_for_address_calc(&mut self.output);

                        // 4. 生成 Load 指令 (注意处理大立即数)
                        if offset >= -2048 && offset <= 2047 {
                            writeln!(self.output, "  lw    {}, {}(sp)", tmp_reg, offset).unwrap();
                        } else {
                            // 偏移量太大，需要先算地址
                            writeln!(self.output, "  li    {}, {}", tmp_reg, offset).unwrap();
                            writeln!(self.output, "  add   {}, {}, sp", tmp_reg, tmp_reg).unwrap();
                            writeln!(self.output, "  lw    {}, 0({})", tmp_reg, tmp_reg).unwrap();
                        }

                        // 5. 返回临时寄存器名
                        tmp_reg.to_string()
                    }
                }
            }

            // === 情况 2: 变量 (Value) ===
            _ => {
                // 变量可能在寄存器里，也可能在栈里。
                // 如果在栈里，get_reg_for_operand 会自动生成 lw 指令并更新状态。

                let reg_name = self
                    .ctx
                    .as_mut()
                    .unwrap()
                    .get_reg_for_operand(val, &mut self.output);

                // 返回寄存器名 (因为 phys_regs 是 &'static str，所以要转 String)
                reg_name.to_string()
            }
        }
    }


}
