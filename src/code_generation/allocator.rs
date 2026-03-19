use std::collections::{HashMap, HashSet};

use koopa::ir::{FunctionData, Value};

use super::liveness::{analyze_liveness, LiveInterval};

pub const CALLER_SAVED_FIXED_POOL: [&str; 4] = ["t0", "t1", "t2", "t3"];
pub const CALLEE_SAVED_FIXED_POOL: [&str; 4] = ["s1", "s2", "s3", "s4"];

#[derive(Debug, Clone)]
pub struct RegisterAllocation {
    pub fixed_registers: HashMap<Value, &'static str>,
    pub intervals: HashMap<Value, LiveInterval>,
    pub positions: HashMap<Value, usize>,
    pub used_callee_saved_regs: Vec<&'static str>,
}

#[derive(Debug, Clone, Copy)]
struct ActiveInterval {
    value: Value,
    end: usize,
    reg: &'static str,
}

pub fn allocate_registers(func: &FunctionData) -> RegisterAllocation {
    let liveness = analyze_liveness(func);
    let call_positions = collect_call_positions(func, &liveness.positions);
    let fixed_registers = linear_scan_allocate(&liveness.intervals, &call_positions);
    let used_callee_saved_regs = collect_used_callee_saved_regs(&fixed_registers);

    RegisterAllocation {
        fixed_registers,
        intervals: liveness.intervals,
        positions: liveness.positions,
        used_callee_saved_regs,
    }
}

fn linear_scan_allocate(
    intervals: &HashMap<Value, LiveInterval>,
    call_positions: &HashSet<usize>,
) -> HashMap<Value, &'static str> {
    let mut ordered_intervals: Vec<(Value, LiveInterval)> = intervals
        .iter()
        .map(|(&value, &interval)| (value, interval))
        .collect();
    ordered_intervals.sort_by_key(|(_, interval)| (interval.start, interval.end));

    let mut active: Vec<ActiveInterval> = Vec::new();
    let mut free_caller_saved = CALLER_SAVED_FIXED_POOL.to_vec();
    let mut free_callee_saved = CALLEE_SAVED_FIXED_POOL.to_vec();
    let mut assignments = HashMap::new();

    for (value, interval) in ordered_intervals {
        expire_old_intervals(
            interval.start,
            &mut active,
            &mut free_caller_saved,
            &mut free_callee_saved,
        );

        let crosses_call = interval_crosses_call(interval, call_positions);

        if let Some(reg) =
            take_free_register(crosses_call, &mut free_caller_saved, &mut free_callee_saved)
        {
            assignments.insert(value, reg);
            insert_active(
                &mut active,
                ActiveInterval {
                    value,
                    end: interval.end,
                    reg,
                },
            );
        } else {
            let spill_idx = active
                .iter()
                .enumerate()
                .max_by_key(|(_, active_interval)| active_interval.end)
                .map(|(idx, _)| idx)
                .unwrap();
            let spill = active[spill_idx];

            if spill.end > interval.end {
                active.remove(spill_idx);
                assignments.remove(&spill.value);
                assignments.insert(value, spill.reg);
                insert_active(
                    &mut active,
                    ActiveInterval {
                        value,
                        end: interval.end,
                        reg: spill.reg,
                    },
                );
            }
        }
    }

    assignments
}

fn expire_old_intervals(
    current_start: usize,
    active: &mut Vec<ActiveInterval>,
    free_caller_saved: &mut Vec<&'static str>,
    free_callee_saved: &mut Vec<&'static str>,
) {
    let mut idx = 0;
    while idx < active.len() {
        if active[idx].end < current_start {
            release_register(active[idx].reg, free_caller_saved, free_callee_saved);
            active.remove(idx);
        } else {
            idx += 1;
        }
    }
}

fn insert_active(active: &mut Vec<ActiveInterval>, interval: ActiveInterval) {
    active.push(interval);
    active.sort_by_key(|item| item.end);
}

fn collect_call_positions(
    func: &FunctionData,
    positions: &HashMap<Value, usize>,
) -> HashSet<usize> {
    positions
        .iter()
        .filter_map(|(&inst, &pos)| {
            matches!(func.dfg().value(inst).kind(), koopa::ir::ValueKind::Call(_)).then_some(pos)
        })
        .collect()
}

fn interval_crosses_call(interval: LiveInterval, call_positions: &HashSet<usize>) -> bool {
    call_positions
        .iter()
        .any(|&call_pos| interval.start < call_pos && call_pos < interval.end)
}

fn take_free_register(
    crosses_call: bool,
    free_caller_saved: &mut Vec<&'static str>,
    free_callee_saved: &mut Vec<&'static str>,
) -> Option<&'static str> {
    if crosses_call {
        free_callee_saved.pop().or_else(|| free_caller_saved.pop())
    } else {
        free_caller_saved.pop().or_else(|| free_callee_saved.pop())
    }
}

fn release_register(
    reg: &'static str,
    free_caller_saved: &mut Vec<&'static str>,
    free_callee_saved: &mut Vec<&'static str>,
) {
    if is_callee_saved_reg(reg) {
        free_callee_saved.push(reg);
    } else {
        free_caller_saved.push(reg);
    }
}

fn collect_used_callee_saved_regs(
    fixed_registers: &HashMap<Value, &'static str>,
) -> Vec<&'static str> {
    let mut regs: Vec<&'static str> = fixed_registers
        .values()
        .copied()
        .filter(|reg| is_callee_saved_reg(reg))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    regs.sort_unstable();
    regs
}

fn is_callee_saved_reg(reg: &str) -> bool {
    matches!(reg, "s1" | "s2" | "s3" | "s4")
}
