use std::collections::{HashMap, HashSet};

use koopa::ir::{dfg::DataFlowGraph, BasicBlock, FunctionData, Value, ValueKind};

#[derive(Debug, Clone, Copy)]
pub struct LiveInterval {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct LivenessInfo {
    pub intervals: HashMap<Value, LiveInterval>,
    pub positions: HashMap<Value, usize>,
    pub block_live_in: HashMap<BasicBlock, HashSet<Value>>,
    pub block_live_out: HashMap<BasicBlock, HashSet<Value>>,
}

#[derive(Debug, Clone, Default)]
struct BlockLiveness {
    insts: Vec<Value>,
    use_set: HashSet<Value>,
    def_set: HashSet<Value>,
    successors: Vec<BasicBlock>,
    live_in: HashSet<Value>,
    live_out: HashSet<Value>,
}

pub fn analyze_liveness(func: &FunctionData) -> LivenessInfo {
    let dfg = func.dfg();
    let positions = collect_positions(func);
    let block_order: Vec<BasicBlock> = func.layout().bbs().into_iter().map(|(&bb, _)| bb).collect();
    let mut blocks = build_block_liveness(func, dfg, &positions, &block_order);

    solve_live_sets(&block_order, &mut blocks);

    let intervals = build_live_intervals(dfg, &positions, &block_order, &blocks);
    let block_live_in = blocks
        .iter()
        .map(|(&bb, info)| (bb, info.live_in.clone()))
        .collect();
    let block_live_out = blocks
        .iter()
        .map(|(&bb, info)| (bb, info.live_out.clone()))
        .collect();

    LivenessInfo {
        intervals,
        positions,
        block_live_in,
        block_live_out,
    }
}

fn collect_positions(func: &FunctionData) -> HashMap<Value, usize> {
    let mut positions = HashMap::new();
    let mut position = 0usize;

    for (_, bb) in func.layout().bbs() {
        for (&inst, _) in bb.insts() {
            positions.insert(inst, position);
            position += 1;
        }
    }

    positions
}

fn build_block_liveness(
    func: &FunctionData,
    dfg: &DataFlowGraph,
    positions: &HashMap<Value, usize>,
    block_order: &[BasicBlock],
) -> HashMap<BasicBlock, BlockLiveness> {
    let block_insts: HashMap<BasicBlock, Vec<Value>> = func
        .layout()
        .bbs()
        .into_iter()
        .map(|(&bb, node)| {
            let insts = node.insts().into_iter().map(|(&inst, _)| inst).collect();
            (bb, insts)
        })
        .collect();

    let mut blocks = HashMap::new();

    for (idx, &bb) in block_order.iter().enumerate() {
        let insts = block_insts.get(&bb).cloned().unwrap_or_default();

        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        for &inst in &insts {
            for used in used_values(inst, dfg) {
                if is_tracked_value(used, dfg, positions) && !def_set.contains(&used) {
                    use_set.insert(used);
                }
            }

            if is_tracked_value(inst, dfg, positions) {
                def_set.insert(inst);
            }
        }

        let successors = block_successors(&insts, dfg, block_order.get(idx + 1).copied());

        blocks.insert(
            bb,
            BlockLiveness {
                insts,
                use_set,
                def_set,
                successors,
                live_in: HashSet::new(),
                live_out: HashSet::new(),
            },
        );
    }

    blocks
}

fn solve_live_sets(block_order: &[BasicBlock], blocks: &mut HashMap<BasicBlock, BlockLiveness>) {
    loop {
        let mut changed = false;

        for &bb in block_order.iter().rev() {
            let successors = blocks[&bb].successors.clone();
            let def_set = blocks[&bb].def_set.clone();
            let use_set = blocks[&bb].use_set.clone();

            let mut new_live_out = HashSet::new();
            for succ in successors {
                if let Some(succ_info) = blocks.get(&succ) {
                    new_live_out.extend(succ_info.live_in.iter().copied());
                }
            }

            let mut new_live_in = use_set;
            new_live_in.extend(new_live_out.difference(&def_set).copied());

            let info = blocks.get_mut(&bb).unwrap();
            if info.live_in != new_live_in || info.live_out != new_live_out {
                info.live_in = new_live_in;
                info.live_out = new_live_out;
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }
}

fn build_live_intervals(
    dfg: &DataFlowGraph,
    positions: &HashMap<Value, usize>,
    block_order: &[BasicBlock],
    blocks: &HashMap<BasicBlock, BlockLiveness>,
) -> HashMap<Value, LiveInterval> {
    let mut intervals = HashMap::new();

    for (&inst, &pos) in positions {
        if should_track_value(inst, dfg) {
            intervals.insert(
                inst,
                LiveInterval {
                    start: pos,
                    end: pos,
                },
            );
        }
    }

    for &bb in block_order {
        let Some(block) = blocks.get(&bb) else {
            continue;
        };

        let Some(&last_inst) = block.insts.last() else {
            continue;
        };
        let block_end = positions[&last_inst];

        for &value in &block.live_out {
            if let Some(interval) = intervals.get_mut(&value) {
                interval.end = interval.end.max(block_end);
            }
        }

        let mut live = block.live_out.clone();
        for &inst in block.insts.iter().rev() {
            let pos = positions[&inst];

            for &value in &live {
                if let Some(interval) = intervals.get_mut(&value) {
                    interval.end = interval.end.max(pos);
                }
            }

            if should_track_value(inst, dfg) {
                live.remove(&inst);
            }

            for used in used_values(inst, dfg) {
                if is_tracked_value(used, dfg, positions) {
                    if let Some(interval) = intervals.get_mut(&used) {
                        interval.end = interval.end.max(pos);
                    }
                    live.insert(used);
                }
            }
        }
    }

    intervals
}

fn block_successors(
    insts: &[Value],
    dfg: &DataFlowGraph,
    fallthrough: Option<BasicBlock>,
) -> Vec<BasicBlock> {
    let Some(&last_inst) = insts.last() else {
        return fallthrough.into_iter().collect();
    };

    match dfg.value(last_inst).kind() {
        ValueKind::Branch(br) => vec![br.true_bb(), br.false_bb()],
        ValueKind::Jump(jump) => vec![jump.target()],
        ValueKind::Return(_) => Vec::new(),
        _ => fallthrough.into_iter().collect(),
    }
}

fn should_track_value(inst: Value, dfg: &DataFlowGraph) -> bool {
    let value_data = dfg.value(inst);
    if value_data.ty().is_unit() {
        return false;
    }

    !matches!(value_data.kind(), ValueKind::Alloc(_))
}

fn is_tracked_value(value: Value, dfg: &DataFlowGraph, positions: &HashMap<Value, usize>) -> bool {
    positions.contains_key(&value) && should_track_value(value, dfg)
}

fn used_values(inst: Value, dfg: &DataFlowGraph) -> Vec<Value> {
    match dfg.value(inst).kind() {
        ValueKind::Binary(bin) => vec![bin.lhs(), bin.rhs()],
        ValueKind::Load(load) => vec![load.src()],
        ValueKind::Store(store) => vec![store.value(), store.dest()],
        ValueKind::Branch(br) => vec![br.cond()],
        ValueKind::Return(ret) => ret.value().into_iter().collect(),
        ValueKind::Call(call) => call.args().to_vec(),
        ValueKind::GetPtr(ptr) => vec![ptr.src(), ptr.index()],
        ValueKind::GetElemPtr(ptr) => vec![ptr.src(), ptr.index()],
        _ => Vec::new(),
    }
}
