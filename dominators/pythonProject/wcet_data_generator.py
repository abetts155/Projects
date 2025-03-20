import argparse
import random
import sympy

from utils import command_line, wcet_data
from graph import graphs
from graph import graph_input
from graph import vertices


def generate_expression(max_bound: int, symbolic: bool = False) -> sympy.Expr:
    if not symbolic:
        const = random.randint(1, max_bound)
        return sympy.Integer(const)
    else:
        symbols = [sympy.Symbol('m'), sympy.Symbol('n'), sympy.Symbol('p'), sympy.Symbol('q'), sympy.Symbol('r')]
        if 0 <= random.random() < 0.5:
            return random.choice(symbols)
        else:
            sym1 = random.choice(symbols)
            sym2 = random.choice(symbols)
            operator = random.choice([sympy.Add, sympy.Mul])
            return operator(sym1, sym2)


def generate_loop_bounds(cfg: graphs.ControlFlowGraph, data: wcet_data.CFGTimingData, max_bound: int, max_level: int):
    direction = graphs.GraphDirection.Forwards
    forest = graphs.build_loop_forest(cfg, direction)
    loop: vertices.LoopVertex
    for loop in forest.its_vertices:
        if loop != forest.root:
            (header,) = loop.entries
            ancestor = loop
            level = 1
            while forest.predecessors[ancestor] and level <= max_level:
                (ancestor_edge,) = forest.predecessors[ancestor]
                ancestor = ancestor_edge.point_a
                symbolic = random.choice([True, False])
                expr = generate_expression(max_bound, symbolic)
                data.add_bound_expression(header, level, expr)
                level += 1


def generate_wcets(cfg: graphs.ControlFlowGraph, data: wcet_data.CFGTimingData, max_wcet: int):
    for vertex in cfg.its_vertices:
        data.wcets[vertex] = random.randint(1, max_wcet)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Randomly generate the data required for WCET analysis')

    command_line.add_cfg_options(parser, True)

    parser.add_argument('-W',
                        '--wcet',
                        help='the maximum possible WCET value assigned to a vertex',
                        type=int,
                        default=10)

    parser.add_argument('-B',
                        '--bound',
                        help='the maximum possible execution frequency bound assigned to a loop header',
                        type=int,
                        default=10)

    parser.add_argument('-L',
                        '--level',
                        help='the maximum possible loop-nesting level for which a bound is generated',
                        type=int,
                        default=1)

    parser.add_argument('-O',
                        '--output',
                        help='write the data to this file',
                        metavar='<FILE>.json',
                        required=True)

    args = parser.parse_args()

    if args.wcet < 1:
        raise ValueError("The maximum allowed WCET must be a positive integer.")

    if args.bound < 1:
        raise ValueError("The maximum allowed loop bound must be a positive integer.")

    if args.level < 1:
        raise ValueError("The maximum allowed loop-nesting level must be a positive integer.")

    return args


def main():
    args = parse_the_command_line()
    cfgs = graph_input.read_cfgs(args.program, args.cfgs)
    program_data = []
    for cfg in cfgs.values():
        cfg_data = wcet_data.CFGTimingData(cfg=cfg)
        generate_wcets(cfg, cfg_data, args.wcet)
        generate_loop_bounds(cfg, cfg_data, args.bound, args.level)
        program_data.append(cfg_data)

    wcet_data.store_cfg_timing_analysis_data(args.output, program_data)
    wcet_data.load_cfg_timing_analysis_data(args.output, cfgs)


if __name__ == '__main__':
    main()
