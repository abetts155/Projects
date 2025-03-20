import argparse
import functools
import logging
import numpy
import random
import sys
import threading
import time

from rich.logging import RichHandler

from utils import command_line
from graph import dominators
from graph import graphs
from graph import graph_input
from graph import vertices


logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


def retrieve_articulations(cfg: graphs.ControlFlowGraph,
                           tree: graphs.Tree,
                           direction: graphs.GraphDirection) -> set[vertices.Vertex]:
    if direction == graphs.GraphDirection.Forwards:
        vertex = cfg.get_origin(graphs.GraphDirection.Backwards)
    else:
        vertex = cfg.get_origin(graphs.GraphDirection.Forwards)

    vertex_set = {vertex}
    while vertex != cfg.get_origin(direction):
        (parent,) = tree.predecessors[vertex]
        vertex_set.add(parent)
        vertex = parent

    return vertex_set


def measure(closure: functools.partial) -> float:
    start = time.time()
    closure()
    finish = time.time()
    total_time = finish - start
    return total_time


def compare(left: type, left_time: float, right: type, right_time: float):
    if left_time == right_time:
        logger.info(f'{left.__name__} and {right.__name__} performed equally')
    elif left_time < right_time:
        factor = round(right_time / left_time, 1)
        logger.info(f'{left.__name__} is {factor}X faster than {right.__name__}')
    else:
        factor = round(left_time / right_time, 1)
        logger.info(f'{right.__name__} is {factor}X faster than {left.__name__}')


def strip_outliers(execution_times: list[float]):
    q25, q75 = numpy.percentile(execution_times, 25), numpy.percentile(execution_times, 75)
    iqr = q75 - q25
    cut_off = iqr * 1.5
    lower, upper = q25 - cut_off, q75 + cut_off
    return [x for x in execution_times if lower <= x <= upper]


def super_graph_analysis(cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
    logger.debug('Reducing the finest super graph down to the coarsest super graph')
    super_graph = graphs.create_finest_super_graph(cfg, direction)
    measure(functools.partial(super_graph.reduce))


def loops_analysis(cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
    logger.debug('Analysing the loop-nesting forest')
    origin = cfg.get_origin(direction)
    traveller = cfg.get_traveller(direction)
    forest = measure(functools.partial(graphs.LoopForest, cfg, origin, traveller))


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Do dominator tree analysis')

    command_line.add_cfg_options(parser, True)

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the dominator tree creation this many times',
                        default=10,
                        metavar='<INT>')

    args = parser.parse_args()

    if args.config is None and args.read is None:
        raise ValueError("Either give a config file to generate CFGs or a file containing already generated CFGs.")

    if args.config and args.read:
        raise ValueError(
            "The CFGs can either be generated dynamically or read from a file, but both are not allowed.")

    if args.config and args.cfgs:
        raise ValueError("It is not possible to filter out CFGs when generating them dynamically.")

    return args


def main():
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 30)

    args = parse_the_command_line()
    cfgs = input.read_cfgs(args.program, args.cfgs)

    algorithms = [dominators.LengauerTarjan, dominators.Cooper, dominators.BettsOnline]
    total_time = {algorithm: 0 for algorithm in algorithms}

    for cfg in cfgs:
        direction: graphs.GraphDirection
        for direction in graphs.GraphDirection:
            logger.info(f"Analysing CFG '{cfg.name}' ({direction.upper()}) with {len(cfg.its_vertices)} vertices and "
                        f"{len(cfg.its_edges)} edges")
            random.shuffle(algorithms)
            for algorithm in algorithms:
                execution_times = []
                for i in range(0, args.repeat):
                    execution_time = measure(functools.partial(algorithm, cfg, direction))
                    execution_times.append(execution_time)

                aggregated_time = sum(strip_outliers(execution_times)) / args.repeat
                logger.debug(f"Running '{algorithm.__name__}' {args.repeat} times consumed {aggregated_time:.5f} "
                             f"secs on average")
                total_time[algorithm] += aggregated_time

            betts_tree = dominators.BettsOnline(cfg, direction)
            cooper_tree = dominators.Cooper(cfg, direction)
            assert betts_tree == cooper_tree

    algorithms.sort(key=lambda algorithm: algorithm.__name__)
    for algorithm in algorithms:
        logger.debug(f"Running '{algorithm.__name__}' took {total_time[algorithm]:.5f} seconds")

    for i, left in enumerate(algorithms):
        for right in algorithms[i + 1:]:
            compare(left, total_time[left], right, total_time[right])


if __name__ == '__main__':
    main()
