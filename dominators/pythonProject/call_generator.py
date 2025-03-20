import argparse
import dataclasses
import json
import logging
import pydantic
import random
import rich

from rich.logging import RichHandler

from utils import command_line
from graph import edges
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


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Generate a random call graph for the given control flow graph (CFGs)')

    command_line.add_config_option(parser)
    command_line.add_cfg_options(parser, True)

    parser.add_argument('-O',
                        '--output',
                        help='write the call graph to this file',
                        metavar='<FILE>.json',
                        required=True)

    args = parser.parse_args()

    if args.config == args.output:
        logger.error(f"The configuration and output files are the same: '{args.config}'")

    return args


class Configuration(pydantic.BaseModel):
    call_depth:    pydantic.NonNegativeInt = 0
    maximum_calls: pydantic.PositiveInt = 10 ** 3
    recursion:     pydantic.StrictBool = False


def validate(config: Configuration, number_of_cfgs: int):
    if number_of_cfgs < config.call_depth + 1:
        raise ValueError(f"For a call depth of {config.call_depth} you need at least {config.call_depth + 1} CFGs, "
                         f"and there are only {number_of_cfgs} in the given file.")


base_call_level: int = 0


@dataclasses.dataclass(slots=True)
class SubprogramData:
    call_vertex: vertices.Vertex
    cfg: graphs.ControlFlowGraph
    sites: list[vertices.Vertex]


def assign_call_levels(config: Configuration, program_data: list[SubprogramData]) -> dict[int, list[SubprogramData]]:
    level = base_call_level
    index = len(program_data) - 1
    level_to_data = {}
    program_data = sorted(program_data, key=lambda data: len(data.sites))
    while level <= config.call_depth:
        data = program_data[index]
        level_to_data[level] = [data]

        if level == base_call_level:
            logger.info(f"The root of the call graph is '{data.cfg.name}'")

        index -= 1
        level += 1

    while index >= 0:
        level = random.randint(1, config.call_depth)
        data = program_data[index]
        level_to_data[level].append(data)
        index -= 1

    return level_to_data


def make_tree(config: Configuration, level_to_data: dict[int, list[SubprogramData]], call_graph: graphs.CallGraph):
    for callee_level in range(base_call_level + 1, config.call_depth + 1):
        callees = level_to_data[callee_level]
        callers = level_to_data[callee_level - 1]
        callers = list(filter(lambda data: data.sites, callers))
        for callee_data in callees:
            caller_data: SubprogramData = random.choice(callers)
            site = caller_data.sites.pop()
            edge = edges.CallEdge(caller_data.call_vertex, callee_data.call_vertex, site)
            call_graph.add_edge(edge)


def make_graph(config: Configuration, level_to_data: dict[int, list[SubprogramData]], call_graph: graphs.CallGraph):
    total_calls = len(call_graph.its_edges)
    while total_calls <= config.maximum_calls and random.random() < 0.975:
        max_level = max(level_to_data.keys())
        callee_level = random.randint(base_call_level + 1, max_level)
        caller_level = random.randint(base_call_level, callee_level - 1)
        callees = level_to_data[callee_level]
        callers = level_to_data[caller_level]
        callers = list(filter(lambda data: data.sites, callers))
        if callers:
            callee_data: SubprogramData = random.choice(callees)
            caller_data: SubprogramData = random.choice(callers)
            site = caller_data.sites.pop()
            edge = edges.CallEdge(caller_data.call_vertex, callee_data.call_vertex, site)
            call_graph.add_edge(edge)
            total_calls += 1


def build(config: Configuration, cfgs: dict[str, graphs.ControlFlowGraph]) -> graphs.CallGraph:
    call_graph = graphs.CallGraph()

    program_data = []
    for cfg in cfgs.values():
        cfg.dotify(f'{cfg.name}')
        call_vertex = vertices.Vertex(cfg.name)
        call_graph.add_vertex(call_vertex)
        call_sites = [vertex for vertex in cfg.its_vertices if len(cfg.successors[vertex]) == 1]
        random.shuffle(call_sites)
        program_data.append(SubprogramData(call_vertex, cfg, call_sites))

    level_to_data = assign_call_levels(config, program_data)
    make_tree(config, level_to_data, call_graph)
    if len(call_graph.its_vertices) > 1:
        make_graph(config, level_to_data, call_graph)
    call_graph.set_root()
    return call_graph


def main():
    args = parse_the_command_line()
    cfgs = graph_input.read_cfgs(args.program, args.cfgs)

    logger.info(f"Reading configuration file '{args.config}'")
    with open(args.config, 'r') as in_file:
        data = json.load(in_file)
        config = Configuration(**data)
        rich.print(config)

    validate(config, len(cfgs))
    call_graph: graphs.CallGraph = build(config, cfgs)

    logger.info(f"Writing into output file '{args.output}'")

    with open(args.output, 'w') as out_file:
        json.dump(call_graph.to_json(), out_file, indent=2)
        out_file.write('\n' * 2)


if __name__ == '__main__':
    main()
