import argparse
import json
import logging
import pathlib
import random

from rich.logging import RichHandler

import graphs.edges
import graphs.graph_input
import graphs.graphs
import graphs.vertices
import utils.command_line
import utils.structure

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Instrument the program')
    utils.command_line.add_cfg_options(parser, True)
    utils.command_line.add_call_graph_option(parser)

    parser.add_argument('-B',
                        '--budget',
                        help='the instrumentation budget',
                        type=float,
                        default=10.0)

    args = parser.parse_args()

    min_budget = 5
    max_budget = 100
    if args.budget < min_budget or args.budget > max_budget:
        logger.error(f"The budget must be between {min_budget}% and {max_budget}%")

    return args


def allocate(
        cfgs: dict[str, graphs.graphs.ControlFlowGraph],
        call_graph: graphs.graphs.CallGraph,
        budget: float
) -> dict[graphs.graphs.ControlFlowGraph, int]:
    number_of_basic_blocks = sum(len(cfg.its_vertices) for cfg in cfgs.values())
    number_of_instrumentation_points = round(number_of_basic_blocks / (100 / budget))
    logger.info(f"There are {number_of_instrumentation_points} instrumentation points")

    cfg_order = [cfg for cfg in cfgs.values()]
    random.shuffle(cfg_order)

    root_cfg = cfgs[call_graph.root_vertex.identifier]
    cfg_order.remove(root_cfg)
    cfg_order.insert(0, root_cfg)

    allocation = {cfg: 0 for cfg in cfg_order}
    remaining_instrumentation_points = number_of_instrumentation_points
    index = 0
    while remaining_instrumentation_points > 1 and index < len(cfg_order):
        cfg = cfg_order[index]
        allocation[cfg] += 2
        index += 1
        remaining_instrumentation_points -= 2

    while remaining_instrumentation_points > 0:
        random_index = random.randint(0, len(cfg_order) - 1)
        cfg = cfg_order[random_index]
        max_allocation = len(cfg.its_vertices) - allocation[cfg]
        max_allocation = min(max_allocation, remaining_instrumentation_points)
        random_allocation = random.randint(1, max_allocation)
        allocation[cfg] += random_allocation
        remaining_instrumentation_points -= random_allocation

        if allocation[cfg] == len(cfg.its_vertices):
            del cfg_order[random_index]

    return allocation


def instrument(
        cfg: graphs.graphs.ControlFlowGraph,
        call_graph: graphs.graphs.CallGraph,
        allocated_number: int
) -> graphs.graphs.InstrumentationProfile:
    logger.info(f"{cfg.name} is allocated {allocated_number} instrumentation points")

    profile = graphs.graphs.InstrumentationProfile()
    profile.vertex_points.add(cfg.entry_vertex)
    profile.vertex_points.add(cfg.exit_vertex)

    high_priority = []
    call_vertex = call_graph.get_vertex(cfg.name)
    call_edge: graphs.edges.CallEdge
    for call_edge in call_graph.successors[call_vertex]:
        high_priority.append(call_edge.site)

    loop_forest = graphs.graphs.build_loop_forest(cfg, graphs.graphs.GraphDirection.Forwards)
    loop: graphs.vertices.LoopVertex
    for loop in loop_forest.its_vertices:
        if loop != loop_forest.root:
            (header,) = loop.entries
            high_priority.append(header)

    random.shuffle(high_priority)
    while len(profile.vertex_points) < allocated_number and high_priority:
        vertex = high_priority.pop()
        profile.vertex_points.add(vertex)
        logger.debug(f"High priority: {vertex.identifier} in {cfg.name}")

    low_priority = [vertex for vertex in cfg.its_vertices if vertex not in profile.vertex_points]
    random.shuffle(low_priority)
    while len(profile.vertex_points) < allocated_number:
        vertex = low_priority.pop()
        profile.vertex_points.add(vertex)
        logger.debug(f"Low priority: {vertex.identifier} in {cfg.name}")

    assert allocated_number == len(profile.vertex_points)
    return profile


def store(output_dir: pathlib.Path,
          profiles: dict[graphs.graphs.ControlFlowGraph, graphs.graphs.InstrumentationProfile]):
    instrumentation_file = output_dir.joinpath("instrumentation.json")
    logger.info(f"Writing into output file '{instrumentation_file}'")
    with open(instrumentation_file, 'w', encoding='utf-8') as out_file:
        json_dict = {
            cfg.name: [str(vertex) for vertex in profile.vertex_points] for cfg, profile in profiles.items()
        }
        json.dump(json_dict, out_file, indent=2)
        out_file.write('\n' * 2)


def main(cfg_file: pathlib.Path, calls_file: pathlib.Path, budget: float):
    cfgs = graphs.graph_input.read_cfgs(cfg_file)
    logger.info(f"There are {len(cfgs)} CFGs")

    call_graph = graphs.graph_input.read_call_graph(calls_file, cfgs)
    logger.info(f"{call_graph.root_vertex.identifier} is the root of the call graph")

    allocation = allocate(cfgs, call_graph, budget)
    profiles: dict[graphs.graphs.ControlFlowGraph, graphs.graphs.InstrumentationProfile] = {}
    for cfg in cfgs.values():
        allocated_number = allocation[cfg]
        if allocated_number > 0:
            profile = instrument(cfg, call_graph, allocated_number)
            profiles[cfg] = profile

    store(cfg_file.parent, profiles)


if __name__ == '__main__':
    args = parse_the_command_line()
    main(args.program, args.calls, args.budget)
