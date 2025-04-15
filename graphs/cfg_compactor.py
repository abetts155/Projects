import argparse
import collections
import logging

from rich.logging import RichHandler

import graphs.edges
import graphs.graphs
import graphs.graph_input
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


def reduce(cfg: graphs.graphs.ControlFlowGraph, call_sites: list[graphs.vertices.Vertex]):
    logger.info(f"Compacting CFG '{cfg.name}'")
    visited = set()
    queue = collections.deque([cfg.entry_vertex])
    linear_sequences = {}
    while queue:
        root = queue.popleft()
        visited.add(root)

        linear_sequences[root] = [root]
        dead_ends = set()
        extended = True
        while extended:
            old_length = len(linear_sequences[root])
            vertex = linear_sequences[root][-1]

            if len(cfg.successors[vertex]) == 1 and vertex not in call_sites:
                (successor_edge,) = cfg.successors[vertex]
                successor = successor_edge.point_b

                if len(cfg.predecessors[successor]) == 1 and successor not in call_sites:
                    linear_sequences[root].append(successor)
                else:
                    dead_ends.add(successor)
            else:
                for successor_edge in cfg.successors[vertex]:
                    successor = successor_edge.point_b
                    dead_ends.add(successor)

            extended = old_length != len(linear_sequences[root])

        for vertex in dead_ends:
            if vertex not in visited:
                queue.append(vertex)

    for vertex, sequence in linear_sequences.items():
        if len(sequence) > 1:
            last = sequence[-1]
            for successor_edge in cfg.successors[last]:
                replacement_edge = graphs.edges.Edge(vertex, successor_edge.point_b)
                cfg.add_edge(replacement_edge)

            for dead in sequence[1:]:
                cfg.remove_vertex(dead)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Collapse linear chains in the control flow graphs (CFGs)')
    utils.command_line.add_cfg_options(parser, True)
    utils.command_line.add_call_graph_option(parser)
    return parser.parse_args()


def main():
    args = parse_the_command_line()
    cfgs = graphs.graph_input.read_cfgs(args.program, args.cfgs)
    call_graph = graphs.graph_input.read_call_graph(args.calls, cfgs)

    for subprogram in call_graph.its_vertices:
        cfg = cfgs[subprogram.identifier]
        reduce(cfg, call_graph.get_call_sites(subprogram))

    graphs.graph_input.write_cfgs(args.program, list(cfgs.values()))


if __name__ == '__main__':
    main()