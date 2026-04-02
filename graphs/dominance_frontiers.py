import argparse
import collections
import logging
import pathlib
import sys
import threading

from rich.logging import RichHandler

import graphs.dominators
import graphs.edges
import graphs.graph_input
import graphs.graphs
import graphs.vertices
import utils.command_line

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


def cycling_edges(origin: graphs.vertices.Vertex, traveller: graphs.graphs.GraphTraveller) -> set[graphs.edges.Edge]:
    visited = set()
    stack = set()
    back_edges = set()

    def dfs(vertex: graphs.vertices.Vertex):
        visited.add(vertex)
        stack.add(vertex)

        for edge in traveller.forwards[vertex]:
            step = traveller.forward(edge)
            if step not in visited:
                dfs(step)
            elif step in stack:
                back_edges.add(edge)

        stack.remove(vertex)

    dfs(origin)
    return back_edges



def jail_and_free(
        traveller: graphs.graphs.GraphTraveller,
        vertex: graphs.vertices.Vertex,
        effective_predecessors: dict[graphs.vertices.Vertex, int],
        back_edges: set[graphs.edges.Edge]
) -> set[graphs.vertices.Vertex]:
    counts = {}
    queue = collections.deque([vertex])
    released = set()
    frontier = set()
    while queue:
        searcher = queue.popleft()
        released.add(searcher)

        for edge in traveller.forwards[searcher]:
            step = traveller.forward(edge)

            if edge in back_edges:
                if step not in released or step == vertex:
                    frontier.add(step)
            else:
                if step not in counts:
                    counts[step] = 0
                counts[step] += 1

                if counts[step] < effective_predecessors[step]:
                    frontier.add(step)
                else:
                    if effective_predecessors[step] > 1:
                        frontier.remove(step)
                    queue.append(step)

    return frontier


def main(file: pathlib.Path, name_filter: list[str]):
    cfgs = graphs.graph_input.read_cfgs(file, name_filter)

    for cfg in cfgs.values():
        #cfg.dotify(f"{cfg.name}")

        for direction in graphs.graphs.GraphDirection:
            print(cfg.name, direction)
            dominator = graphs.dominators.LengauerTarjan(cfg, direction)
            traveller = cfg.get_traveller(direction)
            df = graphs.dominators.DominanceFrontiers(cfg, dominator, direction)

            back_edges = cycling_edges(cfg.get_origin(direction), traveller)
            effective_predecessors = {vertex: 0 for vertex in cfg.its_vertices}
            for edge in cfg.its_edges:
                if edge not in back_edges:
                    if direction == graphs.graphs.GraphDirection.Forwards:
                        effective_predecessors[edge.point_b] += 1
                    else:
                        effective_predecessors[edge.point_a] += 1

            for vertex in cfg.its_vertices:
                gold_frontier = {edge.point_b for edge in df.successors[vertex]}
                #print(f"DF({vertex.identifier}) = {{ {', '.join(str(join) for join in gold_frontier)} }}")
                frontier = jail_and_free(traveller, vertex, effective_predecessors, back_edges)
                #print(f"DF({vertex.identifier}) = {{ {', '.join(str(join) for join in frontier)} }}")
                assert frontier == gold_frontier


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Do dominance frontier analysis')
    utils.command_line.add_cfg_options(parser, True)
    return parser.parse_args()


if __name__ == '__main__':
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 30)
    args = parse_the_command_line()
    main(args.program, args.cfgs)
