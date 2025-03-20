import argparse
import collections
import concurrent.futures
import functools
import json
import logging
import math
import pydantic
import random
import rich
import time
import typing_extensions

from rich.logging import RichHandler

from utils import command_line
from graph import edges
from graph import graphs
from graph import vertices

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


class Configuration(pydantic.BaseModel):
    number_of_vertices: typing_extensions.Annotated[int, pydantic.Field(strict=True, ge=2, le=10**5)]
    control_flow_graphs: typing_extensions.Annotated[int, pydantic.Field(strict=True, ge=1, le=10**3)]
    loops:              pydantic.NonNegativeInt = 0
    multi_entry_loops:  pydantic.NonNegativeInt = 0
    multi_exit_loops:   pydantic.NonNegativeInt = 0
    nesting_depth:      pydantic.NonNegativeInt = 0
    local_spaghetti:    typing_extensions.Annotated[int, pydantic.Field(strict=True, ge=0, le=100)] = 0
    global_spaghetti:   typing_extensions.Annotated[int, pydantic.Field(strict=True, ge=0, le=100)] = 0
    spaghetti_timer:    typing_extensions.Annotated[int, pydantic.Field(strict=True, ge=1, le=10)] = 3

    @pydantic.model_validator(mode='after')
    def validate_loop_settings(self):
        lower_bound = 2 * (self.loops + 1)
        if self.number_of_vertices < lower_bound:
            raise ValueError(f"For {self.loops} loops, the CFG requires at least {lower_bound} vertices")

        if self.multi_entry_loops > self.loops:
            raise ValueError(f"The number of multi-entry loops '{self.multi_entry_loops}' exceeds the number of loops "
                             f"'{self.loops}'")

        if self.multi_exit_loops > self.loops:
            raise ValueError(f"The number of multi-exit loops '{self.multi_exit_loops}' exceeds the number of loops "
                             f"'{self.loops}'")

        if 0 < self.loops < self.nesting_depth:
            raise ValueError(f"The nesting depth '{self.nesting_depth}' exceeds the number of loops '{self.loops}'")

        if 0 == self.nesting_depth < self.loops:
            raise ValueError(f"The nesting depth must be greater than 0 when loops are being generated")

        return self


class GeneratorError(Exception):
    pass


def validate(cfg: graphs.ControlFlowGraph, hierarchy: graphs.Tree, config: Configuration):
    direction = graphs.GraphDirection.Forwards
    forest = graphs.LoopForest(cfg, cfg.get_origin(direction), cfg.get_traveller(direction))

    if len(forest.its_vertices) < len(hierarchy.its_vertices):
        raise GeneratorError(f"The CFG '{cfg.name}' has too few loops")
    elif len(forest.its_vertices) > len(hierarchy.its_vertices):
        raise GeneratorError(f"The CFG '{cfg.name}' has too many loops")

    multi_entry_loops: int = 0
    multi_exit_loops: int = 0
    loop: vertices.LoopVertex
    for loop in forest.its_vertices:
        if len(loop.entries) > 1:
            multi_entry_loops += 1

        if len(loop.exits) > 1:
            multi_exit_loops += 1

    if multi_entry_loops < config.multi_entry_loops:
        raise GeneratorError(f"The CFG '{cfg.name}' has too few multi-entry loops")
    elif multi_entry_loops > config.multi_entry_loops:
        raise GeneratorError(f"The CFG '{cfg.name}' has too many multi-entry loops")

    if multi_exit_loops < config.multi_exit_loops:
        raise GeneratorError(f"The CFG '{cfg.name}' has too few multi-exit loops")
    elif multi_exit_loops > config.multi_exit_loops:
        raise GeneratorError(f"The CFG '{cfg.name}' has too many multi-exit loops")

    if hierarchy.height() < config.nesting_depth:
        raise GeneratorError(f"The CFG '{cfg.name}' has a too shallow loop hierarchy")
    elif hierarchy.height() > config.nesting_depth:
        raise GeneratorError(f"The CFG '{cfg.name}' has a too deep loop hierarchy")

    if len(hierarchy.its_vertices) == 1 and not graphs.is_dag(cfg):
        raise GeneratorError(f"The CFG '{cfg.name}' is not a DAG")


def go_ahead(weight: float):
    return random.random() < weight


def create_loop_hierarchy(config: Configuration) -> graphs.Tree:
    # Add vertices to the tree, one per requested loop, as well as an extra vertex to represent the dummy outer loop.
    hierarchy = graphs.Tree()
    for i in range(config.loops + 1):
        vertex = vertices.Vertex(vertices.get_vertex_identifier('L'))
        hierarchy.add_vertex(vertex)

    # Connect loops, thus creating a hierarchy.
    vertex_to_level = {vertex: 0 for vertex in hierarchy.its_vertices}
    hierarchy.root = random.choice(list(hierarchy.its_vertices))

    parent_vertex = hierarchy.root
    for vertex in hierarchy.its_vertices:
        if vertex != hierarchy.root:
            new_level = vertex_to_level[parent_vertex] + 1
            if new_level <= config.nesting_depth:
                edge = edges.Edge(parent_vertex, vertex)
                hierarchy.add_edge(edge)
                vertex_to_level[vertex] = new_level
            else:
                # The height of the tree now exceeds the maximum depth, so backtrack to an arbitrary proper ancestor.
                ancestor_vertex = parent_vertex
                while True:
                    (ancestor_edge,) = hierarchy.predecessors[ancestor_vertex]
                    ancestor_vertex = ancestor_edge.point_a
                    if go_ahead(0.5) or ancestor_vertex == hierarchy.root:
                        break

                parent_vertex = ancestor_vertex
                edge = edges.Edge(parent_vertex, vertex)
                hierarchy.add_edge(edge)
                vertex_to_level[vertex] = vertex_to_level[parent_vertex] + 1
            parent_vertex = vertex

    return hierarchy


class Region:
    def __init__(self, vertex_set: set[vertices.Vertex]):
        assert len(vertex_set) >= 2
        self.vertices: set[vertices.Vertex] = vertex_set
        self.singles: set[vertices.Vertex] = set()
        a, b = random.sample(list(vertex_set), 2)
        self.entry: vertices.Vertex = a
        self.exit: vertices.Vertex = b

    def build(self, cfg: graphs.ControlFlowGraph):
        for vertex in self.vertices:
            if vertex not in [self.entry, self.exit]:
                edge = edges.Edge(self.entry, vertex)
                cfg.add_edge(edge)
                edge = edges.Edge(vertex, self.exit)
                cfg.add_edge(edge)
                self.singles.add(vertex)

        if len(self.vertices) == 2:
            edge = edges.Edge(self.entry, self.exit)
            cfg.add_edge(edge)

        if len(self.vertices) == 3 and go_ahead(0.5):
            edge = edges.Edge(self.entry, self.exit)
            cfg.add_edge(edge)


class BlackBox:
    def __init__(self):
        self.entries: set[vertices.Vertex] = set()
        self.exits: set[vertices.Vertex] = set()

    def pick_entries(self, region: Region, multiple_entries: bool):
        self.entries.add(region.entry)
        if multiple_entries:
            # Ensure that there are at least two entries; then pick liberally.
            candidates = list(region.vertices.difference(self.entries))
            while len(self.entries) < 2 or (candidates and go_ahead(0.05)):
                vertex = random.choice(candidates)
                self.entries.add(vertex)
                candidates.remove(vertex)

    def pick_exits(self, region: Region, multiple_exits: bool):
        if go_ahead(0.5):
            # Mimic a do-while loop construct.
            self.exits.add(region.exit)
        else:
            # Mimic a for-loop construct.
            self.exits.add(region.entry)

        if multiple_exits:
            # Ensure that there are at least two exits; then pick liberally.
            candidates = list(region.vertices.difference(self.exits))
            while len(self.exits) < 2 or (candidates and go_ahead(0.05)):
                vertex = random.choice(candidates)
                self.exits.add(vertex)
                candidates.remove(vertex)


def nest_regions(master: Region, slave: Region, cfg: graphs.ControlFlowGraph) -> Region:
    (predecessor,) = random.sample(list(master.singles), 1)
    master.singles.remove(predecessor)
    (unique_edge,) = cfg.successors[predecessor]
    successor = unique_edge.point_b

    # Should the chosen predecessor be a branch or not?
    if go_ahead(0.5):
        edge = edges.Edge(predecessor, successor)
        cfg.remove_edge(edge)

    edge = edges.Edge(predecessor, slave.entry)
    cfg.add_edge(edge)
    edge = edges.Edge(slave.exit, successor)
    cfg.add_edge(edge)

    master.vertices.update(slave.vertices)
    master.singles.update(slave.singles)
    return master


def stitch_regions(before: Region, after: Region, cfg: graphs.ControlFlowGraph) -> Region:
    edge = edges.Edge(before.exit, after.entry)
    cfg.add_edge(edge)
    before.singles.add(before.exit)
    before.exit = after.exit
    before.vertices.update(after.vertices)
    before.singles.update(after.singles)
    return before


def create_acyclic_region(vertex_list: list[vertices.Vertex], cfg: graphs.ControlFlowGraph) -> Region:
    # Partition the vertices into regions.
    partition = []
    minimum_size = 2
    maximum_size = 5
    while len(vertex_list) >= minimum_size:
        size = random.randint(minimum_size, min(maximum_size, len(vertex_list)))
        region_vertices = set()
        while len(region_vertices) < size:
            vertex = vertex_list.pop()
            region_vertices.add(vertex)
        partition.append(region_vertices)

    while vertex_list:
        random.shuffle(partition)
        vertex = vertex_list.pop()
        partition[-1].add(vertex)

    # Create separate single-entry, single-exit regions.
    regions = []
    for region_vertices in partition:
        region = Region(region_vertices)
        region.build(cfg)
        regions.append(region)

    # Create a monolithic single-entry, single-exit region.
    while len(regions) > 1:
        random.shuffle(regions)
        left = regions.pop()
        right = regions.pop()
        if left.singles and go_ahead(0.75):
            bigger = nest_regions(left, right, cfg)
        elif right.singles and go_ahead(0.75):
            bigger = nest_regions(right, left, cfg)
        else:
            if go_ahead(0.5):
                left, right = right, left
            bigger = stitch_regions(left, right, cfg)
        regions.append(bigger)

    region = regions.pop()
    return region


def get_topological_ordering(cfg: graphs.ControlFlowGraph, region: Region) -> list[vertices.Vertex]:
    ordering = []
    jailed = {vertex: 0 for vertex in region.vertices}
    freed = collections.deque([region.entry])
    while freed:
        vertex = freed.popleft()
        ordering.append(vertex)
        for edge in cfg.successors[vertex]:
            successor = edge.point_b
            jailed[successor] += 1
            if jailed[successor] == len(cfg.predecessors[successor]):
                freed.append(successor)
    return ordering


def add_spagetti_edges(cfg: graphs.ControlFlowGraph, legal_edges: set[edges.Edge], bound: int):
    # The given bound is a limit on the percentage of legal candidate edges to add to the CFG; choose a random
    # percentage greater than 0.
    chosen_percentage = random.randint(1, bound)
    chosen_number = round(chosen_percentage * len(legal_edges) / 100)
    logger.debug(f"Adding {chosen_percentage}% of the legal candidate edges (of size {len(legal_edges)}) to the CFG "
                 f"'{cfg.name}'")
    chosen_edges = random.sample(list(legal_edges), chosen_number)
    for edge in chosen_edges:
        cfg.add_edge(edge)


def create_local_spaghetti(cfg: graphs.ControlFlowGraph, region: Region, bound: int, time_limit_secs: int):
    ordering = get_topological_ordering(cfg, region)
    # If there are just two vertices V followed by W, then there is nothing to do as the edge V => W must exist.
    if len(ordering) > 2:
        # There are O(n**2) possible new edges, and this is too much in the general case. We impose a time limit to
        # allow the generation to complete in good time.

        legal_edges: set[edges.Edge] = set()
        now = start = time.time()
        selected_time_limit_secs = random.randint(1, time_limit_secs)
        predecessor_indices = list(range(0, len(ordering) - 1))
        while now - start <= selected_time_limit_secs or not legal_edges:
            now = time.time()
            i = random.choice(predecessor_indices)
            predecessor = ordering[i]
            successor_indices = list(range(i + 1, len(ordering)))
            j = random.choice(successor_indices)
            successor = ordering[j]
            edge = edges.Edge(predecessor, successor)
            if edge not in cfg.its_edges:
                legal_edges.add(edge)

        add_spagetti_edges(cfg, legal_edges, bound)


def connected_nested_loops(cfg: graphs.ControlFlowGraph,
                           region: Region,
                           black_box: BlackBox,
                           nested_black_boxes: list[BlackBox]):
    while len(nested_black_boxes) > 1 and go_ahead(0.5):
        left = nested_black_boxes.pop()
        right = nested_black_boxes.pop()
        for exit_vertex in left.exits:
            for entry_vertex in right.entries:
                edge = edges.Edge(exit_vertex, entry_vertex)
                cfg.add_edge(edge)

        coalesced = BlackBox()
        coalesced.entries = left.entries
        coalesced.exits = right.exits
        nested_black_boxes.append(coalesced)

    predecessor_indices = []
    ordering = get_topological_ordering(cfg, region)
    for index, vertex in enumerate(ordering):
        if vertex != region.exit:
            if len(cfg.successors[vertex]) == 1:
                predecessor_indices.append(index)

    if not predecessor_indices:
        # All vertices except the exit vertex of the region are candidate predecessors.
        predecessor_indices = list(range(0, len(ordering) - 1))

    for nested_black_box in nested_black_boxes:
        max_predecessor_index = 0
        for entry_vertex in nested_black_box.entries:
            predecessor_index = random.choice(predecessor_indices)
            max_predecessor_index = max(max_predecessor_index, predecessor_index)
            predecessor = ordering[predecessor_index]
            edge = edges.Edge(predecessor, entry_vertex)
            cfg.add_edge(edge)

        successor_index = random.randint(max_predecessor_index + 1, len(ordering) - 1)
        successor = ordering[successor_index]
        for exit_vertex in nested_black_box.exits:
            edge = edges.Edge(exit_vertex, successor)
            cfg.add_edge(edge)


def create_global_spaghetti(cfg: graphs.ControlFlowGraph, bound: int, time_limit_secs: int):
    assert cfg.entry_vertex is not None and cfg.exit_vertex is not None

    predecessors = [vertex for vertex in cfg.its_vertices if vertex != cfg.exit_vertex]
    successors = [vertex for vertex in cfg.its_vertices if vertex != cfg.entry_vertex]
    legal_edges: set[edges.Edge] = set()
    now = start = time.time()
    selected_time_limit_secs = random.randint(1, time_limit_secs)
    while now - start <= selected_time_limit_secs or not legal_edges:
        now = time.time()
        predecessor = random.choice(predecessors)
        successor = random.choice(successors)
        edge = edges.Edge(predecessor, successor)
        if edge not in cfg.its_edges:
            legal_edges.add(edge)

    add_spagetti_edges(cfg, legal_edges, bound)


def create_cfg(name: str, vertex_set: list[vertices.Vertex], config: Configuration) -> graphs.ControlFlowGraph:
    logger.info(f"Creating CFG '{name}'")
    cfg = graphs.ControlFlowGraph(name)
    for vertex in vertex_set:
        cfg.add_vertex(vertex)

    # Create outline of the loop hierarchy.
    hierarchy = create_loop_hierarchy(config)

    # Guarantee each loop has at least 2 vertices.
    remaining_vertices = list(vertex_set)
    partition = []
    for _ in hierarchy.its_vertices:
        partition.append([])
        vertex = remaining_vertices.pop()
        partition[-1].append(vertex)
        vertex = remaining_vertices.pop()
        partition[-1].append(vertex)

    # Arbitrarily distribute the remaining vertices.
    while remaining_vertices:
        vertex = remaining_vertices.pop()
        index = random.randint(0, len(partition) - 1)
        partition[index].append(vertex)

    # Decide which (non-artificial) loops have multiple entries and which have multiple exits.
    real_mccoy_loops = hierarchy.its_vertices - {hierarchy.root}
    multi_entry_loops = random.sample(list(real_mccoy_loops), config.multi_entry_loops)
    multi_exit_loops = random.sample(list(real_mccoy_loops), config.multi_exit_loops)

    loops: dict[vertices.Vertex, tuple[Region, BlackBox]] = {}
    for loop_vertex, vertex_list in zip(hierarchy.its_vertices, partition):
        region = create_acyclic_region(vertex_list, cfg)

        if config.local_spaghetti:
            create_local_spaghetti(cfg, region, config.local_spaghetti, config.spaghetti_timer)

        black_box = BlackBox()
        black_box.pick_entries(region, loop_vertex in multi_entry_loops)
        black_box.pick_exits(region, loop_vertex in multi_exit_loops)

        loops[loop_vertex] = (region, black_box)

    stack = collections.deque([hierarchy.root])
    loop_ordering = []
    while stack:
        loop_vertex = stack.popleft()
        loop_ordering.append(loop_vertex)
        for edge in hierarchy.successors[loop_vertex]:
            stack.append(edge.point_b)

    for loop_vertex in reversed(loop_ordering):
        region, black_box = loops[loop_vertex]

        if hierarchy.successors[loop_vertex]:
            nested_black_boxes = []
            for edge in hierarchy.successors[loop_vertex]:
                _, nested_black_box = loops[edge.point_b]
                nested_black_boxes.append(nested_black_box)
            connected_nested_loops(cfg, region, black_box, nested_black_boxes)

        if loop_vertex != hierarchy.root:
            # Turn the exit into a loop tail and the entry into a loop header.
            must_have_back_edge = edges.Edge(region.exit, region.entry)
            cfg.add_edge(must_have_back_edge)

    cfg.set_entry_and_exit()

    if config.global_spaghetti:
        # If we are creating global spaghetti then the loop hierarchy we worked hard so create will likely be in ruin,
        # and any attempt to validate the CFG properties against the loop config parameters will therefore fail.
        create_global_spaghetti(cfg, config.global_spaghetti, config.spaghetti_timer)
    else:
        validate(cfg, hierarchy, config)

    sparse_upper_bound = len(cfg.its_vertices) * math.log(len(cfg.its_vertices))
    if len(cfg.its_edges) <= sparse_upper_bound:
        if graphs.is_dag(cfg):
            description = 'sparse and acyclic'
        else:
            description = 'sparse and cyclic'
    else:
        if graphs.is_dag(cfg):
            description = 'dense and acyclic'
        else:
            description = 'dense and cyclic'

    logger.debug(f"The CFG '{cfg.name}' has {len(cfg.its_vertices)} vertices and {len(cfg.its_edges)} edges; "
                 f"it is {description}")
    return cfg


def generate(config_filename: str) -> list[graphs.ControlFlowGraph]:
    logger.info(f"Reading configuration file '{config_filename}'")
    with open(config_filename, 'r') as in_file:
        data = json.load(in_file)
        config = Configuration(**data)
        rich.print(config)

    vertex_set = []
    for _ in range(1, config.number_of_vertices + 1):
        identifier = vertices.get_vertex_identifier(vertices.Vertex.IDENTIFIER_PREFIX)
        vertex = vertices.Vertex(identifier)
        vertex_set.append(vertex)

    cfgs = []
    worker = functools.partial(create_cfg, vertex_set=vertex_set, config=config)
    with concurrent.futures.ProcessPoolExecutor(max_workers=4) as executor:
        cfg_names = [f'C{i}' for i in range(1, config.control_flow_graphs + 1)]
        for cfg in executor.map(worker, cfg_names):
            cfgs.append(cfg)

    return cfgs


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Generate random control flow graphs (CFGs)')

    command_line.add_config_option(parser)

    parser.add_argument('-O',
                        '--output',
                        help='write the CFGs to this file',
                        metavar='<FILE>.json',
                        required=True)

    args = parser.parse_args()

    if args.config == args.output:
        logger.error(f"The configuration and output files are the same: '{args.config}'")

    return args


def main():
    args = parse_the_command_line()
    cfgs = generate(args.config)

    logger.info(f"Writing into output file '{args.output}'")
    with open(args.output, 'w') as out_file:
        dicts = []
        for cfg in cfgs:
            dicts.append(cfg.to_json())
        json.dump(dicts, out_file, indent=2)
        out_file.write('\n' * 2)


if __name__ == '__main__':
    main()
