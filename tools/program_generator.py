from argparse import Action, ArgumentError, ArgumentParser, Namespace
from graphs import edges, graphs, vertices
from low_level import instructions
from random import choice, choices, getrandbits, random, randint, sample, shuffle
from sys import setrecursionlimit
from system import programs
from threading import stack_size
from typing import Dict, List
from utils.messages import debug_message, error_message


def go_ahead(weight=None):
    if weight:
        return random() < weight
    else:
        return bool(getrandbits(1))


class LoopBody:
    def __init__(self,
                 cfg: graphs.ControlFlowGraph,
                 number_of_vertices: int,
                 dense: bool,
                 nested_loops: List,
                 artificial: bool,
                 multi_entry: bool):
        assert number_of_vertices >= 2
        if number_of_vertices <= 3:
            # Guarantee a separate level for: (a) the entry, (b) the exit, and (c) the spare vertex, if there is one.
            total_levels = number_of_vertices
        else:
            # The rationale here is to allow all levels to have access to multiple vertices. This will prevent all
            # vertices being bunched into a single level or all levels having access to just a single vertex; in both
            # cases, the generation is tied to specific structures.
            total_levels = number_of_vertices // 2 + 1

        self._level_to_vertices = {}
        for level in range(total_levels + 1):
            self._level_to_vertices[level] = []

        self._lowest_level = 0
        self._highest_level = total_levels - 1
        self._vertex_to_level = {}

        self._vertices = []
        self.add_vertices(cfg, number_of_vertices)
        self._entry_vertex, self._exit_vertex = sample(self._vertices, 2)
        self._entries = {self._entry_vertex}
        self._exits = {self._exit_vertex}

        self.position_vertices(number_of_vertices)
        self.add_edges(cfg)
        self.connect_nested_loops(cfg, nested_loops)
        self.connect_dead_ends(cfg)
        self.add_more_forward_branches(cfg)

        if multi_entry:
            self.pick_more_entries()

        if not artificial:
            self.pick_more_exits()
            cfg.add_edge(edges.Edge(self._exit_vertex, self._entry_vertex))
            self.add_backedges(cfg)

        if dense:
            self.densify(cfg)

    def add_vertices(self, cfg: graphs.ControlFlowGraph, number_of_vertices: int):
        while number_of_vertices > 0:
            vertex = vertices.BasicBlock(vertices.Vertex.get_vertex_id())
            cfg.add_vertex(vertex)
            self._vertices.append(vertex)
            number_of_vertices -= 1

    def position_vertices(self, number_of_vertices: int):
        self._level_to_vertices[self._lowest_level].append(self._entry_vertex)
        self._vertex_to_level[self._entry_vertex] = self._lowest_level

        self._level_to_vertices[self._highest_level].append(self._exit_vertex)
        self._vertex_to_level[self._exit_vertex] = self._highest_level

        candidates = [vertex for vertex in self._vertices if vertex not in [self._entry_vertex, self._exit_vertex]]
        if candidates:
            for level in range(self._lowest_level + 1, self._highest_level):
                vertex = candidates.pop()
                self._level_to_vertices[level].append(vertex)
                self._vertex_to_level[vertex] = level

        while candidates:
            vertex = candidates.pop()
            level = randint(self._lowest_level + 1, self._highest_level - 1)
            self._level_to_vertices[level].append(vertex)
            self._vertex_to_level[vertex] = level

    def add_edges(self, cfg: graphs.ControlFlowGraph):
        preferred = {}
        for level in range(self._lowest_level, self._highest_level):
            preferred[level] = [vertex for vertex in self._level_to_vertices[level] if len(cfg.successors(vertex)) == 0]

        for level in range(self._lowest_level + 1, self._highest_level + 1):
            for successor in self._level_to_vertices[level]:
                options = preferred[level - 1]
                if options:
                    predecessor = choice(options)
                    options.remove(predecessor)
                else:
                    predecessor = choice(self._level_to_vertices[level - 1])

                cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))

    def connect_nested_loops(self, cfg: graphs.ControlFlowGraph, nested_loops: List["ArtificialLoopBody"]):
        options = [vertex for vertex in self._vertices
                   if len(cfg.successors(vertex)) == 0 and self._vertex_to_level[vertex] < self._highest_level]

        for loop in nested_loops:
            if options:
                candidate_predecessors = options
            else:
                candidate_predecessors = [vertex for vertex in self._vertices
                                          if self._vertex_to_level[vertex] < self._highest_level]

            highest_level = 0
            for entry_vertex in loop.entries:
                predecessor = choice(candidate_predecessors)
                cfg.add_edge(edges.ControlFlowEdge(predecessor, entry_vertex))
                highest_level = max(highest_level, self._vertex_to_level[predecessor])

            candidate_successors = [vertex for vertex in self._vertices
                                    if self._vertex_to_level[vertex] > highest_level]
            for exit_vertex in loop.exits:
                successor = choice(candidate_successors)
                cfg.add_edge(edges.ControlFlowEdge(exit_vertex, successor))

    def connect_dead_ends(self, cfg: graphs.ControlFlowGraph):
        for level in range(self._lowest_level, self._highest_level):
            dead = [vertex for vertex in self._level_to_vertices[level] if len(cfg.successors(vertex)) == 0]
            for predecessor in dead:
                higher_level = randint(level + 1, self._highest_level)
                successor = choice(self._level_to_vertices[higher_level])
                cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))

    def add_more_forward_branches(self, cfg: graphs.ControlFlowGraph):
        stepping_stones = [vertex for vertex in self._vertices if len(cfg.successors(vertex)) == 1]
        while stepping_stones and go_ahead(0.2):
            predecessor = choice(stepping_stones)
            higher_level = randint(self._vertex_to_level[predecessor] + 1, self._highest_level)
            successor = choice(self._level_to_vertices[higher_level])
            if not cfg.has_edge(predecessor, successor):
                stepping_stones.remove(predecessor)
                cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))

    def pick_more_entries(self):
        candidates = [vertex for vertex in self._vertices if vertex != self._entry_vertex]

        # Ensure there are at least two entries; then pick liberally.
        vertex = choice(candidates)
        self._entries.add(vertex)
        candidates.remove(vertex)

        while candidates and go_ahead(0.05):
            vertex = choice(candidates)
            self._entries.add(vertex)
            candidates.remove(vertex)

    def pick_more_exits(self):
        candidates = [vertex for vertex in self._vertices if vertex != self._exit_vertex]
        while candidates and go_ahead(0.05):
            vertex = choice(candidates)
            self._exits.add(vertex)
            candidates.remove(vertex)

    def add_backedges(self, cfg: graphs.ControlFlowGraph):
        has_backedges = randint(0, len(self._entries))
        chosen_headers = sample(self._entries, has_backedges)
        candidate_tails = [vertex for vertex in self._vertices if vertex not in self._entries]
        if candidate_tails:
            for header_vertex in chosen_headers:
                tail_vertex = choice(candidate_tails)
                if not cfg.has_edge(tail_vertex, header_vertex):
                    cfg.add_edge(edges.ControlFlowEdge(tail_vertex, header_vertex))

    def densify(self, cfg: graphs.ControlFlowGraph):
        for level in range(self._lowest_level + 1, self._highest_level + 1):
            for successor in self._level_to_vertices[level]:
                lower_level = randint(self._lowest_level, level - 1)
                for predecessor in self._level_to_vertices[lower_level]:
                    if go_ahead(0.5) and not cfg.has_edge(predecessor, successor):
                        cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))

    @property
    def entries(self):
        return self._entries

    @property
    def exits(self):
        return self._exits


def create_loop_hierarchy(total_loops: int, nesting_depth: int, number_of_vertices: int):
    # Add vertices to the tree, one per requested loop, as well as an extra vertex to represent the dummy outer loop.
    lnt = graphs.DirectedGraph()
    for _ in range(1, total_loops + 2):
        lnt.add_vertex(vertices.Vertex(vertices.Vertex.get_vertex_id()))

    # Connect loops, thus creating a hierarchy.
    vertex_to_level = {vertex: 0 for vertex in lnt}
    (root_vertex,) = sample(vertex_to_level.keys(), 1)
    parent_vertex = root_vertex
    for vertex in lnt:
        if vertex != root_vertex:
            new_level = vertex_to_level[parent_vertex] + 1
            if new_level <= nesting_depth:
                lnt.add_edge(edges.Edge(parent_vertex, vertex))
                vertex_to_level[vertex] = new_level
            else:
                # The height of the tree now exceeds the maximum depth, so backtrack to an arbitrary proper ancestor.
                ancestor_vertex = parent_vertex
                while True:
                    (edge,) = lnt.predecessors(ancestor_vertex)
                    ancestor_vertex = edge.predecessor()
                    if go_ahead() or ancestor_vertex == root_vertex:
                        break
                parent_vertex = ancestor_vertex
                lnt.add_edge(edges.Edge(parent_vertex, vertex))
                vertex_to_level[vertex] = vertex_to_level[parent_vertex] + 1
            parent_vertex = vertex

    # Compute number of vertices in each loop.
    number_of_vertices_remaining = number_of_vertices
    for vertex in lnt:
        # Guarantee each loop has at least 2 vertices plus vertices needed to connect inner nested loops.
        vertex.size = 2 + len(lnt.successors(vertex))
        number_of_vertices_remaining -= vertex.size

    # Arbitrarily distribute any remaining vertices to the loop bodies.
    while number_of_vertices_remaining > 0:
        choices = [vertex for vertex in lnt]
        shuffle(choices)
        for vertex in choices:
            additional_vertices = randint(0, number_of_vertices_remaining)
            vertex.size += additional_vertices
            number_of_vertices_remaining -= additional_vertices

    return lnt, root_vertex


def create_loop_bodies(cfg: graphs.ControlFlowGraph,
                       lnt: graphs.DirectedGraph,
                       root_vertex: vertices.Vertex,
                       vertex: vertices.Vertex,
                       is_multi_entry: Dict[vertices.Vertex, bool],
                       bodies: Dict[vertices.Vertex, LoopBody],
                       dense: bool):
    for edge in lnt.successors(vertex):
        create_loop_bodies(cfg, lnt, root_vertex, edge.successor(), is_multi_entry, bodies, dense)

    is_root_vertex = len(lnt.predecessors(vertex)) == 0
    bodies[vertex] = LoopBody(cfg,
                              vertex.size,
                              dense,
                              [bodies[edge.successor()] for edge in lnt.successors(vertex)],
                              vertex == root_vertex,
                              is_multi_entry[vertex])

    if vertex == root_vertex:
        body = bodies[vertex]
        (cfg.entry,) = body.entries
        (cfg.exit,) = body.exits


def is_connected(cfg: graphs.ControlFlowGraph, origin: vertices.Vertex):
    if origin == cfg.entry:
        tentacles = graphs.DirectedGraph.successors
        tentacle = edges.Edge.successor
    else:
        tentacles = graphs.DirectedGraph.predecessors
        tentacle = edges.Edge.predecessor

    visited = {}
    readied = {}
    for vertex in cfg:
        visited[vertex] = False
        readied[vertex] = False

    readied[origin] = True
    stack = [origin]
    while stack:
        vertex = stack.pop()
        visited[vertex] = True
        readied[vertex] = False

        for edge in tentacles(cfg, vertex):
            next_vertex = tentacle(edge)
            if not visited[next_vertex] and not readied[next_vertex]:
                stack.append(next_vertex)
                readied[next_vertex] = True

    for vertex in cfg:
        if not visited[vertex]:
            return False

    return True


def is_well_related(cfg: graphs.ControlFlowGraph):
    visited = set()
    for vertex in cfg:
        visited.clear()
        for edge in cfg.successors(vertex):
            successor = edge.successor()
            if successor in visited:
                print('Violation', vertex, successor)
                return False
            elif vertex == successor:
                print('Self', vertex, successor)
                return False
            else:
                visited.add(successor)
    return True


def is_valid(cfg: graphs.ControlFlowGraph):
    if len(cfg.predecessors(cfg.entry)) > 0:
        return False

    if len(cfg.successors(cfg.exit)) > 0:
        return False

    if not is_connected(cfg, cfg.entry):
        print('Not connected from E')
        return False

    if not is_connected(cfg, cfg.exit):
        print('Not connected from X')
        return False

    if not is_well_related(cfg):
        return False

    return True


def create_control_flow_graph(program: programs.Program,
                              total_loops: int,
                              multi_entry_loops: int,
                              nesting_depth: int,
                              number_of_vertices: int,
                              dense: bool,
                              subprogram_name: str):
    cfg = graphs.ControlFlowGraph(program, subprogram_name)

    # Create outline of the loop hierarchy.
    lnt, root_vertex = create_loop_hierarchy(total_loops, nesting_depth, number_of_vertices)

    # Decide which loops have multiple entries.
    is_multi_entry = {}
    candidates = []
    for vertex in lnt:
        is_multi_entry[vertex] = False
        if vertex != root_vertex:
            candidates.append(vertex)

    while multi_entry_loops:
        multi_entry_loops -= 1
        vertex = choice(candidates)
        candidates.remove(vertex)
        is_multi_entry[vertex] = True

    bodies = {}
    create_loop_bodies(cfg, lnt, root_vertex, root_vertex, is_multi_entry, bodies, dense)
    cfg.dotify()
    assert is_valid(cfg), 'Unable to produce a valid CFG'
    return cfg


def add_subprograms(program: programs.Program,
                    subprograms: int,
                    total_loops: int,
                    multi_entry_loops: int,
                    nesting_depth: int,
                    number_of_vertices: int,
                    dense: bool):
    for subprogram_name in ['s{}'.format(i) for i in range(1, subprograms + 1)]:
        debug_message('Creating CFG with name {}'.format(subprogram_name))
        cfg = create_control_flow_graph(program,
                                        total_loops,
                                        multi_entry_loops,
                                        nesting_depth,
                                        number_of_vertices,
                                        dense,
                                        subprogram_name)
        call_vertex = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), subprogram_name)
        program.add_subprogram(programs.Subprogram(cfg, call_vertex))


def add_calls(program: programs.Program, recursion_enabled):
    subprogram_call_candidates = {}
    for subprogram in program:
        subprogram_call_candidates[subprogram] = []
        # Work out which basic blocks can legitimately make calls
        dfs = graphs.DepthFirstSearch(subprogram.cfg, subprogram.cfg.entry)
        for vertex in subprogram.cfg:
            if len(subprogram.cfg.successors(vertex)) == 1 and vertex != subprogram.cfg.exit:
                (edge,) = subprogram.cfg.successors(vertex)
                # Check that the sole successor is not a loop header
                if edge not in dfs.back_edges(vertex):
                    subprogram_call_candidates[subprogram].append(vertex)

    # Pick a root subprogram with the largest number of call candidates
    root_subprogram = choice(list(subprogram_call_candidates.keys()))
    for subprogram in program:
        if len(subprogram_call_candidates[subprogram]) > len(subprogram_call_candidates[root_subprogram]):
            root_subprogram = subprogram

    # Set the call depth of each subprogram
    subprogram_to_level = {}
    level_to_subprograms = {}
    lowest_level = level = 1
    subprogram_to_level[root_subprogram] = level - 1
    level_to_subprograms[level - 1] = [root_subprogram]
    level_to_subprograms[level] = []
    for subprogram in program:
        if subprogram != root_subprogram:
            total_candidates = sum(len(subprogram_call_candidates[subprogram])
                                   for subprogram in level_to_subprograms[level - 1])
            if total_candidates == len(level_to_subprograms[level]):
                level += 1
            elif random() < 0.05 and level_to_subprograms[level]:
                level += 1
            subprogram_to_level[subprogram] = level
            level_to_subprograms.setdefault(level, []).append(subprogram)

    highest_level = level
    call_graph_edges = {}

    def add_call(callers, callee):
        (caller,) = sample(callers, 1)
        (vertex,) = sample(subprogram_call_candidates[caller], 1)
        vertex.instructions.append(instructions.CallInstruction(callee.name))
        subprogram_call_candidates[caller].remove(vertex)
        key = (caller, callee)
        call_graph_edges.setdefault(key, []).append(vertex)

    # Guarantee each subprogram is reachable from the root
    for level in range(lowest_level, highest_level + 1):
        callees = level_to_subprograms[level]
        for callee in callees:
            callers = level_to_subprograms[level - 1]
            callers = [caller for caller in callers if subprogram_call_candidates[caller]]
            add_call(callers, callee)

    for level in reversed(range(lowest_level, highest_level + 1)):
        callees = level_to_subprograms[level]
        for callee in callees:
            while True:
                lower_level = randint(0, level - 1)
                callers = level_to_subprograms[lower_level]
                callers = [caller for caller in callers if subprogram_call_candidates[caller]]
                if callers and random() < 0.66:
                    add_call(callers, callee)
                else:
                    break

    if recursion_enabled:
        for level in reversed(range(lowest_level, highest_level + 1)):
            callers = level_to_subprograms[level]
            callers = [caller for caller in callers if subprogram_call_candidates[caller]]
            if callers and bool(getrandbits(1)):
                lower_level = randint(0, level - 1)
                callees = level_to_subprograms[lower_level]
                if callees and random() < 0.25:
                    (callee,) = sample(callees, 1)
                    add_call(callers, callee)

    for (caller, callee), sites in call_graph_edges.items():
        for site in sites:
            edge = edges.CallGraphEdge(program[caller.name].call_vertex,
                                       program[callee.name].call_vertex,
                                       site)
            program.call_graph.add_edge(edge)


def add_instructions(program: programs.Program):
    debug_message('Adding instructions')
    for subprogram in program:
        for vertex in subprogram.cfg:
            if random() < 0.05:
                number_of_instructions = randint(10, 20)
            else:
                number_of_instructions = randint(3, 8)

            if len(subprogram.cfg.successors(vertex)) == 1:
                (edge,) = subprogram.cfg.successors(vertex)
                callee = program.call_graph.is_call_site(subprogram.call_vertex, vertex)
                if callee:
                    number_of_instructions -= 1

            for count in range(1, number_of_instructions + 1):
                if len(subprogram.cfg.successors(vertex)) >= 2 and count == number_of_instructions:
                    vertex.instructions.insert(len(vertex.instructions), instructions.BranchInstruction())
                else:
                    # Prefer arithmetic instructions over load-store instructions
                    if random() < 0.33:
                        population = [instructions.LoadInstruction,
                                      instructions.StoreInstruction]
                        weights = [0.5, 0.5]
                    else:
                        population = [instructions.AddInstruction,
                                      instructions.SubtractInstruction,
                                      instructions.MultiplyInstruction,
                                      instructions.DivideInstruction]
                        weights = [0.55, 0.35, 0.05, 0.05]
                    (instruction,) = choices(population, weights=weights)
                    vertex.instructions.insert(0, instruction())


def main(args: Namespace):
    if args.vertices < args.loops * 2:
        error_message('The number of vertices in a control flow graph must be at least twice the number of loops')

    if args.multi > args.loops:
        error_message('The number of multi-entry loops cannot exceed the number of loops')

    program = programs.Program(args.program)
    add_subprograms(program,
                    args.subprograms,
                    args.loops,
                    args.multi,
                    args.nesting_depth,
                    args.vertices,
                    args.dense)

    if not args.no_calls:
        add_calls(program, args.recursion)

    if not args.no_instructions:
        add_instructions(program)

    programs.IO.write(program, args.program)
    program.call_graph.dotify()


class CheckForPositiveValue(Action):
    def __call__(self, parser, namespace, value, option_string=None):
        if value <= 0:
            raise ArgumentError('Argument {} requires a positive integer'.format(option_string))
        setattr(namespace, self.dest, value)


def parse_the_command_line():
    parser = ArgumentParser(description='Generate a random program')

    parser.add_argument('--program',
                        help='write the program to this file',
                        required=True)

    parser.add_argument('--subprograms',
                        action=CheckForPositiveValue,
                        type=int,
                        help='the number of subprograms',
                        metavar='<INT>',
                        default=1)

    parser.add_argument('--loops',
                        type=int,
                        help='the number of loops in a CFG',
                        metavar='<INT>',
                        default=0)

    parser.add_argument('--multi',
                        type=int,
                        help='the number of multi-entry loops in a CFG',
                        metavar='<INT>',
                        default=0)

    parser.add_argument('--nesting-depth',
                        type=int,
                        help='the maximum nesting depth of loops',
                        metavar='<INT>',
                        default=1)

    parser.add_argument('--vertices',
                        type=int,
                        action=CheckForPositiveValue,
                        help='the number of basic blocks in a CFG',
                        metavar='<INT>',
                        default=10)

    parser.add_argument('--dense',
                        action='store_true',
                        help='allow dense graphs',
                        default=False)

    parser.add_argument('--recursion',
                        action='store_true',
                        help='allow recursive calls',
                        default=False)

    parser.add_argument('--no-calls',
                        action='store_true',
                        help='do not add calls between subprograms',
                        default=False)

    parser.add_argument('--no-instructions',
                        action='store_true',
                        help='do not add instructions to basic blocks',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    stack_size(2 ** 26)
    setrecursionlimit(2 ** 30)
    args = parse_the_command_line()
    main(args)
