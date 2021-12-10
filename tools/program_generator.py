from argparse import Action, ArgumentError, ArgumentParser
from graphs import edges, graphs, vertices
from low_level import instructions
from random import choice, getrandbits, random, randint, sample
from sys import setrecursionlimit
from system import programs
from threading import stack_size
from time import time
from utils import messages


def go_ahead(weight=None):
    if weight:
        return random() < weight
    else:
        return bool(getrandbits(1))


class ArtificialLoopBody:
    def __init__(self, fan_out, cfg, number_of_vertices, nested_loops: list, outermost_loop):
        self._header = None
        self._exits = set()
        self._vertices = set()
        self.add_vertices(cfg, number_of_vertices)
        self.level_to_vertices = {}
        self._vertex_to_level = {}
        self.position_vertices(fan_out, len(nested_loops), outermost_loop)
        self.add_edges(cfg, fan_out)
        self.connect_nested_loops(cfg, nested_loops)
        self.connect_terminal_vertices(cfg)
        self.add_backedges(cfg)
        self.set_exits(cfg)

    @property
    def header(self):
        return self._header

    @property
    def exits(self):
        return self._exits

    def add_vertices(self, cfg, number_of_vertices):
        while number_of_vertices > 0:
            basic_block = vertices.BasicBlock(vertices.Vertex.get_vertex_id())
            cfg.add_vertex(basic_block)
            self._vertices.add(basic_block)
            number_of_vertices -= 1

    def position_vertices(self, fan_out, number_of_nested_loops, outermost_loop):
        level = 0
        for vertex in self._vertices:
            self._vertex_to_level[vertex] = level
            self.level_to_vertices.setdefault(level, []).append(vertex)

            if level == 0:
                self._header = vertex
                level += 1
            elif number_of_nested_loops > 0:
                number_of_nested_loops -= 1
                level += 1
            elif len(self.level_to_vertices[level]) == fan_out * len(self.level_to_vertices[level - 1]):
                level += 1
            elif go_ahead(0.25):
                level += 1

        highest_level = max(self.level_to_vertices.keys())
        loop_tails = self.level_to_vertices[highest_level]

        # Does this loop body have too many loop tails?
        # Yes if either
        # a) this is the outermost loop and there is no single loop tail, or
        # b) we decide the proportion of loop tails to the number of loop body
        #    vertices is too great.
        if len(loop_tails) > 1:
            if outermost_loop or len(loop_tails)/len(self._vertex_to_level) > 0.2:
                # Promote a random vertex to be the unique loop tail
                new_highest_level = highest_level + 1
                v = loop_tails[randint(0, len(loop_tails)-1)]
                self.level_to_vertices.setdefault(new_highest_level, []).append(v)
                self.level_to_vertices[highest_level].remove(v)
                self._vertex_to_level[v] = new_highest_level

    def add_edges(self, cfg, fan_out):
        for level in sorted(self.level_to_vertices.keys(), reverse=True):
            if level > 0:
                for s in self.level_to_vertices[level]:
                    candidates = [v for v in self.level_to_vertices[level-1] if len(cfg.successors(v)) < fan_out]
                    (p,) = sample(candidates, 1)
                    cfg.add_edge(edges.ControlFlowEdge(p, s))
                    if len(cfg.successors(p)) == fan_out:
                        candidates.remove(p)

    def connect_nested_loops(self, cfg, nested_loops):
        highest_level = max(self.level_to_vertices.keys())
        candidate_entry_sources = [v for v in self._vertices if 0 < self._vertex_to_level[v] < highest_level]
        for loop in nested_loops:
            (p,) = sample(candidate_entry_sources, 1)
            cfg.add_edge(edges.ControlFlowEdge(p, loop.header))
            for exit_source in loop.exits:
                higher_level = randint(self._vertex_to_level[p] + 1, highest_level)
                candidate_exit_destinations = self.level_to_vertices[higher_level]
                (s,) = sample(candidate_exit_destinations, 1)
                cfg.add_edge(edges.ControlFlowEdge(exit_source, s))

    def connect_terminal_vertices(self, cfg):
        highest_level = max(self.level_to_vertices.keys())
        for level in sorted(self.level_to_vertices.keys(), reverse=True):
            if 0 < level < highest_level:
                candidate_predecessors = [v for v in self.level_to_vertices[level] if len(cfg.successors(v)) == 0]
                higher_level = randint(level + 1, highest_level)
                candidate_successors = self.level_to_vertices[higher_level]
                for p in candidate_predecessors:
                    (s,) = sample(candidate_successors, 1)
                    cfg.add_edge(edges.ControlFlowEdge(p, s))

    def add_backedges(self, cfg):
        highest_level = max(self.level_to_vertices.keys())
        for v in self.level_to_vertices[highest_level]:
            cfg.add_edge(edges.ControlFlowEdge(v, self._header))

    def set_exits(self, cfg):
        selection_probability = 1.0
        for vertex in self._vertex_to_level:
            if len(cfg.successors(vertex)) == 1 and selection_probability > random() and vertex != self._header:
                self._exits.add(vertex)
                selection_probability /= 2

        if not self._exits:
            self._exits.add(self._header)


def create_control_flow_graph(the_program,
                              loops,
                              nesting_depth,
                              dense,
                              irreducible,
                              number_of_vertices,
                              fan_out,
                              subprg_name):
    def create_artificial_loop_hierarchy():
        # Add abstract vertices to the tree, including an extra one
        # for the dummy outer loop
        lnt = graphs.DirectedGraph()
        for _ in range(1, loops+2):
            lnt.add_vertex(vertices.Vertex(vertices.Vertex.get_vertex_id()))

        # Add edges to the tree
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
                    # The height of the tree now exceeds the maximum depth, so
                    # backtrack to an arbitrary proper ancestor
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

        # Compute number of vertices in each loop
        number_of_vertices_remaining = number_of_vertices
        for vertex in lnt:
            # Guarantee each loop has at least 2 vertices plus vertices needed
            # to connect inner nested loops
            vertex.size = 2 + len(lnt.successors(vertex))
            number_of_vertices_remaining -= vertex.size

        # Arbitrarily distribute any remaining vertices to the loop bodies
        while number_of_vertices_remaining > 0:
            for vertex in lnt:
                additional_vertices = randint(0, number_of_vertices_remaining)
                vertex.size += additional_vertices
                number_of_vertices_remaining -= additional_vertices
        return lnt, root_vertex

    def create_loop_body(vertex):
        for edge in lnt.successors(vertex):
            create_loop_body(edge.successor())

        # Post-order actions
        nested_loops = [loops[edge.successor()] for edge in lnt.successors(vertex)]
        loops[vertex] = ArtificialLoopBody(fan_out, cfg, vertex.size, nested_loops, len(lnt.predecessors(vertex)) == 0)

        if len(lnt.predecessors(vertex)) == 0:
            (edge,) = cfg.predecessors(loops[vertex].header)
            cfg.entry = edge.successor()
            cfg.exit = edge.predecessor()

    cfg = graphs.ControlFlowGraph(the_program, subprg_name)
    if not irreducible:
        lnt, root_vertex = create_artificial_loop_hierarchy()
        loops = {}
        create_loop_body(root_vertex)
    else:
        loop = ArtificialLoopBody(fan_out, cfg, number_of_vertices, [], True)
        (edge,) = cfg.predecessors(loop.header)
        cfg.entry = edge.successor()
        cfg.exit = edge.predecessor()
        candidates = [vertex for vertex in cfg if vertex != cfg.entry and vertex != cfg.exit]

        min_level = min(loop.level_to_vertices.keys())
        max_level = max(loop.level_to_vertices.keys())
        candidate_levels = [level for level in range(min_level+1, max_level)]

        if dense:
            max_edges = len(candidates) * len(candidates) * len(candidates)
        else:
            max_edges = len(candidates) + int(len(candidates)/2)

        level_edges = randint(1, max_edges)
        anywhere_edges = max_edges - level_edges

        start = time()
        if len(candidate_levels) > 2:
            for _ in range(level_edges):
                p_level = choice(candidate_levels[2:])
                s_level = choice(candidate_levels[:p_level-1])
                p = choice(loop.level_to_vertices[p_level])
                s = choice(loop.level_to_vertices[s_level])
                if not cfg.has_edge(p, s):
                    cfg.add_edge(edges.ControlFlowEdge(p, s))

                if time() - start > 5:
                    break

        start = time()
        for _ in range(anywhere_edges):
            p = choice(candidates)
            s = choice(candidates)
            if not cfg.has_edge(p, s):
                cfg.add_edge(edges.ControlFlowEdge(p, s))

            if time() - start > 5:
                break

    return cfg


def add_subprograms(the_program:        programs.Program,
                    subprograms:        int,
                    loops:              int,
                    nesting_depth:      int,
                    dense:              bool,
                    irreducible:        bool,
                    number_of_vertices: int,
                    fan_out:            int):
    for subprogram_name in ['s{}'.format(i) for i in range(1, subprograms+1)]:
        messages.debug_message('Creating CFG with name {}'.format(subprogram_name))
        cfg = create_control_flow_graph(the_program,
                                        loops,
                                        nesting_depth,
                                        dense,
                                        irreducible,
                                        number_of_vertices,
                                        fan_out,
                                        subprogram_name)
        call_vertex = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), subprogram_name)
        the_program.add_subprogram(programs.Subprogram(cfg, call_vertex))


def add_calls(the_program: programs.Program, recursion_enabled):
    subprogram_call_candidates = {}
    for subprogram in the_program:
        subprogram_call_candidates[subprogram] = []
        # Work out which basic blocks can legitimately make calls
        dfs = graphs.DepthFirstSearch(subprogram.cfg, subprogram.cfg.entry)
        for vertex in subprogram.cfg:
            if len(subprogram.cfg.successors(vertex)) == 1:
                (edge,) = subprogram.cfg.successors(vertex)
                # Check that the sole successor is not a loop header
                if edge not in dfs.back_edges(vertex):
                    subprogram_call_candidates[subprogram].append(vertex)

    # Pick a root subprogram with the largest number of call candidates
    root_subprogram = choice(list(subprogram_call_candidates.keys()))
    for subprogram in the_program:
        if len(subprogram_call_candidates[subprogram]) > len(subprogram_call_candidates[root_subprogram]):
            root_subprogram = subprogram

    # Set the call depth of each subprogram
    subprogram_to_level = {}
    level_to_subprograms = {}
    lowest_level = level = 1
    subprogram_to_level[root_subprogram] = level - 1
    level_to_subprograms[level - 1] = [root_subprogram]
    level_to_subprograms[level] = []
    for subprogram in the_program:
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
                if callers and random() < 0.33:
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
    else:
        def alive(vertex):
            return True

        sccs = graphs.StrongComponents(the_program.call_graph, alive)
        assert len(sccs.singletons) == the_program.call_graph.number_of_vertices()

    for (caller, callee), sites in call_graph_edges.items():
        for site in sites:
            edge = edges.CallGraphEdge(the_program[caller.name].call_vertex,
                                       the_program[callee.name].call_vertex,
                                       site)
            the_program.call_graph.add_edge(edge)


def annotate_control_flow_edges(the_program: programs.Program):
    for subprogram in the_program:
        for vertex in subprogram.cfg:
            if len(subprogram.cfg.successors(vertex)) == 1:
                (edge,) = subprogram.cfg.successors(vertex)
                callee = the_program.call_graph.is_call_site(subprogram.call_vertex, vertex)
                if callee:
                    edge.set_return(callee)
                elif edge.successor() == subprogram.cfg.entry:
                    edge.direction = edges.Direction.UNREACHABLE
                else:
                    edge.direction = edges.Direction.CONTINUE
            elif len(subprogram.cfg.successors(vertex)) == 2:
                for edge, direction in zip(subprogram.cfg.successors(vertex), [edges.Direction.THEN, edges.Direction.ELSE]):
                    edge.direction = direction
            elif len(subprogram.cfg.successors(vertex)) > 2:
                for edge in subprogram.cfg.successors(vertex):
                    edge.direction = edges.Direction.CASE


def main(**kwargs):
    if kwargs['vertices'] < kwargs['loops'] * 2:
        messages.error_message(
            'The number of vertices in a control flow graph must be at least twice the number of loops')

    the_program = programs.Program(kwargs['program'])
    add_subprograms(the_program,
                    kwargs['subprograms'],
                    kwargs['loops'],
                    kwargs['nesting_depth'],
                    kwargs['dense'],
                    kwargs['irreducible'],
                    kwargs['vertices'],
                    kwargs['fan_out'])

    if not kwargs['no_calls']:
        add_calls(the_program, kwargs['recursion'])

    annotate_control_flow_edges(the_program)

    programs.IO.write(the_program, kwargs['program'])
    the_program.call_graph.dotify()


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
                        help='number of subprograms',
                        metavar='<INT>',
                        default=1)

    parser.add_argument('--loops',
                        type=int,
                        help='maximum number of loops in a control flow graph',
                        metavar='<INT>',
                        default=0)

    parser.add_argument('--nesting-depth',
                        type=int,
                        help='maximum nesting depth of loops',
                        metavar='<INT>',
                        default=1)

    parser.add_argument('--fan-out',
                        action=CheckForPositiveValue,
                        type=int,
                        help='select maximum fan out of a basic block',
                        metavar='<INT>',
                        default=2)

    parser.add_argument('--vertices',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum number of basic blocks in a control flow graph',
                        metavar='<INT>',
                        default=10)

    parser.add_argument('--dense',
                        action='store_true',
                        help='allow dense graphs',
                        default=False)

    parser.add_argument('--irreducible',
                        action='store_true',
                        help='create arbitrary looping structures',
                        default=False)

    parser.add_argument('--recursion',
                        action='store_true',
                        help='allow recursive calls',
                        default=False)

    parser.add_argument('--no-calls',
                        action='store_true',
                        help='do not add calls between subprograms',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    stack_size(2 ** 26)
    setrecursionlimit(2 ** 30)
    main(**vars(parse_the_command_line()))
