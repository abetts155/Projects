import argparse
import random

from graphs import (edges, graphs, vertices)
from sys import setrecursionlimit
from system import programs
from threading import stack_size
from time import time
from utils import messages


def go_ahead(weight=None):
    if weight:
        return random.random() < weight
    else:
        return bool(random.getrandbits(1))


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
            v = vertices.Vertex(vertices.Vertex.get_vertex_id())
            cfg.add_vertex(v)
            self._vertices.add(v)
            number_of_vertices -= 1

    def position_vertices(self, fan_out, number_of_nested_loops, outermost_loop):
        level = 0
        for v in self._vertices:
            self._vertex_to_level[v] = level
            self.level_to_vertices.setdefault(level, []).append(v)

            if level == 0:
                self._header = v
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
                v = loop_tails[random.randint(0, len(loop_tails)-1)]
                self.level_to_vertices.setdefault(new_highest_level, []).append(v)
                self.level_to_vertices[highest_level].remove(v)
                self._vertex_to_level[v] = new_highest_level

    def add_edges(self, cfg, fan_out):
        for level in sorted(self.level_to_vertices.keys(), reverse=True):
            if level > 0:
                for s in self.level_to_vertices[level]:
                    candidates = [v for v in self.level_to_vertices[level-1] if len(cfg.successors(v)) < fan_out]
                    (p,) = random.sample(candidates, 1)
                    cfg.add_edge(edges.ControlFlowEdge(p, s))
                    if len(cfg.successors(p)) == fan_out:
                        candidates.remove(p)

    def connect_nested_loops(self, cfg, nested_loops):
        highest_level = max(self.level_to_vertices.keys())
        candidate_entry_sources = [v for v in self._vertices if 0 < self._vertex_to_level[v] < highest_level]
        for loop in nested_loops:
            (p,) = random.sample(candidate_entry_sources, 1)
            cfg.add_edge(edges.ControlFlowEdge(p, loop.header))
            for exit_source in loop.exits:
                higher_level = random.randint(self._vertex_to_level[p] + 1, highest_level)
                candidate_exit_destinations = self.level_to_vertices[higher_level]
                (s,) = random.sample(candidate_exit_destinations, 1)
                cfg.add_edge(edges.ControlFlowEdge(exit_source, s))

    def connect_terminal_vertices(self, cfg):
        highest_level = max(self.level_to_vertices.keys())
        for level in sorted(self.level_to_vertices.keys(), reverse=True):
            if 0 < level < highest_level:
                candidate_predecessors = [v for v in self.level_to_vertices[level] if len(cfg.successors(v)) == 0]
                higher_level = random.randint(level + 1, highest_level)
                candidate_successors = self.level_to_vertices[higher_level]
                for p in candidate_predecessors:
                    (s,) = random.sample(candidate_successors, 1)
                    cfg.add_edge(edges.ControlFlowEdge(p, s))

    def add_backedges(self, cfg):
        highest_level = max(self.level_to_vertices.keys())
        for v in self.level_to_vertices[highest_level]:
            cfg.add_edge(edges.ControlFlowEdge(v, self._header))

    def set_exits(self, cfg):
        selection_probability = 1.0
        for v in self._vertex_to_level:
            if len(cfg.successors(v)) == 1 and selection_probability > random.random() and v != self._header:
                self._exits.add(v)
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
        vertex_to_level = {v: 0 for v in lnt}
        (root_v,) = random.sample(vertex_to_level.keys(), 1)
        parent_v = root_v
        for v in lnt:
            if v != root_v:
                new_level = vertex_to_level[parent_v] + 1
                if new_level <= nesting_depth:
                    lnt.add_edge(edges.Edge(parent_v, v))
                    vertex_to_level[v] = new_level
                else:
                    # The height of the tree now exceeds the maximum depth, so
                    # backtrack to an arbitrary proper ancestor
                    ancestor_v = parent_v
                    while True:
                        (e,) = lnt.predecessors(ancestor_v)
                        ancestor_v = e.predecessor()
                        if go_ahead() or ancestor_v == root_v:
                            break
                    parent_v = ancestor_v
                    lnt.add_edge(edges.Edge(parent_v, v))
                    vertex_to_level[v] = vertex_to_level[parent_v] + 1
                parent_v = v

        # Compute number of vertices in each loop
        number_of_vertices_remaining = number_of_vertices
        for v in lnt:
            # Guarantee each loop has at least 2 vertices plus vertices needed
            # to connect inner nested loops
            v.size = 2 + len(lnt.successors(v))
            number_of_vertices_remaining -= v.size

        # Arbitrarily distribute any remaining vertices to the loop bodies
        while number_of_vertices_remaining > 0:
            for v in lnt:
                additional_vertices = random.randint(0, number_of_vertices_remaining)
                v.size += additional_vertices
                number_of_vertices_remaining -= additional_vertices
        return lnt, root_v

    def create_loop_body(v):
        for e in lnt.successors(v):
            create_loop_body(e.successor())

        # Post-order actions
        nested_loops = [loops[e.successor()] for e in lnt.successors(v)]
        loops[v] = ArtificialLoopBody(fan_out, cfg, v.size, nested_loops, len(lnt.predecessors(v)) == 0)

        if len(lnt.predecessors(v)) == 0:
            (e,) = cfg.predecessors(loops[v].header)
            cfg.entry = e.successor()
            cfg.exit = e.predecessor()

    cfg = graphs.ControlFlowGraph(the_program, subprg_name)
    if not irreducible:
        lnt, root_v = create_artificial_loop_hierarchy()
        loops = {}
        create_loop_body(root_v)
    else:
        loop = ArtificialLoopBody(fan_out, cfg, number_of_vertices, [], True)
        (e,) = cfg.predecessors(loop.header)
        cfg.entry = e.successor()
        cfg.exit = e.predecessor()
        candidates = [v for v in cfg if v != cfg.entry and v != cfg.exit]

        min_level = min(loop.level_to_vertices.keys())
        max_level = max(loop.level_to_vertices.keys())
        candidate_levels = [level for level in range(min_level+1, max_level)]

        if dense:
            max_edges = len(candidates) * len(candidates) * len(candidates)
        else:
            max_edges = len(candidates) + int(len(candidates)/2)

        level_edges = random.randint(1, max_edges)
        anywhere_edges = max_edges - level_edges

        start = time()
        if len(candidate_levels) > 2:
            for _ in range(level_edges):
                p_level = random.choice(candidate_levels[2:])
                s_level = random.choice(candidate_levels[:p_level-1])
                p = random.choice(loop.level_to_vertices[p_level])
                s = random.choice(loop.level_to_vertices[s_level])
                if not cfg.has_edge(p, s):
                    cfg.add_edge(edges.ControlFlowEdge(p, s))

                if time() - start > 5:
                    break

        start = time()
        for _ in range(anywhere_edges):
            p = random.choice(candidates)
            s = random.choice(candidates)
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
    class Subprogram:
        __slots__ = ['name', 'candidates', 'level']

        def __init__(self, name):
            self.name = name
            self.candidates = []
            self.level = None

    data = []
    for subprogram in the_program:
        subprogram_view = Subprogram(subprogram.cfg.name)
        data.append(subprogram_view)
        # Work out which basic blocks can legitimately make calls
        dfs = graphs.DepthFirstSearch(subprogram.cfg, subprogram.cfg.entry)
        for v in subprogram.cfg:
            if len(subprogram.cfg.successors(v)) == 1:
                (e,) = subprogram.cfg.successors(v)
                # Check that the sole successor is not a loop header
                if e not in dfs.back_edges:
                    subprogram_view.candidates.append(v)

    # Sort the subprograms by the number of call sites, greatest number first
    data.sort(key=lambda t: len(t.candidates), reverse=True)

    frontier = 0
    for subprogram_view in data:
        subprogram_view.level = frontier
        if frontier == 0 or (len(subprogram_view.candidates) > 0 and bool(random.getrandbits(1))):
            frontier += 1

    # Add calls
    def add_call():
        (caller,) = random.sample(callers, 1)
        (v,) = random.sample(caller.candidates, 1)
        call_graph_edges.setdefault((caller, callee), []).append(v)

        # Clean up
        caller.candidates.remove(v)
        if not caller.candidates:
            data.remove(caller)

    call_graph_edges = {}
    for level in reversed(range(1, frontier + 1)):
        callees = [subprogram_view for subprogram_view in data if subprogram_view.level == level]
        for callee in callees:
            if random.random() < 0.2:
                lower_level = random.randint(0, level - 1)
            else:
                lower_level = level - 1

            callers = [subprogram_view for subprogram_view in data if subprogram_view.level == lower_level]
            if callers:
                add_call()

    for level in reversed(range(1, frontier + 1)):
        callees = [subprogram_view for subprogram_view in data if subprogram_view.level == level]
        for callee in callees:
            if bool(random.getrandbits(1)):
                while True:
                    lower_level = random.randint(0, level - 1)
                    callers = [subprogram_view for subprogram_view in data if subprogram_view.level == lower_level]
                    if callers and random.random() < 0.75:
                        add_call()
                    else:
                        break

    if recursion_enabled:
        for level in reversed(range(1, frontier + 1)):
            callers = [subprogram_view for subprogram_view in data if subprogram_view.level == level]
            if callers and bool(random.getrandbits(1)):
                lower_level = random.randint(0, level - 1)
                callees = [subprogram_view for subprogram_view in data if subprogram_view.level == lower_level]
                if callees and random.random() < 0.25:
                    (callee,) = random.sample(callees, 1)
                    add_call()

    for (caller, callee), sites in call_graph_edges.items():
        for site in sites:
            e = edges.CallGraphEdge(the_program[caller.name].call_vertex, the_program[callee.name].call_vertex, site)
            the_program.call_graph.add_edge(e)


def annotate_control_flow_edges(the_program: programs.Program):
    for subprogram in the_program:
        for v in subprogram.cfg:
            if len(subprogram.cfg.successors(v)) == 1:
                (e,) = subprogram.cfg.successors(v)
                callee = the_program.call_graph.is_call_site(subprogram.call_vertex, v)
                if callee:
                    e.set_return(callee)
                elif e.successor() == subprogram.cfg.entry:
                    e.direction = edges.Direction.UNREACHABLE
                else:
                    e.direction = edges.Direction.CONTINUE
            elif len(subprogram.cfg.successors(v)) == 2:
                for e, direction in zip(subprogram.cfg.successors(v), [edges.Direction.THEN, edges.Direction.ELSE]):
                    e.direction = direction
            elif len(subprogram.cfg.successors(v)) > 2:
                for e in subprogram.cfg.successors(v):
                    e.direction = edges.Direction.CASE


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
    programs.IO.write(the_program, kwargs['program'])


class CheckForPositiveValue(argparse.Action):
    def __call__(self, parser, namespace, value, option_string=None):
        if value <= 0:
            raise argparse.ArgumentError('Argument {} requires a positive integer'.format(option_string))
        setattr(namespace, self.dest, value)


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Generate a random program')

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
