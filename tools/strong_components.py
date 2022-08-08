from argparse import ArgumentParser
from collections import deque
from cProfile import Profile
from enum import auto, Enum
from graphs import edges, graphs, vertices
from miscellaneous.helpful import error_message
from numpy import percentile
from pstats import Stats
from random import shuffle
from sys import setrecursionlimit
from system import programs
from threading import stack_size
from time import sleep, time
from typing import Dict, List, Set, Tuple


class SetUnion:
    __slots__ = ['parents', 'ranks']

    def __init__(self):
        self.parents = {}
        self.ranks = {}

    def make_set(self, x: vertices.Vertex):
        self.parents[x] = x
        self.ranks[x] = 0

    def union(self, x: vertices.Vertex, y: vertices.Vertex) -> vertices.Vertex:
        u = self.find_set(x)
        v = self.find_set(y)

        if self.ranks[u] == self.ranks[v]:
            self.ranks[u] += 1
            self.parents[v] = u
            return u
        elif self.ranks[u] > self.ranks[v]:
            self.parents[v] = u
            return u
        else:
            self.parents[u] = v
            return v

    def find_set(self, x: vertices.Vertex) -> vertices.Vertex:
        if self.parents[x] != x:
            self.parents[x] = self.find_set(self.parents[x])
        return self.parents[x]


def szudzik(a: vertices.Vertex, b: vertices.Vertex) -> int:
    if b.id_ > a.id_:
        x, y = b.id_, a.id_
    else:
        x, y = a.id_, b.id_

    if x >= y:
        return x * x + x + y
    else:
        return x + y * y


class SuperVertex:
    __slots__ = ['representative', 'predecessors', 'successors', 'vertices']

    def __init__(self, representative: vertices.Vertex):
        self.representative = representative
        self.predecessors = set()
        self.successors = set()
        self.vertices = set()


class SuperGraph:
    __slots__ = ['super_vertices', 'super_edges']

    def __init__(self):
        self.super_vertices = {}
        self.super_edges = {}

    def add_super_vertex(self, vertex: vertices.Vertex):
        if vertex not in self.super_vertices:
            super_vertex = SuperVertex(vertex)
            self.super_vertices[vertex] = super_vertex

    def get_super_vertex(self, vertex: vertices.Vertex) -> SuperVertex:
        return self.super_vertices[vertex]

    def has_super_vertex(self, vertex: vertices.Vertex):
        return vertex in self.super_vertices

    def repoint(self, master: vertices.Vertex, slave: vertices.Vertex):
        self.super_vertices[slave] = self.super_vertices[master]

    def remove_super_vertex(self, vertex: vertices.Vertex):
        del self.super_vertices[vertex]

    def add_super_edge(self, predecessor: vertices.Vertex, successor: vertices.Vertex, edge):
        self.super_vertices[predecessor].successors.add(successor)
        self.super_vertices[successor].predecessors.add(predecessor)

    def has_super_edge(self, predecessor: vertices.Vertex, successor: vertices.Vertex):
        return predecessor in self.super_vertices[successor].predecessors

    def __iter__(self):
        for super_vertex in self.super_vertices.values():
            yield super_vertex

    def __len__(self):
        return len(self.super_vertices)

    def __str__(self):
        value = ''
        for id_ in sorted(self.super_vertices.keys()):
            super_vertex = self.super_vertices[id_]
            value += 'super {}: ' \
                     'set={} ' \
                     'predecessors={} ' \
                     'successors={}'.format(id_,
                                            ','.join(str(an_id) for an_id in sorted(super_vertex.vertices)),
                                            ','.join(str(an_id) for an_id in sorted(super_vertex.predecessors)),
                                            ','.join(str(an_id) for an_id in sorted(super_vertex.successors)))
            value += '\n'
        return value


class Book:
    __slots__ = ['root', 'edges', 'roots', 'visited']

    def __init__(self, root):
        self.root = root
        self.edges = []
        self.roots = set()
        self.visited = False


class Reduction(Enum):
    T0 = auto()
    T1 = auto()
    T2 = auto()


def search(root, root_super_vertex, cfg, super_graph, library, candidates):
    root_book = library[root]
    assert not root_book.visited
    root_book.visited = True
    merges = set()
    symmetry = set()
    queue = deque([root])
    while queue:
        vertex = queue.popleft()
        root_super_vertex.vertices.add(vertex.id_)

        for edge in cfg.successors(vertex):
            successor = edge.successor()
            if len(cfg.predecessors(successor)) == 1:
                queue.append(successor)
            else:
                if successor not in library:
                    book = Book(successor)
                    library[successor] = book

                book = library[successor]
                book.edges.append(edge)
                book.roots.add(root)

                if super_graph.has_super_edge(successor, root):
                    symmetry.add(successor)
                else:
                    merges.add(successor)

                if len(book.edges) == len(cfg.predecessors(successor)) and len(book.roots) == 1:
                    queue.append(successor)
                    merges.remove(successor)

    if symmetry:
        for vertex in symmetry:
            super_vertex = super_graph.get_super_vertex(vertex)
            root_super_vertex.vertices.update(super_vertex.vertices)

            for predecessor in super_vertex.predecessors:
                root_super_vertex.predecessors.add(predecessor)
                super_predecessor = super_graph.get_super_vertex(predecessor)
                super_predecessor.successors.remove(vertex)
                super_predecessor.successors.add(root)

            for successor in super_vertex.successors:
                root_super_vertex.successors.add(successor)
                super_successor = super_graph.get_super_vertex(successor)
                super_successor.predecessors.remove(vertex)
                super_successor.predecessors.add(root)

        for vertex in symmetry:
            super_graph.repoint(root, vertex)
            super_graph.remove_super_vertex(vertex)

        super_graph.add_super_edge(root, root, None)
        candidates.add(root)

    frontier = []
    for merge in merges:
        super_graph.add_super_vertex(merge)
        super_graph.add_super_edge(root, merge, None)
        if root != merge and len(library[merge].roots) == 1:
            frontier.append(merge)

    return frontier


def is_t1_reducible(super_graph: SuperGraph, super_vertex: SuperVertex):
    return len(super_vertex.predecessors) == 2 and super_graph.has_super_edge(super_vertex.representative,
                                                                              super_vertex.representative)


def is_t2_reducible(super_vertex: SuperVertex):
    return len(super_vertex.predecessors) == 1


def reduce(super_graph: SuperGraph, candidates):
    reductions = deque()
    for vertex in candidates:
        if super_graph.has_super_vertex(vertex):
            super_vertex = super_graph.get_super_vertex(vertex)
            if is_t1_reducible(super_graph, super_vertex):
                reductions.append((Reduction.T1, super_vertex, vertex))

    changed = set()
    while len(super_graph) > 1:
        changed.clear()

        while reductions:
            reduction_type, super_vertex, vertex = reductions.popleft()
            print(super_graph)
            print(vertex)

            if reduction_type == Reduction.T1:
                (predecessor_one, predecessor_two) = super_vertex.predecessors
                if predecessor_one == super_vertex.representative:
                    predecessor = predecessor_two
                else:
                    predecessor = predecessor_one
            else:
                (predecessor,) = super_vertex.predecessors

            for successor in super_vertex.successors:
                super_graph.add_super_edge(predecessor, successor, None)
                if successor != super_vertex.representative:
                    changed.add(successor)

            super_predecessor = super_graph.get_super_vertex(predecessor)
            super_predecessor.vertices.update(super_vertex.vertices)

            for predecessor in super_vertex.predecessors:
                super_predecessor = super_graph.get_super_vertex(predecessor)
                super_predecessor.successors.remove(vertex)

            for successor in super_vertex.successors:
                super_successor = super_graph.get_super_vertex(successor)
                super_successor.predecessors.remove(vertex)

            super_graph.remove_super_vertex(super_vertex.representative)

        for vertex in changed:
            if super_graph.has_super_vertex(vertex):
                super_vertex = super_graph.get_super_vertex(vertex)
                if is_t1_reducible(super_graph, super_vertex):
                    reductions.append((Reduction.T1, super_vertex, vertex))
                elif is_t2_reducible(super_vertex):
                    reductions.append((Reduction.T2, super_vertex, vertex))


def go(cfg: graphs.ControlFlowGraph):
    cfg.dotify()
    book = Book(cfg.entry)
    library = {cfg.entry: book}

    super_graph = SuperGraph()
    super_graph.add_super_vertex(cfg.entry)

    #profiler = Profile()
    #profiler.enable()
    frontier = [cfg.entry]
    candidates = set()
    while frontier:
        merge = frontier.pop(0)
        merge_super_vertex = super_graph.get_super_vertex(merge)
        merges = search(merge, merge_super_vertex, cfg, super_graph, library, candidates)
        frontier.extend(merges)

    print(super_graph)
    reduce(super_graph, candidates)
    #profiler.disable()
    #stats = Stats(profiler).sort_stats('cumtime')
    #stats.print_stats()
    assert len(super_graph.get_super_vertex(cfg.entry).vertices) == cfg.number_of_vertices()


def strip_outliers(data: List[float]):
    q25, q75 = percentile(data, 25), percentile(data, 75)
    iqr = q75 - q25
    cut_off = iqr * 1.5
    lower, upper = q25 - cut_off, q75 + cut_off
    return [x for x in data if lower <= x <= upper]


def main(filename: str, subprogram_names: List[str], repeat: int):
    program = programs.IO.read(filename)
    program.cleanup()

    if subprogram_names:
        program.keep_only(subprogram_names)

    for subprogram in program:
        subprogram.cfg.remove_edge(edges.Edge(subprogram.cfg.exit, subprogram.cfg.entry))
        print(subprogram.cfg.name, subprogram.cfg.number_of_vertices(), subprogram.cfg.number_of_edges())

        betts_times = []
        for i in range(repeat):
            #subprogram.cfg.shuffle_edges()
            begin = time()
            go(subprogram.cfg)
            betts_times.append(time() - begin)
        total_time = strip_outliers(betts_times)
        print('Betts  {}'.format(sum(total_time) / len(total_time)))

        tarjan_times = []
        for i in range(repeat):
            begin = time()
            graphs.StrongComponents(subprogram.cfg, subprogram.cfg.entry)
            tarjan_times.append(time() - begin)
        total_time = strip_outliers(tarjan_times)
        print('Tarjan {}'.format(sum(total_time) / len(total_time)))


def parse_command_line():
    parser = ArgumentParser(description='Compute strongly connected components')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the computations this many times',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('-s',
                        '--subprograms',
                        nargs='+',
                        help='only apply the algorithms to these subprograms',
                        metavar='<NAME>')

    return parser.parse_args()


if __name__ == '__main__':
    stack_size(2 ** 26)
    setrecursionlimit(2 ** 30)
    args = parse_command_line()
    main(args.program, args.subprograms, args.repeat)

