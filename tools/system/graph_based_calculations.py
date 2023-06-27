from graphs.edges import Edge
from graphs.graphs import ControlFlowGraph
from graphs.vertices import Vertex
from typing import List, Set


class LoopHierarchy:
    def __init__(self):
        self._loops = []
        self._representatives = {}
        self._predecessors = {}
        self._successors = {}
        self._parent = {}

    def add_loop(self, loop: List[Vertex]):
        self._loops.append(loop)

        for vertex in loop:
            if vertex in self._parent:
                self._parent[vertex].remove(vertex)
            self._parent[vertex] = loop

    def get_loop(self, vertex: Vertex):
        return self._parent[vertex]

    def add_edges(self, cfg: ControlFlowGraph):
        for loop in self._loops:
            loop.sort()
            representative = loop[0]
            self._predecessors[representative] = set()
            self._successors[representative] = set()

        for loop in self._loops:
            representative = loop[0]

            for vertex in loop:
                for edge in cfg.predecessors(vertex):
                    other_loop = self.get_loop(edge.predecessor())
                    if other_loop != loop:
                        other_representative = other_loop[0]
                        self._successors[other_representative].add(representative)
                        self._predecessors[representative].add(other_representative)

                for edge in cfg.successors(vertex):
                    other_loop = self.get_loop(edge.successor())
                    if other_loop != loop:
                        other_representative = other_loop[0]
                        self._predecessors[other_representative].add(representative)
                        self._successors[representative].add(other_representative)

    def __str__(self):
        value = ''
        for loop in self._loops:
            representative = loop[0]
            value += 'Loop: ' + ' '.join(str(vertex) for vertex in loop) + '\n'
            for other_representative in self._successors[representative]:
                value += '{}=>{}'.format(representative, other_representative) + '\n'
            value += '\n'
        return value

    def __len__(self):
        return len(self._loops)

    def __iter__(self):
        for loop in self._loops:
            yield loop


def identify_loops(cfg: ControlFlowGraph):
    pre_id = 0
    stack = []
    pre_order = {}
    low_link = {}
    on_stack = {}
    dead_edges = set()

    def explore(vertex: Vertex, killed_vertices: Set[Vertex], loops: Set[Set[Vertex]]):
        nonlocal pre_id

        pre_id += 1
        pre_order[vertex] = pre_id
        low_link[vertex] = pre_id
        on_stack[vertex] = True
        stack.append(vertex)

        for edge in cfg.successors(vertex):
            if edge not in dead_edges:
                successor = edge.successor()

                if pre_order[successor] == 0:
                    explore(successor, killed_vertices, forest)
                    low_link[vertex] = min(low_link[vertex], low_link[successor])
                elif on_stack[successor]:
                    low_link[vertex] = min(low_link[vertex], pre_order[successor])

        if low_link[vertex] == pre_order[vertex]:
            loop = []
            done = False
            while not done:
                z = stack.pop()
                on_stack[z] = False
                loop.append(z)
                done = z == vertex

            if len(loop) == 1 and not cfg.has_edge(vertex, vertex):
                killed_vertices.add(vertex)
            else:
                loops.append(loop)

    forest = LoopHierarchy()
    alive_vertices = {vertex for vertex in cfg}
    while alive_vertices:
        pre_id = 0
        for vertex in alive_vertices:
            pre_order[vertex] = 0
            low_link[vertex] = 0
            on_stack[vertex] = False

        killed_vertices = set()
        loops = []
        for vertex in alive_vertices:
            if pre_order[vertex] == 0:
                explore(vertex, killed_vertices, loops)

        alive_vertices.difference_update(killed_vertices)

        for loop in loops:
            forest.add_loop(loop)
            headers = set()
            for vertex in loop:
                for edge in cfg.predecessors(vertex):
                    if edge.successor() == cfg.entry or edge.predecessor() not in loop:
                        headers.add(vertex)

                if vertex in headers:
                    for edge in cfg.predecessors(vertex):
                        if edge.predecessor() in loop:
                            dead_edges.add(edge)

    forest.add_edges(cfg)
    print(forest)


def calculate(cfg: ControlFlowGraph):
    identify_loops(cfg)
