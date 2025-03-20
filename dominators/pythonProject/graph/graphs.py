import abc
import collections
import dataclasses
import enum
import functools
import itertools
import random
import re
import typing

from graph import edges
from graph import vertices
from utils import dot


class VertexError(Exception):
    pass


class EdgeError(Exception):
    pass


type Edge = edges.Edge
type EdgeList = list[edges.Edge]
type Vertex = vertices.Vertex | vertices.SuperVertex | vertices.LoopVertex
type VertexSet = set[vertices.Vertex] | set[vertices.SuperVertex] | set[vertices.LoopVertex]


def dotify(vertex_list: list[Vertex], edge_list: list[Edge], filename: str):
    vertex_data = [vertex.dotify() for vertex in vertex_list]
    edge_data = [edge.dotify() for edge in edge_list]
    dot.generate(vertex_data + edge_data, filename)


class GraphDirection(enum.StrEnum):
    Forwards = enum.auto()
    Backwards = enum.auto()


class GraphTraveller(abc.ABC):
    def __init__(self, backwards: dict[Vertex, EdgeList], forwards: dict[Vertex, EdgeList]):
        self.backwards = backwards
        self.forwards = forwards
        
    @abc.abstractmethod
    def backward(self, edge: Edge) -> Vertex:
        pass

    @abc.abstractmethod
    def forward(self, edge: Edge) -> Vertex:
        pass


class BackwardTraveller(GraphTraveller):
    def backward(self, edge: Edge) -> Vertex:
        return edge.point_b

    def forward(self, edge: Edge) -> Vertex:
        return edge.point_a


class ForwardTraveller(GraphTraveller):
    def backward(self, edge: Edge) -> Vertex:
        return edge.point_a

    def forward(self, edge: Edge) -> Vertex:
        return edge.point_b


class GraphType(enum.Enum):
    UNIQUE_EDGES = enum.auto()
    MULTI_EDGES = enum.auto()


class DirectedGraph:
    def __init__(self, graph_type: GraphType = GraphType.UNIQUE_EDGES):
        self.its_type:      GraphType = graph_type
        self.its_vertices:  VertexSet = set()
        self.its_edges:     EdgeList = list()
        self.predecessors:  dict[Vertex, EdgeList] = dict()
        self.successors:    dict[Vertex, EdgeList] = dict()
        self._id_to_vertex: dict[str, Vertex] = dict()

    def add_vertex(self, vertex: Vertex):
        if vertex not in self.its_vertices:
            self.its_vertices.add(vertex)
            self.predecessors[vertex] = list()
            self.successors[vertex] = list()
            self._id_to_vertex[vertex.identifier] = vertex

    def get_vertex(self, identifier: str) -> typing.Optional[Vertex]:
        return self._id_to_vertex.get(identifier)

    def remove_vertex(self, vertex: Vertex):
        if vertex not in self.its_vertices:
            raise VertexError(f"The vertex '{vertex}' does not belong to the graph")

        dead_edges = set()
        for edge in self.successors[vertex]:
            self.predecessors[edge.point_b].remove(edge)
            dead_edges.add(edge)

        for edge in self.predecessors[vertex]:
            self.successors[edge.point_a].remove(edge)
            dead_edges.add(edge)

        self.its_vertices.remove(vertex)
        self.its_edges = [edge for edge in self.its_edges if edge not in dead_edges]
        del self.predecessors[vertex]
        del self.successors[vertex]
        del self._id_to_vertex[vertex.identifier]

    def add_edge(self, edge: Edge):
        if edge.point_a.identifier not in self._id_to_vertex:
            raise VertexError(f"The vertex '{edge.point_a}' does not belong to the graph")

        if edge.point_b.identifier not in self._id_to_vertex:
            raise VertexError(f"The vertex '{edge.point_b}' does not belong to the graph")

        if self.its_type == GraphType.UNIQUE_EDGES:
            if edge in self.its_edges:
                raise EdgeError(f"Adding a duplicate edge '{edge}' to a graph whose type is {self.its_type.name}")

        self.its_edges.append(edge)
        self.successors[edge.point_a].append(edge)
        self.predecessors[edge.point_b].append(edge)

    def remove_edge(self, edge: Edge):
        if edge not in self.its_edges:
            raise EdgeError(f"The edge '{edge}' does not belong to the graph")

        self.its_edges.remove(edge)
        self.successors[edge.point_a].remove(edge)
        self.predecessors[edge.point_b].remove(edge)

    def reroute(self, vertex: Vertex):
        for predecessor_edge in self.predecessors[vertex]:
            for successor_edge in self.successors[vertex]:
                bridge_edge = edges.Edge(predecessor_edge.point_a, successor_edge.point_b)
                self.add_edge(bridge_edge)

    def get_traveller(self, direction: GraphDirection) -> GraphTraveller:
        if direction == GraphDirection.Forwards:
            return ForwardTraveller(self.predecessors, self.successors)
        else:
            return BackwardTraveller(self.successors, self.predecessors)

    def dotify(self, filename: str):
        dotify(list(self.its_vertices), self.its_edges, filename)

    def __eq__(self, other: "DirectedGraph"):
        return self.its_vertices == other.its_vertices and self.its_edges == other.its_edges

    def __str__(self):
        return '\n'.join(str(edge) for edge in self.its_edges)


def get_reachable_vertices(directed_graph: DirectedGraph, start: Vertex, direction: GraphDirection) -> set[Vertex]:
    reachable = set()
    queue = collections.deque([start])
    traveller = directed_graph.get_traveller(direction)
    while queue:
        vertex = queue.popleft()
        reachable.add(vertex)

        for edge in traveller.forwards[vertex]:
            forward = traveller.forward(edge)
            if forward not in reachable and forward not in queue:
                queue.append(forward)

    return reachable


def jail_and_free(directed_graph: DirectedGraph) -> list[Vertex]:
    counts = {}
    queue = collections.deque()
    for vertex in directed_graph.its_vertices:
        counts[vertex] = 0
        if not directed_graph.predecessors[vertex]:
            queue.append(vertex)

    ordering = []
    while queue:
        vertex = queue.popleft()
        ordering.append(vertex)
        for edge in directed_graph.successors[vertex]:
            successor = edge.point_b
            counts[successor] += 1
            if counts[successor] == len(directed_graph.predecessors[successor]):
                queue.append(successor)

    return ordering


def is_dag(directed_graph: DirectedGraph) -> bool:
    ordering = jail_and_free(directed_graph)
    return len(ordering) == len(directed_graph.its_vertices)


def breadth_first_search(origin: Vertex, traveller: GraphTraveller) -> list[Vertex]:
    vertex_list = []
    queue = collections.deque([origin])
    while queue:
        vertex = queue.popleft()
        vertex_list.append(vertex)

        for edge in traveller.forwards[vertex]:
            forward = traveller.forward(edge)
            if forward not in queue and forward not in vertex_list:
                queue.append(forward)

    return vertex_list


class CallRootError(Exception):
    pass


class DuplicateSubprogramError(Exception):
    pass


class CallGraph(DirectedGraph):
    def __init__(self):
        super().__init__(GraphType.MULTI_EDGES)
        self.root_vertex: vertices.Vertex | None = None
        self._name_to_vertex: dict[str, Vertex] = {}

    def set_root(self):
        if not self.its_vertices:
            raise CallRootError(f"The call graph has no vertices")

        candidates = [vertex for vertex in self.its_vertices if len(self.predecessors[vertex]) == 0]

        if not candidates:
            raise CallRootError(f"In the call graph, all vertices have predecessors")
        elif len(candidates) > 1:
            candidates_string = ' '.join(str(vertex) for vertex in candidates)
            raise CallRootError(f"In the call graph, multiple vertices ({candidates_string}) have no predecessors")
        else:
            (self.root_vertex,) = candidates

    def add_vertex(self, vertex: Vertex):
        if vertex.identifier in self._name_to_vertex:
            raise DuplicateSubprogramError(f"There is already a vertex for '{vertex.identifier}' in the call graph.")
        self._name_to_vertex[vertex.identifier] = vertex
        super().add_vertex(vertex)

    def get_vertex(self, subprogram_name: str) -> typing.Optional[Vertex]:
        return self._name_to_vertex.get(subprogram_name)

    def get_call_sites(self, vertex: Vertex) -> list[Vertex]:
        call_edge: edges.CallEdge
        return [call_edge.site for call_edge in self.successors[vertex]]

    def copy(self) -> "CallGraph":
        the_copy = CallGraph()

        for vertex in self.its_vertices:
            the_copy.add_vertex(vertex)

        for edge in self.its_edges:
            the_copy.add_edge(edge)

        the_copy.root_vertex = self.root_vertex

        return the_copy

    def to_json(self) -> list[dict]:
        edge_dicts = []
        edge: edges.CallEdge
        for edge in self.its_edges:
            d = {
                'caller': edge.point_a.identifier,
                'callee': edge.point_b.identifier,
                'site': edge.site.identifier
            }
            edge_dicts.append(d)
        return edge_dicts


class EntryExitError(Exception):
    pass


class ControlFlowGraph(DirectedGraph):
    def __init__(self, name: str):
        super().__init__()
        self.name:         str = name
        self.entry_vertex: vertices.Vertex | None = None
        self.exit_vertex:  vertices.Vertex | None = None

    def _check_reachability(self, origin: Vertex, traveller: GraphTraveller):
        stack = [origin]
        visited = set()
        while stack:
            vertex = stack.pop()
            visited.add(vertex)
            for edge in traveller.forwards[vertex]:
                forward = traveller.forward(edge)
                if forward not in visited and forward not in stack:
                    stack.append(forward)

        unreachable = self.its_vertices.difference(visited)
        if unreachable:
            raise EntryExitError(f"In CFG '{self.name}' it is not possible to reach "
                                 f"{' '.join(str(vertex) for vertex in unreachable)} from {origin} ")

    def set_entry_and_exit(self):
        if not self.its_vertices:
            raise EntryExitError(f"The CFG {self.name} has no vertices")

        entry_candidates = set()
        exit_candidates = set()
        for vertex in self.its_vertices:
            if not self.predecessors[vertex]:
                entry_candidates.add(vertex)
            if not self.successors[vertex]:
                exit_candidates.add(vertex)

        if not entry_candidates:
            raise EntryExitError(f"In CFG '{self.name}', all vertices have predecessors")
        elif len(entry_candidates) > 1:
            candidates_string = ' '.join(str(vertex) for vertex in entry_candidates)
            raise EntryExitError(f"In CFG '{self.name}', multiple vertices ({candidates_string}) have no predecessors")
        else:
            (self.entry_vertex,) = entry_candidates

        if not exit_candidates:
            raise EntryExitError(f"In CFG '{self.name}', all vertices have successors")
        elif len(exit_candidates) > 1:
            candidates_string = ' '.join(str(vertex) for vertex in exit_candidates)
            raise EntryExitError(f"In CFG '{self.name}', multiple vertices ({candidates_string}) have no successors")
        else:
            (self.exit_vertex,) = exit_candidates

        self._check_reachability(self.entry_vertex, self.get_traveller(GraphDirection.Forwards))
        self._check_reachability(self.exit_vertex, self.get_traveller(GraphDirection.Backwards))

    def get_origin(self, direction: GraphDirection) -> vertices.Vertex:
        if direction == GraphDirection.Forwards:
            return self.entry_vertex
        else:
            return self.exit_vertex

    def copy(self, name: str = None) -> "ControlFlowGraph":
        if name is not None:
            the_copy = ControlFlowGraph(name)
        else:
            the_copy = ControlFlowGraph(self.name)

        for vertex in self.its_vertices:
            the_copy.add_vertex(vertex)

        for edge in self.its_edges:
            the_copy.add_edge(edge)

        the_copy.entry_vertex = self.entry_vertex
        the_copy.exit_vertex = self.exit_vertex

        return the_copy

    def _kill_unreachable_vertices(self, direction: GraphDirection):
        traveller = self.get_traveller(direction)
        origin = self.get_origin(direction)
        dead = [vertex for vertex in self.its_vertices if len(traveller.backwards[vertex]) == 0 and vertex != origin]

        while dead:
            vertex = dead.pop()

            for edge in traveller.forwards[vertex]:
                if len(traveller.backwards[edge.point_b]) == 1:
                    dead.append(edge.point_b)

            self.remove_vertex(vertex)

    def remove_dead_vertices(self, dead_set: VertexSet):
        for dead in dead_set:
            self.remove_vertex(dead)

        self._kill_unreachable_vertices(GraphDirection.Forwards)
        self._kill_unreachable_vertices(GraphDirection.Backwards)

    def dotify(self, filename: str, *args):
        if self.entry_vertex is not None and self.exit_vertex is not None:
            if GraphDirection.Backwards in args:
                vertex_list = breadth_first_search(self.exit_vertex, self.get_traveller(GraphDirection.Backwards))
                edge_set = {edges.Edge(edge.point_b, edge.point_a) for edge in self.its_edges}
            else:
                vertex_list = breadth_first_search(self.entry_vertex, self.get_traveller(GraphDirection.Forwards))
                edge_set = self.its_edges

            dotify(vertex_list, list(edge_set), filename)
        else:
            super().dotify(filename)

    def to_json(self) -> dict:
        edge_dicts = []
        for edge in self.its_edges:
            edge_dicts.append(dict(a=edge.point_a.identifier, b=edge.point_b.identifier))
        return dict(cfg_name=self.name, edges=edge_dicts)

    def __hash__(self):
        return hash(self.name)

    def __lt__(self, other: "ControlFlowGraph"):
        return self.name < other.name


def create_control_flow_subgraph(cfg: ControlFlowGraph, vertex: Vertex) -> ControlFlowGraph:
    forward_vertices = get_reachable_vertices(cfg, vertex, GraphDirection.Forwards)
    backward_vertices = get_reachable_vertices(cfg, vertex, GraphDirection.Backwards)
    vertex_set = forward_vertices | backward_vertices

    reduced = ControlFlowGraph(f'{cfg.name}.{vertex.identifier}')
    for vertex in vertex_set:
        reduced.add_vertex(vertex)

    for edge in cfg.its_edges:
        if edge.point_a in vertex_set and edge.point_b in vertex_set:
            reduced.add_edge(edge)

    reduced.set_entry_and_exit()

    return reduced


class Tree(DirectedGraph):
    def __init__(self):
        super().__init__()
        self.root: Vertex | None = None

    def depths(self) -> dict[Vertex, int]:
        assert self.root is not None
        stack = [self.root]
        vertex_to_depth = {self.root: 0}
        while stack:
            vertex: Vertex = stack.pop()
            for edge in self.successors[vertex]:
                vertex_to_depth[edge.point_b] = vertex_to_depth[vertex] + 1
                stack.append(edge.point_b)
        return vertex_to_depth

    def height(self) -> int:
        vertex_to_depth = self.depths()
        return max(vertex_to_depth.values())

    def is_ancestor(self, potential_ancestor: Vertex, potential_descendant: Vertex) -> bool:
        if potential_ancestor == self.root:
            return True
        else:
            vertex = potential_descendant
            while vertex != self.root:
                if vertex == potential_ancestor:
                    return True
                (edge,) = self.predecessors[vertex]
                vertex = edge.point_a
            return False

    def is_parent(self, potential_parent: Vertex, potential_child: Vertex) -> bool:
        if potential_child == self.root:
            return False
        else:
            (edge,) = self.predecessors[potential_child]
            return edge.point_a == potential_parent

    def dotify(self, filename: str):
        if self.root is not None:
            vertex_list = breadth_first_search(self.root, self.get_traveller(GraphDirection.Forwards))
            dotify(vertex_list, list(self.its_edges), filename)
        else:
            super().dotify(filename)

    def __eq__(self, other: "Tree"):
        return super().__eq__(other) and self.root == other.root


@dataclasses.dataclass(slots=True)
class TarjanSCCMetaData:
    traveller: GraphTraveller
    trivial: set[Vertex] = dataclasses.field(default_factory=set)
    non_trivial: set[frozenset[Vertex]] = dataclasses.field(default_factory=set)
    pre_order: dict[Vertex, int] = dataclasses.field(default_factory=dict)
    low_link: dict[Vertex, int] = dataclasses.field(default_factory=dict)
    on_stack: dict[Vertex, bool] = dataclasses.field(default_factory=dict)
    stack: list[Vertex] = dataclasses.field(default_factory=list)
    alive: set[Edge] = dataclasses.field(default_factory=set)
    unexplored: int = 0
    pre_id: int = unexplored


def compute_strong_components(directed_graph: DirectedGraph, direction: GraphDirection) -> set[frozenset[Vertex]]:
    def search(explorer: Vertex, meta: TarjanSCCMetaData):
        meta.pre_id += 1
        meta.pre_order[explorer] = meta.pre_id
        meta.low_link[explorer] = meta.pre_id
        meta.on_stack[explorer] = True
        meta.stack.append(explorer)

        for edge in meta.traveller.forwards[explorer]:
            forward = meta.traveller.forward(edge)
            if meta.pre_order[forward] == meta.unexplored:
                search(forward, meta)
                meta.low_link[explorer] = min(meta.low_link[explorer], meta.low_link[forward])
            elif meta.on_stack[forward]:
                meta.low_link[explorer] = min(meta.low_link[explorer], meta.pre_order[forward])

        if meta.low_link[explorer] == meta.pre_order[explorer]:
            scc = []
            done = False
            while not done:
                stacked = meta.stack.pop()
                meta.on_stack[stacked] = False
                scc.append(stacked)
                done = explorer == stacked

            if len(scc) > 1:
                meta.non_trivial.add(frozenset(scc))
            else:
                meta.trivial.update(scc)

    traveller = directed_graph.get_traveller(direction)
    meta = TarjanSCCMetaData(traveller)
    for vertex in directed_graph.its_vertices:
        meta.pre_order[vertex] = meta.unexplored
        meta.low_link[vertex] = meta.unexplored
        meta.on_stack[vertex] = False

    for vertex in directed_graph.its_vertices:
        if meta.pre_order[vertex] == meta.unexplored:
            search(vertex, meta)

    return meta.non_trivial


class LoopForest(Tree):
    def __init__(self, directed_graph: DirectedGraph, origin: vertices.Vertex, traveller: GraphTraveller):
        super().__init__()
        self.vertex_to_loop: dict[Vertex, vertices.LoopVertex] = {}
        self._prime_encoding: dict[vertices.LoopVertex, list[int]] = {}
        self._build(directed_graph, origin, traveller)

    def _explore(self, vertex: Vertex, meta: TarjanSCCMetaData):
        meta.pre_id += 1
        meta.pre_order[vertex] = meta.pre_id
        meta.low_link[vertex] = meta.pre_id
        meta.on_stack[vertex] = True
        meta.stack.append(vertex)

        for edge in meta.traveller.forwards[vertex]:
            if edge in meta.alive:
                forward = meta.traveller.forward(edge)
                if meta.pre_order[forward] == meta.unexplored:
                    self._explore(forward, meta)
                    meta.low_link[vertex] = min(meta.low_link[vertex], meta.low_link[forward])
                elif meta.on_stack[forward]:
                    meta.low_link[vertex] = min(meta.low_link[vertex], meta.pre_order[forward])

        if meta.low_link[vertex] == meta.pre_order[vertex]:
            scc = []
            z = None
            while z != vertex:
                z = meta.stack.pop()
                meta.on_stack[z] = False
                scc.append(z)

            self_back_edge = edges.Edge(vertex, vertex)
            # If the SCC contains just this vertex V, then the SCC is NOT a loop only if:
            # (a) the edge V => V is NOT in the CFG (that is, we have found a trivial SCC);
            # (b) the edge V => V is in the CFG but it is now dead because it forms part of a larger loop.
            if len(scc) == 1 and self_back_edge not in meta.alive:
                meta.trivial.add(vertex)
            else:
                meta.non_trivial.add(frozenset(scc))

    def _update(self, loop: vertices.LoopVertex, vertex: Vertex):
        if vertex in self.vertex_to_loop:
            parent = self.vertex_to_loop[vertex]
            parent.body.remove(vertex)
            parent.entries.discard(vertex)
            parent.exits.discard(vertex)
        else:
            parent = self.root

        self.vertex_to_loop[vertex] = loop
        loop.body.add(vertex)

        if loop != self.root:
            edge = edges.Edge(parent, loop)
            if edge not in self.its_edges:
                self.add_edge(edge)

    def _build(self, directed_graph: DirectedGraph, origin: vertices.Vertex, traveller: GraphTraveller):
        # Initialise the meta data.
        meta = TarjanSCCMetaData(traveller)
        for vertex in directed_graph.its_vertices:
            meta.pre_order[vertex] = meta.unexplored
            meta.low_link[vertex] = meta.unexplored
            meta.on_stack[vertex] = False

            for edge in traveller.forwards[vertex]:
                meta.alive.add(edge)

        # Start searching from the origin.
        roots = collections.deque([origin])
        while roots:
            root = roots.popleft()
            if meta.pre_order[root] == meta.unexplored:
                self._explore(root, meta)

            if root == origin:
                # This represents the artificial loop which contains all vertices in the trivial SCCs.
                vertex_id = vertices.get_vertex_identifier(vertices.LoopVertex.IDENTIFIER_PREFIX)
                loop = vertices.LoopVertex(vertex_id)
                self.add_vertex(loop)
                self.root = loop
                for vertex in meta.trivial:
                    self._update(loop, vertex)

            # Create a fresh loop for each newly discovered non-trivial SCC.
            for scc in meta.non_trivial:
                vertex_id = vertices.get_vertex_identifier(vertices.LoopVertex.IDENTIFIER_PREFIX)
                loop = vertices.LoopVertex(vertex_id)
                self.add_vertex(loop)

                for vertex in scc:
                    # Set the
                    self._update(loop, vertex)
                    meta.pre_order[vertex] = meta.unexplored
                    meta.low_link[vertex] = meta.unexplored

                    for edge in meta.traveller.forwards[vertex]:
                        forward = meta.traveller.forward(edge)
                        if forward not in scc:
                            meta.alive.discard(edge)
                            loop.exits.add(vertex)

                    for edge in meta.traveller.backwards[vertex]:
                        backward = meta.traveller.backward(edge)
                        if backward not in scc:
                            loop.entries.add(vertex)
                            if len(scc) > 1:
                                roots.append(vertex)

                    if vertex in loop.entries:
                        for edge in meta.traveller.backwards[vertex]:
                            meta.alive.discard(edge)

            meta.trivial.clear()
            meta.non_trivial.clear()

    def remove_vertex(self, vertex: Vertex | vertices.SuperVertex):
        # The dead super vertex is definitely in the body; but it might not be an entry or an exit, hence why we use
        # 'discard' instead of 'remove.'
        loop = self.vertex_to_loop[vertex]
        loop.body.remove(vertex)
        loop.entries.discard(vertex)
        loop.exits.discard(vertex)
        del self.vertex_to_loop[vertex]

    def headers(self) -> set[Vertex]:
        return set(itertools.chain(*(loop.entries for loop in self.its_vertices)))

    def is_t1_t2_reducible(self) -> bool:
        return len(self.its_vertices) == len({loop for loop in self.its_vertices if len(loop.entries) <= 1})

    def __eq__(self, other: "LoopForest"):
        return super().__eq__(other)

    def __str__(self):
        assert self.root is not None
        loop_info = ''
        queue = collections.deque([self.root])
        while queue:
            loop: vertices.LoopVertex = queue.popleft()
            if loop == self.root:
                parent = None
            else:
                (edge,) = self.predecessors[loop]
                parent = edge.point_a

            loop_info += f'Loop:     {loop.identifier}\n'
            loop_info += f'Parent:   {parent.identifier if parent is not None else ''}\n'
            loop_info += f'Body:     {' '.join(str(vertex) for vertex in loop.body)}\n'
            loop_info += f'Entries:  {' '.join(str(vertex) for vertex in loop.entries)}\n'
            loop_info += f'Exits:    {' '.join(str(vertex) for vertex in loop.exits)}\n'
            loop_info += '\n'

            for edge in self.successors[loop]:
                queue.append(edge.point_b)

        delimiter = '*' * 80
        value = delimiter + '\n'
        value += f'Loops: {len(self.its_vertices) - 1}\n'
        value += f'Depth: {self.height()}\n'
        value += delimiter + '\n'
        value += loop_info
        return value


def build_loop_forest(cfg: ControlFlowGraph, direction: GraphDirection) -> LoopForest:
    origin = cfg.get_origin(direction)
    traveller = cfg.get_traveller(direction)
    return LoopForest(cfg, origin, traveller)


class SuperGraph(DirectedGraph):
    def __init__(self, name: str):
        super().__init__()
        self.name: str = f'super.{name}'
        self.origin: vertices.SuperVertex | None = None

    def do_t0_reductions(self, forest: LoopForest):
        loop: vertices.LoopVertex
        for loop in forest.its_vertices:
            if len(loop.entries) > 1:
                super_header = random.choice(list(loop.entries))
                super_vertex: vertices.SuperVertex
                for super_vertex in loop.entries.difference({super_header}):
                    super_header.vertices.update(super_vertex.vertices)

                    for predecessor_edge in self.predecessors[super_vertex]:
                        super_edge = edges.Edge(predecessor_edge.point_a, super_header)
                        self.add_edge(super_edge)

                    for successor_edge in self.successors[super_vertex]:
                        super_edge = edges.Edge(super_header, successor_edge.point_b)
                        self.add_edge(super_edge)

                    self.remove_vertex(super_vertex)
                    forest.remove_vertex(super_vertex)

    def _gather_t1_and_t2_reductions(self,
                                     candidates: set[vertices.SuperVertex],
                                     t1_reductions: collections.deque[vertices.SuperVertex],
                                     t2_reductions: collections.deque[vertices.SuperVertex],
                                     readied: dict[vertices.SuperVertex, bool]):
        for super_vertex in candidates:
            if not readied[super_vertex]:
                if len(self.predecessors[super_vertex]) == 2:
                    self_edge = edges.Edge(super_vertex, super_vertex)
                    if self_edge in self.predecessors[super_vertex]:
                        t1_reductions.append(super_vertex)
                        readied[super_vertex] = True

                if len(self.predecessors[super_vertex]) == 1:
                    t2_reductions.append(super_vertex)
                    readied[super_vertex] = True

    def _do_t1_or_t2_reduction(self, super_predecessor: vertices.SuperVertex, super_vertex: vertices.SuperVertex):
        candidates = set()
        super_predecessor.vertices.update(super_vertex.vertices)
        for edge in self.successors[super_vertex]:
            super_successor = edge.point_b
            if super_successor != super_vertex:
                candidates.add(super_successor)
                super_edge = edges.Edge(super_predecessor, super_successor)
                self.add_edge(super_edge)

        self.remove_vertex(super_vertex)
        return candidates

    def remove_back_edges(self, forest: LoopForest):
        dead_edges: set[Edge] = set()
        loop: vertices.LoopVertex
        for loop in forest.its_vertices:
            if loop != forest.root:
                (super_header,) = loop.entries
                for edge in self.predecessors[super_header]:
                    other_loop = forest.vertex_to_loop[edge.point_a]
                    if forest.is_ancestor(loop, other_loop):
                        dead_edges.add(edge)

        for edge in dead_edges:
            self.remove_edge(edge)

    def reduce(self):
        t1_reductions = collections.deque()
        t2_reductions = collections.deque()
        readied = {super_vertex: False for super_vertex in self.its_vertices}
        self._gather_t1_and_t2_reductions(self.its_vertices, t1_reductions, t2_reductions, readied)

        while len(self.its_vertices) > 1:
            if t1_reductions or t2_reductions:
                if t1_reductions:
                    super_vertex = t1_reductions.popleft()
                    (edge_one, edge_two) = self.predecessors[super_vertex]
                    if edge_one.point_a == super_vertex:
                        super_predecessor = edge_one.point_b
                    else:
                        super_predecessor = edge_one.point_a
                else:
                    super_vertex = t2_reductions.popleft()
                    (edge,) = self.predecessors[super_vertex]
                    super_predecessor = edge.point_a

                candidates = self._do_t1_or_t2_reduction(super_predecessor, super_vertex)
            else:
                traveller = self.get_traveller(GraphDirection.Forwards)
                forest = LoopForest(self, self.origin, traveller)
                self.do_t0_reductions(forest)
                candidates = self.its_vertices

            self._gather_t1_and_t2_reductions(candidates, t1_reductions, t2_reductions, readied)

    def dotify(self, filename: str):
        if self.origin is not None:
            vertex_list = breadth_first_search(self.origin, self.get_traveller(GraphDirection.Forwards))
            dotify(vertex_list, list(self.its_edges), filename)
        else:
            super().dotify(filename)


def create_finest_super_graph(cfg: ControlFlowGraph, direction: GraphDirection) -> SuperGraph:
    # Try and match the numerical part of the super vertex identifier to its underlying vertex identifier.
    regular_identifiers_regex: re.Pattern = re.compile(r'^[a-zA-Z]+([0-9]+)$')
    bijection = {}
    for vertex in cfg.its_vertices:
        match = regular_identifiers_regex.match(vertex.identifier)
        if match:
            vertex_id = match.group(1)
            bijection[vertex.identifier] = f'{vertices.SuperVertex.IDENTIFIER_PREFIX}{vertex_id}'

    if len(bijection) != len(cfg.its_vertices):
        # If we failed to find a match for all vertices, just assign arbitrary identifiers to the super vertices.
        bijection = {}
        for vertex in cfg.its_vertices:
            bijection[vertex.identifier] = vertices.get_vertex_identifier(vertices.SuperVertex.IDENTIFIER_PREFIX)

    super_graph = SuperGraph(cfg.name)
    mapping = {}
    for vertex in cfg.its_vertices:
        super_vertex = vertices.SuperVertex(bijection[vertex.identifier])
        super_vertex.vertices.add(vertex)
        super_graph.add_vertex(super_vertex)
        mapping[vertex] = super_vertex

    traveller = cfg.get_traveller(direction)
    for vertex in cfg.its_vertices:
        for edge in traveller.forwards[vertex]:
            forward = traveller.forward(edge)
            super_vertex_a = mapping[vertex]
            super_vertex_b = mapping[forward]
            super_edge = edges.Edge(super_vertex_a, super_vertex_b)
            super_graph.add_edge(super_edge)

    super_graph.origin = mapping[cfg.get_origin(direction)]
    return super_graph


def create_t0_reduced_super_graph(cfg: ControlFlowGraph, direction: GraphDirection) -> SuperGraph:
    super_graph = create_finest_super_graph(cfg, direction)
    traveller = super_graph.get_traveller(GraphDirection.Forwards)
    forest = LoopForest(super_graph, super_graph.origin, traveller)
    super_graph.do_t0_reductions(forest)
    super_graph.remove_back_edges(forest)
    assert is_dag(super_graph)
    return super_graph


@dataclasses.dataclass
class InstrumentationProfile:
    vertex_points: set[Vertex] = dataclasses.field(default_factory=set)
    edge_points: set[Edge] = dataclasses.field(default_factory=set)


def create_minimal_path_reconstruction_profile(cfg: ControlFlowGraph) -> InstrumentationProfile:
    profile = InstrumentationProfile()
    profile.vertex_points.add(cfg.entry_vertex)
    profile.vertex_points.add(cfg.exit_vertex)

    # Turn the CFG into one amorphous non-trivial SCC.
    dummy_edge = edges.Edge(cfg.exit_vertex, cfg.entry_vertex)
    cfg.add_edge(dummy_edge)

    start = random.choice(list(cfg.its_vertices))
    queue = collections.deque([start])
    visited = set()
    while queue:
        vertex = queue.popleft()
        visited.add(vertex)

        for edge in cfg.successors[vertex]:
            if edge.point_b in visited or edge.point_b in queue:
                profile.edge_points.add(edge)
            else:
                queue.append( edge.point_b)

    cfg.remove_edge(dummy_edge)

    # The dummy edge may have been selected during the search; if so, it is erroneous.
    profile.edge_points.discard(dummy_edge)

    return profile


class AugmentedControlFlowGraph(ControlFlowGraph):
    def __init__(self, cfg: ControlFlowGraph, profile: InstrumentationProfile):
        super().__init__(cfg.name)
        self.instrumentation = set()
        self._create(cfg, profile)

    def _create(self, cfg: ControlFlowGraph, profile: InstrumentationProfile):
        for vertex in cfg.its_vertices:
            self.add_vertex(vertex)

        for vertex in profile.vertex_points:
            identifier = vertices.get_vertex_identifier(vertices.InstrumentationVertex.IDENTIFIER_PREFIX)
            point = vertices.InstrumentationVertex(identifier)
            self.instrumentation.add(point)
            self.add_vertex(point)

            if vertex == cfg.entry_vertex:
                edge = edges.Edge(point, vertex)
                self.add_edge(edge)
            elif vertex == cfg.exit_vertex:
                edge = edges.Edge(vertex, point)
                self.add_edge(edge)
            else:
                edge = edges.Edge(point, vertex)
                self.add_edge(edge)

        for edge in profile.edge_points:
            identifier = vertices.get_vertex_identifier(vertices.InstrumentationVertex.IDENTIFIER_PREFIX)
            point = vertices.InstrumentationVertex(identifier)
            self.instrumentation.add(point)
            self.add_vertex(point)

            edge_a = edges.Edge(edge.point_a, point)
            self.add_edge(edge_a)

            edge_b = edges.Edge(point, edge.point_b)
            self.add_edge(edge_b)

        for edge in cfg.its_edges:
            if edge not in profile.edge_points:
                self.add_edge(edge)

        self.set_entry_and_exit()


class InstrumentationPointGraph(ControlFlowGraph):
    def __init__(self, augmented: AugmentedControlFlowGraph):
        super().__init__(augmented.name)
        self.path_expressions = {}
        for vertex in augmented.instrumentation:
            self.add_vertex(vertex)
        self._create(augmented)

    def _create(self, augmented: AugmentedControlFlowGraph):
        for edge in augmented.its_edges:
            self.path_expressions[edge] = []

        dead = augmented.its_vertices.difference(augmented.instrumentation)
        scrap = augmented.copy()
        for vertex in dead:
            for predecessor_edge in scrap.predecessors[vertex]:
                for successor_edge in scrap.successors[vertex]:
                    predecessor = predecessor_edge.point_a
                    successor = successor_edge.point_b
                    a_to_b = edges.Edge(predecessor, vertex)
                    b_to_c = edges.Edge(vertex, successor)
                    a_to_c = edges.Edge(predecessor, successor)
                    scrap.add_edge(a_to_c)
                    self.path_expressions[a_to_c] = self.path_expressions[a_to_b] + [vertex] + self.path_expressions[b_to_c]

            scrap.remove_vertex(vertex)

        for vertex in scrap.its_vertices:
            self.add_vertex(vertex)

        for edge in scrap.its_edges:
            self.add_edge(edge)
            self.path_expressions[edge] = [edge.point_a] + self.path_expressions[edge] + [edge.point_b]

        self.set_entry_and_exit()
