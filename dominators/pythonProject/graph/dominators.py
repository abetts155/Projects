import collections
import dataclasses

from graph import edges
from graph import graphs
from graph import vertices
from graph.graphs import Edge


@dataclasses.dataclass(slots=True)
class LengauerTarjanMeta:
    idom: dict[vertices.Vertex, vertices.Vertex] = dataclasses.field(default_factory=dict)
    label: dict[vertices.Vertex, vertices.Vertex] = dataclasses.field(default_factory=dict)
    parent: dict[vertices.Vertex, vertices.Vertex] = dataclasses.field(default_factory=dict)
    ancestor: dict[vertices.Vertex, vertices.Vertex] = dataclasses.field(default_factory=dict)
    child: dict[vertices.Vertex, vertices.Vertex] = dataclasses.field(default_factory=dict)
    bucket: dict[vertices.Vertex, set[vertices.Vertex]] = dataclasses.field(default_factory=dict)
    size: dict[vertices.Vertex, int] = dataclasses.field(default_factory=dict)
    semi: dict[vertices.Vertex, int] = dataclasses.field(default_factory=dict)
    pre_numbers: dict[int, vertices.Vertex] = dataclasses.field(default_factory=dict)
    pre_id: int = 0


class LengauerTarjan(graphs.Tree):
    def __init__(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        super().__init__()
        self.root = cfg.get_origin(direction)
        traveller = cfg.get_traveller(direction)
        self._build(traveller)

    def _link(self, left: vertices.Vertex, right: vertices.Vertex, meta: LengauerTarjanMeta) -> None:
        s: vertices.Vertex = right
        while meta.semi[meta.label[right]] < meta.semi[meta.label[meta.child[s]]]:
            if meta.size[s] + meta.size[meta.child[meta.child[s]]] >= 2 * meta.size[meta.child[s]]:
                meta.ancestor[meta.child[s]] = s
                meta.child[s] = meta.child[meta.child[s]]
            else:
                meta.size[meta.child[s]] = meta.size[s]
                meta.ancestor[s] = meta.child[s]
                s = meta.ancestor[s]

        meta.label[s] = meta.label[right]
        meta.size[left] += meta.size[right]

        if meta.size[left] < 2 * meta.size[right]:
            s, meta.child[left] = meta.child[left], s

        while s != self.root:
            meta.ancestor[s] = left
            s = meta.child[s]

    def _compress(self, vertex: vertices.Vertex, meta: LengauerTarjanMeta) -> None:
        if meta.ancestor[meta.ancestor[vertex]] != self.root:
            self._compress(meta.ancestor[vertex], meta)
            if meta.semi[meta.label[meta.ancestor[vertex]]] < meta.semi[meta.label[vertex]]:
                meta.label[vertex] = meta.label[meta.ancestor[vertex]]
            meta.ancestor[vertex] = meta.ancestor[meta.ancestor[vertex]]

    def _evaluate(self, vertex: vertices.Vertex, meta: LengauerTarjanMeta) -> vertices.Vertex:
        if meta.ancestor[vertex] == self.root:
            return meta.label[vertex]
        else:
            self._compress(vertex, meta)
            if meta.semi[meta.label[meta.ancestor[vertex]]] >= meta.semi[meta.label[vertex]]:
                return meta.label[vertex]
            else:
                return meta.label[meta.ancestor[vertex]]

    def _visit(self, vertex: vertices.Vertex, traveller: graphs.GraphTraveller, meta: LengauerTarjanMeta) -> None:
        self.add_vertex(vertex)
        meta.pre_id += 1
        meta.pre_numbers[meta.pre_id] = vertex
        meta.semi[vertex] = meta.pre_id
        meta.label[vertex] = vertex
        meta.ancestor[vertex] = self.root
        meta.child[vertex] = self.root
        meta.size[vertex] = 1
        meta.bucket[vertex] = set()

        for edge in traveller.forwards[vertex]:
            forward = traveller.forward(edge)
            if forward not in self.its_vertices:
                meta.parent[forward] = vertex
                self._visit(forward, traveller, meta)

    def _build(self, traveller: graphs.GraphTraveller):
        meta = LengauerTarjanMeta()
        meta.size[self.root] = 0
        meta.ancestor[self.root] = self.root
        meta.label[self.root] = self.root

        # Step 1: Do depth-first search.
        self._visit(self.root, traveller, meta)

        # Step 2: Compute semi-dominators.
        for i in reversed(range(2, meta.pre_id + 1)):
            w: vertices.Vertex = meta.pre_numbers[i]

            for step in traveller.backwards[w]:
                u: vertices.Vertex = self._evaluate(step, meta)
                if meta.semi[u] < meta.semi[w]:
                    meta.semi[w] = meta.semi[u]

            v: vertices.Vertex = meta.pre_numbers[meta.semi[w]]
            meta.bucket[v].update({w})
            self._link(meta.parent[w], w, meta)

            while meta.bucket[meta.parent[w]]:
                v = meta.bucket[meta.parent[w]].pop()
                u = self._evaluate(v, meta)
                if meta.semi[u] < meta.semi[v]:
                    meta.idom[v] = u
                else:
                    meta.idom[v] = meta.parent[w]

        # Step 3: Set immediate dominators.
        for i in range(2, meta.pre_id + 1):
            w: vertices.Vertex = meta.pre_numbers[i]
            if meta.idom[w] != meta.pre_numbers[meta.semi[w]]:
                meta.idom[w] = meta.idom[meta.idom[w]]

        # Step 4: Turn the immediate dominators into tree edges.
        for vertex in self.vertex_set:
            if vertex != self.root:
                edge = edges.Edge(meta.idom[vertex], vertex)
                self.add_edge(edge)


@dataclasses.dataclass(slots=True)
class VertexOrderingBijection:
    number_to_vertex: dict[int, vertices.Vertex] = dataclasses.field(default_factory=dict)
    vertex_to_number: dict[vertices.Vertex, int] = dataclasses.field(default_factory=dict)

    def set(self, vertex: vertices.Vertex, number: int):
        self.vertex_to_number[vertex] = number
        self.number_to_vertex[number] = vertex

    def get_number(self, vertex: vertices.Vertex) -> int:
        return self.vertex_to_number[vertex]

    def get_vertex(self, number: int) -> vertices.Vertex:
        return self.number_to_vertex[number]


@dataclasses.dataclass(slots=True)
class CooperMeta:
    idom: dict[vertices.Vertex, vertices.Vertex] = dataclasses.field(default_factory=dict)
    post_numbering: VertexOrderingBijection = dataclasses.field(default_factory=VertexOrderingBijection)
    post_id: int = 0


class Cooper(graphs.Tree):
    def __init__(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        super().__init__()
        self.root = cfg.get_origin(direction)
        traveller = cfg.get_traveller(direction)
        self._build(traveller)

    def _visit(self, vertex: vertices.Vertex, traveller: graphs.GraphTraveller, meta: CooperMeta):
        self.add_vertex(vertex)
        meta.idom[vertex] = vertex

        for edge in traveller.forwards[vertex]:
            forward = traveller.forward(edge)
            if forward not in self.its_vertices:
                self._visit(forward, traveller, meta)

        meta.post_id += 1
        meta.post_numbering.set(vertex, meta.post_id)

    def _build(self, traveller: graphs.GraphTraveller):
        meta = CooperMeta()
        self._visit(self.root, traveller, meta)

        changed = True
        while changed:
            changed = False
            for i in reversed(range(1, meta.post_id + 1)):
                vertex: vertices.Vertex = meta.post_numbering.get_vertex(i)
                candidates = [
                    step for step in traveller.backwards[vertex] if meta.idom[step] != step or step == self.root
                ]

                if candidates:
                    possible_idom: vertices.Vertex = candidates.pop()

                    for step in candidates:
                        left_finger: int = meta.post_numbering.get_number(step)
                        right_finger: int = meta.post_numbering.get_number(possible_idom)

                        while left_finger != right_finger:
                            while left_finger < right_finger:
                                ancestor: vertices.Vertex = meta.post_numbering.get_vertex(left_finger)
                                left_finger = meta.post_numbering.get_number(meta.idom[ancestor])

                            while right_finger < left_finger:
                                ancestor: vertices.Vertex = meta.post_numbering.get_vertex(right_finger)
                                right_finger = meta.post_numbering.get_number(meta.idom[ancestor])

                        possible_idom = meta.post_numbering.get_vertex(left_finger)

                    if possible_idom != meta.idom[vertex]:
                        meta.idom[vertex] = possible_idom
                        changed = True

        for vertex in self.its_vertices:
            if vertex != self.root:
                edge = edges.Edge(meta.idom[vertex], vertex)
                self.add_edge(edge)


@dataclasses.dataclass(slots=True)
class LeastCommonAncestor:
    root: vertices.Vertex
    parents: dict[vertices.Vertex, set[vertices.Vertex]]
    level: dict[vertices.Vertex, int] = dataclasses.field(default_factory=dict)

    def __post_init__(self):
        self.level[self.root] = 0

    def answer(self, left: vertices.Vertex, right: vertices.Vertex) -> vertices.Vertex:
        while self.level[left] != self.level[right]:
            if self.level[left] > self.level[right]:
                (left,) = self.parents[left]
            else:
                (right,) = self.parents[right]

        while left != right:
            (left,) = self.parents[left]
            (right,) = self.parents[right]

        return left

    def update_level(self, vertex: vertices.Vertex):
        assert vertex != self.root
        (parent,) = self.parents[vertex]
        self.level[vertex] = self.level[parent] + 1


class BettsOffline(graphs.Tree):
    def __init__(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        super().__init__()
        self.root = cfg.get_origin(direction)
        self._build(cfg, direction)

    def _build(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        super_graph = graphs.create_t0_reduced_super_graph(cfg, direction)
        cfg_traveller = cfg.get_traveller(direction)
        lca = LeastCommonAncestor(self.root, self.predecessors)

        traversed_predecessors: dict[vertices.SuperVertex, int] = {}
        queue: collections.deque[vertices.SuperVertex] = collections.deque([super_graph.origin])
        while queue:
            super_vertex: vertices.SuperVertex = queue.popleft()
            for vertex in super_vertex.vertices:
                self.add_vertex(vertex)

            # Gather the predecessors which will participate in the LCA query.
            left = set()
            super_predecessor: vertices.SuperVertex
            for super_predecessor in super_graph.predecessors[super_vertex]:
                left.update(super_predecessor.vertices)

            right = set()
            for vertex in super_vertex.vertices:
                for step in cfg_traveller.backwards[vertex]:
                    right.add(step)

            predecessors = left.intersection(right)
            if predecessors:
                if len(predecessors) == 1:
                    (idom,) = predecessors
                else:
                    left = predecessors.pop()
                    right = predecessors.pop()
                    idom = lca.answer(left, right)
                    while predecessors:
                        other = predecessors.pop()
                        idom = lca.answer(idom, other)

                for vertex in super_vertex.vertices:
                    edge = edges.Edge(idom, vertex)
                    self.add_edge(edge)
                    lca.update_level(vertex)

            super_successor: vertices.SuperVertex
            for super_successor in super_graph.successors[super_vertex]:
                if super_successor not in traversed_predecessors:
                    traversed_predecessors[super_successor] = 0
                traversed_predecessors[super_successor] += 1

                if traversed_predecessors[super_successor] == len(super_graph.predecessors[super_successor]):
                    queue.append(super_successor)


@dataclasses.dataclass(slots=True)
class BettsOnlineMeta:
    articulations: set[vertices.Vertex] = dataclasses.field(default_factory=set)
    traversed_predecessors: dict[vertices.Vertex, int] = dataclasses.field(default_factory=dict)
    fixed_vertices: set[vertices.Vertex] = dataclasses.field(default_factory=set)
    join_vertices: set[vertices.Vertex] = dataclasses.field(default_factory=set)
    join_graph: dict[vertices.Vertex, set[vertices.Vertex]] = dataclasses.field(default_factory=dict)
    data: dict[vertices.Vertex, list[vertices.Vertex]] = dataclasses.field(default_factory=dict)


def answer_query(left: list[vertices.Vertex], right: list[vertices.Vertex]) -> list[vertices.Vertex]:
    i = min(len(left) - 1, len(right) - 1)
    while i >= 0 and left[i] != right[i]:
        i -= 1
    return left[:i + 1]


class BettsOnline(graphs.Tree):
    def __init__(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        super().__init__()
        self.root = cfg.get_origin(direction)
        self._build(cfg, direction)

    def _completed(self, cfg: graphs.ControlFlowGraph, meta: BettsOnlineMeta):
        return len(cfg.vertex_set) == len(meta.fixed_vertices)

    def _update_join(self,
                     vertex: vertices.Vertex,
                     join: vertices.Vertex,
                     lca: LeastCommonAncestor,
                     meta: BettsOnlineMeta):
        (old_parent,) = self.predecessors[join]
        new_parent = lca.answer(old_parent, vertex)

        if old_parent != new_parent:
            edge = edges.Edge(old_parent, join)
            self.remove_edge(edge)

            edge = edges.Edge(new_parent, join)
            self.add_edge(edge)

            subtree: set[vertices.Vertex] = set()
            stack: list[vertices.Vertex] = [join]
            while stack:
                root = stack.pop()
                lca.update_level(root)
                for child in self.successors[root]:
                    stack.append(child)

                if root in meta.join_graph:
                    subtree.add(root)

            if new_parent in meta.articulations:
                meta.fixed_vertices.add(join)

            for root in subtree:
                for successor in meta.join_graph[root]:
                    if successor not in subtree:
                        self._update_join(root, successor, lca, meta)

    def _analyse_non_join(self,
                          vertex: vertices.Vertex,
                          step: vertices.Vertex,
                          lca: LeastCommonAncestor,
                          meta: BettsOnlineMeta):
        self.add_vertex(step)
        edge = edges.Edge(vertex, step)
        self.add_edge(edge)
        lca.update_level(step)
        meta.fixed_vertices.add(step)

    def _analyse_join(self,
                      vertex: vertices.Vertex,
                      step: vertices.Vertex,
                      lca: LeastCommonAncestor,
                      meta: BettsOnlineMeta):
        if step not in meta.traversed_predecessors:
            self.add_vertex(step)
            meta.traversed_predecessors[step] = 1

            edge = edges.Edge(vertex, step)
            self.add_edge(edge)
            lca.update_level(step)

            if vertex in meta.articulations:
                meta.fixed_vertices.add(step)
        else:
            if step not in meta.fixed_vertices:
                self._update_join(vertex, step, lca, meta)

            meta.traversed_predecessors[step] += 1

    def _old_build(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        meta = BettsOnlineMeta()
        cfg_traveller = cfg.get_traveller(direction)
        lca = LeastCommonAncestor(self.root, self.predecessors)
        self.add_vertex(self.root)
        meta.fixed_vertices.add(self.root)

        frontier = collections.deque([self.root])
        while frontier and not self._completed(cfg, meta):
            search_root = frontier.popleft()

            ready = [search_root]
            while ready and not self._completed(cfg, meta):
                vertex = ready.pop()

                if not meta.join_vertices and not ready and search_root == self.root:
                    meta.articulations.add(vertex)

                for step in cfg_traveller.forwards[vertex]:
                    if len(cfg_traveller.backwards[step]) == 1:
                        self._analyse_non_join(vertex, step, lca, meta)
                        ready.append(step)
                    elif step != search_root:
                        self._analyse_join(vertex, step, lca, meta)
                        meta.join_vertices.add(step)

                        if meta.traversed_predecessors[step] == len(cfg_traveller.backwards[step]):
                            if step not in meta.join_graph:
                                ready.append(step)
                                meta.join_vertices.remove(step)
                                meta.fixed_vertices.add(step)

            for join in meta.join_vertices:
                if join not in meta.join_graph:
                    meta.join_graph[join] = set()
                    if join in meta.fixed_vertices:
                        frontier.appendleft(join)
                    else:
                        frontier.append(join)

                if search_root != self.root and join not in meta.fixed_vertices:
                    meta.join_graph[search_root].add(join)

            meta.join_vertices.clear()

    def _build(self, cfg: graphs.ControlFlowGraph, direction: graphs.GraphDirection):
        meta = BettsOnlineMeta()
        cfg_traveller = cfg.get_traveller(direction)
        self.add_vertex(self.root)
        meta.fixed_vertices.add(self.root)

        frontier = collections.deque([self.root])
        while frontier and not self._completed(cfg, meta):
            search_root = frontier.popleft()

            ready = [search_root]
            while ready and not self._completed(cfg, meta):
                vertex = ready.pop()

                if not meta.join_vertices and not ready:
                    meta.data[vertex] = list()

                if len(cfg_traveller.forwards[vertex]) > 1:
                    meta.data[vertex].append(vertex)

                for step in cfg_traveller.forwards[vertex]:
                    if len(cfg_traveller.backwards[step]) == 1:
                        self.add_vertex(step)
                        edge = edges.Edge(vertex, step)
                        self.add_edge(edge)
                        meta.fixed_vertices.add(step)
                        meta.data[step] = list(meta.data[vertex])
                        ready.append(step)
                    else:
                        if step not in meta.traversed_predecessors:
                            self.add_vertex(step)
                            meta.traversed_predecessors[step] = 1
                            meta.join_vertices.add(step)

                            if meta.data[vertex][-1] in meta.articulations:
                                edge = edges.Edge(meta.data[vertex][-1], step)
                                self.add_edge(edge)
                                meta.fixed_vertices.add(step)
                        else:
                            if step not in meta.fixed_vertices:
                                meta.traversed_predecessors[step] += 1

                                if meta.traversed_predecessors[step] == len(cfg_traveller.backwards[step]):
                                    for i, back_step in enumerate(cfg_traveller.backwards[step]):
                                        if i == 0:
                                            meta.data[step] = list(meta.data[back_step])
                                        else:
                                            meta.data[step] = answer_query(meta.data[step], meta.data[back_step])

                                    edge = edges.Edge(meta.data[step][-1], step)
                                    self.add_edge(edge)
                                    meta.fixed_vertices.add(step)
                                    meta.join_vertices.remove(step)
                                    ready.append(step)

            for join in meta.join_vertices:
                if join not in meta.join_graph:
                    meta.join_graph[join] = set()
                    if join in meta.fixed_vertices:
                        frontier.appendleft(join)
                    else:
                        frontier.append(join)

                if search_root != self.root and join not in meta.fixed_vertices:
                    meta.join_graph[search_root].add(join)

            meta.join_vertices.clear()


class DominanceFrontiers(graphs.DirectedGraph):
    def __init__(self, cfg: graphs.ControlFlowGraph, dominator: graphs.Tree, direction: graphs.GraphDirection):
        assert cfg.get_origin(direction) == dominator.root
        super().__init__()
        traveller = cfg.get_traveller(direction)
        self._build(dominator, traveller)

    def _build(self, dominator: graphs.Tree, traveller: graphs.GraphTraveller):
        merges = set()
        for vertex in dominator.its_vertices:
            self.add_vertex(vertex)
            if len(traveller.backwards[vertex]) > 1:
                merges.add(vertex)

        for merge in merges:
            (idom,) = dominator.predecessors[merge]
            for edge in traveller.backwards[merge]:
                backward = traveller.backward(edge)
                runner = backward
                while runner != idom:
                    frontier_edge = edges.Edge(runner, merge)
                    self.add_edge(frontier_edge)
                    (runner,) = dominator.predecessors[runner]
