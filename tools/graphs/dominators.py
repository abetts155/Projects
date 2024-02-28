from collections import deque
from random import sample
from typing import Dict, Set

from graphs.edges import Edge
from graphs.graphs import ControlFlowGraph, DirectedGraph, Tarjan
from graphs.vertices import Vertex, SuperBlock
from utils import dot, messages


class DominatorTree:
    def __init__(self):
        self.idom = {}
        self.level = {}
        self.children = {}
        self.root = None

    def set_root(self, vertex: Vertex):
        self.root = vertex

    def add_vertex(self, vertex: Vertex):
        self.idom[vertex] = None
        self.level[vertex] = 0
        self.children[vertex] = set()

    def add_edge(self, predecessor: Vertex, successor: Vertex):
        self.children[predecessor].add(successor)
        self.idom[successor] = predecessor
        self.level[successor] = self.level[predecessor] + 1

    def remove_edge(self, predecessor: Vertex, successor: Vertex):
        self.children[predecessor].remove(successor)
        self.idom[successor] = None

    def do_lca(self, left, right) -> Vertex:
        while self.level[left] != self.level[right]:
            if self.level[left] > self.level[right]:
                left = self.idom[left]
            else:
                right = self.idom[right]

        while left != right:
            left = self.idom[left]
            right = self.idom[right]

        return left

    def __str__(self):
        assert self.root is not None
        value = ''
        queue = deque([self.root])
        while queue:
            vertex = queue.popleft()
            for child in self.children[vertex]:
                value += 'idom({}) = {}\n'.format(child, vertex)
                queue.append(child)
        return value


def update_join(predecessor: Vertex,
                join: Vertex,
                overlay: Dict[Vertex, Set],
                tree: DominatorTree,
                articulations: Set,
                fixed: Set):
    before = tree.idom[join]
    after = tree.do_lca(before, predecessor)

    if before != after:
        tree.remove_edge(before, join)
        tree.add_edge(after, join)

        if after in articulations:
            fixed.add(join)

        subtree = set()
        stack = [join]
        while stack:
            root = stack.pop()
            for child in tree.children[root]:
                tree.level[child] = tree.level[root] + 1
                stack.append(child)

            if root in overlay:
                subtree.add(root)

        for root in subtree:
            for successor in overlay[root]:
                if successor not in subtree:
                    update_join(root, successor, overlay, tree, articulations, fixed)


def is_dominator_tree_fixed(cfg: ControlFlowGraph, fixed: Set):
    return cfg.number_of_vertices() - 1 == len(fixed)


def jail_and_free(origin: Vertex, cfg: ControlFlowGraph, tree: DominatorTree):
    overlay = {}
    explored = {}
    joins = set()
    articulations = set()
    fixed = set()

    frontier = deque([origin])
    while frontier and not is_dominator_tree_fixed(cfg, fixed):
        joins.clear()
        root = frontier.popleft()

        ready = [root]
        while ready and not is_dominator_tree_fixed(cfg, fixed):
            vertex = ready.pop()

            if not joins and not ready and root == origin:
                articulations.add(vertex)

            for edge in cfg.successors(vertex):
                predecessor = edge.predecessor()
                successor = edge.successor()

                if len(cfg.predecessors(successor)) == 1:
                    tree.add_vertex(successor)
                    tree.add_edge(predecessor, successor)
                    ready.append(successor)
                    fixed.add(successor)
                elif root != successor.id_:
                    joins.add(successor)
                    if successor not in explored:
                        explored[successor] = 1
                        tree.add_vertex(successor)
                        tree.add_edge(predecessor, successor)

                        if predecessor in articulations:
                            fixed.add(successor)
                    else:
                        if successor not in fixed:
                            update_join(predecessor, successor, overlay, tree, articulations, fixed)

                        explored[successor] += 1
                        if explored[successor] == len(cfg.predecessors(successor)) and successor not in overlay:
                            ready.append(successor)
                            joins.remove(successor)
                            fixed.add(successor)

        for join in joins:
            if join not in overlay:
                overlay[join] = set()
                if join in fixed:
                    frontier.appendleft(join)
                else:
                    frontier.append(join)

            if root != origin and join not in fixed:
                overlay[root].add(join)


def online(cfg: ControlFlowGraph, origin: Vertex):
    tree = DominatorTree()
    tree.add_vertex(origin)
    jail_and_free(origin, cfg, tree)
    return tree.idom


class SuperGraph(DirectedGraph):
    def __init__(self, name):
        DirectedGraph.__init__(self)
        self._name = name
        self._vertex_to_super_vertex = {}
        self._headers = set()

    def create_super_vertex(self, vertices: Set[Vertex], represents_headers: bool):
        super_vertex = SuperBlock(Vertex.get_vertex_id())
        self.add_vertex(super_vertex)
        super_vertex.extend(vertices)
        for vertex in vertices:
            self._vertex_to_super_vertex[vertex] = super_vertex

        if represents_headers:
            self._headers.add(super_vertex)

    def is_header(self, super_vertex: SuperBlock) -> bool:
        return super_vertex in self._headers

    def get_super_vertex(self, vertex: Vertex) -> SuperBlock:
        return self._vertex_to_super_vertex[vertex]

    def has_super_vertex(self, vertex: Vertex) -> bool:
        return vertex in self._vertex_to_super_vertex

    def dotify(self):
        data = []
        for super_vertex in self:
            label = []
            label.append(dot.HTML.open_html)
            label.append(dot.HTML.open_table)

            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell())
            label.append(','.join(str(vertex.id_) for vertex in super_vertex))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

            label.append(dot.HTML.close_table)
            label.append(dot.HTML.close_html)

            data.append('{} [label={}, shape=record];\n'.format(super_vertex.id_, ''.join(label)))

            for edge in self.successors(super_vertex):
                data.append(edge.dotify())

        filename = '{}.super.dot'.format(self._name)
        dot.generate(filename, data)

    def __str__(self):
        value = ''
        for super_vertex in self:
            value += '{}\n'.format(str(super_vertex))
            for edge in self.successors(super_vertex):
                super_successor = edge.successor()
                value += '{}\n'.format(edge)
            value += '\n'
        return value


class GraphTraveller:
    def __init__(self, forwards, forward, backwards, backward):
        self.forwards = forwards
        self.forward = forward
        self.backwards = backwards
        self.backward = backward


class Loop:
    def __init__(self, identifier: int):
        self.identifier = identifier
        self.body = set()
        self.entries = set()
        self.exits = set()


class LoopForest:
    def __init__(self):
        self.loops = []
        self.parent = {}
        self.children = {}
        self.vertex_to_loop = {}
        self.root = None
        self.cycling_edges = set()

    def add_loop(self) -> Loop:
        loop = Loop(len(self.loops))
        self.loops.append(loop)
        self.parent[loop] = None
        self.children[loop] = set()
        return loop

    def update(self, loop: Loop, vertex: Loop):
        if vertex in self.vertex_to_loop:
            parent = self.vertex_to_loop[vertex]
            parent.body.remove(vertex)
        else:
            parent = self.root

        self.vertex_to_loop[vertex] = loop
        loop.body.add(vertex)

        if loop != self.root:
            self.parent[loop] = parent
            self.children[parent].add(loop)

    def structured(self) -> bool:
        not_multi_entry_loops = [loop for loop in self.loops if len(loop.entries) <= 1]
        return len(not_multi_entry_loops) == len(self.loops) and len(self.cycling_edges) == len(self.loops) - 1

    def __iter__(self):
        for loop in self.loops:
            yield loop

    def __reversed__(self):
        for loop in reversed(self.loops):
            yield loop

    def __len__(self):
        return len(self.loops)

    def __str__(self):
        assert self.root is not None
        depth = {self.root: 0}
        loop_info = ''
        queue = deque([self.root])
        while queue:
            loop = queue.popleft()
            loop_info += 'Loop:     {}\n'.format(loop.identifier)
            loop_info += 'Parent:   {}\n'.format(self.parent[loop].identifier if loop != self.root else '')
            loop_info += 'Body:     {}\n'.format(' '.join(str(vertex) for vertex in sorted(loop.body)))
            loop_info += 'Entries:  {}\n'.format(' '.join(str(vertex) for vertex in sorted(loop.entries)))
            loop_info += 'Exits:    {}\n'.format(' '.join(str(vertex) for vertex in sorted(loop.exits)))
            loop_info += '\n'

            for child in self.children[loop]:
                queue.append(child)
                depth[child] = depth[loop] + 1

        delimiter = '*' * 80
        value = delimiter + '\n'
        value += 'Loops:   {}\n'.format(len(self.loops))
        value += 'Depth:   {}\n'.format(max(depth.values()))
        value += 'Cycling: {}\n'.format(' '.join(str(edge) for edge in self.cycling_edges))
        value += delimiter + '\n'
        value += loop_info
        return value


def identify_loops(cfg: ControlFlowGraph, origin: Vertex, traveller: GraphTraveller):
    pre_order = {}
    low_link = {}
    on_stack = {}
    alive = {}
    stack = []
    trivial = set()
    non_trivial = set()
    pre_id = 0

    def explore(vertex: Vertex):
        nonlocal pre_id
        pre_id += 1
        pre_order[vertex] = pre_id
        low_link[vertex] = pre_id
        on_stack[vertex] = True
        stack.append(vertex)

        for edge in traveller.forwards(cfg, vertex):
            if alive[edge]:
                after = traveller.forward(edge)
                if pre_order[after] == 0:
                    explore(after)
                    low_link[vertex] = min(low_link[vertex], low_link[after])
                elif on_stack[after]:
                    low_link[vertex] = min(low_link[vertex], pre_order[after])

        if low_link[vertex] == pre_order[vertex]:
            scc = []
            z = None
            while z != vertex:
                z = stack.pop()
                on_stack[z] = False
                scc.append(z)

            if len(scc) == 1:
                trivial.add(vertex)
            else:
                non_trivial.add(frozenset(scc))

    for vertex in cfg:
        pre_order[vertex] = 0
        low_link[vertex] = 0
        on_stack[vertex] = False
        for edge in traveller.forwards(cfg, vertex):
            alive[edge] = True

    forest = LoopForest()
    roots = deque([origin])
    while roots:
        root = roots.popleft()
        if pre_order[root] == 0:
            explore(root)

        if root == origin:
            loop = forest.add_loop()
            forest.root = loop
            for vertex in trivial:
                forest.update(loop, vertex)

        for scc in non_trivial:
            loop = forest.add_loop()
            for vertex in scc:
                forest.update(loop, vertex)

                pre_order[vertex] = 0
                low_link[vertex] = 0

                for edge in traveller.forwards(cfg, vertex):
                    after = traveller.forward(edge)
                    if after not in scc:
                        alive[edge] = False
                        loop.exits.add(vertex)

                for edge in traveller.backwards(cfg, vertex):
                    before = traveller.backward(edge)
                    if before not in scc:
                        loop.entries.add(vertex)
                        roots.append(vertex)

                if vertex in loop.entries:
                    for edge in traveller.backwards(cfg, vertex):
                        alive[edge] = False
                        before = traveller.backward(edge)
                        if before in scc:
                            forest.cycling_edges.add(edge)

        trivial.clear()
        non_trivial.clear()

    return forest


def build_maximal_dag(cfg: ControlFlowGraph, traveller: GraphTraveller, forest: LoopForest):
    super_graph = SuperGraph(cfg.name)

    for loop in forest:
        if loop != forest.root:
            super_graph.create_super_vertex(loop.entries, True)

        for vertex in loop.body.difference(loop.entries):
            super_graph.create_super_vertex({vertex}, False)

    for predecessor in cfg:
        super_predecessor = super_graph.get_super_vertex(predecessor)
        for edge in traveller.forwards(cfg, predecessor):
            successor = traveller.forward(edge)
            if edge not in forest.cycling_edges:
                super_successor = super_graph.get_super_vertex(successor)
                if super_predecessor != super_successor and not super_graph.has_edge(super_predecessor, super_successor):
                    super_edge = Edge(super_predecessor, super_successor)
                    super_graph.add_edge(super_edge)

    return super_graph


def build_dominator_tree_from_maximal_dag(super_graph: SuperGraph,
                                          cfg: ControlFlowGraph,
                                          origin: Vertex,
                                          traveller: GraphTraveller):
    tree = DominatorTree()
    tree.set_root(origin)

    counts = {}
    super_origin = super_graph.get_super_vertex(origin)
    queue = deque([super_origin])
    while queue:
        super_vertex = queue.popleft()

        predecessors = set()
        for vertex in super_vertex:
            tree.add_vertex(vertex)
            for edge in traveller.backwards(cfg, vertex):
                predecessors.add(traveller.backward(edge))

        filter = set()
        for super_edge in super_graph.predecessors(super_vertex):
            super_predecessor = super_edge.predecessor()
            filter.update(super_predecessor)

        predecessors = predecessors.intersection(filter)
        if predecessors:
            if len(predecessors) == 1:
                (idom,) = predecessors
            else:
                left = predecessors.pop()
                right = predecessors.pop()
                idom = tree.do_lca(left, right)
                while predecessors:
                    other = predecessors.pop()
                    idom = tree.do_lca(idom, other)

            for vertex in super_vertex:
                tree.add_edge(idom, vertex)

        for edge in super_graph.successors(super_vertex):
            super_successor = edge.successor()
            if super_successor not in counts:
                counts[super_successor] = 0
            counts[super_successor] += 1

            if counts[super_successor] == len(super_graph.predecessors(super_successor)):
                queue.append(super_successor)

    return tree


def verify_immediate_dominators(cfg: ControlFlowGraph, origin: Vertex, tree: DominatorTree):
    tarjan = Tarjan(cfg, origin)
    differences = 0
    for vertex in cfg:
        if vertex != origin:
            left = tree.idom[vertex]
            right = tarjan.idom[vertex]

            if left != right:
                print('{}: betts({}) = {}  tarjan({}) = {}'.format(cfg.name,
                                                                   vertex,
                                                                   left,
                                                                   vertex,
                                                                   right))
                differences += 1

    if differences:
        messages.error_message('Verification failed')
    else:
        print('Verified')


def build_inverse_super_graph(cfg: ControlFlowGraph,
                              origin: Vertex,
                              traveller: GraphTraveller,
                              forest: LoopForest):
    super_graph = SuperGraph(cfg.name)
    for loop in forest:
        if loop != forest.root:
            super_graph.create_super_vertex(loop.exits, True)

        for vertex in loop.body:
            if vertex not in loop.exits:
                super_graph.create_super_vertex({vertex}, False)

    elsewhere = set()
    acyclic_edges = []
    for loop in forest:
        stack = deque()
        if loop == forest.root:
            stack.append(origin)
        else:
            (the_header,) = loop.entries
            for edge in traveller.forwards(cfg, the_header):
                if edge in forest.cycling_edges:
                    after = traveller.forward(edge)
                    if after not in elsewhere:
                        stack.append(after)

        region = set()
        while stack:
            vertex = stack.popleft()
            region.add(vertex)

            for edge in traveller.forwards(cfg, vertex):
                if edge not in forest.cycling_edges:
                    after = traveller.forward(edge)

                    if after not in elsewhere:
                        acyclic_edges.append(edge)

                        if after not in region and after not in stack:
                            stack.append(after)

        if loop != forest.root and region:
            (the_header,) = loop.entries
            for edge in traveller.forwards(cfg, the_header):
                if edge in forest.cycling_edges:
                    after = traveller.forward(edge)
                    if after not in elsewhere:
                        acyclic_edges.append(edge)

        elsewhere.update(region)

    for edge in acyclic_edges:
        super_predecessor = super_graph.get_super_vertex(traveller.backward(edge))
        super_successor = super_graph.get_super_vertex(traveller.forward(edge))
        if super_successor != super_predecessor and not super_graph.has_edge(super_predecessor, super_successor):
            super_edge = Edge(super_predecessor, super_successor)
            super_graph.add_edge(super_edge)

    return super_graph


def remove_back_edges(super_graph: SuperGraph, origin: Vertex):
    back_edges = set()
    pre_id = {}
    post_id = {}
    pre = 0
    post = 0

    def explore(super_vertex: SuperBlock):
        nonlocal pre, post
        pre += 1
        pre_id[super_vertex] = pre

        for edge in super_graph.successors(super_vertex):
            super_successor = edge.successor()
            if pre_id[super_successor] == 0:
                explore(super_successor)
            elif pre_id[super_vertex] < pre_id[super_successor]:
                pass
            elif post_id[super_successor] == 0:
                back_edges.add(edge)

        post += 1
        post_id[super_vertex] = post

    for super_vertex in super_graph:
        pre_id[super_vertex] = 0
        post_id[super_vertex] = 0

    explore(super_graph.get_super_vertex(origin))

    for edge in back_edges:
        super_graph.remove_edge(edge)


def determine_post_ordering(cfg: ControlFlowGraph, origin: Vertex, traveller: GraphTraveller):
    pre_id = {}
    pre = 0
    post_ordering = []

    def explore(vertex: Vertex):
        nonlocal pre
        pre += 1
        pre_id[vertex] = pre

        for edge in traveller.forwards(cfg, vertex):
            after = traveller.forward(edge)
            if pre_id[after] == 0:
                explore(after)

        post_ordering.append(vertex)

    for vertex in cfg:
        pre_id[vertex] = 0

    explore(origin)
    return post_ordering


def reachability(cfg: ControlFlowGraph, origin: Vertex, traveller: GraphTraveller):
    data = {vertex: {vertex} for vertex in cfg}
    post_ordering = determine_post_ordering(cfg, origin, traveller)
    changed = True
    iterations = 0
    while changed:
        changed = False
        iterations += 1

        for vertex in reversed(post_ordering):
            old_size = len(data[vertex])
            for edge in traveller.backwards(cfg, vertex):
                before = traveller.backward(edge)
                data[vertex].update(data[before])

            if len(data[vertex]) != old_size:
                changed = True

    print('#Iterations = {}'.format(iterations))
    for vertex in cfg:
        print(vertex, ':', ' '.join(str(other) for other in data[vertex]))


def offline(cfg: ControlFlowGraph, origin: Vertex):
    cfg.dotify()
    if origin == cfg.entry:
        inverse_origin = cfg.exit
        traveller = GraphTraveller(DirectedGraph.successors,
                                   Edge.successor,
                                   DirectedGraph.predecessors,
                                   Edge.predecessor)
        inverse_traveller = GraphTraveller(DirectedGraph.predecessors,
                                           Edge.predecessor,
                                           DirectedGraph.successors,
                                           Edge.successor)
    else:
        inverse_origin = cfg.entry
        traveller = GraphTraveller(DirectedGraph.predecessors,
                                   Edge.predecessor,
                                   DirectedGraph.successors,
                                   Edge.successor)
        inverse_traveller = GraphTraveller(DirectedGraph.successors,
                                           Edge.successor,
                                           DirectedGraph.predecessors,
                                           Edge.predecessor)

    forest = identify_loops(cfg, origin, traveller)
    inverse_forest = identify_loops(cfg, inverse_origin, inverse_traveller)

    tree = DominatorTree()
    tree.set_root(origin)
    super_graph = build_maximal_dag(cfg, traveller, forest)
    tree = build_dominator_tree_from_maximal_dag(super_graph, cfg, origin, traveller)
    verify_immediate_dominators(cfg, origin, tree)

    #reachability(cfg, origin, traveller)
    #reachability(cfg, inverse_origin, inverse_traveller)

    inverse_super_graph = build_maximal_dag(cfg, inverse_traveller, inverse_forest)
    inverse_tree = build_dominator_tree_from_maximal_dag(inverse_super_graph, cfg, inverse_origin, inverse_traveller)
    verify_immediate_dominators(cfg, inverse_origin, inverse_tree)

    assert forest.structured()
    if forest.structured():
        for loop in forest:
            (vertex,) = sample(loop.body, 1)
            inverse_loop = inverse_forest.vertex_to_loop[vertex]
            assert loop.body == inverse_loop.body

        inverse_super_graph = build_inverse_super_graph(cfg, inverse_origin, inverse_traveller, forest)
        remove_back_edges(inverse_super_graph, inverse_origin)
        inverse_tree = build_dominator_tree_from_maximal_dag(inverse_super_graph, cfg, inverse_origin, inverse_traveller)
        verify_immediate_dominators(cfg, inverse_origin, inverse_tree)

    return tree.idom
