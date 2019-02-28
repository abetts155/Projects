import enum
import random
import collections

from utils import messages
from utils import dot
from graphs import (vertices, edges, instrumentation)


class InvalidVertexError(ValueError):
    pass


class DuplicateVertexError(ValueError):
    pass


class MultiEdgeError(ValueError):
    pass


class VertexData:
    """Models the relation of a vertex to other vertices inside a directed graph. By __not__ attaching predecessor and
    successor information to a vertex, we allow the same vertex to exist in several directed graphs simultaneously. All
    edge information must be requested through the directed graph."""

    __slots__ = ['v', 'predecessors', 'successors']

    def __init__(self, v: vertices.Vertex):
        self.v = v
        self.predecessors = []
        self.successors = []


class DirectedGraph:
    """Models a directed graph: A set of vertices and a set of directed edges"""

    def __init__(self):
        self._data = {}

    def add_vertex(self, v: vertices.Vertex):
        if v.id_ in self._data:
            raise DuplicateVertexError('Graph already contains a vertex with ID {}'.format(v.id_))
        self._data[v.id_] = VertexData(v)

    def _get_vertex_data(self, v: vertices.Vertex) -> VertexData:
        try:
            return self._data[v.id_]
        except KeyError:
            messages.error_message("No data for vertex with ID {}".format(v.id_))

    def remove_vertex(self, v: vertices.Vertex):
        v_info = self._get_vertex_data(v)
        for e in v_info.predecessors:
            self.remove_successor(e.predecessor(), v)
        for e in v_info.successors:
            self.remove_predecessor(e.successor(), v)
        del self._data[v.id_]

    def __contains__(self, v: vertices.Vertex):
        return v.id_ in self._data

    def __iter__(self):
        for _ in self._data.values():
            yield _.v

    def number_of_vertices(self):
        return len(self._data)

    def number_of_edges(self):
        return sum([len(self.successors(v)) for v in self])

    def predecessors(self, v: vertices.Vertex):
        return self._get_vertex_data(v).predecessors

    def successors(self, v: vertices.Vertex):
        return self._get_vertex_data(v).successors

    def add_edge(self, e: edges.Edge) -> None:
        p_info = self._get_vertex_data(e.predecessor())
        p_info.successors.append(e)
        s_info = self._get_vertex_data(e.successor())
        s_info.predecessors.append(e)

    def has_edge(self, p: vertices.Vertex, s: vertices.Vertex):
        return self.has_successor(p, s) and self.has_predecessor(s, p)

    def remove_predecessor(self, v, p):
        v_info = self._get_vertex_data(v)
        updated_predecessors = [e for e in v_info.predecessors if e.predecessor() != p]
        v_info.predecessors = updated_predecessors

    def remove_successor(self, v, s):
        v_info = self._get_vertex_data(v)
        updated_successors = [e for e in v_info.successors if e.successor() != s]
        v_info.successors = updated_successors

    def remove_edge(self, e: edges.Edge):
        self.remove_successor(e.predecessor(), e.successor())
        self.remove_predecessor(e.successor(), e.predecessor())

    def has_predecessor(self, v, p):
        return [e for e in self.predecessors(v) if e.predecessor() == p]

    def has_successor(self, v, s):
        return [e for e in self.successors(v) if e.successor() == s]

    def wipe_edges(self, v):
        v_info = self._get_vertex_data(v)
        v_info.predecessors = []
        v_info.successors = []

    def __str__(self):
        value = ''
        for v in self:
            value += 'V: {}\n'.format(v)
            for edge in self.successors(v):
                value += 'E: {}\n'.format(edge)
            value += '\n'
        return value


class CallGraph(DirectedGraph):
    """Models the calling relationship between subprograms"""

    def __init__(self, program):
        DirectedGraph.__init__(self)
        self.program = program

    def is_call_site(self, call_vertex: vertices.SubprogramVertex, v: vertices.Vertex):
        for e in self.successors(call_vertex):
            if v == e.call_site:
                return e.successor()
        return None

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in sorted([e for e in self.successors(v)], key=lambda e: e.successor().name):
                data.append(e.dotify())

        filename = '{}.call.dot'.format(self.program.basename())
        dot.generate(filename, data)


class ProgramData:
    """Track which program and subprogram a graph belongs to"""

    def __init__(self, program, name):
        self._program = program
        self._name = name

    @property
    def program(self):
        return self._program

    @property
    def name(self):
        return self._name


class FlowGraph(DirectedGraph, ProgramData):
    """Models a directed graph that has a designated entry vertex and a designated exit vertex"""

    def __init__(self, program, name):
        DirectedGraph.__init__(self)
        ProgramData.__init__(self, program, name)
        self._entry = None
        self._exit = None
        self._pre_dominator_tree = None
        self._post_dominator_tree = None

    @property
    def entry(self):
        assert self._entry is not None
        return self._entry

    @entry.setter
    def entry(self, value):
        self._entry = value

    @property
    def exit(self):
        assert self._exit is not None
        return self._exit

    @exit.setter
    def exit(self, value):
        self._exit = value

    def remove_vertex(self, v: vertices.Vertex):
        DirectedGraph.remove_vertex(self, v)
        if v == self._entry:
            self._entry = None
        if v == self._exit:
            self._exit = None

    def pre_dominator_tree(self):
        if self._pre_dominator_tree is None:
            self._pre_dominator_tree = DominatorTree(self, self.entry)
        return self._pre_dominator_tree

    def post_dominator_tree(self):
        if self._post_dominator_tree is None:
            self._post_dominator_tree = DominatorTree(self, self.exit)
        return self._post_dominator_tree


class ControlFlowGraph(FlowGraph):
    """Models a control flow graph at the disassembly code level where:
    a) each vertex is a basic block of instructions and
    b) edges represent intra-procedural control flow"""

    def __init__(self, program, name):
        FlowGraph.__init__(self, program, name)

    def cull_unreachable(self):
        assert self.entry
        # Remove edges known to be unreachable.
        for v in self:
            for e in self.successors(v):
                if e.direction == edges.Direction.UNREACHABLE:
                    messages.debug_message("Edge {} is unreachable in subprogram '{}'".format(e, self.name))
                    self.remove_edge(e)

        # Remove vertices that cannot be reached from the entry vertex.
        dfs = DepthFirstSearch(self, self.entry)
        dead_vertices = [v for v in self if v not in dfs.pre_order()]
        for v in dead_vertices:
            messages.debug_message("Basic block {} is unreachable in subprogram '{}'".format(v, self.name))
            self.remove_vertex(v)

        # Relabel edges of branches that have been flattened.
        for v in self:
            if len(self.successors(v)) == 1:
                (sole_e,) = self.successors(v)
                if sole_e.direction == edges.Direction.ELSE or sole_e.direction == edges.Direction.THEN:
                    sole_e.direction = edges.Direction.CONTINUE

    def dotify(self, suffix=''):
        assert self.entry
        data = []
        queue = [self.entry]
        visited = set()
        while queue:
            v = queue.pop()
            visited.add(v)
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())
                if not (e.successor() in visited or e.successor() in queue):
                    queue.insert(0, e.successor())

        if suffix:
            filename = '{}.{}.cfg.{}.dot'.format(self.program.basename(), self.name, suffix)
        else:
            filename = '{}.{}.cfg.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)


class ProgramPointGraph(FlowGraph):
    @classmethod
    def create_from_control_flow_graph(cls, cfg: ControlFlowGraph):
        ppg = ProgramPointGraph(cfg.program, cfg.name)
        # Add a vertex per basic block
        for basic_block in cfg:
            id_ = vertices.Vertex.get_vertex_id(basic_block.is_dummy())
            v = vertices.ProgramPointVertex(id_, basic_block)
            ppg.add_vertex(v)
            if v.program_point == cfg.entry:
                ppg._entry = v
            if v.program_point == cfg.exit:
                ppg._exit = v

        def add_edge(edge):
            id_ = vertices.Vertex.get_vertex_id(edge.predecessor().is_dummy() or edge.successor().is_dummy())
            v = vertices.ProgramPointVertex(id_, edge)
            ppg.add_vertex(v)
            p = ppg.__program_point_to_vertex[edge.predecessor()]
            s = ppg.__program_point_to_vertex[edge.successor()]
            ppg.add_edge(edges.TransitionEdge(p, v))
            ppg.add_edge(edges.TransitionEdge(v, s))

        # Add a vertex per control flow transition, and join the control flow
        # transition to its end points on both sides
        for v in cfg:
            for edge in cfg.successors(v):
                add_edge(edge)

        ppg.add_dummy_loop_edge()

        return ppg

    def __init__(self, program, name):
        FlowGraph.__init__(self, program, name)
        self.__program_point_to_vertex = {}

    def add_vertex(self, v: vertices.ProgramPointVertex):
        FlowGraph.add_vertex(self, v)
        if isinstance(v.program_point, vertices.Vertex):
            self.__program_point_to_vertex[v.program_point] = v
        else:
            self.__program_point_to_vertex[(v.program_point.predecessor(), v.program_point.successor())] = v

    def remove_vertex(self, v: vertices.ProgramPointVertex):
        FlowGraph.remove_vertex(self, v)
        if isinstance(v.program_point, vertices.Vertex):
            del self.__program_point_to_vertex[v.program_point]
        else:
            del self.__program_point_to_vertex[(v.program_point.predecessor(), v.program_point.successor())]

    def add_dummy_loop_edge(self):
        self.add_edge(edges.TransitionEdge(self.exit, self.entry))

    def __getitem__(self, item: edges.Edge or vertices.Vertex):
        try:
            if isinstance(item, vertices.Vertex):
                return self.__program_point_to_vertex[item]
            else:
                return self.__program_point_to_vertex[(item.predecessor(), item.successor())]
        except KeyError:
            messages.error_message('No vertex in program point graph for program point {}'.format(str(item)))

    def trace_filename(self):
        return '{}.{}.ppg.trace'.format(self.program.basename(), self.name)

    def choose_instrumentation(self):
        def attempt_removal():
            existing_edges = set()
            new_edges = set()
            for predecessor_edge in self.predecessors(v):
                for successor_edge in self.successors(predecessor_edge.predecessor()):
                    existing_edges.add(successor_edge)
                for successor_edge in self.successors(v):
                    new_edges.add(edges.TransitionEdge(predecessor_edge.predecessor(), successor_edge.successor()))

            if existing_edges.intersection(new_edges):
                return False
            else:
                self.remove_vertex(v)
                for edge in new_edges:
                    self.add_edge(edge)
                return True

        changed = True
        while changed:
            changed = False
            candidates = [v for v in self
                          if v != self.entry and v != self.exit and not isinstance(v, vertices.CallVertex)]
            random.shuffle(candidates)
            for v in candidates:
                if attempt_removal():
                    changed = True

    def dotify(self, suffix=''):
        data = []
        if self._entry:
            dfs = DepthFirstSearch(self, self.entry)
            order = list(reversed(dfs.post_order()))
        else:
            order = [v for v in self]
        for v in order:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())
        for v in self:
            if v not in order:
                data.append(v.dotify())
                for e in self.successors(v):
                    data.append(e.dotify())

        if suffix:
            filename = '{}.{}.ppg.{}.dot'.format(self.program.basename(), self.name, suffix)
        else:
            filename = '{}.{}.ppg.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)


class DependenceGraph(DirectedGraph):
    def __init__(self, program, cfg, basic_block: vertices.BasicBlock):
        DirectedGraph.__init__(self, program)
        self._cfg = cfg
        self._basic_block = basic_block

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())

        filename = '{}{}.{}.order.dot'.format(self.program.basename(), self._cfg.name, self._basic_block)
        dot.generate(filename, data)

    def reduce_transitively(self):
        # Set up a dummy entry vertex to satisfy the depth-first search contract
        entry_v = vertices.InstructionVertex(vertices.Vertex.get_vertex_id(), None)
        self.add_vertex(entry_v)
        for v in self:
            if v != entry_v and len(self.predecessors(v)) == 0:
                self.add_edge(edges.Edge(entry_v, v))

        # Compute
        dfs = DepthFirstSearch(self, entry_v)
        data = {v: set() for v in self}
        for v in reversed(dfs.post_order()):
            for e in self.predecessors(v):
                data[v].update(data[e.predecessor()])
            deletions = []
            for e in self.predecessors(v):
                if e.predecessor() in data[v]:
                    deletions.append(e)
            for e in deletions:
                self.remove_edge(e)
            for e in self.predecessors(v):
                data[v].add(e.predecessor())

        for v in reversed(dfs.post_order()):
            for e in self.predecessors(v):
                v.update(e.predecessor())
                v.add(e.predecessor())

        # Clean up
        self.remove_vertex(entry_v)


class Tree(DirectedGraph):
    def __init__(self):
        DirectedGraph.__init__(self)
        self._root = None

    @property
    def root(self):
        return self._root

    def is_proper_ancestor(self, a: vertices.Vertex, v: vertices.Vertex):
        if v == self.root:
            return False
        else:
            (predecessor_e,) = self.predecessors(v)
            if predecessor_e.predecessor() == a:
                return True
            else:
                return self.is_proper_ancestor(a, predecessor_e.predecessor())

    def is_ancestor(self, a: vertices.Vertex, v: vertices.Vertex):
        return a == v or self.is_proper_ancestor(a, v)


class DominatorTree(Tree, ProgramData):
    class Type(enum.Enum):
        PRE = 0
        POST = 1

    def __init__(self, flow_graph: FlowGraph, root: vertices.Vertex):
        Tree.__init__(self)
        ProgramData.__init__(self, flow_graph.program, flow_graph.name)
        self._root = root
        self.__compute(flow_graph)
        assert len(self.predecessors(self._root)) == 0
        if self._root == flow_graph.entry:
            self._type = DominatorTree.Type.PRE
        else:
            self._type = DominatorTree.Type.POST

    def __compute(self, flow_graph):
        # This is an implementation of the Lengauer-Tarjan algorithm
        def link(v, w):
            s = w
            while semi[label[w]] < semi[label[child[s]]]:
                if size[s] + size[child[child[s]]] >= 2 * size[child[s]]:
                    ancestor[child[s]] = s
                    child[s] = child[child[s]]
                else:
                    size[child[s]] = size[s]
                    ancestor[s] = child[s]
                    s = ancestor[s]
            label[s] = label[w]
            size[v] += size[w]
            if size[v] < 2 * size[w]:
                s, child[v] = child[v], s
            while s != self._root:
                ancestor[s] = v
                s = child[s]

        def compress(v):
            if ancestor[ancestor[v]] != self._root:
                compress(ancestor[v])
                if semi[label[ancestor[v]]] < semi[label[v]]:
                    label[v] = label[ancestor[v]]
                ancestor[v] = ancestor[ancestor[v]]

        def evaluate(v):
            if ancestor[v] == self._root:
                return label[v]
            else:
                compress(v)
                if semi[label[ancestor[v]]] >= semi[label[v]]:
                    return label[v]
                else:
                    return label[ancestor[v]]

        def do_search(v: vertices.Vertex):
            nonlocal pre_id
            pre_id += 1
            semi[v] = pre_id
            pre_order[pre_id] = v
            label[v] = v
            ancestor[v] = self._root
            child[v] = self._root
            size[v] = 1
            bucket[v] = set()
            self.add_vertex(v)

            for e in forward_transitions(flow_graph, v):
                if semi[forward_transition(e)] == 0:
                    parent[forward_transition(e)] = v
                    do_search(forward_transition(e))

        class Bijection(dict):
            def __setitem__(self, key, value):
                if key in self:
                    del self[key]
                if value in self:
                    del self[value]
                dict.__setitem__(self, key, value)
                dict.__setitem__(self, value, key)

            def __delitem__(self, key):
                dict.__delitem__(self, self[key])
                dict.__delitem__(self, key)

        if self._root == flow_graph.entry:
            forward_transitions = DirectedGraph.successors
            forward_transition = edges.Edge.successor
            backward_transitions = DirectedGraph.predecessors
            backward_transition = edges.Edge.predecessor
        else:
            forward_transitions = DirectedGraph.predecessors
            forward_transition = edges.Edge.predecessor
            backward_transitions = DirectedGraph.successors
            backward_transition = edges.Edge.successor

        label = {}
        parent = {}
        ancestor = {}
        child = {}
        pre_order = Bijection()
        size = {}
        bucket = {}
        size[self._root] = 0
        ancestor[self._root] = self._root
        label[self._root] = self._root
        semi = {v: 0 for v in flow_graph}
        idom = {}

        # Stage 1: Do depth-first search
        pre_id = 0
        do_search(self._root)

        # Stage 2: Compute semi-dominators
        for i in reversed(range(2, pre_id + 1)):
            # Reverse pre-order
            w = pre_order[i]

            for e in backward_transitions(flow_graph, w):
                u = evaluate(backward_transition(e))
                if semi[u] < semi[w]:
                    semi[w] = semi[u]

            bucket[pre_order[semi[w]]].update({w})
            link(parent[w], w)

            while bucket[parent[w]]:
                v = bucket[parent[w]].pop()
                u = evaluate(v)
                if semi[u] < semi[v]:
                    idom[v] = u
                else:
                    idom[v] = parent[w]

        # Stage 3: Set immediate dominators
        for i in range(2, pre_id + 1):
            w = pre_order[i]
            if idom[w] != pre_order[semi[w]]:
                idom[w] = idom[idom[w]]

        # All done: Add edges to the tree data structure
        for child, parent in idom.items():
            self.add_edge(edges.Edge(parent, child))

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())

        if self._type == DominatorTree.Type.PRE:
            suffix = 'pre'
        else:
            suffix = 'post'
        filename = '{}.{}.{}.dot'.format(self.program.basename(), self.name, suffix)
        dot.generate(filename, data)


class DominatorGraph(FlowGraph):
    def __init__(self, pre_dominator_tree: DominatorTree, post_dominator_tree: DominatorTree):
        FlowGraph.__init__(self, pre_dominator_tree.program, pre_dominator_tree.name)

        for v in pre_dominator_tree:
            self.add_vertex(v)

        for v in pre_dominator_tree:
            for edge in pre_dominator_tree.successors(v):
                self.add_edge(edges.Edge(v, edge.successor()))

        for v in post_dominator_tree:
            for edge in post_dominator_tree.successors(v):
                self.add_edge(edges.Edge(v, edge.successor()))

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for edge in self.successors(v):
                data.append(edge.dotify())

        filename = '{}.{}.dominator.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)


class DominanceFrontiers:
    def __init__(self, g: FlowGraph, t: DominatorTree):
        self._frontier = {v: set() for v in g}
        self.__compute(g, t)

    def __getitem__(self, item):
        return self._frontier[item]

    def __compute(self, g, t):
        if t.root == g.entry:
            backward_transitions = DirectedGraph.predecessors
            backward_transition = edges.Edge.predecessor
        else:
            backward_transitions = DirectedGraph.successors
            backward_transition = edges.Edge.successor

        for v in g:
            if len(backward_transitions(g, v)) > 1:
                (parent_e,) = t.predecessors(v)
                immediate_dominator = parent_e.predecessor()
                for e in backward_transitions(g, v):
                    runner = backward_transition(e)
                    while runner != immediate_dominator:
                        self._frontier[runner].add(v)
                        (parent_e,) = t.predecessors(runner)
                        runner = parent_e.predecessor()


class StrongComponents(set):
    def __init__(self, directed_graph: DirectedGraph):
        set.__init__(self)
        self._scc = {v: None for v in directed_graph}
        self.__compute(directed_graph)

    def __compute(self, directed_graph):
        def explore(v):
            nonlocal pre_id
            pre_id += 1
            pre_order[v] = pre_id
            low_link[v] = pre_id
            stack.append(v)

            for e in directed_graph.successors(v):
                if pre_order[e.successor()] == 0:
                    explore(e.successor())
                    low_link[v] = min(low_link[v], low_link[e.successor()])
                elif e.successor() in stack:
                    low_link[v] = min(low_link[v], pre_order[e.successor()])

            if low_link[v] == pre_order[v]:
                scc = set()
                while True:
                    z = stack.pop()
                    scc.add(z)
                    self._scc[z] = scc
                    if z == v:
                        break
                self.add(frozenset(scc))

        pre_id = 0
        stack = []
        pre_order = {v: 0 for v in directed_graph}
        low_link = {v: 0 for v in directed_graph}
        for v in directed_graph:
            if pre_order[v] == 0:
                explore(v)


class DepthFirstSearch:
    def __init__(self, g: DirectedGraph, root: vertices.Vertex):
        self._pre_order = {}
        self._post_order = {}
        self._back_edges = []
        self.__search(g, root)

    def pre_order(self):
        return list(e[0] for e in sorted(self._pre_order.items(), key=lambda e: e[1]))

    def post_order(self):
        return list(e[0] for e in sorted(self._post_order.items(), key=lambda e: e[1]))

    def pre_order_number(self, v: vertices.Vertex):
        return self._pre_order[v]

    def post_order_number(self, v: vertices.Vertex):
        return self._post_order[v]

    @property
    def back_edges(self):
        return self._back_edges

    def __search(self, g, root):
        def explore(v):
            nonlocal pre_id
            pre_id += 1
            self._pre_order[v] = pre_id

            for e in g.successors(v):
                if e.successor() not in self._pre_order:
                    # Not yet visited
                    explore(e.successor())
                elif self._pre_order[v] < self._pre_order[e.successor()]:
                    pass
                elif e.successor() not in self._post_order:
                    self._back_edges.append(e)

            nonlocal post_id
            post_id += 1
            self._post_order[v] = post_id

        pre_id = 0
        post_id = 0
        explore(root)


class LoopNests(FlowGraph):
    def __init__(self, ppg):
        FlowGraph.__init__(self, ppg.program, ppg.name)
        self._loops = []
        self._headers = {}
        self._tails = {}
        self._loop_entries = {}
        self._loop_exits = {}
        self._program_point_to_loop = {v: None for v in ppg}
        self.__discover_loop_bodies(ppg)
        self.__model_control_flow_into_and_out_of_loop(ppg)
        (self.entry,) = [loop for loop in self if ppg.entry in loop]
        (self.exit,) = [loop for loop in self if ppg.exit in loop]

    def __discover_loop_bodies(self, ppg):
        def do_search(v):
            visited.add(v)
            if v != header:
                for e in ppg.predecessors(v):
                    where_next = containment[e.predecessor()]
                    if where_next not in visited:
                        do_search(where_next)
            order.append(v)

        containment = {v: v for v in ppg}
        data = {v: None for v in ppg}
        dfs = DepthFirstSearch(ppg, ppg.entry)
        for v in reversed(dfs.pre_order()):
            back_edges = [e for e in ppg.predecessors(v) if e in dfs.back_edges]
            if back_edges:
                # Sort back edges according to their post-order numbering, then reverse, so that we visit all successors
                # of a vertex before the vertex itself
                back_edges.sort(key=lambda e: dfs.post_order_number(e.predecessor()), reverse=True)
                order = []
                visited = set()
                header = v
                tails = set()
                for e in back_edges:
                    if not ppg.pre_dominator_tree().is_ancestor(e.successor(), e.predecessor()):
                        messages.error_message(
                            "Edge {} in '{}' identifies an irreducible loop".format(str(e.predecessor()), ppg.name))
                    do_search(e.predecessor())
                    tails.add(e.predecessor())

                for w in reversed(order):
                    containment[w] = header
                    data[w] = header

                    if w != header:
                        # Propagate reachability information concerning loop tails to immediate predecessors.
                        # We ignore the loop header so that the information does not spill out to enclosing loop.
                        for e in ppg.predecessors(w):
                            data[containment[e.predecessor()]] = data[w]

                loop_vertices = {}
                for w in order:
                    # Do not add an inner loop header to the partition for this header.
                    if w not in [loop.header for loop in self._loops]:
                        if data[w] not in loop_vertices:
                            loop = vertices.LoopBody(vertices.Vertex.get_vertex_id(), header)
                            loop_vertices[data[w]] = loop
                            self.add_vertex(loop)
                            self._headers[header] = loop
                            for tail in tails:
                                self._tails[tail] = loop
                        loop = loop_vertices[data[w]]
                        loop.append(w)

                # Clear the reachability information in readiness for enclosing loops.
                data[header] = None

    def __model_control_flow_into_and_out_of_loop(self, ppg):
        transitions = {}
        for v in ppg:
            if isinstance(v.program_point, edges.Edge):
                loop = self.find_loop(v)
                loop_of_predecessor = self.find_loop(ppg[v.program_point.predecessor()])
                loop_of_successor = self.find_loop(ppg[v.program_point.successor()])

                if loop != loop_of_predecessor:
                    if not self.has_edge(loop_of_predecessor, loop):
                        transition = edges.LoopTransition(loop_of_predecessor,
                                                          loop,
                                                          edges.LoopTransition.Direction.EXIT)
                        self.add_edge(transition)
                        transitions[(loop_of_predecessor, loop)] = transition
                    transition = transitions[(loop_of_predecessor, loop)]
                    transition.add(edges.TransitionEdge(ppg[v.program_point.predecessor()], v))
                    self._loop_exits[v] = loop_of_predecessor

                if loop != loop_of_successor:
                    if not self.has_edge(loop, loop_of_successor):
                        transition = edges.LoopTransition(loop,
                                                          loop_of_successor,
                                                          edges.LoopTransition.Direction.ENTRY)
                        self.add_edge(transition)
                        transitions[(loop, loop_of_successor)] = transition
                    transition = transitions[(loop, loop_of_successor)]
                    transition.add(edges.TransitionEdge(v, ppg[v.program_point.successor()]))
                    self._loop_entries[v] = loop_of_successor

                if self.is_tail(v):
                    loop = self.find_loop(v)
                    if not self.has_edge(loop, loop):
                        transition = edges.LoopTransition(loop, loop, edges.LoopTransition.Direction.BACK)
                        self.add_edge(transition)
                        transitions[(loop, loop)] = transition
                    transition = transitions[(loop, loop)]
                    transition.add(edges.TransitionEdge(v, ppg[v.program_point.successor()]))

    def add_vertex(self, v: vertices.Vertex):
        FlowGraph.add_vertex(self, v)
        self._loops.append(v)

    def __iter__(self):
        for loop in self._loops:
            yield loop

    def is_outermost_loop(self, loop):
        return loop == self._loops[-1]

    def is_header(self, v):
        return v in self._headers

    def is_tail(self, v):
        return v in self._tails

    def tails(self):
        for v in self._tails.keys():
            yield v

    def loop_entries(self, loop):
        return [v for v in self._loop_entries if self._loop_entries[v] == loop]

    def is_loop_entry(self, v):
        return v in self._loop_entries

    def loop_exits(self, loop):
        return [v for v in self._loop_entries if self._loop_exits[v] == loop]

    def is_loop_exit(self, v):
        return v in self._loop_exits

    def find_loop(self, v: vertices.ProgramPointVertex):
        if self._program_point_to_loop[v] is None:
            for loop in self:
                if v in loop:
                    self._program_point_to_loop[v] = loop
        return self._program_point_to_loop[v]

    def induce(self, ppg, header, guarantee_single_exit=False):
        messages.debug_message('Inducing loop subgraph for header {}'.format(header))

        induced_graph = ProgramPointGraph(ppg.program, ppg.name)
        loop = self.find_loop(header)

        # Add program points in the loop body.
        for v in loop:
            induced_graph.add_vertex(v)

        # Add edges between program points in the loop body.
        for v in loop:
            for succcessor_edge in ppg.successors(v):
                if succcessor_edge.successor() in induced_graph and succcessor_edge.successor() != header:
                    induced_graph.add_edge(succcessor_edge)

        # Add program points and edges that model control flow out of loop.
        for transition_edge in self.successors(loop):
            if transition_edge.direction == edges.LoopTransition.Direction.EXIT:
                for edge in transition_edge:
                    if edge.successor() not in induced_graph:
                        induced_graph.add_vertex(edge.successor())
                    induced_graph.add_edge(edge)
            elif transition_edge.direction == edges.LoopTransition.Direction.ENTRY:
                for edge in transition_edge:
                    if edge.successor() not in induced_graph:
                        induced_graph.add_vertex(edge.successor())
                    induced_graph.add_edge(edge)

        # Add program points and edges that model control flow into the loop from inner loops.
        for transition_edge in self.predecessors(loop):
            if transition_edge.direction == edges.LoopTransition.Direction.EXIT:
                for edge in transition_edge:
                    induced_graph.add_edge(edges.TransitionEdge(transition_edge.predecessor().header, edge.successor()))

        # Set the entry of the induced graph.
        induced_graph.entry = header

        if guarantee_single_exit:
            # Set the exit of the induced graph, if instructed.
            exits = [v for v in induced_graph if len(induced_graph.successors(v)) == 0]
            if len(exits) == 1:
                (induced_graph.exit,) = exits
            else:
                dummy_program_point = vertices.ProgramPointVertex(vertices.Vertex.get_vertex_id(True),
                                                                  vertices.Vertex(vertices.Vertex.get_vertex_id()))
                induced_graph.add_vertex(dummy_program_point)
                induced_graph.exit = dummy_program_point
                for exit_program_point in exits:
                    induced_graph.add_edge(edges.TransitionEdge(exit_program_point, dummy_program_point))
        return induced_graph

    def dotify(self, suffix=''):
        data = []
        if self._entry:
            queue = [self._entry]
            visited = set()
            while queue:
                v = queue.pop()
                visited.add(v)
                data.append(v.dotify())
                for e in self.successors(v):
                    data.append(e.dotify())
                    if not(e.successor() in queue or e.successor() in visited):
                        queue.insert(0, e.successor())

        if suffix:
            filename = '{}.{}.lnt.{}.dot'.format(self.program.basename(), self.name, suffix)
        else:
            filename = '{}.{}.lnt.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)


class InstrumentationPointGraph(ProgramPointGraph):
    @staticmethod
    def create(ppg: ProgramPointGraph, lnt: LoopNests):
        def in_ipg(v: vertices.ProgramPointVertex):
            return (isinstance(v.program_point, instrumentation.Instrumentation) or
                    isinstance(v.program_point, vertices.CallVertex))

        # Add selected instrumented program points.
        ipg = InstrumentationPointGraph(ppg.program, ppg.name)
        for v in ppg:
            ipg.add_vertex(v)

        for v in ppg:
            for successor_edge in ppg.successors(v):
                ipg.add_edge(successor_edge)

        def disconnect(ipg, v, back_edge, loop):
            for predecessor_edge in ipg.predecessors(v):
                if predecessor_edge.predecessor() != v:
                    for successor_edge in ipg.successors(v):
                        if successor_edge.successor() != v:
                            edge = edges.TransitionEdge(predecessor_edge.predecessor(), successor_edge.successor())
                            edge.extend(predecessor_edge)
                            edge.extend([v])
                            edge.extend(successor_edge)
                            ipg.add_edge(edge)

                            if (back_edge and
                                    in_ipg(predecessor_edge.predecessor()) and
                                    in_ipg(predecessor_edge.successor()) and
                                    predecessor_edge.predecessor() in loop and
                                    successor_edge.successor() in loop):
                                edge.set_back_edge()
            ipg.remove_vertex(v)

        loop_union = {}
        for loop in lnt:
            union = loop[:]
            loop_union[loop] = union
            for successor_edge in lnt.successors(loop):
                if successor_edge.direction == edges.LoopTransition.Direction.ENTRY:
                    nested_loop = successor_edge.successor()
                    union.extend(loop_union[nested_loop])

            to_remove = [v for v in loop if not in_ipg(v)]

            for v in to_remove:
                if v != loop.header and not lnt.is_tail(v):
                    disconnect(ipg, v, False, union)

            for v in to_remove:
                if v in ipg:
                    disconnect(ipg, v, True, union)

        class Successor:
            __slots__ = ['code', 'number']

            def __init__(self, code, number):
                self.code = code
                self.number = number

        # If a vertex has multiple edges to the same successor, flatten these into a single edge.
        successors = {}
        for v in ipg:
            successors[v] = {}
            for edge in ipg.successors(v):
                if edge.successor() not in successors[v]:
                    successors[v][edge.successor()] = Successor(edge, 1)
                else:
                    data = successors[v][edge.successor()]
                    data.code.extend(edge)
                    data.number += 1

        for v in ipg:
            ipg.wipe_edges(v)

        for v in ipg:
            for s in successors[v]:
                data = successors[v][s]
                edge = edges.TransitionEdge(v, s)
                if data.number == 1:
                    edge.extend(data.code)
                else:
                    edge.extend(list(set(data.code)))
                ipg.add_edge(edge)

        (ipg.entry,) = [v for v in ipg if len(ipg.predecessors(v)) == 0]
        return ipg

    def dotify(self, suffix=''):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())

        if suffix:
            filename = '{}.{}.ipg.{}.dot'.format(self.program.basename(), self.name, suffix)
        else:
            filename = '{}.{}.ipg.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)

    def trace_filename(self):
        return '{}.{}.ipg.trace'.format(self.program.basename(), self.name)


class SuperBlockGraph(DirectedGraph, ProgramData):
    def __init__(self, ppg: ProgramPointGraph, lnt: LoopNests):
        DirectedGraph.__init__(self)
        ProgramData.__init__(self, ppg.program, ppg.name)
        self.__pre_dominator_tree = None
        self.__post_dominator_tree = None
        self.__create_dominator_trees(ppg, lnt)
        self.__program_point_to_vertex = {v: None for v in ppg}
        self.__add_super_blocks(lnt)
        self.__forks = []
        self.__merges = []
        self.__add_edges(ppg)

    def __create_dominator_trees(self, ppg: ProgramPointGraph, lnt: LoopNests):
        added = set()
        for v in lnt.tails():
            edge = edges.Edge(v, ppg.exit)
            ppg.add_edge(edge)
            added.add(edge)

        self.__pre_dominator_tree = DominatorTree(ppg, ppg.entry)
        self.__post_dominator_tree = DominatorTree(ppg, ppg.exit)

        for edge in added:
            ppg.remove_edge(edge)

    def __add_super_blocks(self, lnt: LoopNests):
        dominator_graph = DominatorGraph(self.__pre_dominator_tree, self.__post_dominator_tree)
        strong_components = StrongComponents(dominator_graph)

        for scc in strong_components:
            loop_to_super_block = {loop: None for loop in lnt}
            for v in scc:
                loop = lnt.find_loop(v)
                if loop_to_super_block[loop] is None:
                    super_block = vertices.SuperBlock(vertices.Vertex.get_vertex_id())
                    self.add_vertex(super_block)
                    loop_to_super_block[loop] = super_block
                super_block = loop_to_super_block[loop]
                super_block.append(v)
                self.__program_point_to_vertex[v] = super_block
                if isinstance(v.program_point, vertices.Vertex):
                    super_block.representative = v
                elif not super_block.representative:
                    super_block.representative = v

    def __add_edges(self, ppg: ProgramPointGraph):
        for v in ppg:
            if len(ppg.successors(v)) > 1:
                fork = vertices.Fork(vertices.Vertex.get_vertex_id(), v)
                self.__forks.append(fork)
                self.add_vertex(fork)
                self.add_edge(edges.Edge(self[v], fork))
                for successor_edge in ppg.successors(v):
                    self.add_edge(edges.Edge(fork, self[successor_edge.successor()]))

            if len(ppg.predecessors(v)) > 1:
                merge = vertices.Merge(vertices.Vertex.get_vertex_id(), v)
                self.__merges.append(merge)
                self.add_vertex(merge)
                self.add_edge(edges.Edge(merge, self[v]))
                for predecessor_edge in ppg.predecessors(v):
                    self.add_edge(edges.Edge(self[predecessor_edge.predecessor()], merge))

    def super_blocks(self):
        for v in self:
            if isinstance(v, vertices.SuperBlock):
                yield v

    def forks(self):
        for v in self.__forks:
            yield v

    def merges(self):
        for v in self.__merges:
            yield v

    @property
    def pre_dominator_tree(self):
        return self.__pre_dominator_tree

    @property
    def post_dominator_tree(self):
        return self.__post_dominator_tree

    def __getitem__(self, v: vertices.ProgramPointVertex):
        return self.__program_point_to_vertex[v]

    def dotify(self, suffix=''):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())

        if suffix:
            filename = '{}.{}.super.{}.dot'.format(self.program.basename(), self.name, suffix)
        else:
            filename = '{}.{}.super.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)


class SyntaxTree(Tree, ProgramData):
    def __init__(self, ppg: ProgramPointGraph):
        Tree.__init__(self)
        ProgramData.__init__(self, ppg.program, ppg.name)
        self._cache = {}
        self._post_dominator_tree = DominatorTree(ppg, ppg.exit)
        self._post_dominance_frontier = DominanceFrontiers(ppg, self._post_dominator_tree)
        self._root = self.__make_sequence_subtree(ppg, ppg.entry, ppg.exit, True)

    def __make_sequence_subtree(self, ppg, source: vertices.Vertex, target: vertices.Vertex, inclusive: bool):
        seq = vertices.Sequence(vertices.Vertex.get_vertex_id())
        self.add_vertex(seq)

        walker = source
        while walker != target:
            if walker != source and len(ppg.predecessors(walker)) > 1 and len(self._post_dominance_frontier[walker]) > 1:
                if walker not in self._cache:
                    self._cache[walker] = self.__make_sequence_subtree(ppg, walker, target, False)
                self.add_edge(edges.Edge(seq, self._cache[walker]))
                walker = target
            else:
                self.add_vertex(walker)
                self.add_edge(edges.Edge(seq, walker))
                if len(ppg.successors(walker)) == 1:
                    (edge,) = ppg.successors(walker)
                    walker = edge.successor()
                else:
                    self.add_edge(edges.Edge(seq, self.__make_alternative_subtree(ppg, walker)))
                    (dominator_edge,) = ppg.post_dominator_tree().predecessors(walker)
                    walker = dominator_edge.predecessor()

        if inclusive:
            self.add_vertex(target)
            self.add_edge(edges.Edge(seq, target))

        return seq

    def __make_alternative_subtree(self, ppg, branch: vertices.Vertex):
        alt = vertices.Alternative(vertices.Vertex.get_vertex_id())
        self.add_vertex(alt)

        (dominator_edge,) = self._post_dominator_tree.predecessors(branch)
        for e in ppg.successors(branch):
            seq = self.__make_sequence_subtree(ppg, e.successor(), dominator_edge.predecessor(), False)
            self.add_edge(edges.Edge(alt, seq))

        return alt

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())
        filename = '{}.{}.ast.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)

