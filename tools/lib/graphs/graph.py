import abc

from ..utils import messages
from . import vertex
from . import edge


class InvalidVertexError(ValueError):
    pass


class MultiEdgeError(ValueError):
    pass


class VertexData:
    def __init__(self, v: vertex.Vertex):
        self.v = v
        self.predecessors = []
        self.successors = []


class DirectedGraph(metaclass=abc.ABCMeta):
    def __init__(self):
        self._vertices = {}

    @property
    def vertices(self):
        return (_.v for _ in self._vertices.values())

    def number_of_vertices(self):
        return len(self._vertices)

    def number_of_edges(self):
        m = 0
        for v in self.vertices:
            for e in self.successors(v):
                m += 1
        return m

    def get_vertex_data(self, id: int) -> VertexData:
        try:
            return self._vertices[id]
        except KeyError:
            messages.error_message("No data for vertex with ID {}".format(id))

    def get_vertex_id(self):
        return max(self._vertices.keys(), default=0) + 1

    def add_vertex(self, v: vertex.Vertex):
        self._vertices[v.id] = VertexData(v)

    def has_vertex(self, v: vertex.Vertex):
        return v.id in self._vertices

    def predecessors(self, v: vertex.Vertex):
        return self.get_vertex_data(v.id).predecessors

    def successors(self, v: vertex.Vertex):
        return self.get_vertex_data(v.id).successors

    def has_predecessor(self, v, p):
        return [e for e in self.predecessors(v) if e.predecessor().id == p.id]

    def has_successor(self, v, s):
        return [e for e in self.successors(v) if e.successor().id == s.id]

    def add_edge(self, e: edge.Edge):
        p_info = self.get_vertex_data(e.predecessor().id)
        if self.has_successor(e.predecessor(), e.successor()):
            raise MultiEdgeError('Vertex {} already has successor {}'.format(e.predecessor().id, e.successor().id))
        p_info.successors.append(e)

        s_info = self.get_vertex_data(e.successor().id)
        if self.has_predecessor(e.successor(), e.predecessor()):
            raise MultiEdgeError('Vertex {} already has predecessor {}'.format(e.successor().id, e.predecessor().id))
        s_info.predecessors.append(e)

    def remove_predecessor(self, v, p):
        v_info = self.get_vertex_data(v.id)
        updated_predecessors = [e for e in v_info.predecessors if e.predecessor() != p]
        if len(updated_predecessors) == len(v_info.predecessors):
            raise InvalidVertexError(
                'Vertex {} has no predecessor edge with source {} to remove'.format(v.id, p.id))
        v_info.predecessors = updated_predecessors

    def remove_successor(self, v, s):
        v_info = self.get_vertex_data(v.id)
        updated_successors = [e for e in v_info.successors if e.successor() != s]
        if len(updated_successors) == len(v_info.successors):
            raise InvalidVertexError(
                'Vertex {} has no successor edge with destination {} to remove'.format(v.id, s.id))
        v_info.successors = updated_successors

    def remove_edge(self, e: edge.Edge):
        self.remove_successor(e.predecessor(), e.successor())
        self.remove_predecessor(e.successor(), e.predecessor())

    def remove_vertex(self, v: vertex.Vertex):
        v_info = self.get_vertex_data(v.id)
        for e in v_info.predecessors:
            self.remove_successor(e.predecessor(), v)
        for e in v_info.successors:
            self.remove_predecessor(e.successor(), v)
        del self._vertices[v.id]


class CallGraph(DirectedGraph):
    def __init__(self):
        DirectedGraph.__init__(self)
        self._name_to_vertex = {}
        self._root = None

    def __getitem__(self, name):
        assert name in self._name_to_vertex, 'No vertex in call graph for subprogram {}'.format(name)
        return self._name_to_vertex[name]

    def __contains__(self, name):
        return name in self._name_to_vertex

    @property
    def root(self):
        return self._root

    @root.setter
    def root(self, v):
        assert isinstance(v, vertex.SubprogramVertex)
        self._root = v

    def add_vertex(self, v):
        DirectedGraph.add_vertex(self, v)
        self._name_to_vertex[v.name] = v

    def remove_vertex(self, v: vertex.SubprogramVertex):
        DirectedGraph.remove_vertex(self, v)
        del self._name_to_vertex[v.name]

    def subprograms_under_analysis(self):
        if self._root is not None:
            stack = [self.root]
            reachable = []
            while stack:
                v = stack.pop()
                if v not in reachable:
                    reachable.append(v)
                for e in self.successors(v):
                    if e.successor() not in reachable:
                        stack.append(e.successor())
            return reversed(reachable)
        else:
            return [data.v for data in self._vertices.values()]


class FlowGraph(DirectedGraph):
    def __init__(self, name):
        DirectedGraph.__init__(self)
        self._name = name
        self._entry = None
        self._exit = None

    @property
    def name(self):
        return self._name

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

    def remove_vertex(self, v: vertex.Vertex):
        DirectedGraph.remove_vertex(self, v)
        if v == self._entry:
            self._entry = None
        if v == self._exit:
            self._exit = None


class ControlFlowGraph(FlowGraph):
    def __init__(self, name):
        FlowGraph.__init__(self, name)

    def remove_unreachable_code(self):
        assert self.entry
        dfs = DepthFirstSearch(self, self.entry)
        dead_code = [v for v in self.vertices if v not in dfs.pre_order()]
        for v in dead_code:
            messages.debug_message("Basic block {} is UNREACHABLE in subprogram '{}'".format(v.id, self.name))
            self.remove_vertex(v)


class ProgramPointGraph(FlowGraph):
    def __init__(self, cfg: ControlFlowGraph):
        FlowGraph.__init__(self, cfg.name)
        self.__build(cfg)

    def __build(self, cfg):
        # Add a vertex per basic block
        for v in cfg.vertices:
            self.add_vertex(v)

        # Set entry and exit
        self._entry = cfg.entry
        self._exit = cfg.exit

        # Add a vertex per control flow transition, and join the control flow
        # transition to its end points on both sides
        for v in cfg.vertices:
            for e in cfg.successors(v):
                program_point_v = vertex.ProgramPoint(self.get_vertex_id(), e)
                self.add_vertex(program_point_v)
                self.add_edge(edge.ControlFlowEdge(e.predecessor(), program_point_v, e.direction))
                self.add_edge(
                    edge.ControlFlowEdge(program_point_v, e.successor(), edge.ControlFlowEdge.Direction.CONTINUE))

        # Add dummy edge
        e = edge.Edge(self._exit, self._entry)
        program_point_v = vertex.ProgramPoint(self.get_vertex_id(), e)
        self.add_vertex(program_point_v)
        self.add_edge(edge.ControlFlowEdge(self._exit, program_point_v, edge.ControlFlowEdge.Direction.CONTINUE))
        self.add_edge(edge.ControlFlowEdge(program_point_v, self._entry, edge.ControlFlowEdge.Direction.CONTINUE))


class InstrumentationPointGraph(FlowGraph):
    def __init__(self, name):
        FlowGraph.__init__(self, name)

    def reduce(self):
        candidates = [v for v in self.vertices
                      if not (len(v.instructions) == 1 and v.instructions[0].is_instrumentation_point())]
        for v in candidates:
            for pred_e in self.predecessors(v):
                for succ_e in self.successors(v):
                    if not self.has_successor(pred_e.predecessor(), succ_e.successor()):
                        e = edge.TransitionEdge(pred_e.predecessor(), succ_e.successor())
                        for i in pred_e.instructions:
                            e.instructions.add_instruction(i)
                        for i in v.instructions:
                            e.instructions.add_instruction(i)
                        for i in succ_e.instructions:
                            e.instructions.add_instruction(i)
                        self.add_edge(e)
            self.remove_vertex(v)
        (self.entry,) = [v for v in self.vertices if len(self.predecessors(v)) == 0]

    @staticmethod
    def create(cfg: ControlFlowGraph):
        def split_basic_blocks_on_instrumentation(v):
            splits = [[]]
            for i in v.instructions:
                if i.is_instrumentation_point():
                    if splits[-1]:
                        # End last sequence and create a sequence that only contains the instrumentation point
                        splits.append([i])
                    else:
                        # Last sequence is still empty, so add instrumentation point
                        splits[-1].append(i)
                    if i != v.instructions[-1]:
                        # Not the last instruction, so continue splitting
                        splits.append([])
                else:
                    # Add instruction to end of current sequence
                    splits[-1].append(i)
            return splits

        # Split basic blocks using instrumentation as delimiters
        instruction_sequences = {v: split_basic_blocks_on_instrumentation(v) for v in cfg.vertices}

        # Create basic blocks based on the split
        ipg = InstrumentationPointGraph(cfg.name)
        sub_vertices = {v: [] for v in cfg.vertices}
        for v, splits in instruction_sequences.items():
            for s in splits:
                sub_v = vertex.BasicBlock(ipg.get_vertex_id())
                ipg.add_vertex(sub_v)
                sub_vertices[v].append(sub_v)
                for i in s:
                    sub_v.instructions.add_instruction(i)

        # Add edges between the basic sub-blocks
        for v, subs in sub_vertices.items():
            for i in range(1, len(subs)):
                ipg.add_edge(edge.TransitionEdge(subs[i - 1], subs[i]))

        # Add control flow edges across basic sub-block boundaries
        for p in sub_vertices.keys():
            sub_p = sub_vertices[p][-1]
            for e in cfg.successors(p):
                sub_s = sub_vertices[e.successor()][0]
                ipg.add_edge(edge.TransitionEdge(sub_p, sub_s))

        # Mirror the entry vertex of the CFG
        (ipg.entry,) = [v for v in ipg.vertices if len(ipg.predecessors(v)) == 0]

        # Assume the entry vertex has instructions
        assert len(ipg.entry.instructions) > 0

        # Add a new dummy entry vertex
        new_entry = vertex.BasicBlock(ipg.get_vertex_id())
        ipg.add_vertex(new_entry)
        ipg.add_edge(edge.TransitionEdge(new_entry, ipg.entry))
        ipg.entry = new_entry

        # Mirror the exit vertex of the CFG
        (ipg.exit,) = [v for v in ipg.vertices if len(ipg.successors(v)) == 0]

        if ipg.exit.instructions:
            # Add a dummy exit vertex, but only if we have not done so while constructing the CFG
            new_exit = vertex.BasicBlock(ipg.get_vertex_id())
            ipg.add_vertex(new_exit)
            ipg.add_edge(edge.TransitionEdge(ipg.exit, new_exit))
            ipg.exit = new_exit

        return ipg


class Tree(DirectedGraph):
    def __init__(self):
        DirectedGraph.__init__(self)
        self._root = None

    @property
    def root(self):
        return self._root

    def is_proper_ancestor(self, a: vertex.Vertex, v: vertex.Vertex):
        if v == self.root:
            return False
        else:
            (predecessor_e,) = self.predecessors(v)
            if predecessor_e.predecessor() == a:
                return True
            else:
                return self.is_proper_ancestor(a, predecessor_e.predecessor())

    def is_ancestor(self, a: vertex.Vertex, v: vertex.Vertex):
        return a == v or self.is_proper_ancestor(a, v)


class DominatorTree(Tree):
    def __init__(self, g: FlowGraph, root: vertex.Vertex):
        Tree.__init__(self)
        self.__compute(g, root)

    def __compute(self, g, root):
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
            while s != root:
                ancestor[s] = v
                s = child[s]

        def compress(v):
            if ancestor[ancestor[v]] != root:
                compress(ancestor[v])
                if semi[label[ancestor[v]]] < semi[label[v]]:
                    label[v] = label[ancestor[v]]
                ancestor[v] = ancestor[ancestor[v]]

        def evaluate(v):
            if ancestor[v] == root:
                return label[v]
            else:
                compress(v)
                if semi[label[ancestor[v]]] >= semi[label[v]]:
                    return label[v]
                else:
                    return label[ancestor[v]]

        def do_search(v: vertex.Vertex):
            nonlocal pre_id
            pre_id += 1
            semi[v] = pre_id
            pre_order[pre_id] = v
            label[v] = v
            ancestor[v] = root
            child[v] = root
            size[v] = 1
            bucket[v] = set()
            self.add_vertex(v)

            for e in forward_transitions(g, v):
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

        if root == g.entry:
            forward_transitions = DirectedGraph.successors
            forward_transition = edge.Edge.successor
            backward_transitions = DirectedGraph.predecessors
            backward_transition = edge.Edge.predecessor
        else:
            forward_transitions = DirectedGraph.predecessors
            forward_transition = edge.Edge.predecessor
            backward_transitions = DirectedGraph.successors
            backward_transition = edge.Edge.successor

        label = {}
        parent = {}
        ancestor = {}
        child = {}
        pre_order = Bijection()
        size = {}
        bucket = {}
        size[root] = 0
        ancestor[root] = root
        label[root] = root
        semi = {v: 0 for v in g.vertices}
        idom = {}

        # Stage 1: Do depth-first search
        pre_id = 0
        do_search(root)

        # Stage 2: Compute semi-dominators
        for i in reversed(range(2, pre_id + 1)):
            # Reverse pre-order
            w = pre_order[i]

            for e in backward_transitions(g, w):
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
            self.add_edge(edge.Edge(parent, child))
        assert len(self.predecessors(root)) == 0
        self._root = root


class DominanceFrontiers:
    def __init__(self, g: FlowGraph, t: DominatorTree):
        self._frontier = {v: set() for v in g.vertices}
        self.__compute(g, t)

    def __getitem__(self, item):
        return self._frontier[item]

    def __compute(self, g, t):
        if t.root == g.entry:
            backward_transitions = DirectedGraph.predecessors
            backward_transition = edge.Edge.predecessor
        else:
            backward_transitions = DirectedGraph.successors
            backward_transition = edge.Edge.successor

        for v in g.vertices:
            if len(backward_transitions(g, v)) > 1:
                (parent_e,) = t.predecessors(v)
                immediate_dominator = parent_e.predecessor()
                for e in backward_transitions(g, v):
                    runner = backward_transition(e)
                    while runner != immediate_dominator:
                        self._frontier[runner].add(v)
                        (parent_e,) = t.predecessors(runner)
                        runner = parent_e.predecessor()


class StrongComponents:
    def __init__(self, flow_g: FlowGraph):
        self._scc = {v: None for v in flow_g.vertices}
        self.__compute(flow_g)

    def __getitem__(self, item):
        return self._scc[item]

    def __compute(self, flow_g):
        def explore(v):
            nonlocal pre_id
            pre_id += 1
            pre_order[v] = pre_id
            low_link[v] = pre_id
            stack.append(v)

            for e in flow_g.successors(v):
                if pre_order[e.successor()] == 0:
                    explore(e.successor())
                    low_link[v] = min(low_link[v], low_link[e.successor()])
                elif e.successor() in stack:
                    low_link[v] = min(low_link[v], pre_order[e.successor()])

            if low_link[v] == pre_order[v]:
                nonlocal scc_id
                scc_id += 1
                while True:
                    z = stack.pop()
                    self._scc[z] = scc_id
                    if z == v:
                        break

        scc_id = 0
        pre_id = 0
        stack = []
        pre_order = {v: 0 for v in flow_g.vertices}
        low_link = {v: 0 for v in flow_g.vertices}
        explore(flow_g.entry)


class DepthFirstSearch:
    def __init__(self, g: DirectedGraph, root: vertex.Vertex):
        self._pre_order = {}
        self._post_order = {}
        self._back_edges = []
        self.__search(g, root)

    def pre_order(self):
        return list(e[0] for e in sorted(self._pre_order.items(), key=lambda e: e[1]))

    def post_order(self):
        return list(e[0] for e in sorted(self._post_order.items(), key=lambda e: e[1]))

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


class LoopNests(DirectedGraph):
    def __init__(self, ppg: ProgramPointGraph):
        DirectedGraph.__init__(self)
        self._ppg = ppg
        self._containment = {}
        self._headers = []
        self.__compute()

    @property
    def root(self):
        return self._root

    def is_header(self, v):
        return v in self._headers

    def __iter__(self):
        for h in self._headers:
            yield h

    def __compute(self):
        def find_loop_body():
            # Now move up the graph from the tail until the header and add
            # vertices as we go
            work_list = [back_edge.predecessor()]
            while work_list:
                v = work_list.pop()
                loop.vertices.add(v)
                visited[v] = True

                if v != back_edge.successor():
                    # Continue the search only if we have not reached the header
                    for e in self._ppg.predecessors(v):
                        if e not in dfs.back_edges:
                            representative = parent[e.predecessor()]
                            while representative != parent[representative]:
                                representative = parent[representative]
                            if not (representative in work_list
                                    or representative in loop.vertices
                                    or visited[representative]):
                                work_list.append(representative)
            return loop

        # A vertex's parent is initially itself
        parent = {v: v for v in self._ppg.vertices}
        # No vertex has been visited yet
        visited = {v: False for v in self._ppg.vertices}
        dominators = DominatorTree(self._ppg, self._ppg.entry)
        dfs = DepthFirstSearch(self._ppg, self._ppg.entry)
        for v in reversed(dfs.pre_order()):
            back_edges = [e for e in self._ppg.predecessors(v) if e in dfs.back_edges]
            if back_edges:
                loops = set()
                for back_edge in back_edges:
                    if not dominators.is_ancestor(back_edge.successor(), back_edge.predecessor()):
                        messages.error_message(
                            'Edge {}=>{} in CFG {} identifies an irreducible loop'.format(back_edge.predecessor().id,
                                                                                          back_edge.successor().id,
                                                                                          self._ppg.name))

                    messages.debug_message("Edge {} in CFG '{}' is a back edge".format(back_edge.predecessor().edge,
                                                                                       self._ppg.name))
                    # Loop detected
                    loop = vertex.LoopBody(self.get_vertex_id(), v)
                    self.add_vertex(loop)
                    loops.add(loop)
                    self._headers.append(v)
                    find_loop_body()
                    for w in loop.vertices:
                        visited[w] = False

                # Update current parent information
                for loop in loops:
                    for w in loop.vertices:
                        parent[w] = v

                # Compute common part for loops sharing header
                if len(loops) > 1:
                    shared_loop = vertex.LoopBody(self.get_vertex_id(), v)
                    self.add_vertex(shared_loop)
                    for loop in loops:
                        if not shared_loop.vertices:
                            shared_loop.vertices.update(loop.vertices)
                        else:
                            shared_loop.vertices.intersection_update(loop.vertices)

                    for loop in loops:
                        loop.vertices.difference_update(shared_loop.vertices)

        # Remove inner loop headers from loop body
        for loop in self.vertices:
            loop.vertices.difference_update(set(self._headers) - {loop.header})

        # Set the start state
        (self._root,) = [loop for loop in self.vertices if loop.header == self._ppg.entry]

        # Assume each program point is in a loop
        for loop in self.vertices:
            for v in loop.vertices:
                assert v not in self._containment
                self._containment[v] = loop

        # Add links to reflect entries into and exits out of loops
        for loop in list(self.vertices):
            for v in list(loop.vertices):
                if isinstance(v, vertex.ProgramPoint):
                    if v.edge.predecessor() in loop.vertices and v.edge.successor() not in loop.vertices:
                        bridge = vertex.SuperBlock(self.get_vertex_id())
                        bridge.vertices.add(v)
                        loop.vertices.remove(v)
                        self.add_vertex(bridge)
                        self.add_edge(edge.Edge(loop, bridge))
                        inner_loop = self._containment[v.edge.successor()]
                        self.add_edge(edge.Edge(bridge, inner_loop))

                    elif v.edge.predecessor() not in loop.vertices and v.edge.successor() in loop.vertices:
                        bridge = vertex.SuperBlock(self.get_vertex_id())
                        bridge.vertices.add(v)
                        loop.vertices.remove(v)
                        self.add_vertex(bridge)
                        self.add_edge(edge.Edge(bridge, loop))
                        inner_loop = self._containment[v.edge.predecessor()]
                        self.add_edge(edge.Edge(inner_loop, bridge))
