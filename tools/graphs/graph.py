import enum
import random
import sys
import typing

from miscellaneous import helpful


class VertexPool:
    """Manage allocation and de-allocation of vertices."""

    CUTOFF = 0

    def __init__(self):
        self._max = VertexPool.CUTOFF
        self._allocation = {}

    def positive(self):
        self._max += 1
        return self._max

    def register(self, v):
        if not isinstance(v.id_, int):
            raise TypeError('Vertex ID {} is not an integer'.format(v.id_))
        if v.id_ in self._allocation:
            raise ValueError('Vertex ID {} already taken'.format(v.id_))
        if v.id_ > VertexPool.CUTOFF:
            self._max = max(self._max, v.id_)
        self._allocation[v.id_] = v

    def __getitem__(self, id_):
        return self._allocation[id_]

    def __contains__(self, v):
        return v.id_ in self._allocation


class Vertex:
    # Used to generate IDs of vertices and keep track of allocation.
    id_pool = VertexPool()

    @classmethod
    def get_vertex_id(cls):
        return Vertex.id_pool.positive()

    def __init__(self, id_: int):
        try:
            self.id_ = id_
            self.id_pool.register(self)
        except ValueError as e:
            pass

    def __eq__(self, other):
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        # Two vertices with the same ID are considered equivalent.
        return self.id_

    def __str__(self):
        return str(self.id_)

    def set_id(self, id_):
        self.id_ = id_

    def __lt__(self, other):
        return self.id_ < other.id_


class ControlPoint(enum.Enum):
    NONE = 0
    IF_BRANCH = 1
    IF_MERGE = 2
    SWITCH_BRANCH = 3
    SWITCH_MERGE = 4
    LOOP_HEADER = 5
    LOOP_TAIL = 6


class BasicBlock(Vertex):
    def __init__(self, id_):
        Vertex.__init__(self, id_)
        self._statements = []
        self._declarations = []
        self._control = []

    @property
    def statements(self):
        return self._statements

    @property
    def declarations(self):
        return self._declarations

    @property
    def control(self):
        return self._control


class Loop(Vertex):
    def __init__(self, id_):
        Vertex.__init__(self, id_)
        self._size = 0
        self._body = None

    @property
    def size(self):
        return self._size

    @size.setter
    def size(self, value):
        self._size = value

    @property
    def body(self):
        return self._body

    @body.setter
    def body(self, value):
        self._body = value


class SubprogramVertex(Vertex):
    def __init__(self, id_, subprogram):
        Vertex.__init__(self, id_)
        self._subprogram = subprogram

    @property
    def subprogram(self):
        return self._subprogram


class Edge:
    def __init__(self, predecessor, successor):
        self._predecessor = predecessor
        self._successor = successor

    def predecessor(self):
        return self._predecessor

    def successor(self):
        return self._successor

    def __eq__(self, other):
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        # Hash on vertex identifiers that compose the edge.
        # Not collision resistant, but probably good enough for the vast majority of graphs.
        return hash((self.predecessor(), self.successor()))

    def __str__(self):
        return "({}, {})".format(self._predecessor, self._successor)


class ControlFlow(enum.Enum):
    FORWARD = 0
    THEN = 1
    ELSE = 2
    END_IF = 3
    BEGIN_CASE = 4
    END_CASE = 5
    MERGE = 6
    ENTER_LOOP = 7
    EXIT_LOOP = 8
    ITERATE_LOOP = 9


class ControlFlowEdge(Edge):
    def __init__(self, predecessor, successor, flow: ControlFlow):
        Edge.__init__(self, predecessor, successor)
        self._flow = flow

    @property
    def flow(self):
        return self._flow


class VertexData:
    """Models the relation of a vertex to other vertices inside a directed graph. By __not__ attaching predecessor and
    successor information to a vertex, we allow the same vertex to exist in several directed graphs simultaneously. All
    edge information must be requested through the directed graph."""

    __slots__ = ['v', 'predecessors', 'successors']

    def __init__(self, v: Vertex):
        self.v = v
        self.predecessors = []
        self.successors = []


class DirectedGraph:
    def __init__(self):
        self._data = {}

    def add_vertex(self, v: Vertex):
        self._data[v.id_] = VertexData(v)

    def _get_vertex_data(self, v: Vertex) -> VertexData:
        try:
            return self._data[v.id_]
        except KeyError:
            print("No data for vertex with ID {}".format(v.id_))
            sys.exit(1)

    def __contains__(self, v: Vertex):
        return v.id_ in self._data

    def __iter__(self):
        for _ in self._data.values():
            yield _.v

    def predecessors(self, v: Vertex):
        return self._get_vertex_data(v).predecessors

    def successors(self, v: Vertex):
        return self._get_vertex_data(v).successors

    def add_edge(self, e: Edge):
        p_info = self._get_vertex_data(e.predecessor())
        p_info.successors.append(e)
        s_info = self._get_vertex_data(e.successor())
        s_info.predecessors.append(e)

    def has_predecessor(self, v, p):
        return [e for e in self.predecessors(v) if e.predecessor() == p]

    def has_successor(self, v, s):
        return [e for e in self.successors(v) if e.successor() == s]


class FlowGraph(DirectedGraph):
    def __init__(self):
        DirectedGraph.__init__(self)
        self._entry = None
        self._exit = None

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


class DepthFirstSearch:
    def __init__(self, g: DirectedGraph, root: Vertex):
        self._pre_order = {}
        self._post_order = {}
        self._back_edges = []
        self.__search(g, root)

    def pre_order(self):
        return list(e[0] for e in sorted(self._pre_order.items(), key=lambda e: e[1]))

    def post_order(self):
        return list(e[0] for e in sorted(self._post_order.items(), key=lambda e: e[1]))

    def pre_order_number(self, v: Vertex):
        return self._pre_order[v]

    def post_order_number(self, v: Vertex):
        return self._post_order[v]

    @property
    def back_edges(self):
        return self._back_edges

    def __search(self, g, root):
        def explore(v):
            nonlocal pre_id
            pre_id += 1
            self._pre_order[v] = pre_id

            visit_order = []
            for e in g.successors(v):
                if e.flow in [ControlFlow.ELSE, ControlFlow.EXIT_LOOP]:
                    visit_order.insert(0, e)
                else:
                    visit_order.append(e)

            for e in visit_order:
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


def create_tree(g:     DirectedGraph,
                root:  Vertex,
                depth: int):
    vertex_to_level = {v: 0 for v in g}
    parent = root
    for v in g:
        if v != root:
            new_level = vertex_to_level[parent] + 1
            if new_level <= depth:
                g.add_edge(Edge(parent, v))
                vertex_to_level[v] = new_level
            else:
                # The height of the tree now exceeds the maximum depth, so backtrack to an arbitrary proper ancestor.
                ancestor = parent
                while True:
                    (e,) = g.predecessors(ancestor)
                    ancestor = e.predecessor()
                    if helpful.go_ahead() or ancestor == root:
                        break
                parent = ancestor
                g.add_edge(Edge(parent, v))
                vertex_to_level[v] = vertex_to_level[parent] + 1
            parent = v
    return vertex_to_level


def random_loop_hierarchy(basic_blocks:    int,
                          number_of_loops: int,
                          nesting_depth:   int) -> DirectedGraph:
    # Add enough vertices for the specified number of loops, including an extra one for the dummy outer loop.
    loop_hierarchy = DirectedGraph()
    for _ in range(number_of_loops+1):
        v = Loop(Vertex.get_vertex_id())
        loop_hierarchy.add_vertex(v)

    root = random.choice([v for v in loop_hierarchy])
    _ = create_tree(loop_hierarchy, root, nesting_depth)

    # Compute number of basic blocks in each loop.
    basic_blocks_remaining = basic_blocks
    for v in loop_hierarchy:
        # Guarantee each loop has at least 2 basic blocks per inner nested loop, so that we can easily connect them.
        v.size = 2 + 2 * len(loop_hierarchy.successors(v))
        basic_blocks_remaining -= v.size

    # Arbitrarily distribute any remaining basic blocks to the loop bodies.
    while basic_blocks_remaining > 0:
        for v in loop_hierarchy:
            additional_basic_blocks = random.randint(0, basic_blocks_remaining)
            v.size += additional_basic_blocks
            basic_blocks_remaining -= additional_basic_blocks

    return loop_hierarchy


class SingleEntrySingleExit:
    __slots__ = ['the_entry', 'the_exit']

    def __init__(self, the_entry, the_exit):
        self.the_entry = the_entry
        self.the_exit = the_exit


def random_control_flow_graph(loop_nest: DirectedGraph) -> FlowGraph:

    def create_loop_body(cycles: typing.List[SingleEntrySingleExit],
                         size:   int):
        acyclic_subgraphs = []
        for _ in range(size+1):
            v = BasicBlock(Vertex.get_vertex_id())
            cfg.add_vertex(v)
            acyclic_subgraphs.append(SingleEntrySingleExit(v, v))

        while cycles:
            cycle = helpful.pick_element(cycles, True)
            pre_header = helpful.pick_element(acyclic_subgraphs, True)
            post_header = helpful.pick_element(acyclic_subgraphs, True)
            cfg.add_edge(ControlFlowEdge(pre_header.the_exit, cycle.the_entry, ControlFlow.FORWARD))
            cfg.add_edge(ControlFlowEdge(cycle.the_entry, post_header.the_entry, ControlFlow.EXIT_LOOP))
            acyclic_subgraphs.append(SingleEntrySingleExit(pre_header.the_entry, post_header.the_exit))

        while len(acyclic_subgraphs) > 1:
            if len(acyclic_subgraphs) > 3 and helpful.go_ahead():
                # Create an if-statement with a then-statement and an else-statement.
                branch = helpful.pick_element(acyclic_subgraphs, True)
                merge = helpful.pick_element(acyclic_subgraphs, True)
                then_side = helpful.pick_element(acyclic_subgraphs, True)
                else_side = helpful.pick_element(acyclic_subgraphs, True)
                branch.the_exit.control.append(ControlPoint.IF_BRANCH)
                merge.the_entry.control.append(ControlPoint.IF_MERGE)
                cfg.add_edge(ControlFlowEdge(branch.the_exit, then_side.the_entry, ControlFlow.THEN))
                cfg.add_edge(ControlFlowEdge(branch.the_exit, else_side.the_entry, ControlFlow.ELSE))
                cfg.add_edge(ControlFlowEdge(then_side.the_exit, merge.the_entry, ControlFlow.END_IF))
                cfg.add_edge(ControlFlowEdge(else_side.the_exit, merge.the_entry, ControlFlow.END_IF))
                acyclic_subgraphs.append(SingleEntrySingleExit(branch.the_entry, merge.the_exit))
            elif len(acyclic_subgraphs) > 2 and helpful.go_ahead():
                # Create an if-statement with a then-statement but no else-statement.
                branch = helpful.pick_element(acyclic_subgraphs, True)
                merge = helpful.pick_element(acyclic_subgraphs, True)
                then_side = helpful.pick_element(acyclic_subgraphs, True)
                branch.the_exit.control.append(ControlPoint.IF_BRANCH)
                merge.the_entry.control.append(ControlPoint.IF_MERGE)
                cfg.add_edge(ControlFlowEdge(branch.the_exit, then_side.the_entry, ControlFlow.THEN))
                cfg.add_edge(ControlFlowEdge(then_side.the_exit, merge.the_entry, ControlFlow.END_IF))
                cfg.add_edge(ControlFlowEdge(branch.the_exit, merge.the_entry, ControlFlow.FORWARD))
                acyclic_subgraphs.append(SingleEntrySingleExit(branch.the_entry, merge.the_exit))
            elif len(acyclic_subgraphs) > 4 and helpful.go_ahead():
                # Create a switch-statement
                switch = helpful.pick_element(acyclic_subgraphs, True)
                merge = helpful.pick_element(acyclic_subgraphs, True)
                switch.the_exit.control.append(ControlPoint.SWITCH_BRANCH)
                merge.the_entry.control.append(ControlPoint.SWITCH_MERGE)
                arms = random.randint(3, len(acyclic_subgraphs))
                for _ in range(arms):
                    arm = helpful.pick_element(acyclic_subgraphs, True)
                    cfg.add_edge(ControlFlowEdge(switch.the_exit, arm.the_entry, ControlFlow.BEGIN_CASE))
                    cfg.add_edge(ControlFlowEdge(arm.the_exit, merge.the_entry, ControlFlow.END_CASE))
                acyclic_subgraphs.append(SingleEntrySingleExit(switch.the_entry, merge.the_exit))
            else:
                before = helpful.pick_element(acyclic_subgraphs, True)
                after = helpful.pick_element(acyclic_subgraphs, True)
                cfg.add_edge(ControlFlowEdge(before.the_exit, after.the_entry, ControlFlow.FORWARD))
                acyclic_subgraphs.append(SingleEntrySingleExit(before.the_entry, after.the_exit))

        return acyclic_subgraphs.pop()

    def traverse(loop):
        for e in loop_nest.successors(loop):
            traverse(e.successor())

        # Post-order actions
        nested_bodies = [e.successor().body for e in loop_nest.successors(loop)]
        if loop_nest.predecessors(loop):
            body = create_loop_body(nested_bodies, loop.size-1)
            v = BasicBlock(Vertex.get_vertex_id())
            cfg.add_vertex(v)
            v.control.append(ControlPoint.LOOP_HEADER)
            body.the_exit.control.append(ControlPoint.LOOP_TAIL)
            cfg.add_edge(ControlFlowEdge(v, body.the_entry, ControlFlow.ENTER_LOOP))
            cfg.add_edge(ControlFlowEdge(body.the_exit, v, ControlFlow.ITERATE_LOOP))
            loop.body = SingleEntrySingleExit(v, v)
        else:
            loop.body = create_loop_body(nested_bodies, loop.size)
            (cfg.entry,) = [v for v in cfg if not cfg.predecessors(v)]

    cfg = FlowGraph()
    (root,) = [loop for loop in loop_nest if not loop_nest.predecessors(loop)]
    traverse(root)

    return cfg


def random_call_graph(main_declaration,
                      subprogram_declarations,
                      call_depth:              int,
                      recursion_allowed:       bool) -> DirectedGraph:
    call_graph = DirectedGraph()
    root = None
    for subprogram_declaration in subprogram_declarations:
        v = Vertex(Vertex.get_vertex_id())
        v.subprogram_declaration = subprogram_declaration
        call_graph.add_vertex(v)
        if subprogram_declaration == main_declaration:
            root = v

    depth = random.randint(1, call_depth)
    helpful.verbose_message('Chosen call graph depth is {}'.format(depth))
    vertex_to_level = create_tree(call_graph, root, depth)

    # The call graph is currently a tree. Now decide whether to add edges to make it into a graph.
    level_to_vertices = {level: set() for level in set(vertex_to_level.values())}
    for v, level in vertex_to_level.items():
        level_to_vertices[level].add(v)

    # Keep adding edges while maintaining an acyclic graph.
    highest_level = max(level_to_vertices.keys())
    for level in sorted(level_to_vertices.keys()):
        if level < highest_level:
            for caller in level_to_vertices[level]:
                if helpful.go_ahead():
                    random_level = random.randint(level+1, highest_level)
                    (callee,) = random.sample(level_to_vertices[random_level], 1)
                    if not call_graph.has_successor(caller, callee):
                        call_graph.add_edge(Edge(caller, callee))

    if recursion_allowed:
        helpful.verbose_message('Attempting to introduce recursion into the call graph')
        # The lowest level to where a recursive call is headed is one above that of the main subprogram.
        # We probably do not want a subprogram to call the main subprogram.
        lowest_level = min(level_to_vertices.keys()) + 1
        for level in reversed(range(lowest_level, highest_level+1)):
            for caller in level_to_vertices[level]:
                if helpful.go_ahead(0.05):
                    random_level = random.randint(lowest_level, level)
                    (callee,) = random.sample(level_to_vertices[random_level], 1)
                    if not call_graph.has_successor(caller, callee):
                        call_graph.add_edge(Edge(caller, callee))

    return call_graph
