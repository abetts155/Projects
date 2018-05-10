import enum
import re
import random

from low_level import instructions
from utils import messages
from utils import dot


class VertexPool:
    """Manage allocation and deallocation of vertices."""

    CUTOFF = 0

    def __init__(self):
        self._max = VertexPool.CUTOFF
        self._min = VertexPool.CUTOFF
        self._allocation = {}

    def negative(self):
        self._min -= 1
        return self._min

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
        elif v.id_ < VertexPool.CUTOFF:
            self._min = min(self._min, v.id_)
        self._allocation[v.id_] = v

    def deregister(self, v):
        del self._allocation[v.id_]

    def __getitem__(self, id_):
        return self._allocation[id_]


class Vertex:
    """Base vertex class"""

    # Used to generate IDs of vertices and keep track of allocation.
    id_pool = VertexPool()

    @classmethod
    def get_vertex_id(cls, dummy=False):
        if dummy:
            return Vertex.id_pool.negative()
        else:
            return Vertex.id_pool.positive()

    def __init__(self, id_: int):
        try:
            self.id_ = id_
            self.id_pool.register(self)
        except ValueError as e:
            messages.error_message(e)

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

    def __del__(self):
        self.id_pool.deregister(self)

    def is_dummy(self):
        return self.id_ < 0

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(str(self))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=record];\n'.format(self, ''.join(label))


class NamedVertex(Vertex):
    """Models a vertex that carries a name, usually identifying a subprogram"""

    def __init__(self, id_, name):
        if not re.match(r'\w+', name):
            raise ValueError('Vertex name must not be empty and must only contain characters in the set [a-zA-Z0-9_]')
        Vertex.__init__(self, id_)
        self.__name = name

    @property
    def name(self):
        return self.__name


class SubprogramVertex(NamedVertex):
    """Models subprograms in a call graph"""

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(self.name)
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=record];\n'.format(self, ''.join(label))


class BasicBlock(NamedVertex, instructions.Instructions):
    """Models a sequence of instructions and control into and out of that sequence"""

    def change_review_status_of_instructions(self,
                                             current_status: instructions.ReviewStatus,
                                             new_status: instructions.ReviewStatus):
        for instruction in self:
            for field in instruction:
                if field.status == current_status:
                    field.status = new_status

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        column_span = 2
        if self:
            column_span += len(max(self, key=lambda instruction: len(instruction)))

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell(column_span=column_span))
        label.append(self.name)
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        if __debug__:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell(color=dot.Colors.cyan, border=5, column_span=column_span))
            label.append('id={}'.format(self))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        for i in self:
            label.extend(i.dotify())

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=box];\n'.format(self, ''.join(label))


class InstructionVertex(Vertex, set):
    """Models an instruction in a dependency graph of instruction scheduling. The set component contains all
    instructions on which this instruction depends inside a basic block"""

    def __init__(self, id_, instruction: instructions.Instruction):
        Vertex.__init__(self, id_)
        self.__instruction = instruction

    @property
    def instruction(self) -> instructions.Instruction:
        return self.__instruction

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)
        label.extend(self.__instruction.dotify())
        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return '{} [label={}, shape=record]'.format(self, ''.join(label))


class Edge:
    """Base edge class"""

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

    def dotify(self):
        return '{}->{};\n'.format(self._predecessor.id_, self._successor.id_)


class Direction(enum.Enum):
    """Direction of control flow of an edge P => S:
    NONE means there is no direction information for P => S.
    RETURN means P only has one successor, S, but P transfers control to another subprogram.
    CONTINUE means P only has one successor, S, and the first instruction of S immediately follows the last instruction
    of P.
    ELSE means P is a 2-way branch and P => S is the false arm.
    THEN means P is a 2-way branch and P => S is the true arm.
    CASE means P is an n-way branch and P => S is one arm of the branch.
    UNREACHABLE means control flow can never traverse P => S"""

    NONE = 0
    RETURN = 1
    CONTINUE = 2
    ELSE = 3
    THEN = 4
    CASE = 5
    UNREACHABLE = 6


class ControlFlowEdge(Edge):
    """Models a transfer of control between basic blocks"""

    def __init__(self, predecessor, successor, direction=Direction.NONE):
        Edge.__init__(self, predecessor, successor)
        self.__direction = direction
        self.__callee = None

    @property
    def direction(self):
        return self.__direction

    @direction.setter
    def direction(self, value: Direction):
        self.__direction = value

    def set_return(self, callee: SubprogramVertex):
        self.__direction = Direction.RETURN
        self.__callee = callee

    def dotify(self):
        if not(self.direction == Direction.NONE or self.direction == Direction.CONTINUE):
            label = self.direction.name.lower()
        else:
            label = ''
        color = dot.Colors.black
        style = dot.Styles.solid

        if self.direction == Direction.RETURN:
            label = '{} ({})'.format(self.direction.name.lower(), self.__callee.name if self.__callee else '?')
            color = dot.Colors.blue
            style = dot.Styles.bold
        elif self.direction == Direction.UNREACHABLE:
            color = dot.Colors.red
            style = dot.Styles.dotted
        return '{}->{} [label="{}", fontcolor={}, color={}, style={}];\n'.format(self._predecessor.id_,
                                                                                 self._successor.id_,
                                                                                 label,
                                                                                 color,
                                                                                 color,
                                                                                 style)


class TransitionEdge(Edge, list):
    """Models a transition between two program points and the sequence of instructions executed during the transition"""

    def __init__(self, predecessor, successor):
        Edge.__init__(self, predecessor, successor)
        self._back_edge = False

    @property
    def back_edge(self):
        return self._back_edge

    def set_back_edge(self):
        self._back_edge = True

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        for arg in self:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell(color=dot.Colors.red, border=2))
            label.append(str(arg.program_point))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        if not self:
            label.extend(dot.HTML.dummy_row())

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{}->{} [label={}, style={}];\n'.format(self._predecessor.id_,
                                                       self._successor.id_,
                                                       ''.join(label),
                                                       dot.Styles.dotted if self._back_edge else dot.Styles.bold)


class LoopTransition(Edge, set):
    class Direction(enum.Enum):
        ENTRY = 0
        EXIT = 1
        BACK = 2

    """Models the transitions that execute when control passes into and out of loops"""
    def __init__(self, predecessor, successor, direction):
        Edge.__init__(self, predecessor, successor)
        self._direction = direction

    @property
    def direction(self):
        return self._direction

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        if self._direction == LoopTransition.Direction.ENTRY:
            color = dot.Colors.red
        elif self._direction == LoopTransition.Direction.EXIT:
            color = dot.Colors.cyan
        else:
            color = dot.Colors.light_grey

        if self:
            for transition in self:
                label.append(dot.HTML.open_row)
                label.append(dot.HTML.open_cell(color=color, border=2))
                label.append('{} to {}'.format(str(transition.predecessor()), str(transition.successor())))
                label.append(dot.HTML.close_cell)
                label.append(dot.HTML.close_row)
        else:
            label.extend(dot.HTML.dummy_row())

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{}->{} [label={}, color={}];\n'.format(self._predecessor.id_,
                                                       self._successor.id_,
                                                       ''.join(label),
                                                       color)


class CallGraphEdge(Edge):
    def __init__(self, predecessor, successor, call_site):
        Edge.__init__(self, predecessor, successor)
        self.__call_site = call_site

    @property
    def call_site(self):
        return self.__call_site

    def dotify(self):
        return '{}->{} [label="{}"];\n'.format(self._predecessor, self._successor, self.__call_site)


class ProgramPointVertex(Vertex):
    """A program point is either a vertex or an edge"""

    def __init__(self, id_, program_point: Edge or Vertex):
        if not isinstance(program_point, (Edge, Vertex)):
            raise TypeError('Program point must be a vertex or an edge')
        Vertex.__init__(self, id_)
        self.__program_point = program_point

    @property
    def program_point(self) -> Edge or Vertex:
        return self.__program_point

    def __str__(self):
        return str(self.__program_point)

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell(color=dot.Colors.cyan, border=5))
        label.append('id={}'.format(self.id_))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(str(self.__program_point))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=record];\n'.format(self.id_, ''.join(label))


class Junction(Vertex):
    def __init__(self, id_, representative):
        Vertex.__init__(self, id_)
        self._representative = representative

    @property
    def representative(self):
        return self._representative

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(str(self.representative.program_point))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=circle];\n'.format(self, ''.join(label))


class SuperBlock(Vertex, list):
    """A super block is a set of program points"""

    def __init__(self, id_):
        Vertex.__init__(self, id_)
        self.__representative = None

    def append(self, element):
        if not isinstance(element, ProgramPointVertex):
            raise TypeError('Only program points can be added to super blocks')
        list.append(self, element)

    @property
    def representative(self):
        return self.__representative

    @representative.setter
    def representative(self, v):
        self.__representative = v

    def _label(self, *args):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell(color=dot.Colors.cyan, border=5))
        label.append('id={}'.format(self))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        for arg in args:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell(color=dot.Colors.red, border=2))
            label.append(arg)
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        for v in self:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell())
            label.append(str(v.program_point))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return label

    def dotify(self):
        return '{} [label={}, shape=record];\n'.format(self, ''.join(self._label()))


class LoopBody(SuperBlock):
    """Models a loop body"""

    def __init__(self, id_, header: Vertex):
        SuperBlock.__init__(self, id_)
        self.__header = header
        self.__tails = set()

    @property
    def header(self) -> Vertex:
        return self.__header

    @property
    def tails(self):
        return self.__tails

    @tails.setter
    def tails(self, value):
        self.__tails.update(value)


class Sequence(Vertex):
    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)
        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell(color=dot.Colors.yellow, border=5))
        label.append('SEQ')
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)
        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return '{} [label={}]'.format(self, ''.join(label))


class Alternative(Vertex):
    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)
        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell(color=dot.Colors.red, border=5))
        label.append('ALT')
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)
        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return '{} [label={}]'.format(self, ''.join(label))


class Loop(Vertex):
    pass


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

    def __init__(self, v: Vertex):
        self.v = v
        self.predecessors = []
        self.successors = []


class DirectedGraph:
    """Models a directed graph: A set of vertices and a set of directed edges"""

    def __init__(self):
        self._data = {}

    def add_vertex(self, v: Vertex):
        if v.id_ in self._data:
            raise DuplicateVertexError('Graph already contains a vertex with ID {}'.format(v.id_))
        self._data[v.id_] = VertexData(v)

    def _get_vertex_data(self, v: Vertex) -> VertexData:
        try:
            return self._data[v.id_]
        except KeyError:
            messages.error_message("No data for vertex with ID {}".format(v.id_))

    def remove_vertex(self, v: Vertex):
        v_info = self._get_vertex_data(v)
        for e in v_info.predecessors:
            self.remove_successor(e.predecessor(), v)
        for e in v_info.successors:
            self.remove_predecessor(e.successor(), v)
        del self._data[v.id_]

    def __contains__(self, v: Vertex):
        return v.id_ in self._data

    def __iter__(self):
        for _ in self._data.values():
            yield _.v

    def number_of_vertices(self):
        return len(self._data)

    def number_of_edges(self):
        return sum([len(self.successors(v)) for v in self])

    def predecessors(self, v: Vertex):
        return self._get_vertex_data(v).predecessors

    def successors(self, v: Vertex):
        return self._get_vertex_data(v).successors

    def add_edge(self, e: Edge) -> None:
        p_info = self._get_vertex_data(e.predecessor())
        p_info.successors.append(e)
        s_info = self._get_vertex_data(e.successor())
        s_info.predecessors.append(e)

    def has_edge(self, p: Vertex, s: Vertex):
        return self.has_successor(p, s) and self.has_predecessor(s, p)

    def remove_predecessor(self, v, p):
        v_info = self._get_vertex_data(v)
        updated_predecessors = [e for e in v_info.predecessors if e.predecessor() != p]
        v_info.predecessors = updated_predecessors

    def remove_successor(self, v, s):
        v_info = self._get_vertex_data(v)
        updated_successors = [e for e in v_info.successors if e.successor() != s]
        v_info.successors = updated_successors

    def remove_edge(self, e: Edge):
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

    def is_call_site(self, call_vertex: SubprogramVertex, basic_block: BasicBlock):
        for e in self.successors(call_vertex):
            if e.call_site == basic_block:
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

    def remove_vertex(self, v: Vertex):
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
                if e.direction == Direction.UNREACHABLE:
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
                if sole_e.direction == Direction.ELSE or sole_e.direction == Direction.THEN:
                    sole_e.direction = Direction.CONTINUE

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
        for v in cfg:
            program_point = ProgramPointVertex(Vertex.get_vertex_id(), v)
            ppg.add_vertex(program_point)
            ppg.__program_point_to_vertex[v] = program_point
            if v == cfg.entry:
                ppg._entry = program_point
            if v == cfg.exit:
                ppg._exit = program_point

        def add_edge():
            program_point = ProgramPointVertex(Vertex.get_vertex_id(), e)
            ppg.add_vertex(program_point)
            ppg.__program_point_to_vertex[e] = program_point
            p = ppg.__program_point_to_vertex[e.predecessor()]
            s = ppg.__program_point_to_vertex[e.successor()]
            ppg.add_edge(ControlFlowEdge(p, program_point, e.direction))
            ppg.add_edge(ControlFlowEdge(program_point, s, Direction.CONTINUE))

        # Add a vertex per control flow transition, and join the control flow
        # transition to its end points on both sides
        for v in cfg:
            for e in cfg.successors(v):
                add_edge()

        # Add dummy edge
        e = ControlFlowEdge(cfg.exit, cfg.entry, Direction.CONTINUE)
        add_edge()

        return ppg

    def __init__(self, program, name):
        FlowGraph.__init__(self, program, name)
        self.__program_point_to_vertex = {}

    def get_vertex(self, program_point: Edge or Vertex):
        try:
            return self.__program_point_to_vertex[program_point]
        except KeyError:
            messages.error_message('No vertex in program point graph for program point {}'.format(str(program_point)))

    def choose_instrumentation(self):
        def attempt_removal():
            existing_edges = set()
            new_edges = set()
            for predecessor_edge in self.predecessors(v):
                for successor_edge in self.successors(predecessor_edge.predecessor()):
                    existing_edges.add(successor_edge)
                for successor_edge in self.successors(v):
                    new_edges.add(Edge(predecessor_edge.predecessor(), successor_edge.successor()))

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
            candidates = [v for v in self if v != self.entry and v != self.exit]
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


class DominatorGraph(FlowGraph):
    def __init__(self, flow_graph):
        FlowGraph.__init__(self, flow_graph.program, flow_graph.name)
        self.__add_vertices(flow_graph)
        self.__add_edges(flow_graph)

    def __add_vertices(self, flow_graph):
        for v in flow_graph.pre_dominator_tree():
            self.add_vertex(v)

    def __add_edges(self, flow_graph):
        for v in flow_graph.pre_dominator_tree():
            for succ_edge in flow_graph.pre_dominator_tree().successors(v):
                self.add_edge(Edge(v, succ_edge.successor()))
        for v in flow_graph.post_dominator_tree():
            for succ_edge in flow_graph.post_dominator_tree().successors(v):
                self.add_edge(Edge(v, succ_edge.successor()))

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for succ_edge in self.successors(v):
                data.append(succ_edge.dotify())

        filename = '{}.{}.dominator.dot'.format(self.program.basename(), self.name)
        dot.generate(filename, data)


class DependenceGraph(DirectedGraph):
    def __init__(self, program, cfg, basic_block: BasicBlock):
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
        entry_v = InstructionVertex(Vertex.get_vertex_id(), None)
        self.add_vertex(entry_v)
        for v in self:
            if v != entry_v and len(self.predecessors(v)) == 0:
                self.add_edge(Edge(entry_v, v))

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

    def is_proper_ancestor(self, a: Vertex, v: Vertex):
        if v == self.root:
            return False
        else:
            (predecessor_e,) = self.predecessors(v)
            if predecessor_e.predecessor() == a:
                return True
            else:
                return self.is_proper_ancestor(a, predecessor_e.predecessor())

    def is_ancestor(self, a: Vertex, v: Vertex):
        return a == v or self.is_proper_ancestor(a, v)


class DominatorTree(Tree, ProgramData):
    class Type(enum.Enum):
        PRE = 0
        POST = 1

    def __init__(self, flow_graph: FlowGraph, root: Vertex):
        Tree.__init__(self)
        ProgramData.__init__(self, flow_graph.program, flow_graph.name)
        self._root = root
        self.__compute(flow_graph)
        assert len(self.predecessors(self._root)) == 0
        if self._root == flow_graph.entry:
            self._type = DominatorTree.Type.PRE
        else:
            self._type = DominatorTree.Type.POST

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

        def do_search(v: Vertex):
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
            forward_transition = Edge.successor
            backward_transitions = DirectedGraph.predecessors
            backward_transition = Edge.predecessor
        else:
            forward_transitions = DirectedGraph.predecessors
            forward_transition = Edge.predecessor
            backward_transitions = DirectedGraph.successors
            backward_transition = Edge.successor

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
            self.add_edge(Edge(parent, child))


class DominanceFrontiers:
    def __init__(self, g: FlowGraph, t: DominatorTree):
        self._frontier = {v: set() for v in g}
        self.__compute(g, t)

    def __getitem__(self, item):
        return self._frontier[item]

    def __compute(self, g, t):
        if t.root == g.entry:
            backward_transitions = DirectedGraph.predecessors
            backward_transition = Edge.predecessor
        else:
            backward_transitions = DirectedGraph.successors
            backward_transition = Edge.successor

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


class StrongComponents:
    def __init__(self, directed_graph: DirectedGraph):
        self._scc = {v: None for v in directed_graph}
        self.__compute(directed_graph)

    def __getitem__(self, item):
        return self._scc[item]

    def __iter__(self):
        for scc_id in set(self._scc.values()):
            yield scc_id

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
        pre_order = {v: 0 for v in directed_graph}
        low_link = {v: 0 for v in directed_graph}
        for v in directed_graph:
            if pre_order[v] == 0:
                explore(v)


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
    @staticmethod
    def create(ppg: ProgramPointGraph, unify_shared_loops=True):
        lnt = LoopNests(ppg)
        lnt.discover_loop_bodies(ppg, unify_shared_loops)
        lnt.set_entry(ppg)
        lnt.set_exit(ppg)

        edges = {}
        for v in ppg:
            if isinstance(v.program_point, Edge):
                loop = lnt.find_loop(v)
                loop_of_predecessor = lnt.find_loop(ppg.get_vertex(v.program_point.predecessor()))
                loop_of_successor = lnt.find_loop(ppg.get_vertex(v.program_point.successor()))
                if loop != loop_of_predecessor:
                    if not lnt.has_edge(loop_of_predecessor, loop):
                        edge = LoopTransition(loop_of_predecessor, loop, LoopTransition.Direction.EXIT)
                        lnt.add_edge(edge)
                        edges[(loop_of_predecessor, loop)] = edge
                    edge = edges[(loop_of_predecessor, loop)]
                    edge.add(Edge(ppg.get_vertex(v.program_point.predecessor()), v))
                if loop != loop_of_successor:
                    if not lnt.has_edge(loop, loop_of_successor):
                        edge = LoopTransition(loop, loop_of_successor, LoopTransition.Direction.ENTRY)
                        lnt.add_edge(edge)
                        edges[(loop, loop_of_successor)] = edge
                    edge = edges[(loop, loop_of_successor)]
                    edge.add(Edge(v, ppg.get_vertex(v.program_point.successor())))

                if lnt.is_tail(v):
                    loop = lnt.find_loop(v)
                    if not lnt.has_edge(loop, loop):
                        edge = LoopTransition(loop, loop, LoopTransition.Direction.BACK)
                        lnt.add_edge(edge)
                        edges[(loop, loop)] = edge
                    edge = edges[(loop, loop)]
                    edge.add(Edge(v, ppg.get_vertex(v.program_point.successor())))
        return lnt

    def __init__(self, flow_graph):
        FlowGraph.__init__(self, flow_graph.program, flow_graph.name)
        self._loops = []
        self._headers = set()
        self._tails = set()
        self._program_point_to_loop = {v: None for v in flow_graph}

    def set_entry(self, flow_graph):
        (self.entry,) = [loop for loop in self if flow_graph.entry in loop]

    def set_exit(self, flow_graph):
        (self.exit,) = [loop for loop in self if flow_graph.exit in loop]

    def discover_loop_bodies(self, flow_graph, unify_shared_loops):
        def do_search(v):
            visited.add(v)
            if v != header:
                for e in flow_graph.predecessors(v):
                    where_next = containment[e.predecessor()]
                    if where_next not in visited:
                        do_search(where_next)
            order.append(v)

        containment = {v: v for v in flow_graph}
        data = {v: set() for v in flow_graph}
        dfs = DepthFirstSearch(flow_graph, flow_graph.entry)
        for v in reversed(dfs.pre_order()):
            back_edges = [e for e in flow_graph.predecessors(v) if e in dfs.back_edges]
            if back_edges:
                # Sort back edges according to their post-order numbering, then reverse, so that we visit all successors
                # of a vertex before the vertex itself
                back_edges.sort(key=lambda e: dfs.post_order_number(e.predecessor()), reverse=True)
                order = []
                visited = set()
                header = v
                tails = set()
                for e in back_edges:
                    if not flow_graph.pre_dominator_tree().is_ancestor(e.successor(), e.predecessor()):
                        messages.error_message(
                            "Edge {} in '{}' identifies an irreducible loop".format(str(e.predecessor()), flow_graph.name))
                    do_search(e.predecessor())
                    tails.add(e.predecessor())

                for w in reversed(order):
                    containment[w] = header
                    if unify_shared_loops:
                        data[w].add(header)
                    elif w in tails:
                        data[w].add(w)

                    if w != header:
                        # Propagate reachability information concerning loop tails to immediate predecessors.
                        # We ignore the loop header so that the information does not spill out to enclosing loop.
                        for e in flow_graph.predecessors(w):
                            data[containment[e.predecessor()]].update(data[w])

                loop_vertices = {}
                for w in order:
                    # Do not add an inner loop header to the partition for this header.
                    if w not in [loop.header for loop in self._loops]:
                        if frozenset(data[w]) not in loop_vertices:
                            loop = LoopBody(Vertex.get_vertex_id(), header)
                            loop.tails = tails
                            loop_vertices[frozenset(data[w])] = loop
                            self.add_vertex(loop)
                        loop = loop_vertices[frozenset(data[w])]
                        loop.append(w)

                # Clear the reachability information in readiness for enclosing loops.
                data[header].clear()

    def add_vertex(self, v: Vertex):
        FlowGraph.add_vertex(self, v)
        self._loops.append(v)

    def __iter__(self):
        for loop in self._loops:
            yield loop

    def is_outermost_loop(self, loop):
        return loop == self._loops[-1]

    def is_header(self, v):
        if v in self._headers:
            return True
        elif len(self._headers) != self.number_of_vertices():
            for loop in self:
                if v == loop.header:
                    self._headers.add(v)
                    return True
        return False

    def is_tail(self, v):
        if v in self._tails:
            return True
        else:
            for loop in self:
                if v in loop.tails:
                    self._tails.add(v)
                    return True
        return False

    def find_loop(self, v: ProgramPointVertex):
        if self._program_point_to_loop[v] is None:
            for loop in self:
                if v in loop:
                    self._program_point_to_loop[v] = loop
        return self._program_point_to_loop[v]

    def induce(self, flow_graph, header, guarantee_single_exit=False):
        messages.debug_message('Inducing loop subgraph for header {}'.format(header))

        induced_graph = ProgramPointGraph(flow_graph.program, flow_graph.name)
        loop = self.find_loop(header)

        # Add program points in the loop body.
        for v in loop:
            induced_graph.add_vertex(v)

        # Add edges between program points in the loop body.
        for v in loop:
            for succcessor_edge in flow_graph.successors(v):
                if succcessor_edge.successor() in induced_graph and succcessor_edge.successor() != header:
                    induced_graph.add_edge(succcessor_edge)

        # Add program points and edges that model control flow out of loop.
        for transition_edge in self.successors(loop):
            if transition_edge.direction == LoopTransition.Direction.EXIT:
                for edge in transition_edge:
                    if edge.successor() not in induced_graph:
                        induced_graph.add_vertex(edge.successor())
                    induced_graph.add_edge(edge)
            elif transition_edge.direction == LoopTransition.Direction.ENTRY:
                for edge in transition_edge:
                    if edge.successor() not in induced_graph:
                        induced_graph.add_vertex(edge.successor())
                    induced_graph.add_edge(edge)

        # Add program points and edges that model control flow into the loop from inner loops.
        for transition_edge in self.predecessors(loop):
            if transition_edge.direction == LoopTransition.Direction.EXIT:
                for edge in transition_edge:
                    induced_graph.add_edge(Edge(transition_edge.predecessor().header, edge.successor()))

        # Set the entry of the induced graph.
        induced_graph.entry = header

        if guarantee_single_exit:
            # Set the exit of the induced graph, if instructed.
            exits = [v for v in induced_graph if len(induced_graph.successors(v)) == 0 or self.is_tail(v)]
            if len(exits) == 1:
                (induced_graph.exit,) = exits
            else:
                dummy_program_point = ProgramPointVertex(Vertex.get_vertex_id(True), Vertex(Vertex.get_vertex_id()))
                induced_graph.add_vertex(dummy_program_point)
                induced_graph.exit = dummy_program_point
                for exit_program_point in exits:
                    induced_graph.add_edge(ControlFlowEdge(exit_program_point, dummy_program_point))
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


class InstrumentationPointGraph(FlowGraph):
    @staticmethod
    def create(ppg: ProgramPointGraph, lnt: LoopNests, db):
        # Add selected instrumented program points.
        ipg = InstrumentationPointGraph(ppg.program, ppg.name)
        for v in ppg:
            ipg.add_vertex(v)

        for v in ppg:
            for successor_edge in ppg.successors(v):
                ipg.add_edge(TransitionEdge(v, successor_edge.successor()))

        def disconnect(ipg, v, back_edge, loop):
            for predecessor_edge in ipg.predecessors(v):
                if predecessor_edge.predecessor() != v:
                    for successor_edge in ipg.successors(v):
                        if successor_edge.successor() != v:
                            edge = TransitionEdge(predecessor_edge.predecessor(), successor_edge.successor())
                            edge.extend(predecessor_edge)
                            edge.extend([v])
                            edge.extend(successor_edge)
                            ipg.add_edge(edge)

                            if (back_edge and
                                    db.is_instrumentation(predecessor_edge.predecessor().program_point) and
                                    db.is_instrumentation(successor_edge.successor().program_point) and
                                    predecessor_edge.predecessor() in loop and
                                    successor_edge.successor() in loop):
                                edge.set_back_edge()
            ipg.remove_vertex(v)

        loop_union = {}
        for loop in lnt:
            union = loop[:]
            loop_union[loop] = union
            for successor_edge in lnt.successors(loop):
                if successor_edge.direction == LoopTransition.Direction.ENTRY:
                    nested_loop = successor_edge.successor()
                    union.extend(loop_union[nested_loop])

            to_remove = [v for v in loop if not db.is_instrumentation(v.program_point)]

            for v in to_remove:
                if v != loop.header and not lnt.is_tail(v):
                    disconnect(ipg, v, False, union)

            for v in to_remove:
                if v in ipg:
                    disconnect(ipg, v, True, union)
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


class SuperBlockGraph(DirectedGraph, ProgramData):
    def __init__(self, lnt: LoopNests):
        DirectedGraph.__init__(self)
        ProgramData.__init__(self, lnt.program, lnt.name)
        self.__create(lnt)

    def __create(self, lnt):
        for induced_subgraph in lnt.induce():
            dominator_graph = DominatorGraph(induced_subgraph)
            dominator_graph.dotify()
            strong_components = StrongComponents(dominator_graph)
            scc_to_super_block, junctions = self._add_vertices(induced_subgraph, strong_components)
            self.__add_edges(induced_subgraph, strong_components, scc_to_super_block, junctions)

    def _add_vertices(self, induced_subgraph, strong_components):
        scc_to_super_block = {}
        for scc_id in strong_components:
            super_block = SuperBlock(Vertex.get_vertex_id())
            scc_to_super_block[scc_id] = super_block
            self.add_vertex(super_block)

        dfs = DepthFirstSearch(induced_subgraph, induced_subgraph.entry)
        for v in reversed(dfs.post_order()):
            if not v.is_dummy():
                scc_id = strong_components[v]
                super_block = scc_to_super_block[scc_id]
                super_block.append(v)
                if isinstance(v.program_point, Vertex):
                    super_block.representative = v

        for super_block in scc_to_super_block.values():
            if not super_block.representative:
                # No representative, so pick the first
                super_block.representative = super_block[0]

        junctions = {}
        edges = set()
        for super_block in scc_to_super_block.values():
            for v in super_block:
                if len(induced_subgraph.successors(v)) > 1:
                    junctions[v] = Junction(Vertex.get_vertex_id(), v)
                    edges.add(Edge(super_block, junctions[v]))

        for edge in edges:
            self.add_vertex(edge.successor())
            self.add_edge(edge)

        return scc_to_super_block, junctions

    def __add_edges(self, induced_subgraph, strong_components, scc_to_super_block, junctions):
        for super_block in scc_to_super_block.values():
            v = super_block[0]
            if not isinstance(v.program_point, Vertex):
                (pred_edge,) = induced_subgraph.predecessors(v)
                predecessor = junctions[pred_edge.predecessor()]
                self.add_edge(Edge(predecessor, super_block))
            else:
                for pred_edge in induced_subgraph.predecessors(v):
                    predecessor = scc_to_super_block[strong_components[pred_edge.predecessor()]]
                    self.add_edge(Edge(predecessor, super_block))

    def super_blocks(self):
        for v in self:
            if isinstance(v, SuperBlock):
                yield v

    def junctions(self):
        for v in self:
            if isinstance(v, Junction):
                yield v

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())
        filename = '{}.{}.super.dot'.format(self.program.basename, self.name)
        dot.generate(filename, data)


class SyntaxTree(Tree):
    def __init__(self, g: FlowGraph):
        Tree.__init__(self)
        self.g = g
        self._cache = {}
        self._post_dominance_frontier = DominanceFrontiers(g, g.post_dominator_tree())
        self._root = self.__make_sequence_subtree(g.entry, g.exit, True)

    def __make_sequence_subtree(self, source: Vertex, target: Vertex, inclusive: bool):
        seq = Sequence(Vertex.get_vertex_id())
        self.add_vertex(seq)

        walker = source
        while walker != target:
            if len(self.g.predecessors(walker)) > 1 and len(self._post_dominance_frontier[walker]) > 1:
                if walker != source:
                    if walker not in self._cache:
                        subroot = self.__make_sequence_subtree(walker, target, False)
                        self._cache[walker] = subroot
                    else:
                        subroot = self._cache[walker]
                    self.add_edge(Edge(seq, subroot))
                walker = target
            else:
                self.add_vertex(walker)
                self.add_edge(Edge(seq, walker))
                if len(self.g.successors(walker)) == 1:
                    print(self.g.name, source, target, walker)
                    (e,) = self.g.successors(walker)
                    walker = e.successor()
                else:
                    self.add_edge(Edge(seq, self.__make_alternative_subtree(walker)))
                    (dominator_e,) = self.g.post_dominator_tree().predecessors(walker)
                    walker = dominator_e.predecessor()

        if inclusive:
            self.add_vertex(target)
            self.add_edge(Edge(seq, target))

        return seq

    def __make_alternative_subtree(self, branch: Vertex):
        alt = Alternative(Vertex.get_vertex_id())
        self.add_vertex(alt)

        (dominator_e,) = self.g.post_dominator_tree().predecessors(branch)
        for e in self.g.successors(branch):
            seq = self.__make_sequence_subtree(e.successor(), dominator_e.predecessor(), False)
            self.add_edge(Edge(alt, seq))

        return alt

    def dotify(self):
        data = []
        for v in self:
            data.append(v.dotify())
            for e in self.successors(v):
                data.append(e.dotify())
        filename = '{}.{}.ast.dot'.format(self.g.program, self.g.name)
        dot.generate(filename, data)

