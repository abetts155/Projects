import sys
import inspect
import argparse
import enum
import random
import typing


def go_ahead(weight=None):
    if weight:
        return random.random() < weight
    else:
        return bool(random.getrandbits(1))


def pick_element(a_list, remove=False):
    element = a_list[random.randint(0, len(a_list) - 1)]
    if remove:
        a_list.remove(element)
    return element


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

    def __contains__(self, v):
        return v.id_ in self._allocation


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

    def __del__(self):
        if self in self.id_pool:
            self.id_pool.deregister(self)


class SubprogramVertex(Vertex):
    pass


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


class ControlFlow(enum.Enum):
    FORWARD = 0
    THEN = 1
    ELSE = 2
    CASE = 3
    BREAK = 4
    MERGE = 5


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


class DuplicateVertexError(ValueError):
    pass


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
            print("No data for vertex with ID {}".format(v.id_))
            sys.exit(1)

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


class FlowGraph(DirectedGraph):
    """Models a directed graph that has a designated entry vertex and a designated exit vertex"""

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

    def remove_vertex(self, v: Vertex):
        DirectedGraph.remove_vertex(self, v)
        if v == self._entry:
            self._entry = None
        if v == self._exit:
            self._exit = None


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
                if e.flow == ControlFlow.ELSE:
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


class Loop(Vertex):
    def __init__(self, id_):
        Vertex.__init__(self, id_)
        self._size = 0

    @property
    def size(self):
        return self._size

    @size.setter
    def size(self, value):
        self._size = value


class LoopNest(DirectedGraph):
    def __init__(self):
        DirectedGraph.__init__(self)
        self._root = None

    @property
    def root(self):
        return self._root

    @root.setter
    def root(self, v):
        self._root = v


def create_loop_hierarchy(basic_blocks:    int,
                          number_of_loops: int,
                          nesting_depth:   int) -> LoopNest:
    # Add abstract vertices to the tree, including an extra one for the dummy outer loop.
    lnt = LoopNest()
    for _ in range(1, number_of_loops+2):
        v = Loop(Vertex.get_vertex_id())
        lnt.add_vertex(v)

    # Add edges to the tree.
    vertex_to_level = {v: 0 for v in lnt}
    (lnt.root,) = random.sample(vertex_to_level.keys(), 1)
    parent = lnt.root
    for v in lnt:
        if v != lnt.root:
            new_level = vertex_to_level[parent] + 1
            if new_level <= nesting_depth:
                lnt.add_edge(Edge(parent, v))
                vertex_to_level[v] = new_level
            else:
                # The height of the tree now exceeds the maximum depth, so backtrack to an arbitrary proper ancestor.
                ancestor = parent
                while True:
                    (e,) = lnt.predecessors(ancestor)
                    ancestor = e.predecessor()
                    if go_ahead() or ancestor == lnt.root:
                        break
                parent = ancestor
                lnt.add_edge(Edge(parent, v))
                vertex_to_level[v] = vertex_to_level[parent] + 1
            parent = v

    # Compute number of basic blocks in each loop.
    basic_blocks_remaining = basic_blocks
    for v in lnt:
        # Guarantee each loop has at least 2 basic blocks plus basic blocks needed to connect inner nested loops.
        v.size = 2 + len(lnt.successors(v))
        basic_blocks_remaining -= v.size

    # Arbitrarily distribute any remaining basic blocks to the loop bodies.
    while basic_blocks_remaining > 0:
        for v in lnt:
            additional_basic_blocks = random.randint(0, basic_blocks_remaining)
            v.size += additional_basic_blocks
            basic_blocks_remaining -= additional_basic_blocks

    return lnt


class SingleEntrySingleExit:
    __slots__ = ['the_entry', 'the_exit']

    def __init__(self, the_entry, the_exit):
        self.the_entry = the_entry
        self.the_exit = the_exit


def generate_control_flow_graph(lnt: LoopNest) -> FlowGraph:

    def create_loop_body(loop: Loop):
        subgraphs = []
        for _ in range(random.randint(1, loop.size)):
            v = Vertex(Vertex.get_vertex_id())
            cfg.add_vertex(v)
            subgraphs.append(SingleEntrySingleExit(v, v))

        while len(subgraphs) > 1:
            if len(subgraphs) > 3 and go_ahead():
                # Create an if-statement with a then-statement and an else-statement.
                branch = pick_element(subgraphs, True)
                merge = pick_element(subgraphs, True)
                then_side = pick_element(subgraphs, True)
                else_side = pick_element(subgraphs, True)
                cfg.add_edge(ControlFlowEdge(branch.the_exit, then_side.the_entry, ControlFlow.THEN))
                cfg.add_edge(ControlFlowEdge(branch.the_exit, else_side.the_entry, ControlFlow.ELSE))
                cfg.add_edge(ControlFlowEdge(then_side.the_exit, merge.the_entry, ControlFlow.MERGE))
                cfg.add_edge(ControlFlowEdge(else_side.the_exit, merge.the_entry, ControlFlow.MERGE))
                subgraphs.append(SingleEntrySingleExit(branch.the_entry, merge.the_exit))
            elif len(subgraphs) > 2 and go_ahead():
                # Create an if-statement with a then-statement but no else-statement.
                branch = pick_element(subgraphs, True)
                merge = pick_element(subgraphs, True)
                then_side = pick_element(subgraphs, True)
                cfg.add_edge(ControlFlowEdge(branch.the_exit, then_side.the_entry, ControlFlow.THEN))
                cfg.add_edge(ControlFlowEdge(then_side.the_exit, merge.the_entry, ControlFlow.MERGE))
                cfg.add_edge(ControlFlowEdge(branch.the_exit, merge.the_entry, ControlFlow.FORWARD))
                subgraphs.append(SingleEntrySingleExit(branch.the_entry, merge.the_exit))
            elif len(subgraphs) > 4 and go_ahead():
                # Create a switch-statement
                switch = pick_element(subgraphs, True)
                merge = pick_element(subgraphs, True)
                arms = random.randint(3, len(subgraphs))
                for _ in range(arms):
                    arm = pick_element(subgraphs, True)
                    cfg.add_edge(ControlFlowEdge(switch.the_exit, arm.the_entry, ControlFlow.CASE))
                    cfg.add_edge(ControlFlowEdge(arm.the_exit, merge.the_entry, ControlFlow.BREAK))
                subgraphs.append(SingleEntrySingleExit(switch.the_entry, merge.the_exit))
            else:
                before = pick_element(subgraphs, True)
                after = pick_element(subgraphs, True)
                cfg.add_edge(ControlFlowEdge(before.the_exit, after.the_entry, ControlFlow.FORWARD))
                subgraphs.append(SingleEntrySingleExit(before.the_entry, after.the_exit))

    cfg = FlowGraph()
    for loop in sorted([loop for loop in lnt], key=lambda loop: len(lnt.predecessors(loop)), reverse=True):
        create_loop_body(loop)

    (cfg.entry,) = [v for v in cfg if len(cfg.predecessors(v)) == 0]
    return cfg


class Unparser:
    @staticmethod
    def blanks(length):
        return ' ' * length

    @staticmethod
    def newlines(length=1):
        return '\n' * length

    open_block = '{'
    close_block = '}'
    statement_terminator = ';'


class Language(enum.Enum):
    C = 0
    Ada = 1


class Analysis(enum.Enum):
    TIMING = 0
    COVERAGE = 1


class Keywords(enum.Enum):
    IF = 0
    ELSE = 1
    SWITCH = 2
    CASE = 3
    RETURN = 4
    BREAK = 5
    DEFAULT = 6


class UnaryOperator:
    pass


class BinaryOperator:
    pass


class Operator:
    pass


class RelationalOperator(Operator):
    pass


class ArithmeticOperator(Operator):
    pass


class LogicalOperator(Operator):
    pass


class BitwiseOperator(Operator):
    pass


class Equality(RelationalOperator, BinaryOperator):
    def unparse(self):
        return '=='


class Inequality(RelationalOperator, BinaryOperator):
    def unparse(self):
        return '!='


class LessThan(RelationalOperator, BinaryOperator):
    def unparse(self):
        return '<'


class LessThanEqual(RelationalOperator, BinaryOperator):
    def unparse(self):
        return '<='


class GreaterThan(RelationalOperator, BinaryOperator):
    def unparse(self):
        return '>'


class GreaterThanEqual(RelationalOperator, BinaryOperator):
    def unparse(self):
        return '>='


class Add(ArithmeticOperator, BinaryOperator):
    def unparse(self):
        return '+'


class Subtract(ArithmeticOperator, BinaryOperator):
    def unparse(self):
        return '-'


class Multiply(ArithmeticOperator, BinaryOperator):
    def unparse(self):
        return '*'


class Divide(ArithmeticOperator, BinaryOperator):
    def unparse(self):
        return '/'


class Modulus(ArithmeticOperator, BinaryOperator):
    def unparse(self):
        return '%'


class Plus(ArithmeticOperator, UnaryOperator):
    def unparse(self):
        return '+'


class Minus(ArithmeticOperator, UnaryOperator):
    def unparse(self):
        return '-'


class Negation(LogicalOperator, UnaryOperator):
    def unparse(self):
        return '!'


class And(LogicalOperator, BinaryOperator):
    def unparse(self):
        return '&&'


class Or(LogicalOperator, BinaryOperator):
    def unparse(self):
        return '||'


class Xor(LogicalOperator, BinaryOperator):
    def unparse(self):
        return '^'


class Complement(BitwiseOperator, UnaryOperator):
    def unparse(self):
        return '~'


class ShiftLeft(BitwiseOperator, BinaryOperator):
    def unparse(self):
        return '<<'


class ShiftRight(BitwiseOperator, BinaryOperator):
    def unparse(self):
        return '>>'


def get_operators(predicate):
    ops = inspect.getmembers(sys.modules[__name__], lambda member: inspect.isclass(member) and predicate(member))
    return [op[1] for op in ops]


class Identifier:
    def __init__(self, name):
        self._name = name

    def unparse(self):
        return self._name


class IntegerType:
    def unparse(self):
        return 'int'


class VoidType:
    def unparse(self):
        return 'void'


class ParameterDecl:
    def __init__(self, base_type, name):
        self._base_type = base_type
        self._name = name
        self._initialiser = None

    @property
    def base_type(self):
        return self._base_type

    @property
    def name(self):
        return self._name

    @property
    def initialiser(self):
        return self._initialiser

    @initialiser.setter
    def initialiser(self, expr):
        self._initialiser = expr

    def unparse(self):
        if self.initialiser:
            return '{} {} = {}'.format(self._base_type.unparse(),
                                       self._name.unparse(),
                                       self._initialiser.unparse())
        else:
            return '{} {}'.format(self._base_type.unparse(), self._name.unparse())


class VariableDecl(ParameterDecl):
    def unparse(self):
        return '{}{}'.format(super().unparse(), Unparser.statement_terminator)


class Literal:
    def __init__(self, value):
        self._value = value

    def unparse(self):
        if self._value < 0:
            return '({})'.format(self._value)
        else:
            return '{}'.format(self._value)


class VariableReference:
    def __init__(self, name):
        self.name = name

    def unparse(self):
        return self.name.unparse()


class CallExpr:
    def __init__(self, name, arguments):
        self._name = name
        self._arguments = arguments

    def unparse(self):
        return '{}({})'.format(self._name.unparse(), ', '.join(a.unparse() for a in self._arguments))


class UnaryExpr:
    def __init__(self, op: UnaryOperator, child):
        self._op = op
        self._child = child

    def unparse(self):
        return '({}{})'.format(self._op.unparse(), self._child.unparse())


class BinaryExpr:
    def __init__(self, left, op: BinaryOperator, right):
        self._left = left
        self._op = op
        self._right = right

    def unparse(self):
        return '({}{}{})'.format(self._left.unparse(), self._op.unparse(), self._right.unparse())


def is_strict_subclass(candidate, base_class):
    return issubclass(candidate, base_class) and candidate != base_class


def random_expr(language: Language, number_of_operations, ignore_operators, subprograms, variables):
    def random_arithmetic_op():
        if language == Language.C:
            def is_arithmetic(a_class):
                return is_strict_subclass(a_class, ArithmeticOperator)

            choices = [op for op in get_operators(is_arithmetic) if op not in ignore_operators]
            op = random.choice(choices)
            return op()

    def random_unary_op():
        if language == Language.C:
            def is_unary(a_class):
                return is_strict_subclass(a_class, UnaryOperator)

            choices = [op for op in get_operators(is_unary)]
            op = random.choice(choices)
            return op()

    def random_8_bit_integer():
        return random.randint(-2**4, 2**4-1)

    def random_leaf():
        if variables and go_ahead():
            variable = random.choice(variables)
            return VariableReference(variable.name)
        elif subprograms and go_ahead():
            subprogram = random.choice(subprograms)
            subprogram.called = True
            arguments = []
            for i in range(len(subprogram.formals)):
                if not variables or go_ahead():
                    arguments.append(Literal(random_8_bit_integer()))
                else:
                    variable = random.choice(variables)
                    arguments.append(VariableReference(variable.name))
            return CallExpr(subprogram.name, arguments)
        else:
            return Literal(random_8_bit_integer())

    root_level = 1
    height = number_of_operations
    subtrees = {}
    while height >= root_level:
        if height == number_of_operations:
            root = BinaryExpr(random_leaf(),
                              random_arithmetic_op(),
                              random_leaf())
        else:
            if go_ahead():
                root = UnaryExpr(random_unary_op(),
                                 subtrees[height + 1])
            else:
                if go_ahead():
                    root = BinaryExpr(random_leaf(),
                                      random_arithmetic_op(),
                                      subtrees[height + 1])
                else:
                    root = BinaryExpr(subtrees[height + 1],
                                      random_arithmetic_op(),
                                      random_leaf())
        subtrees[height] = root
        height -= 1
    return subtrees[root_level]


def true_expr():
    return BinaryExpr(Literal(1), Equality(), Literal(1))


def false_expr():
    return BinaryExpr(Literal(1), Equality(), Literal(0))


class IfStmt:
    def __init__(self, expr):
        self._expr = expr

    def unparse(self):
        return '{} ({})'.format(Keywords.IF.name.lower(), self._expr.unparse())


class SwitchStmt:
    def __init__(self, expr):
        self._expr = expr

    def unparse(self):
        return '{} ({})'.format(Keywords.SWITCH.name.lower(), self._expr.unparse())


class ReturnStmt:
    def __init__(self, expr):
        self._expr = expr

    def unparse(self):
        return '{} {}{}'.format(Keywords.RETURN.name.lower(), self._expr.unparse(), Unparser.statement_terminator)


class NullStmt:
    def unparse(self):
        return '{}'.format(Unparser.statement_terminator)


class AssignStmt:
    def __init__(self, lhs, rhs):
        self._lhs = lhs
        self._rhs = rhs

    def unparse(self):
        return '{} = {}{}'.format(self._lhs.unparse(), self._rhs.unparse(), Unparser.statement_terminator)


class Subprogram:
    def __init__(self, language: Language, name):
        self._language = language
        self._name = name
        self._return_type = None
        self._formals = []
        self._main = False
        self._cfg = None
        self._called = False
        self._annotations = []
        self._subprograms = []

    def unparse_signature_and_body(self):
        signature_text = '{} {} ({})'.format(self._return_type.unparse(),
                                             self.name.unparse(),
                                             ', '.join(f.unparse() for f in self._formals))
        body_text = Unparser.open_block
        body_text += Unparser.newlines()
        dfs = DepthFirstSearch(self._cfg, self._cfg.entry)
        indent_width = 2
        blank_columns = indent_width
        for v in reversed(dfs.post_order()):
            if len(self._cfg.predecessors(v)) == 1:
                (e,) = self._cfg.predecessors(v)
                if e.flow in [ControlFlow.THEN, ControlFlow.ELSE, ControlFlow.CASE]:
                    if e.flow == ControlFlow.ELSE:
                        body_text += Unparser.blanks(blank_columns)
                        body_text += Keywords.ELSE.name.lower()
                        body_text += Unparser.newlines()
                    elif e.flow == ControlFlow.CASE:
                        body_text += Unparser.blanks(blank_columns)
                        if e.expr:
                            body_text += '{} {}:'.format(Keywords.CASE.name.lower(), e.expr.unparse())
                        else:
                            body_text += '{}:'.format(Keywords.DEFAULT.name.lower())
                        body_text += Unparser.newlines()

                    body_text += Unparser.blanks(blank_columns)
                    body_text += Unparser.open_block
                    body_text += Unparser.newlines()
                    blank_columns += indent_width
            elif len(self._cfg.predecessors(v)) > 2:
                blank_columns -= indent_width
                body_text += Unparser.blanks(blank_columns)
                body_text += Unparser.close_block
                body_text += Unparser.newlines()

            if len(self._cfg.successors(v)) == 2:
                (stmt,) = v.statements
                body_text += '{}{}'.format(Unparser.blanks(blank_columns), stmt.unparse())
                body_text += Unparser.newlines()
            elif len(self._cfg.successors(v)) > 2:
                (stmt,) = v.statements
                body_text += '{}{}'.format(Unparser.blanks(blank_columns), stmt.unparse())
                body_text += Unparser.newlines()
                body_text += Unparser.blanks(blank_columns)
                body_text += Unparser.open_block
                body_text += Unparser.newlines()
                blank_columns += indent_width
            else:
                for stmt in v.statements:
                    body_text += '{}{}'.format(Unparser.blanks(blank_columns), stmt.unparse())
                    if len(self._cfg.successors(v)) > 0 or stmt != v.statements[-1]:
                        body_text += Unparser.newlines()

            if len(self._cfg.successors(v)) == 1:
                (e,) = self._cfg.successors(v)
                if e.flow in [ControlFlow.MERGE, ControlFlow.BREAK]:
                    if e.flow == ControlFlow.BREAK:
                        if go_ahead():
                            body_text += Unparser.blanks(blank_columns)
                            body_text += Keywords.BREAK.name.lower()
                            body_text += Unparser.statement_terminator
                            body_text += Unparser.newlines()

                    blank_columns -= indent_width
                    body_text += Unparser.blanks(blank_columns)
                    body_text += Unparser.close_block
                    body_text += Unparser.newlines()

        body_text += Unparser.newlines()
        body_text += Unparser.close_block
        body_text += Unparser.newlines()
        return signature_text + Unparser.newlines() + body_text

    def unparse(self):
        text = ''.join(a.unparse() for a in self.annotations)
        if self.subprograms:
            text += Unparser.newlines() + ''.join(SubprogramDecl(s).unparse() for s in self.subprograms)
            text += Unparser.newlines() + ''.join(s.unparse() for s in self.subprograms)

        if self.is_fake():
            return text
        else:
            return text + self.unparse_signature_and_body() + Unparser.newlines()

    def generate_body(self, expression_depth, ignore_operators, block_length, subprograms, call_graph):
        def subprogram_subset():
            if self.main:
                # If any call expressions are generated, try to use as yet uncalled subprograms.
                subset = [s for s in subprograms if not s.called]
                if not subset:
                    return subprograms
                else:
                    return subset
            else:
                return [e.successor().subprogram for e in call_graph.successors(call_vertex)]

        (call_vertex,) = [v for v in call_graph if v.subprogram == self]
        dfs = DepthFirstSearch(self._cfg, self._cfg.entry)
        for v in reversed(dfs.post_order()):
            v.statements = []
            if len(self._cfg.successors(v)) == 2:
                expr = random_expr(self._language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   subprogram_subset(),
                                   self._formals)
                if_stmt = IfStmt(expr)
                v.statements.append(if_stmt)
            elif len(self._cfg.successors(v)) > 2:
                expr = random_expr(self._language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   subprogram_subset(),
                                   self._formals)
                switch_stmt = SwitchStmt(expr)
                v.statements.append(switch_stmt)

                default = False
                # We need as many 32-bit integers as there are case arms, so pick an upper bound for the random
                # number generator that will not exceed a 32-bit representation.
                upper_bound = 2**32 - 1 - len(self._cfg.successors(v))
                for idx, e in enumerate(self._cfg.successors(v), start=random.randint(0, upper_bound)):
                    if not default and go_ahead():
                        default = True
                        e.expr = None
                    else:
                        # Use the value of the enumerator to guarantee case arms have unique values.
                        e.expr = Literal(idx)

            elif len(self._cfg.successors(v)) == 0:
                if self.main:
                    return_stmt = ReturnStmt(false_expr())
                    v.statements.append(return_stmt)
                else:
                    expr = random_expr(self._language,
                                       random.randint(1, expression_depth),
                                       ignore_operators,
                                       subprogram_subset(),
                                       self._formals)
                    return_stmt = ReturnStmt(expr)
                    v.statements.append(return_stmt)
            else:
                for i in range(random.randint(1,block_length)):
                    if go_ahead(0.8):
                        stmt = VariableDecl(IntegerType(), Identifier('i{}_{}'.format(v, i)))
                        stmt.initialiser = random_expr(self._language,
                                                       random.randint(1, expression_depth),
                                                       ignore_operators,
                                                       subprogram_subset(),
                                                       self._formals)
                    else:
                        stmt = NullStmt()
                    v.statements.append(stmt)

    @property
    def name(self):
        return self._name

    @property
    def return_type(self):
        return self._return_type

    @return_type.setter
    def return_type(self, value):
        self._return_type = value

    @property
    def formals(self):
        return self._formals

    def create_formals(self, number_of_formals):
        for i in range(1, number_of_formals):
            identifier = Identifier('formal_{}'.format(i))
            self._formals.append(ParameterDecl(IntegerType(), identifier))

    @property
    def main(self):
        return self._main

    @main.setter
    def main(self, value):
        self._main = value

    @property
    def called(self):
        return self._called

    @called.setter
    def called(self, value):
        self._called = value

    @property
    def annotations(self):
        return self._annotations

    def add_annotation(self, annotation):
        self._annotations.append(annotation)

    @property
    def subprograms(self):
        return self._subprograms

    def add_subprogram(self, subprogram):
        self._subprograms.append(subprogram)

    def is_fake(self):
        return self._name is None

    @property
    def cfg(self):
        return self._cfg

    @cfg.setter
    def cfg(self, cfg):
        self._cfg = cfg


class SubprogramDecl:
    def __init__(self, subprogram: Subprogram):
        self._name = subprogram.name
        self._return_type = subprogram.return_type
        self._formals = subprogram.formals

    def unparse(self):
        return '{} {} ({});{}'.format(self._return_type.unparse(),
                                      self._name.unparse(),
                                      ', '.join(f.unparse() for f in self._formals),
                                      Unparser.newlines())


class InstrumentationProfile(enum.Enum):
    DEFAULT = 0
    MANUAL = 1
    TIME_FULL = 2
    TIME_FUNCTIONS = 3
    TIME_START_OF_SCOPES = 4
    COV_178_DAL_A = 5
    COV_178_DAL_B = 6
    COV_178_DAL_C = 7
    COV_MCDC = 8
    COV_STATEMENTS = 9
    COV_CALLS = 10
    COV_BRANCHES = 11
    COV_DECISIONS = 12
    COV_FUNCTIONS = 13
    COV_FUNCTION_EXITS = 14
    COV_26262_HR_ASIL_A = 15
    COV_26262_HR_ASIL_B = 16
    COV_26262_HR_ASIL_C = 17

    @classmethod
    def timing_profiles(cls):
        return [cls.DEFAULT,
                cls.MANUAL,
                cls.TIME_FULL,
                cls.TIME_FUNCTIONS,
                cls.TIME_START_OF_SCOPES]

    @classmethod
    def coverage_profiles(cls):
        return [cls.DEFAULT,
                cls.MANUAL,
                cls.COV_178_DAL_A,
                cls.COV_178_DAL_B,
                cls.COV_178_DAL_C,
                cls.COV_MCDC,
                cls.COV_STATEMENTS,
                cls.COV_CALLS,
                cls.COV_BRANCHES,
                cls.COV_DECISIONS,
                cls.COV_FUNCTIONS,
                cls.COV_FUNCTION_EXITS,
                cls.COV_26262_HR_ASIL_A,
                cls.COV_26262_HR_ASIL_B,
                cls.COV_26262_HR_ASIL_C]


class InstrumentAnnotation:
    def __init__(self, language: Language, profile: InstrumentationProfile, subprogram: Subprogram, on: bool = True):
        self._language = language
        self._profile = profile
        self._subprogram = subprogram
        self._on = on

    def unparse(self):
        if self._subprogram.is_fake() or (self._language == Language.Ada and self._subprogram.main):
            return '#pragma RVS default_instrument("{}", "{}");{}'.format(True, self._profile.name, Unparser.newlines())
        elif self._profile != InstrumentationProfile.DEFAULT:
            return '#pragma RVS instrument("{}", "{}", "{}");{}'.format(self._subprogram.name.unparse(),
                                                                        self._on,
                                                                        self._profile.name,
                                                                        Unparser.newlines())
        else:
            return '#pragma RVS instrument("{}", "{}");{}'.format(self._subprogram.name.unparse(),
                                                                  self._on,
                                                                  Unparser.newlines())



def create_call_graph(call_depth:      int,
                      main_subprogram: Subprogram,
                      subprograms:     typing.List[Subprogram]):
    call_graph = DirectedGraph()
    root = None
    for subprogram in subprograms:
        v = Vertex(Vertex.get_vertex_id())
        v.subprogram = subprogram
        call_graph.add_vertex(v)
        if subprogram == main_subprogram:
            root = v

    vertex_to_level = {v: 0 for v in call_graph}
    parent = root
    for v in call_graph:
        if v != root:
            new_level = vertex_to_level[parent] + 1
            if new_level <= call_depth:
                call_graph.add_edge(Edge(parent, v))
                vertex_to_level[v] = new_level
            else:
                # The height of the tree now exceeds the maximum depth, so backtrack to an arbitrary proper ancestor.
                ancestor = parent
                while True:
                    (e,) = call_graph.predecessors(ancestor)
                    ancestor = e.predecessor()
                    if go_ahead() or ancestor == root:
                        break
                parent = ancestor
                call_graph.add_edge(Edge(parent, v))
                vertex_to_level[v] = vertex_to_level[parent] + 1
            parent = v

    return call_graph


def create_subprogram_artifacts(language:               Language,
                                analysis:               Analysis,
                                default_profile:        InstrumentationProfile,
                                formal_parameter_limit: int,
                                expression_depth:       int,
                                ignore_operators:       typing.List[ArithmeticOperator],
                                block_length:           int,
                                basic_block_limit:      int,
                                number_of_loops:        int,
                                nesting_depth:          int,
                                top_level_subprogram:   Subprogram,
                                subprograms:            typing.List[Subprogram],
                                call_graph:             DirectedGraph):
    annotation = InstrumentAnnotation(language, default_profile, top_level_subprogram)
    top_level_subprogram.add_annotation(annotation)

    for subprogram in subprograms:
        if go_ahead(0.1):
            # Override the default instrumentation profile
            if analysis == Analysis.TIMING:
                profile = random.choice(InstrumentationProfile.timing_profiles())
            else:
                profile = random.choice(InstrumentationProfile.coverage_profiles())
            annotation = InstrumentAnnotation(language, profile, subprogram, go_ahead())
            subprogram.add_annotation(annotation)

        subprogram.return_type = IntegerType()
        if not subprogram.main:
            subprogram.create_formals(random.randint(1, formal_parameter_limit))
        lnt = create_loop_hierarchy(basic_block_limit, number_of_loops, nesting_depth)
        subprogram.cfg = generate_control_flow_graph(lnt)
        subprogram.generate_body(expression_depth,
                                 ignore_operators,
                                 block_length,
                                 top_level_subprogram.subprograms,
                                 call_graph)
        top_level_subprogram.add_subprogram(subprogram)


def create_empty_subprograms(language:              Language,
                             number_of_subprograms: int):
    subprograms = []
    main_subprogram = None
    for subprogram_id in range(1, number_of_subprograms + 1):
        if subprogram_id == number_of_subprograms:
            main_subprogram = Subprogram(language, Identifier('main'))
            main_subprogram.main = True
            subprograms.append(main_subprogram)
        else:
            subprogram = Subprogram(language, Identifier('subprogram_{}'.format(subprogram_id)))
            subprograms.append(subprogram)

    if language == Language.C:
        # Create a fake subprogram, as we cannot nest subprograms in C.
        top_level_subprogram = Subprogram(language, None)
    else:
        # This is the subprogram where execution begins in Ada.
        top_level_subprogram = main_subprogram

    return top_level_subprogram, main_subprogram, subprograms


def is_arithmetic_and_binary(a_class):
    return issubclass(a_class, ArithmeticOperator) and issubclass(a_class, BinaryOperator)


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Generate a program')

    parser.add_argument('--filename',
                        help='write into this file',
                        required=True)

    parser.add_argument('--language',
                        choices=[language.name for language in Language],
                        help='output language',
                        required=True)

    parser.add_argument('--analysis',
                        choices=[analysis.name.lower() for analysis in Analysis],
                        help='the type of program analysis',
                        required=True)

    parser.add_argument('--subprograms',
                        type=int,
                        help='number of subprograms',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--default-instrumentation',
                        choices=[profile.name.lower() for profile in InstrumentationProfile
                                 if profile != InstrumentationProfile.DEFAULT],
                        help='choose the default instrumentation profile annotation',
                        default=None)

    parser.add_argument('--formal-parameter-limit',
                        type=int,
                        help='maximum number of formal parameters in a subprogram',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--expression-depth',
                        type=int,
                        help='maximum number of operations in an expression',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--ignore-operators',
                        choices=[a_class.__name__ for a_class in get_operators(is_arithmetic_and_binary)],
                        help='do not use these binary arithmetic operators in expressions',
                        nargs='+',
                        default=[])

    parser.add_argument('--block-length',
                        type=int,
                        help='maximum number of statements in a basic block (excluding nested basic blocks)',
                        default=5,
                        metavar='<INT>')

    parser.add_argument('--basic-blocks',
                        type=int,
                        help='maximum number of basic blocks in a subprograms',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--loops',
                        type=int,
                        help='maximum number of loops in a subprogram',
                        metavar='<INT>',
                        default=0)

    parser.add_argument('--loop-depth',
                        type=int,
                        help='maximum nesting depth of loops',
                        metavar='<INT>',
                        default=1)

    parser.add_argument('--call-depth',
                        type=int,
                        help='maximum nesting depth of calls',
                        metavar='<INT>',
                        default=1)

    args = vars(parser.parse_args())
    return args


def standardise_arguments(args):
    # The raw argument is a list, but we really want a set.
    ignore_operators = set(args['ignore_operators'])
    # Create an operator class for each operator name.
    args['ignore_operators'] = [getattr(sys.modules[__name__], op) for op in ignore_operators]

    args['language'] = Language[args['language'].upper()]
    args['analysis'] = Analysis[args['analysis'].upper()]

    if args['default_instrumentation']:
        args['default_instrumentation'] = InstrumentationProfile[args['default_instrumentation'].upper()]
    else:
        # A default instrumentation profile is required by RVS.
        if args['analysis'] == Analysis.TIMING:
            choices = [profile for profile in InstrumentationProfile.timing_profiles()
                       if profile != InstrumentationProfile.DEFAULT]
            args['default_instrumentation'] = random.choice(choices)
        else:
            choices = [profile for profile in InstrumentationProfile.coverage_profiles()
                       if profile != InstrumentationProfile.DEFAULT]
            args['default_instrumentation'] = random.choice(choices)


def error_message(*args):
    print('ERROR:', *args, file=sys.stderr, flush=True)
    sys.exit(1)


def check_arguments(args):
    # Error handling.
    if args['basic_blocks'] < args['loops'] * 2:
        error_message('The number of basic blocks in a subprogram must be at least twice the number of loops')

    if len(args['ignore_operators']) == len(get_operators(is_arithmetic_and_binary)):
        error_message('All binary arithmetic operators are ignored: The program generator needs at least one to create '
                      'expressions')

    if args['default_instrumentation']:
        analysis = args['analysis']
        default = args['default_instrumentation']
        if ((analysis == Analysis.COVERAGE and default not in InstrumentationProfile.coverage_profiles()) or
                (analysis == Analysis.TIMING and default not in InstrumentationProfile.timing_profiles())):
            error_message("Invalid default instrumentation profile '{}' for analysis '{}'".format(default.name,
                                                                                                  analysis.name))

    return args


if __name__ == '__main__':
    args = parse_the_command_line()
    standardise_arguments(args)
    check_arguments(args)

    top_level_subprogram, main_subprogram, subprograms = create_empty_subprograms(args['language'],
                                                                                  args['subprograms'])

    call_graph = create_call_graph(args['call_depth'],
                                   main_subprogram,
                                   subprograms)

    create_subprogram_artifacts(args['language'],
                                args['analysis'],
                                args['default_instrumentation'],
                                args['formal_parameter_limit'],
                                args['expression_depth'],
                                args['ignore_operators'],
                                args['block_length'],
                                args['basic_blocks'],
                                args['loops'],
                                args['loop_depth'],
                                top_level_subprogram,
                                subprograms,
                                call_graph)

    with open(args['filename'], 'w') as wd:
        wd.write(top_level_subprogram.unparse())
