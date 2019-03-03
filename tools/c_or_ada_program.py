import sys

import argparse
import enum
import random


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


def decide():
    return bool(random.getrandbits(1))


def blanks(length):
    return ' ' * length


language = None
C_language = 'C'
Ada_language = 'Ada'


class RelationalOperator(enum.Enum):
    EQUALITY = '=='
    INEQUALITY = '!='
    LESS_THAN = '<'
    GREATER_THAN = '>'
    LESS_THAN_OR_EQUAL = '<='
    GREATER_THAN_OR_EQUAL = '>='


class ArithmeticOperator(enum.Enum):
    PLUS = '+'
    MINUS = '-'
    MULTIPLY = '*'
    DIVIDE = '/'


class LogicalOperator(enum.Enum):
    NEGATION = '!'


class BitwiseOperator(enum.Enum):
    NEGATION = '~'


class Identifier:
    def __init__(self, name):
        self._name = name

    def unparse(self):
        return self._name


class IntegerType:
    def unparse(self):
        return 'int'


class Operator:
    def __init__(self, op: RelationalOperator or ArithmeticOperator):
        self._op = op

    def unparse(self):
        return '{}'.format(self._op.value)


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
        return '{};'.format(super().unparse())


class Literal:
    def __init__(self, value):
        self._value = value

    def unparse(self):
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
    def __init__(self, op, child):
        self._op = op
        self._child = child

    def unparse(self):
        return '({}{})'.format(self._op.unparse(), self._child.unparse())


class BinaryExpr:
    def __init__(self, left, op, right):
        self._left = left
        self._op = op
        self._right = right

    def unparse(self):
        return '({}{}{})'.format(self._left.unparse(), self._op.unparse(), self._right.unparse())


def random_expr(number_of_operations, subprograms, variables):
    def random_arithmetic_op():
        choices = [ArithmeticOperator.PLUS,
                   ArithmeticOperator.MINUS,
                   ArithmeticOperator.MULTIPLY,
                   ArithmeticOperator.DIVIDE]
        op = choices[random.randint(0, len(choices) - 1)]
        return Operator(op)

    def random_unary_op():
        choices = [ArithmeticOperator.MINUS,
                   LogicalOperator.NEGATION,
                   BitwiseOperator.NEGATION]
        op = choices[random.randint(0, len(choices) - 1)]
        return Operator(op)

    def random_leaf():
        if variables and decide():
            variable = variables[random.randint(0, len(variables) - 1)]
            return VariableReference(variable.name)
        elif subprograms and decide():
            subprogram = subprograms[random.randint(0, len(subprograms) - 1)]
            subprogram.called = True
            arguments = []
            for i in range(len(subprogram.formals)):
                if not variables or decide():
                    arguments.append(Literal(random.randint(1, 10)))
                else:
                    variable = variables[random.randint(0, len(variables) - 1)]
                    arguments.append(VariableReference(variable.name))
            return CallExpr(subprogram.name, arguments)
        else:
            return Literal(random.randint(1, 10))

    root_level = 1
    height = number_of_operations
    subtrees = {}
    while height >= root_level:
        if height == number_of_operations:
            root = BinaryExpr(random_leaf(),
                              random_arithmetic_op(),
                              random_leaf())
        else:
            if decide():
                root = UnaryExpr(random_unary_op(),
                                 subtrees[height + 1])
            else:
                if decide():
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
    return BinaryExpr(Literal(1), Operator(RelationalOperator.EQUALITY), Literal(1))


def false_expr():
    return BinaryExpr(Literal(1), Operator(RelationalOperator.EQUALITY), Literal(0))


class ReturnStmt:
    def __init__(self, expr):
        self._expr = expr

    def unparse(self):
        return 'return {};'.format(self._expr.unparse())


class Subprogram:
    def __init__(self, name):
        self._name = name
        self._return_type = None
        self._formals = []
        self._main = False
        self._cfg = None
        self._called = False
        self._annotations = []
        self._subprograms = []

    def unparse(self):
        signature_text = '{} {} ({})'.format(self._return_type.unparse(),
                                             self.name.unparse(),
                                             ', '.join(f.unparse() for f in self._formals))
        body_text = '{'
        body_text += '\n'
        dfs = DepthFirstSearch(self._cfg, self._cfg.entry)
        indent = 2
        for v in reversed(dfs.post_order()):
            body_text += blanks(indent)
            body_text += '{'
            body_text += '\n'
            indent += 2
            for stmt in v.statements:
                body_text += '{}{}'.format(blanks(indent), stmt.unparse())
                body_text += '\n'
            indent -= 2
            body_text += blanks(indent)
            body_text += '}'
        body_text += '\n'
        body_text += '}'
        return signature_text + '\n' + body_text + '\n'

    def generate_control_flow(self):
        self._cfg = FlowGraph()
        v = Vertex(Vertex.get_vertex_id())
        self._cfg.add_vertex(v)
        self._cfg.entry = v

    def generate_body(self, expression_depth, block_length, subprograms):
        def subprogram_subset():
            if self.main:
                # If any call expressions are generated, try to use as yet uncalled subprograms.
                subset = [s for s in subprograms if not s.called]
                if not subset:
                    return subprograms
                else:
                    return subset
            else:
                return subprograms

        dfs = DepthFirstSearch(self._cfg, self._cfg.entry)
        for v in reversed(dfs.post_order()):
            v.statements = []
            if len(self._cfg.successors(v)) == 0:
                if decide():
                    for i in range(1, block_length):
                        decl_stmt = VariableDecl(IntegerType(), Identifier('i{}_{}'.format(v, i)))
                        decl_stmt.initialiser = random_expr(random.randint(1, expression_depth),
                                                            subprogram_subset(),
                                                            self._formals)
                        v.statements.append(decl_stmt)

                if self.main:
                    return_stmt = ReturnStmt(false_expr())
                    v.statements.append(return_stmt)
                else:
                    return_stmt = ReturnStmt(random_expr(random.randint(1, expression_depth),
                                                         subprogram_subset(),
                                                         self._formals))
                    v.statements.append(return_stmt)

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
            identifier = Identifier('{}_{}'.format(self._name.unparse(), i))
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


class SubprogramDecl:
    def __init__(self, subprogram: Subprogram):
        self._name = subprogram.name
        self._return_type = subprogram.return_type
        self._formals = subprogram.formals

    def unparse(self):
        return '{} {} ({});'.format(self._return_type.unparse(),
                                    self._name.unparse(),
                                    ', '.join(f.unparse() for f in self._formals))


class Analysis(enum.Enum):
    TIMING = 0
    COVERAGE = 1


class InstrumentationProfile(enum.Enum):
    DEFAULT = 0
    TIME_FULL = 1
    TIME_FUNCTIONS = 2
    COV_DAL_A = 3
    COV_DAL_B = 4
    COV_DAL_C = 5

    @classmethod
    def timing_profiles(cls):
        return [cls.DEFAULT, cls.TIME_FULL, cls.TIME_FUNCTIONS]

    @classmethod
    def coverage_profiles(cls):
        return [cls.DEFAULT, cls.COV_DAL_A, cls.COV_DAL_B, cls.COV_DAL_C]


class InstrumentAnnotation:
    def __init__(self, profile: InstrumentationProfile, subprogram: Subprogram, on: bool = True):
        self._profile = profile
        self._subprogram = subprogram
        self._on = on

    def unparse(self):
        if self._subprogram.is_fake():
            return '#pragma RVS default_instrument("{}", "{}");'.format(True, self._profile.name)
        elif self._profile != InstrumentationProfile.DEFAULT:
            return '#pragma RVS instrument("{}", "{}", "{}");'.format(self._subprogram.name.unparse(),
                                                                      self._on,
                                                                      self._profile.name)
        else:
            return '#pragma RVS instrument("{}", "{}");'.format(self._subprogram.name.unparse(),
                                                                self._on)


def generate(analysis, required_number, formal_parameter_limit, expression_depth, block_length):
    if language == C_language:
        top_level_subprogram = Subprogram(None)
    else:
        assert False

    annotation = InstrumentAnnotation(InstrumentationProfile.TIME_FULL, top_level_subprogram)
    top_level_subprogram.add_annotation(annotation)

    for subprogram_id in range(1, required_number+1):
        if subprogram_id == required_number and language == C_language:
            subprogram = Subprogram(Identifier('main'))
            subprogram.return_type = IntegerType()
            subprogram.main = True
        else:
            name = 'func{}'.format(subprogram_id)
            subprogram = Subprogram(Identifier(name))
            subprogram.return_type = IntegerType()
            subprogram.create_formals(random.randint(1, formal_parameter_limit))

            if decide():
                # Override the default instrumentation profile
                choice = random.randint(0, len(InstrumentationProfile) - 1)
                annotation = InstrumentAnnotation(InstrumentationProfile(choice), subprogram, decide())
                subprogram.add_annotation(annotation)

        subprogram.generate_control_flow()
        subprogram.generate_body(expression_depth, block_length, top_level_subprogram.subprograms)
        top_level_subprogram.add_subprogram(subprogram)

    return top_level_subprogram


def write(filename, repeat_annotations, top_level_subprogram: Subprogram):
    with open(filename, 'w') as wd:
        for a in top_level_subprogram.annotations:
            wd.write(a.unparse())
            wd.write('\n')

        wd.write('\n')
        for s in top_level_subprogram.subprograms:
            for i in range(random.randint(1, 5)):
                decl = SubprogramDecl(s)
                wd.write(decl.unparse())
                wd.write('\n')

        wd.write('\n')
        for s in top_level_subprogram.subprograms:
            for a in s.annotations:
                for i in range(0, repeat_annotations):
                    wd.write(a.unparse())
                    wd.write('\n')
            wd.write(s.unparse())
            wd.write('\n')

        wd.write('\n')


def main(**kwargs):
    global language
    language = kwargs['language']
    top_level_subprogram = generate(kwargs['analysis'],
                                    kwargs['subprograms'],
                                    kwargs['formal_parameter_limit'],
                                    kwargs['expression_depth'],
                                    kwargs['block_length'])
    write(kwargs['filename'], kwargs['repeat_annotations'], top_level_subprogram)


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Generate a program')

    parser.add_argument('--filename',
                        help='write into this file',
                        required=True)

    parser.add_argument('--language',
                        choices=[C_language, Ada_language],
                        help='output language',
                        required=True)

    parser.add_argument('--analysis',
                        choices=[analysis.name for analysis in Analysis],
                        help='the type of program analysis')

    parser.add_argument('--subprograms',
                        type=int,
                        help='number of subprograms',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--repeat-annotations',
                        type=int,
                        help='repeat an annotation this many times',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--default-instrumentation',
                        choices=[profile.name for profile in InstrumentationProfile],
                        help='choose the default instrumentation profile annotation',
                        default=InstrumentationProfile.TIME_FULL.name)

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

    parser.add_argument('--block-length',
                        type=int,
                        help='maximum number of statements in a block',
                        default=5,
                        metavar='<INT>')

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
