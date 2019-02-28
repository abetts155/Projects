import argparse
import enum
import random
import sys


from graphs import (edges, vertices, graphs)

file_annotations = []
subprograms = []
annotations = {}


def decide():
    return bool(random.getrandbits(1))


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
            for i in range(len(subprogram.arguments)):
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
    def __init__(self, name, return_type, number_of_formals):
        self._name = name
        self._return_type = return_type
        self._arguments = []
        for i in range(1, number_of_formals):
            identifier = Identifier('{}_{}'.format(self._name.unparse(), i))
            self._arguments.append(ParameterDecl(IntegerType(), identifier))
        self._main = False
        self._cfg = None
        self._called = False

    def unparse(self):
        def blanks(length):
            return ' ' * length

        signature_text = '{} {} ({})'.format(self._return_type.unparse(),
                                             self.name.unparse(),
                                             ', '.join(a.unparse() for a in self._arguments))
        body_text = '{'
        body_text += '\n'
        dfs = graphs.DepthFirstSearch(self._cfg, self._cfg.entry)
        indent = 2
        for v in reversed(dfs.post_order()):
            for stmt in v.statements:
                body_text += '{}{}'.format(blanks(indent), stmt.unparse())
                if stmt != v.statements[-1]:
                    body_text += '\n'
        body_text += '\n'
        body_text += '}'
        return signature_text + '\n' + body_text + '\n'

    def generate_control_flow(self):
        self._cfg = graphs.ControlFlowGraph(None, self.name)
        v = vertices.Vertex(vertices.Vertex.get_vertex_id())
        self._cfg.add_vertex(v)
        self._cfg.entry = v

    def generate_body(self, expression_depth, block_length):
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

        dfs = graphs.DepthFirstSearch(self._cfg, self._cfg.entry)
        for v in reversed(dfs.post_order()):
            v.statements = []
            if len(self._cfg.successors(v)) == 0:
                if decide():
                    for i in range(1, block_length):
                        decl_stmt = VariableDecl(IntegerType(), Identifier('i{}_{}'.format(v, i)))
                        decl_stmt.initialiser = random_expr(random.randint(1, expression_depth),
                                                            subprogram_subset(),
                                                            self.arguments)
                        v.statements.append(decl_stmt)

                if self.main:
                    return_stmt = ReturnStmt(false_expr())
                    v.statements.append(return_stmt)
                else:
                    return_stmt = ReturnStmt(random_expr(random.randint(1, expression_depth),
                                                         subprogram_subset(),
                                                         self.arguments))
                    v.statements.append(return_stmt)

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
    def name(self):
        return self._name

    @property
    def return_type(self):
        return self._return_type

    @property
    def arguments(self):
        return self._arguments


class SubprogramDecl:
    def __init__(self, subprogram: Subprogram):
        self._name = subprogram.name
        self._return_type = subprogram.return_type
        self._arguments = subprogram.arguments

    def unparse(self):
        return '{} {} ({});'.format(self._return_type.unparse(),
                                    self._name.unparse(),
                                    ', '.join(a.unparse() for a in self._arguments))


class InstrumentationProfile(enum.Enum):
    DEFAULT = 0
    TIME_FULL = 1
    TIME_FUNCTIONS = 2


class DefaultInstrumentAnnotation:
    def __init__(self, profile: InstrumentationProfile, on):
        self._profile = profile
        self._on = on

    def unparse(self):
        # Ignore ON/OFF switch for now
        return '#pragma RVS default_instrument("{}", "{}");'.format("TRUE", self._profile.name)


class InstrumentAnnotation:
    def __init__(self, subprogram: Subprogram, on: bool):
        self._subprogram = subprogram
        self._on = on

    def unparse(self):
        return '#pragma RVS instrument("{}", "{}");'.format(self._subprogram.name.unparse(),
                                                            "TRUE" if self._on else "FALSE")


def generate(required_number, formal_parameter_limit, expression_depth, block_length):
    annotation = DefaultInstrumentAnnotation(InstrumentationProfile.TIME_FULL, decide())
    file_annotations.append(annotation)
    for subprogram_id in range(1, required_number+1):
        if subprogram_id == required_number:
            subprogram = Subprogram(Identifier('main'),
                                    IntegerType(),
                                    0)
            subprogram.main = True
        else:
            name = 'f{}'.format(subprogram_id)
            subprogram = Subprogram(Identifier(name),
                                    IntegerType(),
                                    random.randint(1, formal_parameter_limit))
            if decide():
                # Override the default instrumentation profile
                annotation = InstrumentAnnotation(subprogram, decide())
                annotations.setdefault(subprogram, []).append(annotation)
        subprogram.generate_control_flow()
        subprogram.generate_body(expression_depth, block_length)
        subprograms.append(subprogram)


def write(filename, repeat_annotations):
    with open(filename, 'w') as wd:
        for a in file_annotations:
            wd.write(a.unparse())
            wd.write('\n')

        wd.write('\n')
        for s in subprograms:
            decl = SubprogramDecl(s)
            wd.write(decl.unparse())
            wd.write('\n')

        wd.write('\n')
        for s in subprograms:
            if s in annotations:
                for a in annotations[s]:
                    for i in range(0, repeat_annotations):
                        wd.write(a.unparse())
                        wd.write('\n')
            wd.write(s.unparse())
            wd.write('\n')

        wd.write('\n')


def main(**kwargs):
    generate(kwargs['subprograms'], kwargs['formal_parameter_limit'], kwargs['expression_depth'],
             kwargs['block_length'])
    write(kwargs['filename'], kwargs['repeat_annotations'])


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Generate a program')

    parser.add_argument('--filename',
                        help='write into this file',
                        required=True)

    parser.add_argument('--subprograms',
                        type=int,
                        help='number of subprograms',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--formal-parameter-limit',
                        type=int,
                        help='maximum number of formal parameters in a subprogram',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--repeat-annotations',
                        type=int,
                        help='repeat an annotation this many times',
                        default=1,
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
