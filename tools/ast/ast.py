import enum
import inspect
import random
import sys
import typing

from graphs import graph
from miscellaneous import helpful


class Language(enum.Enum):
    C = 0
    ADA = 1


class Punctuation(enum.Enum):
    COMMA = ','
    SPACE = ' '
    OPEN_PARENTHESIS = '('
    CLOSE_PARENTHESIS = ')'
    OPEN_BRACE = '{'
    CLOSE_BRACE = '}'
    COLON = ':'
    SEMI_COLON = ';'
    HASH = '#'
    DOUBLE_QUOTES = '"'
    SINGLE_QUOTES = "'"


class Keywords(enum.Enum):
    IF = 0
    ELSE = 1
    SWITCH = 2
    CASE = 3
    RETURN = 4
    BREAK = 5
    DEFAULT = 6
    FOR = 7
    DO = 8
    WHILE = 9
    CONTINUE = 10
    PRAGMA = 11
    THEN = 12
    FUNCTION = 13
    PROCEDURE = 14
    IS = 15
    BEGIN = 16
    END = 17


def open_block(language: Language):
    if language == Language.C:
        return Punctuation.OPEN_BRACE.value
    else:
        return Keywords.BEGIN.name


def close_block(language: Language):
    if language == Language.C:
        return Punctuation.CLOSE_BRACE.value
    else:
        return Keywords.END.name + Punctuation.SEMI_COLON.value


def assignment(language: Language):
    if language == Language.C:
        return '='
    else:
        return ':='


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
    def unparse(self, stream: typing.TextIO):
        stream.write('==')


class Inequality(RelationalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('!=')


class LessThan(RelationalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('<')


class LessThanEqual(RelationalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('<=')


class GreaterThan(RelationalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('>')


class GreaterThanEqual(RelationalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('>=')


class Add(ArithmeticOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('+')


class Subtract(ArithmeticOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('-')


class Multiply(ArithmeticOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('*')


class Divide(ArithmeticOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('/')


class Modulo(ArithmeticOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('%')


class Plus(ArithmeticOperator, UnaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('+')


class Minus(ArithmeticOperator, UnaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('-')


class Negation(LogicalOperator, UnaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('!')


class And(LogicalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('&&')


class Or(LogicalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('||')


class Xor(LogicalOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('^')


class Complement(BitwiseOperator, UnaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('~')


class ShiftLeft(BitwiseOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('<<')


class ShiftRight(BitwiseOperator, BinaryOperator):
    def unparse(self, stream: typing.TextIO):
        stream.write('>>')


class Identifier:
    def __init__(self, name):
        self._name = name

    def unparse(self, stream: typing.TextIO):
        stream.write(self._name)


class IntegerType:
    def __init__(self, language: Language):
        self._language = language

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            stream.write('int')
        else:
            stream.write('Integer')


class VoidType:
    def unparse(self, stream: typing.TextIO):
        stream.write('void')


class VariableDecl:
    def __init__(self, language: Language, base_type, name):
        self._language = language
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

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            self._base_type.unparse(stream)
            stream.write(Punctuation.SPACE.value)
            self._name.unparse(stream)
        else:
            self._name.unparse(stream)
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.COLON.value)
            stream.write(Punctuation.SPACE.value)
            self._base_type.unparse(stream)

        if self.initialiser:
            stream.write(Punctuation.SPACE.value)
            stream.write(assignment(self._language))
            stream.write(Punctuation.SPACE.value)
            self._initialiser.unparse(stream)


class Literal:
    def __init__(self, value):
        self._value = value

    def unparse(self, stream: typing.TextIO):
        if self._value < 0:
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            stream.write(str(self._value))
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)
        else:
            stream.write(str(self._value))


class VariableRef:
    def __init__(self, variable_declaration: VariableDecl):
        self._name = variable_declaration.name

    def unparse(self, stream: typing.TextIO):
        self._name.unparse(stream)


class CallExpr:
    def __init__(self, name, arguments):
        self._name = name
        self._arguments = arguments

    def unparse(self, stream: typing.TextIO):
        self._name.unparse(stream)
        stream.write('(')
        for actual in self._arguments:
            actual.unparse(stream)
            if actual != self._arguments[-1]:
                stream.write(Punctuation.COMMA.value)
                stream.write(Punctuation.SPACE.value)
        stream.write(')')


class UnaryExpr:
    def __init__(self, op: UnaryOperator, child):
        self._op = op
        self._child = child

    def unparse(self, stream: typing.TextIO):
        stream.write(Punctuation.OPEN_PARENTHESIS.value)
        self._op.unparse(stream)
        self._child.unparse(stream)
        stream.write(Punctuation.CLOSE_PARENTHESIS.value)


class BinaryExpr:
    def __init__(self, left, op: BinaryOperator, right):
        self._left = left
        self._op = op
        self._right = right

    def unparse(self, stream: typing.TextIO):
        stream.write(Punctuation.OPEN_PARENTHESIS.value)
        self._left.unparse(stream)
        self._op.unparse(stream)
        self._right.unparse(stream)
        stream.write(Punctuation.CLOSE_PARENTHESIS.value)


class Stmt:
    def __init__(self, language: Language):
        self._language = language
        self._label = None

    @property
    def label(self):
        return self._label

    @label.setter
    def label(self, value: Identifier):
        self._label = value


class IfStmt(Stmt):
    def __init__(self, language: Language, expr):
        Stmt.__init__(self, language)
        self._expr = expr

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            stream.write(Keywords.IF.name.lower())
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            self._expr.unparse(stream)
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)
        else:
            stream.write(Keywords.IF.name.lower())
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            self._expr.unparse(stream)
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)
            stream.write(Punctuation.SPACE.value)
            stream.write(Keywords.THEN.name.lower())


class ForStmt(Stmt):
    def __init__(self, language: Language, initialiser, guard, updater):
        Stmt.__init__(self, language)
        self._initialiser = initialiser
        self._guard = guard
        self._updater = updater

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            stream.write(Keywords.FOR.name.lower())
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            self._initialiser.unparse(stream)
            stream.write(Punctuation.SEMI_COLON.value)
            stream.write(Punctuation.SPACE.value)
            self._guard.unparse(stream)
            stream.write(Punctuation.SEMI_COLON.value)
            stream.write(Punctuation.SPACE.value)
            self._updater.unparse(stream)
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)


class SwitchStmt(Stmt):
    def __init__(self, language: Language, expr):
        Stmt.__init__(self, language)
        self._expr = expr

    def unparse(self, stream: typing.TextIO):
        stream.write(Keywords.SWITCH.name.lower())
        stream.write(Punctuation.SPACE.value)
        stream.write(Punctuation.OPEN_PARENTHESIS.value)
        self._expr.unparse(stream)
        stream.write(Punctuation.CLOSE_PARENTHESIS.value)


class ReturnStmt(Stmt):
    def __init__(self, language: Language, expr):
        Stmt.__init__(self, language)
        self._expr = expr

    def unparse(self, stream: typing.TextIO):
        stream.write(Keywords.RETURN.name.lower())
        if self._expr:
            stream.write(Punctuation.SPACE.value)
            self._expr.unparse(stream)


class CallStmt(Stmt):
    def __init__(self, language: Language, expr):
        Stmt.__init__(self, language)
        self._expr = expr

    def unparse(self, stream: typing.TextIO):
        self._expr.unparse(stream)


class AssignStmt(Stmt):
    def __init__(self, language: Language, lhs, rhs):
        Stmt.__init__(self, language)
        self._lhs = lhs
        self._rhs = rhs

    def unparse(self, stream: typing.TextIO):
        self._lhs.unparse(stream)
        stream.write(Punctuation.SPACE.value)
        stream.write(assignment(self._language))
        stream.write(Punctuation.SPACE.value)
        self._rhs.unparse(stream)


class BreakStmt(Stmt):
    def unparse(self, stream: typing.TextIO):
        stream.write(Keywords.BREAK.name.lower())


class ContinueStmt(Stmt):
    def unparse(self, stream: typing.TextIO):
        stream.write(Keywords.CONTINUE.name.lower())


class NullStmt(Stmt):
    def unparse(self, stream: typing.TextIO):
        pass


class SubprogramDecl:
    def __init__(self, language, name: Identifier):
        self._language = language
        self._name = name
        self._return_type = None
        self._formals = []
        self._called = False

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
            self._formals.append(VariableDecl(self._language, IntegerType(self._language), identifier))

    @property
    def called(self):
        return self._called

    @called.setter
    def called(self, value):
        self._called = value

    def is_function(self):
        return not isinstance(self._return_type, VoidType)

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            self._return_type.unparse(stream)
            stream.write(Punctuation.SPACE.value)
            self._name.unparse(stream)
        else:
            if self.is_function():
                stream.write(Keywords.FUNCTION.name.lower())
            else:
                stream.write(Keywords.PROCEDURE.name.lower())
            stream.write(Punctuation.SPACE.value)
            self._name.unparse(stream)

        stream.write(Punctuation.SPACE.value)
        stream.write(Punctuation.OPEN_PARENTHESIS.value)
        for formal in self._formals:
            formal.unparse(stream)
            if formal != self._formals[-1]:
                stream.write(Punctuation.COMMA.value)
        stream.write(Punctuation.CLOSE_PARENTHESIS.value)


class Subprogram:
    def __init__(self, language: Language, subprogram_declaration: SubprogramDecl):
        self._language = language
        self._subprogram_declaration = subprogram_declaration
        self._main = False
        self._cfg = None
        self._annotations = []
        self._subprograms = []

    @property
    def language(self):
        return self._language

    @property
    def subprogram_declaration(self):
        return self._subprogram_declaration

    @property
    def main(self):
        return self._main

    @main.setter
    def main(self, value):
        self._main = value

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

    @property
    def fake(self):
        return self.subprogram_declaration.name is None

    @property
    def cfg(self):
        return self._cfg

    @cfg.setter
    def cfg(self, cfg):
        self._cfg = cfg

    def _subprogram_candidates(self,
                               call_graph: graph.DirectedGraph,
                               functions:  bool):
        (call_vertex,) = [v for v in call_graph if v.subprogram_declaration == self.subprogram_declaration]
        candidates = [e.successor().subprogram_declaration for e in call_graph.successors(call_vertex)]
        candidates = [subprogram_declaration for subprogram_declaration in candidates
                      if (functions and subprogram_declaration.is_function())
                      or (not functions and not subprogram_declaration.is_function())]
        if self.main:
            # Prioritise as yet uncalled subprograms.
            subset = [subprogram_declaration for subprogram_declaration in candidates
                      if not subprogram_declaration.called]
            if subset:
                candidates = subset
        return candidates

    def _generate_for_statement(self, v: graph.Vertex):
        # We generate a for-loop whose counter increases monotonically so that it terminates provided its
        # loop body terminates.
        loop_counter = VariableDecl(self.language, IntegerType(self.language), Identifier('count_{}'.format(v)))
        lower_bound = random.randint(0, 10)
        upper_bound = random.randint(lower_bound, 1000)
        step = random.randint(lower_bound, upper_bound)
        initialiser = AssignStmt(self.language, loop_counter, Literal(lower_bound))
        guard = BinaryExpr(VariableRef(loop_counter), LessThan(), Literal(upper_bound))
        updater = AssignStmt(self.language,
                             VariableRef(loop_counter),
                             BinaryExpr(VariableRef(loop_counter), Add(), Literal(step)))
        return ForStmt(self.language, initialiser, guard, updater)

    def _add_case_arm_expressions(self, v: graph.Vertex):
        # We will use 16-bit integers for case arms. So pick an upper bound for the random number generator that
        # will not exceed the largest 16-bit integer.
        upper_bound = 2 ** 16 - 1 - len(self._cfg.successors(v))
        # Assume the default arm has not been generated.
        default = False
        for idx, e in enumerate(self._cfg.successors(v), start=random.randint(0, upper_bound)):
            if not default and helpful.go_ahead():
                default = True
                e.expr = None
            else:
                # Use the value of the enumerator to guarantee case arms have unique values.
                e.expr = Literal(idx)

    def _generate_return_statement(self, expression_depth, ignore_operators, call_graph):
        if self.main:
            return ReturnStmt(self.language, false_expr())
        elif self.subprogram_declaration.is_function():
            expr = random_expr(self._language,
                               random.randint(1, expression_depth),
                               ignore_operators,
                               self._subprogram_candidates(call_graph, True),
                               self.subprogram_declaration.formals)
            return ReturnStmt(self.language, expr)
        else:
            return ReturnStmt(self.language, None)

    def _generate_sequence_of_statements(self,
                                         expression_depth: int,
                                         ignore_operators: int,
                                         block_length:     int,
                                         call_graph:       graph.DirectedGraph,
                                         v:                graph.BasicBlock,
                                         control_stack:    typing.List[graph.ControlPoint]):
        # Generate a sequence of statements that are executed unconditionally.
        choices = [VariableDecl]
        if self._subprogram_candidates(call_graph, False):
            choices.append(CallStmt)
        if control_stack[-1] == graph.ControlPoint.LOOP_HEADER:
            loop_choices = [BreakStmt, ContinueStmt]
            # Shuffle so that a break-statement is not preferred over a continue-statement, or vice versa.
            random.shuffle(loop_choices)
            choices.extend(loop_choices)
        if control_stack[-1] == graph.ControlPoint.SWITCH_BRANCH:
            choices.append(BreakStmt)
        choices.append(NullStmt)

        # Choices are in order: the first is preferred to the second, the second to the third,  and so on.
        # Weight these choices so that a is n times as preferable as b, for adjacent choices a, b.
        n = 10
        weights = [n ** x for x in reversed(range(len(choices)))]

        for i in range(random.randint(1, block_length)):
            (stmt_class,) = random.choices(population=choices, weights=weights)
            if stmt_class is VariableDecl:
                stmt = VariableDecl(self.language, IntegerType(self.language), Identifier('i{}_{}'.format(v, i)))
                stmt.initialiser = random_expr(self.language,
                                               random.randint(1, expression_depth),
                                               ignore_operators,
                                               self._subprogram_candidates(call_graph, True),
                                               self.subprogram_declaration.formals)
            elif stmt_class is CallStmt:
                subprogram_declaration = random.choice(self._subprogram_candidates(call_graph, False))
                expr = random_call_expr(subprogram_declaration,
                                        self.subprogram_declaration.formals)
                stmt = CallStmt(self.language, expr)
            else:
                stmt = stmt_class(self.language)
            v.statements.append(stmt)

    def generate_body(self, expression_depth, ignore_operators, block_length, call_graph):
        # Remember which for-loops and switch-statements are in control at each program point so that we can decide
        # on applicable control-flow statements, e.g. break and continue.
        # We start with a non-empty stack to avoid needless checking for the empty case.
        control_stack = [graph.ControlPoint.NONE]

        dfs = graph.DepthFirstSearch(self._cfg, self._cfg.entry)
        v: graph.BasicBlock
        for v in reversed(dfs.post_order()):
            if graph.ControlPoint.SWITCH_MERGE in v.control:
                assert control_stack[-1] == graph.ControlPoint.SWITCH_BRANCH
                control_stack.pop()
            if graph.ControlPoint.LOOP_TAIL in v.control:
                assert control_stack[-1] == graph.ControlPoint.LOOP_HEADER
                control_stack.pop()

            if graph.ControlPoint.LOOP_HEADER in v.control:
                for_stmt = self._generate_for_statement(v)
                v.statements.append(for_stmt)
            elif graph.ControlPoint.IF_BRANCH in v.control:
                expr = random_expr(self._language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   self._subprogram_candidates(call_graph, True),
                                   self.subprogram_declaration.formals)
                if_stmt = IfStmt(self.language, expr)
                v.statements.append(if_stmt)
            elif graph.ControlPoint.SWITCH_BRANCH in v.control:
                expr = random_expr(self._language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   self._subprogram_candidates(call_graph, True),
                                   self.subprogram_declaration.formals)
                switch_stmt = SwitchStmt(self.language, expr)
                v.statements.append(switch_stmt)
                self._add_case_arm_expressions(v)
            elif len(self._cfg.successors(v)) == 0:
                return_stmt = self._generate_return_statement(expression_depth, ignore_operators, call_graph)
                v.statements.append(return_stmt)
            else:
                self._generate_sequence_of_statements(expression_depth,
                                                      ignore_operators,
                                                      block_length,
                                                      call_graph,
                                                      v,
                                                      control_stack)

            if graph.ControlPoint.SWITCH_BRANCH in v.control:
                control_stack.append(graph.ControlPoint.SWITCH_BRANCH)
            if graph.ControlPoint.LOOP_HEADER in v.control:
                control_stack.append(graph.ControlPoint.LOOP_HEADER)

        assert len(control_stack) == 1 and control_stack[0] == graph.ControlPoint.NONE

    def unparse_body(self, stream: typing.TextIO):
        indent_width = 2
        blank_columns = indent_width

        stream.write(open_block(self.language))
        stream.write(helpful.newlines())

        dfs = graph.DepthFirstSearch(self._cfg, self._cfg.entry)
        v: graph.BasicBlock
        for v in reversed(dfs.post_order()):
            if len(self._cfg.predecessors(v)) == 1:
                (e,) = self._cfg.predecessors(v)
                if e.flow in [graph.ControlFlow.THEN,
                              graph.ControlFlow.ELSE,
                              graph.ControlFlow.CASE,
                              graph.ControlFlow.ENTER_LOOP]:
                    if e.flow == graph.ControlFlow.ELSE:
                        stream.write(helpful.blanks(blank_columns))
                        stream.write(Keywords.ELSE.name.lower())
                        stream.write(helpful.newlines())
                    elif e.flow == graph.ControlFlow.CASE:
                        stream.write(helpful.blanks(blank_columns))
                        if e.expr:
                            stream.write(Keywords.CASE.name.lower())
                            stream.write(Punctuation.SPACE.value)
                            e.expr.unparse(stream)
                            stream.write(Punctuation.COLON.value)
                        else:
                            stream.write(Keywords.DEFAULT.name.lower())
                            stream.write(Punctuation.COLON.value)
                        stream.write(helpful.newlines())

                    stream.write(helpful.blanks(blank_columns))
                    stream.write(open_block(self.language))
                    stream.write(helpful.newlines())
                    blank_columns += indent_width
            elif graph.ControlPoint.SWITCH_MERGE in v.control:
                blank_columns -= indent_width
                stream.write(helpful.blanks(blank_columns))
                stream.write(close_block(self.language))
                stream.write(helpful.newlines())

            if graph.ControlPoint.IF_BRANCH in v.control or graph.ControlPoint.LOOP_HEADER in v.control:
                (stmt,) = v.statements
                stream.write(helpful.blanks(blank_columns))
                stmt.unparse(stream)
                stream.write(helpful.newlines())
            elif graph.ControlPoint.SWITCH_BRANCH in v.control:
                (stmt,) = v.statements
                stream.write(helpful.blanks(blank_columns))
                stmt.unparse(stream)
                stream.write(helpful.newlines())
                stream.write(helpful.blanks(blank_columns))
                stream.write(open_block(self.language))
                stream.write(helpful.newlines())
                blank_columns += indent_width
            else:
                for stmt in v.statements:
                    stream.write(helpful.blanks(blank_columns))
                    stmt.unparse(stream)
                    stream.write(Punctuation.SEMI_COLON.value)
                    if len(self._cfg.successors(v)) > 0 or stmt != v.statements[-1]:
                        stream.write(helpful.newlines())

            if len(self._cfg.successors(v)) == 1:
                (e,) = self._cfg.successors(v)
                if e.flow in [graph.ControlFlow.MERGE, graph.ControlFlow.BREAK, graph.ControlFlow.ITERATE_LOOP]:
                    blank_columns -= indent_width
                    stream.write(helpful.blanks(blank_columns))
                    stream.write(close_block(self.language))
                    stream.write(helpful.newlines())

        stream.write(helpful.newlines())
        stream.write(close_block(self.language))
        stream.write(helpful.newlines())

    def unparse(self, stream: typing.TextIO):
        for annotation in self.annotations:
            annotation.unparse(stream)
            stream.write(helpful.newlines())

        if self.subprograms:
            for subprogram in self.subprograms:
                subprogram.subprogram_declaration.unparse(stream)
                stream.write(Punctuation.SEMI_COLON.value)
                stream.write(helpful.newlines())

            stream.write(helpful.newlines())

            for subprogram in self.subprograms:
                subprogram.unparse(stream)

        if not self.fake:
            self.subprogram_declaration.unparse(stream)
            stream.write(helpful.newlines())
            self.unparse_body(stream)
            stream.write(helpful.newlines())


def true_expr():
    return BinaryExpr(Literal(1), Equality(), Literal(1))


def false_expr():
    return BinaryExpr(Literal(1), Equality(), Literal(0))


def random_8_bit_integer():
    return random.randint(-2 ** 4, 2 ** 4 - 1)


def random_call_expr(subprogram_declaration: SubprogramDecl, variables):
    subprogram_declaration.called = True
    arguments = []
    for i in range(len(subprogram_declaration.formals)):
        if not variables or helpful.go_ahead():
            arguments.append(Literal(random_8_bit_integer()))
        else:
            variable = random.choice(variables)
            arguments.append(VariableRef(variable))
    return CallExpr(subprogram_declaration.name, arguments)


def random_expr(language: Language, number_of_operations, ignore_operators, subprograms, variables):
    def random_arithmetic_op():
        def is_arithmetic(a_class):
            return helpful.is_strict_subclass(a_class, ArithmeticOperator)

        choices = [op for op in get_operators(is_arithmetic) if op not in ignore_operators]
        op = random.choice(choices)
        return op()

    def random_unary_op():
        def is_unary(a_class):
            return helpful.is_strict_subclass(a_class, UnaryOperator)

        choices = [op for op in get_operators(is_unary)]
        op = random.choice(choices)
        return op()

    def random_leaf():
        if variables and helpful.go_ahead():
            variable = random.choice(variables)
            return VariableRef(variable)
        elif subprograms and helpful.go_ahead():
            subprogram_declaration = random.choice(subprograms)
            return random_call_expr(subprogram_declaration, variables)
        else:
            return Literal(random_8_bit_integer())

    root_level = 1
    height = number_of_operations
    subtrees = {}
    while height >= root_level:
        if height == number_of_operations:
            root = BinaryExpr(random_leaf(), random_arithmetic_op(), random_leaf())
        else:
            if helpful.go_ahead():
                root = UnaryExpr(random_unary_op(), subtrees[height + 1])
            else:
                if helpful.go_ahead():
                    root = BinaryExpr(random_leaf(), random_arithmetic_op(), subtrees[height + 1])
                else:
                    root = BinaryExpr(subtrees[height + 1], random_arithmetic_op(), random_leaf())
        subtrees[height] = root
        height -= 1
    return subtrees[root_level]


def get_operators(predicate):
    ops = inspect.getmembers(sys.modules[__name__], lambda member: inspect.isclass(member) and predicate(member))
    return [op[1] for op in ops]
