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
    DOT = '.'
    EQUALS = '='
    OPEN_ANGLE = '<'
    CLOSE_ANGLE = '>'


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
    IN = 18
    LOOP = 19
    EXIT = 20
    WHEN = 21
    OTHERS = 22
    NULL = 23
    DECLARE = 24


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


def is_unary(a_class):
    return helpful.is_strict_subclass(a_class, UnaryOperator)


def is_arithmetic(a_class):
    return helpful.is_strict_subclass(a_class, ArithmeticOperator)


def is_binary_arithmetic(a_class):
    return (helpful.is_strict_subclass(a_class, ArithmeticOperator) and
            helpful.is_strict_subclass(a_class, BinaryOperator))


class Identifier:
    def __init__(self, name):
        self._name = name

    def unparse(self, stream: typing.TextIO):
        stream.write(self._name)

    def __str__(self):
        return self._name


class IntegerWidth:
    BITS_16 = 16
    BITS_32 = 32


class IntegerType:
    def __init__(self, language: Language, size: IntegerWidth=IntegerWidth.BITS_16):
        self._language = language
        self._size = size

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            if self._size == IntegerWidth.BITS_16:
                stream.write('int')
            elif self._size == IntegerWidth.BITS_32:
                stream.write('long int')
            else:
                assert False
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
            if self._language == Language.C:
                stream.write(Punctuation.EQUALS.value)
            else:
                stream.write(Punctuation.COLON.value)
                stream.write(Punctuation.EQUALS.value)
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
    def __init__(self, lhs, op: BinaryOperator, rhs):
        self._lhs = lhs
        self._op = op
        self._rhs = rhs

    @property
    def left_hand_side(self):
        return self._lhs

    @property
    def right_hand_side(self):
        return self._rhs

    def unparse(self, stream: typing.TextIO):
        stream.write(Punctuation.OPEN_PARENTHESIS.value)
        self.left_hand_side.unparse(stream)
        self._op.unparse(stream)
        self.right_hand_side.unparse(stream)
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
        else:
            stream.write(Keywords.FOR.name.lower())
            stream.write(Punctuation.SPACE.value)
            self._initialiser.left_hand_side.name.unparse(stream)
            stream.write(Punctuation.SPACE.value)
            stream.write(Keywords.IN.name.lower())
            stream.write(Punctuation.SPACE.value)
            self._initialiser.right_hand_side.unparse(stream)
            stream.write(Punctuation.DOT.value)
            stream.write(Punctuation.DOT.value)
            self._guard.right_hand_side.unparse(stream)
            stream.write(Punctuation.SPACE.value)
            stream.write(Keywords.LOOP.name.lower())


class SwitchStmt(Stmt):
    def __init__(self, language: Language, expr):
        Stmt.__init__(self, language)
        self._expr = expr

    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            stream.write(Keywords.SWITCH.name.lower())
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            self._expr.unparse(stream)
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)
        else:
            stream.write(Keywords.CASE.name.lower())
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            self._expr.unparse(stream)
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)
            stream.write(Punctuation.SPACE.value)
            stream.write(Keywords.IS.name.lower())


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

    @property
    def left_hand_side(self):
        return self._lhs

    @property
    def right_hand_side(self):
        return self._rhs

    def unparse(self, stream: typing.TextIO):
        self.left_hand_side.unparse(stream)
        stream.write(Punctuation.SPACE.value)
        if self._language == Language.C:
            stream.write(Punctuation.EQUALS.value)
        else:
            stream.write(Punctuation.COLON.value)
            stream.write(Punctuation.EQUALS.value)
        stream.write(Punctuation.SPACE.value)
        self.right_hand_side.unparse(stream)


class CompoundAssignStmt(AssignStmt):
    def __init__(self, language: Language, lhs, rhs, operator):
        AssignStmt.__init__(self, language, lhs, rhs)
        self._operator = operator

    def unparse(self, stream: typing.TextIO):
        self.left_hand_side.unparse(stream)
        stream.write(Punctuation.SPACE.value)
        if self._language == Language.C:
            self._operator.unparse(stream)
            stream.write(Punctuation.EQUALS.value)
        else:
            stream.write(Punctuation.COLON.value)
            stream.write(Punctuation.EQUALS.value)
            stream.write(Punctuation.SPACE.value)
            self.left_hand_side.unparse(stream)
            stream.write(Punctuation.SPACE.value)
            self._operator.unparse(stream)
        stream.write(Punctuation.SPACE.value)
        self.right_hand_side.unparse(stream)


class BreakStmt(Stmt):
    def unparse(self, stream: typing.TextIO):
        if self._language == Language.C:
            stream.write(Keywords.BREAK.name.lower())
        else:
            stream.write(Keywords.EXIT.name.lower())


class ContinueStmt(Stmt):
    def unparse(self, stream: typing.TextIO):
        stream.write(Keywords.CONTINUE.name.lower())


class NullStmt(Stmt):
    def unparse(self, stream: typing.TextIO):
        if self._language == Language.ADA:
            stream.write(Keywords.NULL.name.lower())


class SymbolTable:
    def __init__(self):
        self._symbols = []
        self._frames = []
        self._end_of_formal_parameters = 0

    def push_frame(self):
        self._frames.append(len(self._symbols))

    def pop_frame(self):
        for i in range(self._frames[-1], len(self._symbols)):
            self._symbols.pop()
        self._frames.pop()

    def add_declaration(self, decl: VariableDecl, end_of_formal_parameters=False):
        self._symbols.append(decl)
        if end_of_formal_parameters:
            self._end_of_formal_parameters = len(self._symbols)

    def in_scope(self, formal_arguments=True):
        if formal_arguments:
            return self._symbols
        else:
            return self._symbols[self._end_of_formal_parameters:]


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
            self._formals.append(VariableDecl(self._language,
                                              IntegerType(self._language, IntegerWidth.BITS_32),
                                              identifier))

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
            stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.OPEN_PARENTHESIS.value)
            for formal in self._formals:
                formal.unparse(stream)
                if formal != self._formals[-1]:
                    stream.write(Punctuation.COMMA.value)
                    stream.write(Punctuation.SPACE.value)
            stream.write(Punctuation.CLOSE_PARENTHESIS.value)
        else:
            if self.is_function():
                stream.write(Keywords.FUNCTION.name.lower())
            else:
                stream.write(Keywords.PROCEDURE.name.lower())
            stream.write(Punctuation.SPACE.value)
            self._name.unparse(stream)
            if self._formals:
                stream.write(Punctuation.SPACE.value)
                stream.write(Punctuation.OPEN_PARENTHESIS.value)
                for formal in self._formals:
                    formal.unparse(stream)
                    if formal != self._formals[-1]:
                        stream.write(Punctuation.SEMI_COLON.value)
                        stream.write(Punctuation.SPACE.value)
                stream.write(Punctuation.CLOSE_PARENTHESIS.value)

            if self.is_function():
                stream.write(Punctuation.SPACE.value)
                stream.write(Keywords.RETURN.name.lower())
                stream.write(Punctuation.SPACE.value)
                self._return_type.unparse(stream)


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
        loop_counter = VariableDecl(self.language,
                                    IntegerType(self.language, IntegerWidth.BITS_16),
                                    Identifier('count_{}'.format(v)))
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
            if not default and (self.language == Language.ADA or helpful.go_ahead()):
                default = True
                e.expr = None
            else:
                # Use the value of the enumerator to guarantee case arms have unique values.
                e.expr = Literal(idx)

    def _generate_return_statement(self,
                                   expression_depth: int,
                                   ignore_operators: typing.List,
                                   call_graph:       graph.DirectedGraph,
                                   symbol_table:     SymbolTable):
        if self.main:
            return ReturnStmt(self.language, false_expr())
        elif self.subprogram_declaration.is_function():
            expr = random_expr(self._language,
                               random.randint(1, expression_depth),
                               ignore_operators,
                               self._subprogram_candidates(call_graph, True),
                               symbol_table)
            return ReturnStmt(self.language, expr)
        else:
            return ReturnStmt(self.language, None)

    def _generate_sequence_of_statements(self,
                                         expression_depth: int,
                                         ignore_operators: typing.List,
                                         block_length:     int,
                                         call_graph:       graph.DirectedGraph,
                                         symbol_table:     SymbolTable,
                                         v:                graph.BasicBlock,
                                         control_stack:    typing.List[graph.ControlPoint]):
        # Generate a sequence of statements that are executed unconditionally.
        choices = []
        if symbol_table.in_scope(False):
            choices.append(AssignStmt)
            choices.append(CompoundAssignStmt)
        if self._subprogram_candidates(call_graph, False):
            choices.append(CallStmt)
        if control_stack[-1] == graph.ControlPoint.LOOP_HEADER:
            if self._language == Language.C:
                loop_choices = [BreakStmt, ContinueStmt]
                # Shuffle so that a break-statement is not preferred over a continue-statement, or vice versa.
                random.shuffle(loop_choices)
                choices.extend(loop_choices)
            else:
                # No continue-statement in Ada.
                choices.append(BreakStmt)
        if control_stack[-1] == graph.ControlPoint.SWITCH_BRANCH:
            choices.append(BreakStmt)
        choices.append(NullStmt)

        # Choices are in order: the first is preferred to the second, the second to the third,  and so on.
        # Weight these choices so that a is n times as preferable as b, for adjacent choices a, b.
        n = 8
        weights = [n ** x for x in reversed(range(len(choices)))]

        for i in range(random.randint(1, block_length)):
            (chosen_class,) = random.choices(population=choices, weights=weights)
            if chosen_class is AssignStmt:
                decl = random.choice(symbol_table.in_scope(False))
                expr = random_expr(self.language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   self._subprogram_candidates(call_graph, True),
                                   symbol_table)
                stmt = chosen_class(self.language, VariableRef(decl), expr)
                v.statements.append(stmt)
            elif chosen_class is CompoundAssignStmt:
                decl = random.choice(symbol_table.in_scope(False))
                expr = random_expr(self.language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   self._subprogram_candidates(call_graph, True),
                                   symbol_table)
                operators = [op for op in get_operators(is_binary_arithmetic) if op not in ignore_operators]
                operator = random.choice(operators)
                stmt = chosen_class(self.language, VariableRef(decl), expr, operator())
                v.statements.append(stmt)
            elif chosen_class is CallStmt:
                subprogram_declaration = random.choice(self._subprogram_candidates(call_graph, False))
                expr = random_call_expr(subprogram_declaration,
                                        symbol_table)
                stmt = CallStmt(self.language, expr)
                v.statements.append(stmt)
            else:
                stmt = chosen_class(self.language)
                v.statements.append(stmt)

    def generate_body(self, expression_depth, ignore_operators, block_length, call_graph):
        # Remember which for-loops and switch-statements are in control at each program point so that we can decide
        # on applicable control-flow statements, e.g. break and continue.
        # We start with a non-empty stack to avoid needless checking for the empty case.
        control_stack = [graph.ControlPoint.NONE]

        symbol_table = SymbolTable()
        symbol_table.push_frame()
        for formal in self.subprogram_declaration.formals:
            symbol_table.add_declaration(formal, formal == self.subprogram_declaration.formals[-1])

        dfs = graph.DepthFirstSearch(self.cfg, self.cfg.entry)
        v: graph.BasicBlock
        for v in reversed(dfs.post_order()):
            if len(self.cfg.predecessors(v)) == 1:
                (e,) = self.cfg.predecessors(v)
                if e.flow in [graph.ControlFlow.BEGIN_CASE,
                              graph.ControlFlow.THEN,
                              graph.ControlFlow.ELSE,
                              graph.ControlFlow.ENTER_LOOP]:
                    symbol_table.push_frame()

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
                                   symbol_table)
                if_stmt = IfStmt(self.language, expr)
                v.statements.append(if_stmt)
            elif graph.ControlPoint.SWITCH_BRANCH in v.control:
                expr = random_expr(self._language,
                                   random.randint(1, expression_depth),
                                   ignore_operators,
                                   self._subprogram_candidates(call_graph, True),
                                   symbol_table)
                switch_stmt = SwitchStmt(self.language, expr)
                v.statements.append(switch_stmt)
                self._add_case_arm_expressions(v)
            elif self.subprogram_declaration.is_function() and len(self._cfg.successors(v)) == 0:
                return_stmt = self._generate_return_statement(expression_depth,
                                                              ignore_operators,
                                                              call_graph,
                                                              symbol_table)
                v.statements.append(return_stmt)
            else:
                for i in range(random.randint(0, 2)):
                    decl = VariableDecl(self.language,
                                        IntegerType(self.language, IntegerWidth.BITS_32),
                                        Identifier('i{}_{}'.format(v, i)))
                    decl.initialiser = Literal(helpful.random_8_bit_integer())
                    v.declarations.append(decl)
                    symbol_table.add_declaration(decl)

                self._generate_sequence_of_statements(expression_depth,
                                                      ignore_operators,
                                                      block_length,
                                                      call_graph,
                                                      symbol_table,
                                                      v,
                                                      control_stack)

            if graph.ControlPoint.SWITCH_BRANCH in v.control:
                control_stack.append(graph.ControlPoint.SWITCH_BRANCH)
            if graph.ControlPoint.LOOP_HEADER in v.control:
                control_stack.append(graph.ControlPoint.LOOP_HEADER)

            if len(self.cfg.successors(v)) == 1:
                (e,) = self.cfg.successors(v)
                if e.flow in [graph.ControlFlow.END_IF,
                              graph.ControlFlow.END_CASE,
                              graph.ControlFlow.ITERATE_LOOP]:
                    symbol_table.pop_frame()

        symbol_table.pop_frame()
        assert not symbol_table.in_scope(True)
        assert len(control_stack) == 1 and control_stack[0] == graph.ControlPoint.NONE

    def unparse_ada_definition(self, stream: typing.TextIO, indent_width):
        stream.write(helpful.blanks(indent_width))
        stream.write(Keywords.BEGIN.name.lower())
        stream.write(helpful.newlines())

        indent_width += 2
        blank_columns = indent_width

        dfs = graph.DepthFirstSearch(self.cfg, self.cfg.entry)
        v: graph.BasicBlock
        for v in reversed(dfs.post_order()):
            if graph.ControlPoint.SWITCH_MERGE in v.control:
                blank_columns -= indent_width
                stream.write(helpful.blanks(blank_columns))
                stream.write(Keywords.END.name.lower())
                stream.write(Punctuation.SPACE.value)
                stream.write(Keywords.CASE.name.lower())
                stream.write(Punctuation.SEMI_COLON.value)
                stream.write(helpful.newlines())

            if graph.ControlPoint.IF_MERGE in v.control:
                blank_columns -= indent_width
                stream.write(helpful.blanks(blank_columns))
                stream.write(Keywords.END.name.lower())
                stream.write(Punctuation.SPACE.value)
                stream.write(Keywords.IF.name.lower())
                stream.write(Punctuation.SEMI_COLON.value)
                stream.write(helpful.newlines())

            if len(self.cfg.predecessors(v)) == 1:
                (e,) = self.cfg.predecessors(v)
                if e.flow == graph.ControlFlow.ELSE:
                    blank_columns -= indent_width
                    stream.write(helpful.blanks(blank_columns))
                    stream.write(Keywords.ELSE.name.lower())
                    stream.write(helpful.newlines())
                    blank_columns += indent_width

                if e.flow == graph.ControlFlow.THEN:
                    blank_columns += indent_width

                if e.flow == graph.ControlFlow.ENTER_LOOP:
                    blank_columns += indent_width

                if e.flow == graph.ControlFlow.BEGIN_CASE:
                    stream.write(helpful.blanks(blank_columns))
                    if e.expr:
                        stream.write(Keywords.WHEN.name.lower())
                        stream.write(Punctuation.SPACE.value)
                        e.expr.unparse(stream)
                        stream.write(Punctuation.SPACE.value)
                        stream.write(Punctuation.EQUALS.value)
                        stream.write(Punctuation.CLOSE_ANGLE.value)
                    else:
                        stream.write(Keywords.WHEN.name.lower())
                        stream.write(Punctuation.SPACE.value)
                        stream.write(Keywords.OTHERS.name.lower())
                        stream.write(Punctuation.SPACE.value)
                        stream.write(Punctuation.EQUALS.value)
                        stream.write(Punctuation.CLOSE_ANGLE.value)
                    stream.write(helpful.newlines())
                    blank_columns += indent_width

            if len(self.cfg.successors(v)) == 1:
                (e,) = self.cfg.successors(v)
                if e.flow == graph.ControlFlow.END_CASE:
                    blank_columns -= indent_width

            if graph.ControlPoint.IF_BRANCH in v.control:
                (stmt,) = v.statements
                stream.write(helpful.blanks(blank_columns))
                stmt.unparse(stream)
                stream.write(helpful.newlines())
            elif graph.ControlPoint.LOOP_HEADER in v.control:
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
                stream.write(helpful.newlines())
            else:
                if v.declarations:
                    stream.write(helpful.blanks(blank_columns))
                    stream.write(Keywords.DECLARE.name.lower())
                    stream.write(helpful.newlines())
                    blank_columns += indent_width
                    for decl in v.declarations:
                        stream.write(helpful.blanks(blank_columns))
                        decl.unparse(stream)
                        stream.write(Punctuation.SEMI_COLON.value)
                        stream.write(helpful.newlines())
                    blank_columns -= indent_width

                    stream.write(helpful.blanks(blank_columns))
                    stream.write(Keywords.BEGIN.name.lower())
                    stream.write(helpful.newlines())
                    blank_columns += indent_width

                for stmt in v.statements:
                    stream.write(helpful.blanks(blank_columns))
                    stmt.unparse(stream)
                    stream.write(Punctuation.SEMI_COLON.value)
                    stream.write(helpful.newlines())

                if v.declarations:
                    blank_columns -= indent_width
                    stream.write(helpful.blanks(blank_columns))
                    stream.write(Keywords.END.name.lower())
                    stream.write(Punctuation.SEMI_COLON.value)

                if len(self.cfg.successors(v)) > 0:
                    stream.write(helpful.newlines())

            if len(self.cfg.successors(v)) == 1:
                (e,) = self.cfg.successors(v)
                if e.flow == graph.ControlFlow.ITERATE_LOOP:
                    blank_columns -= indent_width
                    stream.write(helpful.blanks(blank_columns))
                    stream.write(Keywords.END.name.lower())
                    stream.write(Punctuation.SPACE.value)
                    stream.write(Keywords.LOOP.name.lower())
                    stream.write(Punctuation.SEMI_COLON.value)
                    stream.write(helpful.newlines())

        indent_width -= 2
        stream.write(helpful.newlines())
        stream.write(helpful.blanks(indent_width))
        stream.write(Keywords.END.name.lower())
        stream.write(Punctuation.SPACE.value)
        self.subprogram_declaration.name.unparse(stream)
        stream.write(Punctuation.SEMI_COLON.value)

    def unparse_c_definition(self, stream: typing.TextIO, indent_width):
        stream.write(helpful.blanks(indent_width))
        stream.write(Punctuation.OPEN_BRACE.value)
        stream.write(helpful.newlines())

        indent_width += 2
        blank_columns = indent_width

        dfs = graph.DepthFirstSearch(self.cfg, self.cfg.entry)
        v: graph.BasicBlock
        for v in reversed(dfs.post_order()):
            if len(self._cfg.predecessors(v)) == 1:
                (e,) = self._cfg.predecessors(v)
                if e.flow in [graph.ControlFlow.THEN,
                              graph.ControlFlow.ELSE,
                              graph.ControlFlow.BEGIN_CASE,
                              graph.ControlFlow.ENTER_LOOP]:
                    if e.flow == graph.ControlFlow.ELSE:
                        stream.write(helpful.blanks(blank_columns))
                        stream.write(Keywords.ELSE.name.lower())
                        stream.write(helpful.newlines())
                    elif e.flow == graph.ControlFlow.BEGIN_CASE:
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
                    stream.write(Punctuation.OPEN_BRACE.value)
                    stream.write(helpful.newlines())
                    blank_columns += indent_width
            elif graph.ControlPoint.SWITCH_MERGE in v.control:
                blank_columns -= indent_width
                stream.write(helpful.blanks(blank_columns))
                stream.write(Punctuation.CLOSE_BRACE.value)
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
                stream.write(Punctuation.OPEN_BRACE.value)
                stream.write(helpful.newlines())
                blank_columns += indent_width
            else:
                for decl in v.declarations:
                    stream.write(helpful.blanks(blank_columns))
                    decl.unparse(stream)
                    stream.write(Punctuation.SEMI_COLON.value)
                    stream.write(helpful.newlines())

                for stmt in v.statements:
                    stream.write(helpful.blanks(blank_columns))
                    stmt.unparse(stream)
                    stream.write(Punctuation.SEMI_COLON.value)
                    if self.cfg.successors(v) or stmt != v.statements[-1]:
                        stream.write(helpful.newlines())

            if len(self._cfg.successors(v)) == 1:
                (e,) = self._cfg.successors(v)
                if e.flow in [graph.ControlFlow.END_IF, graph.ControlFlow.END_CASE, graph.ControlFlow.ITERATE_LOOP]:
                    blank_columns -= indent_width
                    stream.write(helpful.blanks(blank_columns))
                    stream.write(Punctuation.CLOSE_BRACE.value)
                    stream.write(helpful.newlines())

        indent_width -= 2
        stream.write(helpful.newlines())
        stream.write(helpful.blanks(indent_width))
        stream.write(Punctuation.CLOSE_BRACE.value)

    def unparse(self, stream: typing.TextIO, indent_width=0):
        for annotation in self.annotations:
            stream.write(helpful.blanks(indent_width))
            annotation.unparse(stream)
            stream.write(helpful.newlines())

        if not self.fake:
            stream.write(helpful.blanks(indent_width))
            self.subprogram_declaration.unparse(stream)
            stream.write(helpful.newlines())

            if self.language == Language.ADA:
                stream.write(helpful.blanks(indent_width))
                stream.write(Keywords.IS.name.lower())
                stream.write(helpful.newlines())

            indent_width += 2

        if self.subprograms:
            for subprogram in self.subprograms:
                stream.write(helpful.blanks(indent_width))
                subprogram.subprogram_declaration.unparse(stream)
                stream.write(Punctuation.SEMI_COLON.value)
                stream.write(helpful.newlines())

            stream.write(helpful.newlines())

            for subprogram in self.subprograms:
                subprogram.unparse(stream, indent_width)

        if not self.fake:
            indent_width -= 2
            if self.language == Language.C:
                self.unparse_c_definition(stream, indent_width)
            else:
                self.unparse_ada_definition(stream, indent_width)
            stream.write(helpful.newlines(2))


def true_expr():
    return BinaryExpr(Literal(1), Equality(), Literal(1))


def false_expr():
    return BinaryExpr(Literal(1), Equality(), Literal(0))


def random_call_expr(subprogram_declaration: SubprogramDecl,
                     symbol_table:           SymbolTable):
    subprogram_declaration.called = True
    arguments = []
    for i in range(len(subprogram_declaration.formals)):
        if not symbol_table.in_scope() or helpful.go_ahead():
            arguments.append(Literal(helpful.random_8_bit_integer()))
        else:
            variable = random.choice(symbol_table.in_scope())
            arguments.append(VariableRef(variable))
    return CallExpr(subprogram_declaration.name, arguments)


def random_expr(language:             Language,
                number_of_operations: int,
                ignore_operators:     typing.List,
                subprograms:          typing.List[SubprogramDecl],
                symbol_table:         SymbolTable):
    def random_arithmetic_op():
        operators = [op for op in get_operators(is_arithmetic) if op not in ignore_operators]
        operator = random.choice(operators)
        return operator()

    def random_unary_op():
        operators = [op for op in get_operators(is_unary)]
        operator = random.choice(operators)
        return operator()

    def random_leaf():
        if symbol_table.in_scope() and helpful.go_ahead():
            variable = random.choice(symbol_table.in_scope())
            return VariableRef(variable)
        elif subprograms and helpful.go_ahead():
            subprogram_declaration = random.choice(subprograms)
            return random_call_expr(subprogram_declaration, symbol_table)
        else:
            return Literal(helpful.random_8_bit_integer())

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
