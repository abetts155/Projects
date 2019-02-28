import re

from low_level import instructions
from utils import (dot, messages)
from graphs import instrumentation


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
        if self in self.id_pool:
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

        if self.is_dummy():
            return '{} [label={}, shape=record, fillcolor={}, style={}];\n'.format(self,
                                                                                   ''.join(label),
                                                                                   dot.Colors.red,
                                                                                   dot.Styles.filled)
        else:
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


class InstrumentationVertex(NamedVertex, instrumentation.Instrumentation):
    def __init__(self, id_, name, number):
        NamedVertex.__init__(self, id_, name)
        instrumentation.Instrumentation.__init__(self, number)

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(str(self))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append('id:{}'.format(self.number))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=circle, fillcolor={}, style={}];\n'.format(self,
                                                                               ''.join(label),
                                                                               dot.Colors.blue,
                                                                               dot.Styles.filled)


class CallVertex(NamedVertex):
    def __init__(self, id_, name, callee):
        NamedVertex.__init__(self, id_, name)
        self.__callee = callee

    @property
    def callee(self):
        return self.__callee

    def dotify(self):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(str(self))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append('call:{}'.format(self.callee))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=record, fillcolor={}, style={}];\n'.format(self,
                                                                               ''.join(label),
                                                                               dot.Colors.cyan,
                                                                               dot.Styles.filled)


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


class ProgramPointVertex(Vertex):
    """A program point is either a vertex or an edge"""

    def __init__(self, id_, program_point):
        Vertex.__init__(self, id_)
        self.__program_point = program_point

    @property
    def program_point(self):
        return self.__program_point

    @program_point.setter
    def program_point(self, value):
        self.__program_point = value

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


class ControlPoint(Vertex):
    def __init__(self, id_, program_point):
        Vertex.__init__(self, id_)
        self._program_point = program_point

    @property
    def program_point(self):
        return self._program_point

    def _dotify(self, color):
        label = []
        label.append(dot.HTML.open_html)
        label.append(dot.HTML.open_table)

        label.append(dot.HTML.open_row)
        label.append(dot.HTML.open_cell())
        label.append(str(self._program_point))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=circle, fillcolor={}, style=filled];\n'.format(self, ''.join(label), color)


class Fork(ControlPoint):
    def dotify(self):
        return self._dotify(dot.Colors.blue)


class Merge(ControlPoint):
    def dotify(self):
        return self._dotify(dot.Colors.lawn_green)


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

    @property
    def header(self) -> Vertex:
        return self.__header


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

