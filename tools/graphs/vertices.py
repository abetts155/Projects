from low_level import instructions
from graphs import instrumentation
from re import match
from typing import List
from utils import dot, messages


class VertexPool:
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


class InstrumentationVertex(Vertex):
    def __init__(self, id_, label):
        Vertex.__init__(self, id_)
        self._label = label

    @staticmethod
    def ghost_value():
        return 0

    @property
    def label(self):
        return self._label

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
        label.append('label:{}'.format(self.label))
        label.append(dot.HTML.close_cell)
        label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)

        return '{} [label={}, shape=circle, fillcolor={}, style={}];\n'.format(self,
                                                                               ''.join(label),
                                                                               dot.Colors.blue,
                                                                               dot.Styles.filled)


class CallVertex(Vertex):
    def __init__(self, id_, callee):
        Vertex.__init__(self, id_)
        self._callee = callee

    @property
    def callee(self):
        return self._callee

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


class BasicBlock(Vertex):
    def __init__(self, id_):
        Vertex.__init__(self, id_)
        self._instructions = []

    @property
    def instructions(self) -> List[str]:
        return self._instructions

    def dotify(self):
        if self.instructions:
            instruction_text = '\l'.join('{}'.format(x) for x in self.instructions)
            return '{} [label="{}", shape=rectangle]'.format(self.id_,
                                                             '{}\n{}'.format(self.id_, instruction_text + '\l'))
        else:
            return '{} [label="{}", shape=record, fontsize=8]'.format(self.id_, self.id_)


class InstructionVertex(Vertex, set):
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


class SubprogramVertex(Vertex):
    def __init__(self, id_, name):
        Vertex.__init__(self, id_)
        self._name = name

    @property
    def name(self):
        return self._name

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
    def __init__(self, id_):
        Vertex.__init__(self, id_)
        self._representative = None

    def append(self, element):
        list.append(self, element)

    @property
    def representative(self):
        return self._representative

    @representative.setter
    def representative(self, v):
        self._representative = v

    def _label(self, *args):
        label = [dot.HTML.open_html,
                 dot.HTML.open_table,
                 dot.HTML.open_row,
                 dot.HTML.open_cell(color=dot.Colors.cyan, border=5), 'id={}'.format(self),
                 dot.HTML.close_cell,
                 dot.HTML.close_row]

        for arg in args:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell(color=dot.Colors.red, border=2))
            label.append(arg)
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        for vertex in self:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell())
            label.append(str(vertex))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return label

    def dotify(self):
        return '{} [label={}, shape=record];\n'.format(self, ''.join(self._label()))


class LoopBody(Vertex, set):
    def __init__(self, id_):
        Vertex.__init__(self, id_)
        set.__init__(self)


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

