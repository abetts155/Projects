import enum

from graphs import vertices
from graphs import instrumentation
from utils import dot

edge_id = 0


def get_edge_id():
    global edge_id
    edge_id += 1
    return edge_id


class Edge:
    def __init__(self, predecessor, successor):
        self._predecessor = predecessor
        self._successor = successor
        self._id = get_edge_id()

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
        return self._id

    def __str__(self):
        return "({}, {})".format(self._predecessor, self._successor)

    def dotify(self):
        return '{}->{};\n'.format(self._predecessor.id_, self._successor.id_)


class Direction(enum.Enum):
    NONE = 0
    RETURN = 1
    CONTINUE = 2
    ELSE = 3
    THEN = 4
    CASE = 5
    UNREACHABLE = 6


class ControlFlowEdge(Edge):
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

    def set_return(self, callee: vertices.SubprogramVertex):
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


class SuperEdge(Edge, set):
    def __init__(self, predecessor, successor):
        Edge.__init__(self, predecessor, successor)
        set.__init__(self)

    def dotify(self):
        if len(self) > 10:
            return '{}->{};\n'.format(self._predecessor.id_, self._successor.id_)
        else:
            return '{}->{} [label="{}"];\n'.format(self._predecessor.id_,
                                                   self._successor.id_,
                                                   '\n'.join(str(edge) for edge in self))


class InstrumentationEdge(ControlFlowEdge, instrumentation.Instrumentation):
    def __init__(self, predecessor, successor, number, direction=Direction.NONE):
        ControlFlowEdge.__init__(self, predecessor, successor, direction)
        instrumentation.Instrumentation.__init__(self, number)

    def dotify(self):
        label = [dot.HTML.open_html,
                 dot.HTML.open_table,
                 dot.HTML.open_row,
                 dot.HTML.open_cell(border=2),
                 'id:{}'.format(self.number),
                 dot.HTML.close_cell,
                 dot.HTML.close_row,
                 dot.HTML.close_table,
                 dot.HTML.close_html]

        return '{}->{} [label={}, color={}, style={}];\n'.format(self._predecessor.id_,
                                                                 self._successor.id_,
                                                                 ''.join(label),
                                                                 dot.Colors.blue,
                                                                 dot.Styles.bold)


class TransitionEdge(Edge, list):
    def __init__(self, predecessor, successor):
        Edge.__init__(self, predecessor, successor)
        self._back_edge = False

    @property
    def back_edge(self):
        return self._back_edge

    def set_back_edge(self):
        self._back_edge = True

    def dotify(self):
        label = [dot.HTML.open_html,
                 dot.HTML.open_table]

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

    def __init__(self, predecessor, successor, direction):
        Edge.__init__(self, predecessor, successor)
        self._direction = direction

    @property
    def direction(self):
        return self._direction

    def dotify(self):
        label = [dot.HTML.open_html,
                 dot.HTML.open_table]

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
    def __init__(self, predecessor, successor, site):
        Edge.__init__(self, predecessor, successor)
        self._site = site

    @property
    def site(self):
        return self._site

    def dotify(self):
        return '{}->{} [label="{}"];\n'.format(self.predecessor, self.successor, self.site)
