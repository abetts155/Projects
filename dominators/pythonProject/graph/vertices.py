import re

from utils import dot

identifiers_pattern: str = r'[a-zA-Z0-9_]+'
identifiers_regex: re.Pattern = re.compile(f'^{identifiers_pattern}$')


def check_identifier(identifier: str):
    if not identifiers_regex.match(identifier):
        raise ValueError(f"Invalid vertex identifier: '{identifier}'")


class Vertex:
    IDENTIFIER_PREFIX = 'V'

    def __init__(self, identifier: str):
        check_identifier(identifier)
        self.identifier = identifier

    def __lt__(self, other: "Vertex"):
        return self.identifier < other.identifier

    def __hash__(self):
        return hash(self.identifier)

    def __str__(self):
        return f'{self.identifier}'

    def dot_identifier(self) -> str:
        cleaned = self.identifier.replace('@', '__')
        return cleaned

    def dotify(self) -> str:
        return f'{self.dot_identifier()} [label="{self.identifier}", shape=square];\n'


def partition(vertex_set: set[Vertex], cutoff: int = 10) -> list[list[Vertex]]:
    total = len(vertex_set) // cutoff
    if len(vertex_set) % cutoff != 0:
        total += 1

    vertex_lists = [[] for _ in range(total)]
    index = 0
    for vertex in vertex_set:
        vertex_lists[index].append(vertex)
        if len(vertex_lists[index]) == cutoff:
            index += 1

    return vertex_lists


class SuperVertex(Vertex):
    IDENTIFIER_PREFIX = 'S'

    def __init__(self, identifier: str):
        super().__init__(identifier)
        self.vertices: set[Vertex] = set()

    def dotify(self):
        label = [
            dot.HTML.open_html,
            dot.HTML.open_table,
            dot.HTML.open_row,
            dot.HTML.open_cell(color=dot.Colors.CYAN, border=2),
            self.identifier,
            dot.HTML.close_cell,
            dot.HTML.close_row
        ]

        for vertex_set in partition(self.vertices):
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell())
            label.append(' '.join(vertex.identifier for vertex in vertex_set))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return f"{self.identifier} [label={''.join(label)}, shape=record];\n"


class LoopVertex(Vertex):
    IDENTIFIER_PREFIX = 'L'

    def __init__(self, identifier: str):
        super().__init__(identifier)
        self.body: set[Vertex] = set()
        self.entries: set[Vertex] = set()
        self.exits: set[Vertex] = set()

    def dotify(self):
        label = [
            dot.HTML.open_html,
            dot.HTML.open_table,
            dot.HTML.open_row,
            dot.HTML.open_cell(color=dot.Colors.CYAN, border=2),
            self.identifier,
            dot.HTML.close_cell,
            dot.HTML.close_row,
            dot.HTML.open_row,
            dot.HTML.open_cell(border=1),
            'Body',
            dot.HTML.close_cell,
            dot.HTML.close_row
        ]

        for vertex_set in partition(self.body):
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell())
            label.append(' '.join(vertex.identifier for vertex in vertex_set))
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

        if self.entries:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell(border=1))
            label.append('Entries')
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

            for vertex_set in partition(self.entries):
                label.append(dot.HTML.open_row)
                label.append(dot.HTML.open_cell())
                label.append(' '.join(vertex.identifier for vertex in vertex_set))
                label.append(dot.HTML.close_cell)
                label.append(dot.HTML.close_row)

        if self.exits:
            label.append(dot.HTML.open_row)
            label.append(dot.HTML.open_cell(border=1))
            label.append('Exits')
            label.append(dot.HTML.close_cell)
            label.append(dot.HTML.close_row)

            for vertex_set in partition(self.exits):
                label.append(dot.HTML.open_row)
                label.append(dot.HTML.open_cell())
                label.append(' '.join(vertex.identifier for vertex in vertex_set))
                label.append(dot.HTML.close_cell)
                label.append(dot.HTML.close_row)

        label.append(dot.HTML.close_table)
        label.append(dot.HTML.close_html)
        return f"{self.identifier} [label={''.join(label)}, shape=record];\n"


class InstrumentationVertex(Vertex):
    IDENTIFIER_PREFIX = 'I'

    def dotify(self):
        return f"{self.identifier} [label={self.identifier}, shape=circle, fillcolor=dodgerblue, style=filled];\n"


taken_numbers = {}


def get_vertex_identifier(prefix: str = ''):
    if prefix not in taken_numbers:
        taken_numbers[prefix] = []

    numbers = taken_numbers[prefix]
    if not numbers:
        next_number = 1
    else:
        next_number = numbers[-1] + 1
    numbers.append(next_number)

    return f'{prefix}{next_number}'
