import os

from graphs import edges
from graphs import vertices
from graphs import graphs
from system import programs


class TraceElement:
    __slots__ = ['v', 'time']

    def __init__(self, v, time):
        self.v = v
        self.time = time

    def __str__(self):
        if isinstance(self.v.program_point, vertices.Vertex):
            return '{} {}'.format(str(self.v.program_point), self.time)
        else:
            return '{} {} {}'.format(str(self.v.program_point.predecessor()),
                                     str(self.v.program_point.successor()),
                                     self.time)

    @classmethod
    def parse(cls, line):
        lexemes = line.split()
        if lexemes:
            time = int(lexemes[-1])
            if len(lexemes) == 2:
                v = vertices.Vertex.id_pool[int(lexemes[0])]
                return v, time
            else:
                p = vertices.Vertex.id_pool[int(lexemes[0])]
                s = vertices.Vertex.id_pool[int(lexemes[1])]
                edge = edges.ControlFlowEdge(p, s)
                return edge, time


class Trace(list):
    pass


class Traces(list):
    def write(self, filename):
        with open(filename, 'w') as wd:
            for trace in self:
                for element in trace:
                    wd.write(str(element))
                    wd.write('\n')
                wd.write('\n')


class TraceFile:
    @classmethod
    def tokenize(cls, the_program: programs.Program, filename: str):
        _, prefix = os.path.split(the_program.basename())
        _, filename = os.path.split(filename)
        filename = filename[len(prefix):]
        lexemes = filename.split('.')
        assert len(lexemes) == 4
        return lexemes[1:-1]

    @classmethod
    def extract_subprogram(cls, the_program: programs.Program, filename: str):
        lexemes = cls.tokenize(the_program, filename)
        return lexemes[0]

    @classmethod
    def extract_type(cls, the_program: programs.Program, filename: str):
        lexemes = cls.tokenize(the_program, filename)
        if lexemes[1] == 'ppg':
            return graphs.ProgramPointGraph
        else:
            return graphs.InstrumentationPointGraph
