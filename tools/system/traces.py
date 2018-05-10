import os

from graphs import graph
from system import program


class TraceElement:
    __slots__ = ['v', 'time']

    def __init__(self, v, time):
        self.v = v
        self.time = time

    def __str__(self):
        if isinstance(self.v.program_point, graph.Vertex):
            return '{} {}'.format(str(self.v.program_point), self.time)
        else:
            return '{} {} {}'.format(str(self.v.program_point.predecessor()),
                                     str(self.v.program_point.successor()),
                                     self.time)


class Trace(list):
    pass


class Traces(list):
    def write(self, filename):
        with open(filename, 'a') as wd:
            for trace in self:
                for element in trace:
                    wd.write(str(element))
                    wd.write('\n')
                wd.write('\n')


class TraceFile:
    EXT = 'trace'

    @classmethod
    def create(cls, the_program: program.Program, g: graph.ProgramPointGraph or graph.InstrumentationPointGraph):
        if isinstance(g, graph.ProgramPointGraph):
            return '{}.{}.ppg.{}'.format(the_program.basename(), g.name, cls.EXT)
        else:
            assert isinstance(g, graph.InstrumentationPointGraph)
            return '{}.{}.ipg.{}'.format(the_program.basename(), g.name, cls.EXT)

    @classmethod
    def extract_subprogram(cls, the_program: program.Program, trace_file: str):
        _, prefix = os.path.split(the_program.basename())
        _, trace_file = os.path.split(trace_file)
        trace_file = trace_file[len(prefix):]
        lexemes = trace_file.split('.')
        assert len(lexemes) == 4
        return lexemes[1]
