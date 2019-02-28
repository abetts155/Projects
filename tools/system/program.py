import collections
import os
import re

from graphs import (edges, graphs, vertices)
from utils import messages


class Subprogram:
    def __init__(self, cfg: graphs.ControlFlowGraph, call_vertex: vertices.SubprogramVertex):
        if not cfg.name == call_vertex.name:
            raise ValueError('Subprogram name mismatch: Found {} and {}'.format(cfg.name, call_vertex.name))
        self.__cfg = cfg
        self.__call_vertex = call_vertex
        self.__lnt = None
        self.__ipg = None

    @property
    def name(self):
        return self.__cfg.name

    @property
    def cfg(self) -> graphs.ControlFlowGraph:
        return self.__cfg

    @property
    def lnt(self) -> graphs.LoopNests:
        if self.__lnt is None:
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(self.__cfg)
            self.__lnt = graphs.LoopNests(ppg)
        return self.__lnt

    @property
    def call_vertex(self) -> vertices.SubprogramVertex:
        return self.__call_vertex

    @property
    def ipg(self):
        assert self.__ipg, 'No instrumentation point graph for {}'.format(self.__cfg.name)
        return self.__ipg

    @ipg.setter
    def ipg(self, ipg):
        self.__ipg = ipg


class Program:
    def __init__(self, filename):
        self._filename = filename
        self.__subprograms = collections.OrderedDict()
        self.__call_graph = graphs.CallGraph(self)

    def add_subprogram(self, subprogram: Subprogram):
        assert subprogram.name not in self.__subprograms, 'Already have information on subprogram {}'.format(
            subprogram.name)
        self.__subprograms[subprogram.name] = subprogram
        self.__call_graph.add_vertex(subprogram.call_vertex)

    def remove_subprogram(self, subprogram: Subprogram):
        self.__call_graph.remove_vertex(subprogram.call_vertex)
        del self.__subprograms[subprogram.name]

    def __getitem__(self, name) -> Subprogram:
        assert name in self.__subprograms, "No subprogram with name '{}'".format(name)
        return self.__subprograms[name]

    def __contains__(self, name):
        return name in self.__subprograms

    def __iter__(self) -> Subprogram:
        for subprogram in self.__subprograms.values():
            yield subprogram

    @property
    def call_graph(self) -> graphs.CallGraph:
        return self.__call_graph

    @property
    def filename(self):
        return self._filename

    def basename(self):
        base, _ = os.path.splitext(os.path.abspath(self._filename))
        return base

    def cleanup(self):
        for root, dirs, files in os.walk(self.basename()):
            for filename in files:
                filename = os.path.join(root, filename)
                _, ext = os.path.splitext(filename)
                if ext in ['.png', '.dot', '.ilp']:
                    os.remove(filename)


class IO:
    SUBPROGRAM = 'subprogram'
    VERTEX = 'vertex'
    EDGE = 'edge'
    WCET = 'wcet'
    LOCAL_BOUND = 'local_bound'
    GLOBAL_BOUND = 'global_bound'
    CALL = 'call'
    INSTRUMENTATION = 'instrumentation'

    @classmethod
    def new_lines(cls, number=1):
        return '\n' * number

    @classmethod
    def write(cls, the_program: Program, filename: str):
        separator = ':'
        with open(filename, 'w') as wd:
            for subprogram in the_program:
                wd.write('{}{}{}'.format(cls.SUBPROGRAM, separator, subprogram.name))
                wd.write(cls.new_lines())

                for v in subprogram.cfg:
                    if isinstance(v, vertices.Vertex):
                        wd.write('{}{}{}'.format(cls.VERTEX, separator, v))
                        callee = the_program.call_graph.is_call_site(subprogram.call_vertex, v)
                        if callee:
                            wd.write(' {}{}{}'.format(cls.CALL, separator, callee.name))
                        elif isinstance(v, vertices.InstrumentationVertex):
                            wd.write(' {}{}{}'.format(cls.INSTRUMENTATION, separator, v.number))
                        wd.write(cls.new_lines())

                for v in subprogram.cfg:
                    for edge in subprogram.cfg.successors(v):
                        if edge.successor() != subprogram.cfg.entry:
                            wd.write('{}{}{} => {}'.format(cls.EDGE, separator, edge.predecessor(), edge.successor()))

                            if isinstance(edge, edges.InstrumentationEdge):
                                wd.write(' {}{}{}'.format(cls.INSTRUMENTATION, separator, edge.number))

                            wd.write(cls.new_lines())

                    callee = the_program.call_graph.is_call_site(subprogram.call_vertex, v)
                    if callee:
                        wd.write('{}{}{} => {}'.format(cls.EDGE,
                                                       separator,
                                                       v,
                                                       the_program[callee.name].cfg.entry))
                        wd.write(cls.new_lines())
                wd.write(cls.new_lines(2))

    @classmethod
    def read(cls, filename: str) -> Program:
        messages.verbose_message("Reading program from '{}'".format(filename))

        def parse():
            lines = []
            with open(filename, 'r') as rd:
                for line in rd:
                    if not re.match('\s+', line):
                        lexemes = re.split('\W+', line)
                        if lexemes:
                            lines.append(lexemes[:-1])
            return lines

        Data = collections.namedtuple('Data', ['v', 'cfg'])
        cfgs = {}
        vertex_data = {}
        the_program = Program(filename)
        lines = parse()

        for lexemes in lines:
            if lexemes[0] == cls.SUBPROGRAM:
                name = lexemes[1]
                cfg = graphs.ControlFlowGraph(the_program, name)
                cfgs[name] = cfg
            elif lexemes[0] == cls.VERTEX:
                id_ = int(lexemes[1])
                if cls.CALL in lexemes:
                    callee = lexemes[-1]
                    v = vertices.CallVertex(id_, cfg.name, callee)
                elif cls.INSTRUMENTATION in lexemes:
                    number = int(lexemes[-1])
                    v = vertices.InstrumentationVertex(id_, cfg.name, number)
                else:
                    v = vertices.Vertex(id_)
                cfg.add_vertex(v)
                vertex_data[id_] = Data(v, cfg)

        for name, cfg in cfgs.items():
            call = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), name)
            subprogram = Subprogram(cfgs[name], call)
            the_program.add_subprogram(subprogram)

        for lexemes in lines:
            if lexemes[0] == cls.EDGE:
                predecessor, predecessor_cfg = vertex_data[int(lexemes[1])]
                successor, successor_cfg = vertex_data[int(lexemes[2])]
                if predecessor_cfg == successor_cfg:
                    if cls.INSTRUMENTATION in lexemes:
                        number = int(lexemes[-1])
                        predecessor_cfg.add_edge(edges.InstrumentationEdge(predecessor, successor, number))
                    else:
                        predecessor_cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))
                else:
                    caller = the_program[predecessor_cfg.name].call_vertex
                    callee = the_program[successor_cfg.name].call_vertex
                    the_program.call_graph.add_edge(edges.CallGraphEdge(caller, callee, predecessor))

        for subprogram in the_program:
            (subprogram.cfg.entry,) = [v for v in subprogram.cfg if len(subprogram.cfg.predecessors(v)) == 0]
            try:
                (subprogram.cfg.exit,) = [v for v in subprogram.cfg if len(subprogram.cfg.successors(v)) == 0]
            except ValueError:
                exit_vertex = vertices.Vertex(vertices.Vertex.get_vertex_id(True))
                subprogram.cfg.add_vertex(exit_vertex)
                subprogram.cfg.exit = exit_vertex
                for v in subprogram.cfg:
                    if len(subprogram.cfg.successors(v)) == 0 and v != exit_vertex:
                        subprogram.cfg.add_edge(edges.ControlFlowEdge(v, exit_vertex))

        return the_program

    @classmethod
    def read_properties(cls, the_program: Program, filename: str):
        def parse():
            lines = []
            with open(filename, 'r') as rd:
                for line in rd:
                    if not re.match('\s+', line):
                        lexemes = re.split('\W+', line)
                        if lexemes:
                            lines.append(lexemes[:-1])
            return lines

        def set_properties(p: vertices.Vertex or edges.Edge, lexemes):
            i = 0
            while i < len(lexemes):
                if lexemes[i] == cls.INSTRUMENTATION:
                    setattr(p, cls.INSTRUMENTATION, int(lexemes[i + 1]))
                    i += 2
                elif lexemes[i] == cls.WCET:
                    setattr(p, cls.WCET, int(lexemes[i + 1]))
                    i += 2
                elif lexemes[i] == cls.LOCAL_BOUND:
                    setattr(p, cls.LOCAL_BOUND, int(lexemes[i + 1]))
                    i += 2
                elif lexemes[i] == cls.GLOBAL_BOUND:
                    setattr(p, cls.GLOBAL_BOUND, int(lexemes[i + 1]))
                    i += 2
                else:
                    assert False

        lines = parse()
        subprogram = None
        for lexemes in lines:
            if lexemes[0] == cls.SUBPROGRAM:
                name = lexemes[1]
                subprogram = the_program[name]
            elif lexemes[0] == cls.VERTEX:
                assert subprogram is not None
                id_ = int(lexemes[1])
                p = vertices.Vertex.id_pool[id_]
                set_properties(p, lexemes[2:])
            else:
                assert subprogram is not None
                assert lexemes[0] == cls.EDGE
                predecessor_id = int(lexemes[1])
                predecessor = vertices.Vertex.id_pool[predecessor_id]
                successor_id = int(lexemes[2])
                successor = vertices.Vertex.id_pool[successor_id]
                if predecessor in subprogram.cfg and successor in subprogram.cfg:
                    (p,) = [edge for edge in subprogram.cfg.successors(predecessor) if edge.successor() == successor]
                    set_properties(p, lexemes[3:])
