import collections
import os

from graphs import (edges, graphs, vertices)
from utils import messages


class Subprogram:
    def __init__(self, cfg: graphs.ControlFlowGraph, call_vertex: vertices.SubprogramVertex):
        if not cfg.name == call_vertex.name:
            raise ValueError('Subprogram name mismatch: Found {} and {}'.format(cfg.name, call_vertex.name))
        self.__cfg = cfg
        self.__call_vertex = call_vertex
        self.__ipg = None

    @property
    def name(self):
        return self.__cfg.name

    @property
    def cfg(self) -> graphs.ControlFlowGraph:
        return self.__cfg

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


class Properties:
    INSTRUMENTATION = 'instrumentation'
    WCET = 'wcet'
    LOCAL_WFREQ = 'local'
    GLOBAL_WFREQ = 'global'

    def __init__(self):
        self.INSTRUMENTATION = None
        self.WCET = None
        self.LOCAL_WFREQ = None
        self.GLOBAL = None

    @property
    def instrumentation(self):
        return self.INSTRUMENTATION

    @property
    def wcet(self):
        return self.WCET

    @property
    def local_wfreq(self):
        return self.LOCAL_WFREQ

    @property
    def global_wfreq(self):
        return self.GLOBAL_WFREQ


class IO:
    SUBPROGRAM = 'subprogram'
    VERTEX = 'vertex'
    EDGE = 'edge'

    @classmethod
    def new_lines(cls, number=1):
        return '\n' * number

    @classmethod
    def write(cls, the_program: Program, filename: str):
        with open(filename, 'w') as wd:
            for subprogram in the_program:
                wd.write('{} {}'.format(cls.SUBPROGRAM, subprogram.name))
                wd.write(cls.new_lines())

                for v in subprogram.cfg:
                    wd.write('{} {}'.format(cls.VERTEX, v))
                    wd.write(cls.new_lines())
                    for e in subprogram.cfg.successors(v):
                        if e.successor() != subprogram.cfg.entry:
                            wd.write('{} {} => {}'.format(cls.EDGE, e.predecessor(), e.successor()))
                            wd.write(cls.new_lines())
                    callee = the_program.call_graph.is_call_site(subprogram.call_vertex, v)
                    if callee:
                        wd.write('{} {} => {}'.format(cls.EDGE, v, the_program[callee.name].cfg.entry))
                        wd.write(cls.new_lines())
                    wd.write(cls.new_lines())

                wd.write(cls.new_lines(2))

    @classmethod
    def read(cls, filename: str) -> Program:
        Data = collections.namedtuple('Data', ['v', 'cfg'])
        cfgs = {}
        prog = Program(filename)
        vertex_data = {}
        with open(filename, 'r') as rd:
            for line in rd:
                lexemes = line.split()
                if lexemes:
                    if lexemes[0] == cls.SUBPROGRAM:
                        name = lexemes[1]
                        cfg = graphs.ControlFlowGraph(prog, name)
                        cfgs[name] = cfg
                    elif lexemes[0] == cls.VERTEX:
                        id_ = int(lexemes[1])
                        v = vertices.Vertex(id_)
                        cfg.add_vertex(v)
                        vertex_data[id_] = Data(v, cfg)

        with open(filename, 'r') as rd:
            for line in rd:
                lexemes = line.split()
                if lexemes:
                    if lexemes[0] == cls.SUBPROGRAM:
                        name = lexemes[1]
                        call = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), name)
                        subprogram = Subprogram(cfgs[name], call)
                        prog.add_subprogram(subprogram)

        with open(filename, 'r') as rd:
            for line in rd:
                lexemes = line.split()
                if lexemes:
                    if lexemes[0] == cls.EDGE:
                        pred_id = int(lexemes[1])
                        pred_v, pred_cfg = vertex_data[pred_id]
                        succ_id = int(lexemes[3])
                        succ_v, succ_cfg = vertex_data[succ_id]
                        if pred_cfg == succ_cfg:
                            pred_cfg.add_edge(edges.ControlFlowEdge(pred_v, succ_v))
                        else:
                            caller = prog[pred_cfg.name].call_vertex
                            callee = prog[succ_cfg.name].call_vertex
                            prog.call_graph.add_edge(edges.CallGraphEdge(caller, callee, pred_v))

        for subprogram in prog:
            (subprogram.cfg.entry,) = [v for v in subprogram.cfg if len(subprogram.cfg.predecessors(v)) == 0]
            (subprogram.cfg.exit,) = [v for v in subprogram.cfg if len(subprogram.cfg.successors(v)) == 0]
        return prog

    @classmethod
    def read_properties(cls, filename: str):

        def set_properties(property_string, program_point_properties):
            property_string = property_string.strip()
            property_string = property_string.lower()
            properties = property_string.split(',')
            properties = [l.split('=') for l in properties]
            for a_property in properties:
                assert len(a_property) == 2
                name, value = a_property
                if name == Properties.INSTRUMENTATION:
                    try:
                        program_point_properties.INSTRUMENTATION = int(value) == 1
                    except ValueError:
                        messages.error_message('Value of {} for property {} is invalid'.format(value, name.lower()))
                elif name == Properties.WCET:
                    try:
                        program_point_properties.WCET = int(value)
                    except ValueError:
                        messages.error_message('Value of {} for property {} is invalid'.format(value, name.lower()))
                elif name == Properties.LOCAL_WFREQ:
                    try:
                        program_point_properties.LOCAL_WFREQ = int(value)
                    except ValueError:
                        messages.error_message('Value of {} for property {} is invalid'.format(value, name.lower()))
                elif name == Properties.GLOBAL_WFREQ:
                    try:
                        program_point_properties.GLOBAL_WFREQ = int(value)
                    except ValueError:
                        messages.error_message('Value of {} for property {} is invalid'.format(value, name.lower()))

        properties = {}
        with open(filename, 'r') as rd:
            for line in rd:
                l_index = line.find('[')
                r_index = line.rfind(']')
                if l_index > -1 and r_index > -1:
                    prefix = line[:l_index]
                    suffix = line[l_index+1:r_index]

                    lexemes = prefix.split()
                    if lexemes[0] == cls.VERTEX:
                        id_ = int(lexemes[1])
                        program_point = vertices.Vertex.id_pool[id_]
                        properties[program_point] = Properties()
                    else:
                        assert lexemes[0] == cls.EDGE
                        pred_id = int(lexemes[1])
                        p = vertices.Vertex.id_pool[pred_id]
                        succ_id = int(lexemes[3])
                        s = vertices.Vertex.id_pool[succ_id]
                        program_point = edges.ControlFlowEdge(p, s)
                        properties[program_point] = Properties()
                    set_properties(suffix, properties[program_point])

        return properties
