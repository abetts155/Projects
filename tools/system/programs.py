from collections import OrderedDict
from graphs import edges, graphs, vertices
from low_level import instructions
from typing import List
from utils import messages

import json
import os


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
        self._subprograms = OrderedDict()
        self._call_graph = graphs.CallGraph(self)

    def add_subprogram(self, subprogram: Subprogram):
        assert subprogram.name not in self._subprograms, 'Already have information on subprogram {}'.format(
            subprogram.name)
        self._subprograms[subprogram.name] = subprogram
        self._call_graph.add_vertex(subprogram.call_vertex)

    def remove_subprogram(self, subprogram: Subprogram):
        self._call_graph.remove_vertex(subprogram.call_vertex)
        del self._subprograms[subprogram.name]

    def keep_only(self, names: List[str]):
        dead = set()
        for name, subprogram in self._subprograms.items():
            if name not in names:
                dead.add(subprogram)

        for subprogram in dead:
            self.remove_subprogram(subprogram)

    def __getitem__(self, name) -> Subprogram:
        assert name in self._subprograms, "No subprogram with name '{}'".format(name)
        return self._subprograms[name]

    def __contains__(self, name):
        return name in self._subprograms

    def __iter__(self) -> Subprogram:
        for subprogram in self._subprograms.values():
            yield subprogram

    @property
    def call_graph(self) -> graphs.CallGraph:
        return self._call_graph

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
    @classmethod
    def write(cls, the_program: Program, filename: str):
        program_json = {}
        for subprogram in the_program:
            vertices_json = []
            edges_json = []
            for vertex in subprogram.cfg:
                vertex_json = [vertex.id_]
                vertices_json.append(vertex_json)

                instruction_json = []
                vertex_json.append(instruction_json)
                for instruction in vertex.instructions:
                    if isinstance(instruction, instructions.CallInstruction):
                        instruction_json.append([instruction.opcode, instruction.target])

                for edge in subprogram.cfg.successors(vertex):
                    if edge.successor() != subprogram.cfg.entry:
                        edges_json.append((edge.predecessor().id_, edge.successor().id_))

            program_json[subprogram.name] = [vertices_json, edges_json]
            with open(filename, 'w') as outfile:
                json.dump(program_json, outfile, indent=2)


    @classmethod
    def read(cls, filename: str) -> Program:
        messages.debug_message("Reading program from '{}'".format(filename))

        the_program = Program(filename)
        cfgs = []
        calls = []
        with open(filename) as json_file:
            program_json = json.load(json_file)
            for subprogram_name, (vertices_json, edges_json) in program_json.items():
                cfg = graphs.ControlFlowGraph(the_program, subprogram_name)
                cfgs.append(cfg)

                for vertex_json in vertices_json:
                    vertex_id = int(vertex_json[0])
                    vertex = vertices.Vertex(int(vertex_id))
                    cfg.add_vertex(vertex)

                    instruction_json = vertex_json[1]
                    for instruction in instruction_json:
                        if instruction[0] == instructions.CallInstruction.opcode:
                            callee = instruction[1]
                            calls.append([cfg.name, callee, vertex])

                for edge_json in edges_json:
                    predecessor_id, successor_id = edge_json
                    predecessor = vertices.Vertex.id_pool[int(predecessor_id)]
                    successor = vertices.Vertex.id_pool[int(successor_id)]
                    cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))

        for cfg in cfgs:
            call = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), cfg.name)
            subprogram = Subprogram(cfg, call)
            the_program.add_subprogram(subprogram)

            for vertex in cfg:
                if len(cfg.predecessors(vertex)) == 0:
                    assert cfg.entry is None
                    cfg.entry = vertex

                if len(cfg.successors(vertex)) == 0:
                    assert cfg.exit is None
                    cfg.exit = vertex

        for caller, callee, site in calls:
            caller = the_program[caller].call_vertex
            callee = the_program[callee].call_vertex
            the_program.call_graph.add_edge(edges.CallGraphEdge(caller, callee, site))

        #More comments

        return the_program
