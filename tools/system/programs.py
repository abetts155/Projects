from collections import OrderedDict
from graphs import edges, graphs, vertices
from low_level import instructions
from random import randint
from typing import List
from utils import messages

import json
import os


class Subprogram:
    def __init__(self, cfg: graphs.ControlFlowGraph, call_vertex: vertices.SubprogramVertex):
        if not cfg.name == call_vertex.name:
            raise ValueError('Subprogram name mismatch: Found {} and {}'.format(cfg.name, call_vertex.name))
        self._cfg = cfg
        self._call_vertex = call_vertex
        self._lnt = None
        self._ipg = None

    @property
    def name(self):
        return self._cfg.name

    @property
    def cfg(self) -> graphs.ControlFlowGraph:
        return self._cfg

    @property
    def lnt(self) -> graphs.LoopNest:
        return self._lnt

    @lnt.setter
    def lnt(self, lnt):
        self._lnt = lnt

    @property
    def call_vertex(self) -> vertices.SubprogramVertex:
        return self._call_vertex

    @property
    def ipg(self):
        return self._ipg

    @ipg.setter
    def ipg(self, ipg):
        self._ipg = ipg


class Program:
    def __init__(self, filename):
        self._magic = ''.join('{}'.format(randint(0, 9)) for _ in range(32))
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

    def __len__(self):
        return len(self._subprograms)

    @property
    def call_graph(self) -> graphs.CallGraph:
        return self._call_graph

    @property
    def magic(self):
        return self._magic

    @magic.setter
    def magic(self, value):
        self._magic = value

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
    def write(cls, program: Program, filename: str):
        subprograms_json = {}
        program_json = [['magic', program.magic], subprograms_json]
        for subprogram in program:
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
                    else:
                        instruction_json.append([instruction.opcode])

                for edge in subprogram.cfg.successors(vertex):
                    if edge.successor() != subprogram.cfg.entry:
                        edges_json.append((edge.predecessor().id_, edge.successor().id_))

            subprograms_json[subprogram.name] = [vertices_json, edges_json]

        with open(filename, 'w') as outfile:
            json.dump(program_json, outfile, indent=2)
            outfile.write('\n')


    @classmethod
    def read(cls, filename: str) -> Program:
        messages.debug_message("Reading program from '{}'".format(filename))

        program = Program(filename)
        cfgs = []
        calls = []
        with open(filename) as json_file:
            program_json = json.load(json_file)
            magic_info = program_json[0]
            program.magic = magic_info[1]
            subprograms_json = program_json[1]
            for subprogram_name, (vertices_json, edges_json) in subprograms_json.items():
                cfg = graphs.ControlFlowGraph(program, subprogram_name)
                cfgs.append(cfg)

                for vertex_json in vertices_json:
                    vertex_id = int(vertex_json[0])
                    vertex = vertices.BasicBlock(int(vertex_id))
                    cfg.add_vertex(vertex)

                    instruction_json = vertex_json[1]
                    for instruction_text in instruction_json:
                        if instruction_text[0] == instructions.CallInstruction.OPCODE:
                            callee = instruction_text[1]
                            calls.append([cfg.name, callee, vertex])
                            vertex.instructions.append(instructions.CallInstruction(callee))
                        elif instruction_text[0] == instructions.BranchInstruction.OPCODE:
                            vertex.instructions.append(instructions.BranchInstruction())
                        elif instruction_text[0] == instructions.StoreInstruction.OPCODE:
                            vertex.instructions.append(instructions.StoreInstruction())
                        elif instruction_text[0] == instructions.LoadInstruction.OPCODE:
                            vertex.instructions.append(instructions.LoadInstruction())
                        elif instruction_text[0] == instructions.AddInstruction.OPCODE:
                            vertex.instructions.append(instructions.AddInstruction())
                        elif instruction_text[0] == instructions.SubtractInstruction.OPCODE:
                            vertex.instructions.append(instructions.SubtractInstruction())
                        elif instruction_text[0] == instructions.MultiplyInstruction.OPCODE:
                            vertex.instructions.append(instructions.MultiplyInstruction())
                        elif instruction_text[0] == instructions.DivideInstruction.OPCODE:
                            vertex.instructions.append(instructions.DivideInstruction())

                for edge_json in edges_json:
                    predecessor_id, successor_id = edge_json
                    predecessor = vertices.Vertex.id_pool[int(predecessor_id)]
                    successor = vertices.Vertex.id_pool[int(successor_id)]
                    cfg.add_edge(edges.ControlFlowEdge(predecessor, successor))

        for cfg in cfgs:
            call = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), cfg.name)
            subprogram = Subprogram(cfg, call)
            program.add_subprogram(subprogram)

            for vertex in cfg:
                if len(cfg.predecessors(vertex)) == 0:
                    assert cfg.entry is None
                    cfg.entry = vertex

                if len(cfg.successors(vertex)) == 0:
                    assert cfg.exit is None
                    cfg.exit = vertex

            cfg.add_edge(edges.ControlFlowEdge(cfg.exit, cfg.entry))

        for caller, callee, site in calls:
            caller = program[caller].call_vertex
            callee = program[callee].call_vertex
            program.call_graph.add_edge(edges.CallGraphEdge(caller, callee, site))

        return program
