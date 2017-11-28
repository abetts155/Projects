import collections
import os

from ..graphs import graph
from ..graphs import vertex
from ..graphs import edge


class Program:
    def __init__(self, filename):
        self._filename = filename
        self._call_graph = graph.CallGraph()
        self._control_flow_graphs = collections.OrderedDict()
        self._instrumentation_point_graphs = collections.OrderedDict()

    def __setitem__(self, name, cfg):
        assert name not in self._control_flow_graphs, 'Already have information on subprogram {}'.format(name)
        self._control_flow_graphs[name] = cfg
        call_vertex = vertex.SubprogramVertex(self._call_graph.get_vertex_id(), name)
        self._call_graph.add_vertex(call_vertex)

    def __getitem__(self, name) -> graph.ControlFlowGraph:
        assert name in self._control_flow_graphs, 'No control flow graph for {}'.format(name)
        return self._control_flow_graphs[name]

    def __delitem__(self, name):
        assert name in self._control_flow_graphs, 'No control flow graph for {}'.format(name)
        del self._control_flow_graphs[name]
        self._call_graph.remove_vertex(self._call_graph[name])

    def __contains__(self, name):
        return name in self._control_flow_graphs

    def __len__(self):
        return len([callv for callv in self.call_graph.subprograms_under_analysis()])

    def __iter__(self):
        for cfg in self._control_flow_graphs.values():
            yield cfg

    def basename(self):
        base, _ = os.path.splitext(os.path.abspath(self._filename))
        base = os.path.dirname(base)
        path = base + os.sep
        if not os.path.exists(path):
            os.mkdir(path)
        return path

    @property
    def call_graph(self) -> graph.CallGraph:
        return self._call_graph

    def set_root(self, root_name):
        self.call_graph.root = self.call_graph[root_name]

    def add_ipg(self, ipg):
        self._instrumentation_point_graphs[ipg.name] = ipg

    def get_ipg(self, name):
        assert name in self._instrumentation_point_graphs, 'No instrumentation point graph for {}'.format(name)
        return self._instrumentation_point_graphs[name]

    class IO:
        SUBPROGRAM = "SUBPROGRAM"
        VERTEX = "VERTEX"
        INTRA_PROCEDURAL = "INTRA"
        INTER_PROCEDURAL = "CALLS"

        @staticmethod
        def write(prog, filename):
            with open(filename, 'w') as wd:
                for cfg in prog:
                    call_v = prog.call_graph[cfg.name]
                    wd.write('{} {}'.format(Program.IO.SUBPROGRAM, cfg.name))
                    wd.write('\n')
                    for v in cfg.vertices:
                        wd.write('{} {}'.format(Program.IO.VERTEX, str(v.id)))
                        wd.write('\n')
                        transitions = [str(e.successor.id) for e in cfg.successors(v) if e.successor != cfg.entry]
                        wd.write('{} {}'.format(Program.IO.INTRA_PROCEDURAL, ' '.join(transitions)))
                        wd.write('\n')
                        callees = [e.successor.name for e in prog.call_graph.successors(call_v) if v in e.call_sites]
                        wd.write('{} {}'.format(Program.IO.INTER_PROCEDURAL, ' '.join(callees)))
                        wd.write('\n' * 2)
                    wd.write('\n' * 3)

        @staticmethod
        def read(filename):
            def first_pass():
                # Add control flow graphs and vertices
                with open(filename, 'r') as rd:
                    cfg = None
                    for line in rd:
                        if line:
                            if line.startswith(Program.IO.SUBPROGRAM):
                                (_, subprg_name) = line.split()
                                cfg = graph.ControlFlowGraph(subprg_name)
                                prog[subprg_name] = cfg
                            elif line.startswith(Program.IO.VERTEX):
                                assert cfg is not None
                                (_, vertex_id) = line.split()
                                cfg.add_vertex(vertex.Vertex(int(vertex_id)))

            def second_pass():
                # Add intra-procedural and inter-procedural edges
                call_graph_edges = {}
                with open(filename, 'r') as rd:
                    cfg = None
                    p = None
                    for line in rd:
                        if line:
                            if line.startswith(Program.IO.SUBPROGRAM):
                                (_, subprg_name) = line.split()
                                cfg = prog[subprg_name]
                            elif line.startswith(Program.IO.VERTEX):
                                assert cfg is not None
                                (_, vertex_id) = line.split()
                                p = cfg.get_vertex_data(int(vertex_id)).v

                            elif line.startswith(Program.IO.INTRA_PROCEDURAL):
                                assert cfg is not None
                                assert p is not None
                                (_, *ids) = line.split()
                                for id in ids:
                                    s = cfg.get_vertex_data(int(id)).v
                                    e = edge.Edge(p, s)
                                    cfg.add_edge(e)

                            elif line.startswith(Program.IO.INTER_PROCEDURAL):
                                assert cfg is not None
                                assert p is not None
                                (_, *callees) = line.split()
                                if callees:
                                    (callee,) = callees
                                    call_graph_edges.setdefault((cfg.name, callee), []).append(p)

                for (caller, callee), sites in call_graph_edges.items():
                    e = edge.CallGraphEdge(prog.call_graph[caller], prog.call_graph[callee], sites)
                    prog.call_graph.add_edge(e)

            prog = Program(filename)
            first_pass()
            second_pass()

            for cfg in prog:
                (cfg.entry,) = [v for v in cfg.vertices if len(cfg.predecessors(v)) == 0]
                (cfg.exit,) = [v for v in cfg.vertices if len(cfg.successors(v)) == 0]

            (prog.call_graph.root,) = [v for v in prog.call_graph.vertices if len(cfg.predecessors(v)) == 0]

            return prog


