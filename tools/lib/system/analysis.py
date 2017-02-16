import collections
import numpy
import random

from lib.system.vertices import SubprogramVertex, is_basic_block
from lib.system import directed_graphs


class Program:
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like a call-context graph, may be included as well, but it depends on
    the purpose of the analysis.
    """

    def __init__(self):
        self._call_graph = directed_graphs.CallGraph()
        self._control_flow_graphs = collections.OrderedDict()

    @property
    def call_graph(self):
        return self._call_graph

    @call_graph.setter
    def call_graph(self, value):
        self._call_graph = value

    def __delitem__(self, function_name):
        try:
            del self._control_flow_graphs[function_name]
            call_vertex = self._call_graph.get_vertex_with_name(function_name)
            self._call_graph.remove_vertex(call_vertex)
        except KeyError:
            raise KeyError('No function called {} to delete'.
                           format(function_name))

    def __setitem__(self, function_name, control_flow_graph):
        assert function_name not in self._control_flow_graphs, \
            'Duplicate control flow graph with name {}'. \
                format(function_name)
        self._control_flow_graphs[function_name] = control_flow_graph
        self._call_graph.add_vertex(SubprogramVertex
                                    (self._call_graph.get_new_vertex_id(),
                                     control_flow_graph.name))

    def __getitem__(self, function_name):
        try:
            return self._control_flow_graphs[function_name]
        except KeyError:
            raise KeyError('Unable to find control flow graph for function {}'. \
                           format(function_name))

    def __contains__(self, function_name):
        return function_name in self._control_flow_graphs

    def __iter__(self):
        for control_flow_graph in self._control_flow_graphs.values():
            yield control_flow_graph

    def __len__(self):
        return len(self._control_flow_graphs)


class ProgramPointData:
    def __init__(self, control_flow_graph):
        self._control_flow_graph = control_flow_graph
        self._wcets = {}
        self._loop_bounds = {}
        self._instrumented = {}

    def set_wcet(self, program_point, value):
        self._wcets[program_point] = value

    def set_loop_bound(self, program_point, value):
        self._loop_bounds[program_point] = value

    def set_instrumented(self, program_point, value):
        self._instrumented[program_point] = value

    def get_wcet(self, program_point):
        try:
            return self._wcets[program_point]
        except KeyError:
            raise KeyError('No WCET value for program point {}'.
                           format(program_point))

    def get_loop_bound(self, program_point):
        try:
            return self._loop_bounds[program_point]
        except KeyError:
            raise KeyError('No loop bound for program point {}'.
                           format(program_point))

    def get_instrumented(self, program_point):
        try:
            return self._instrumented[program_point]
        except KeyError:
            if program_point is None:
                # This is a dummy vertex added for analysis purposes.  Assume
                # that it is not instrumented.
                return False
            raise KeyError('No information on whether program point {} is'
                           ' instrumented'.format(program_point))

    def create_timing_data(self, redo=False):
        for vertex in self._control_flow_graph:
            if is_basic_block(vertex.program_point):
                if vertex.program_point not in self._wcets or redo:
                    self._wcets[vertex.program_point] = random.randint(1, 20)
            else:
                self._wcets[vertex.program_point] = 0

        maximum_loop_bound = random.randint(1, globals.args['max_loop_bound'])

        def create_loop_bound_tuple_for_header(level, abstract_vertex):
            if level == 0:
                return numpy.array([1])
            elif level == 1:
                return numpy.array([random.randint(1, maximum_loop_bound)])
            else:
                parent_abstract_vertex = loop_nesting_tree.get_vertex \
                    (abstract_vertex.
                     get_ith_predecessor_edge(0).
                     vertex_id)
                loop_bound = []
                for number_of_iterations in self._loop_bounds[parent_abstract_vertex.
                        program_point]:
                    for _ in range(1, number_of_iterations + 1):
                        loop_bound.append(random.randint(1, maximum_loop_bound))
                return numpy.array(loop_bound)

        loop_nesting_tree = self._control_flow_graph.get_loop_nesting_tree()
        for level, tree_vertices in loop_nesting_tree. \
                level_by_level_iterator(False, True):
            for abstract_vertex in tree_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    header = self._control_flow_graph.get_vertex_for_program_point \
                        (abstract_vertex.program_point)
                    if header not in self._loop_bounds or redo:
                        self._loop_bounds[header.program_point] = \
                            create_loop_bound_tuple_for_header(level, abstract_vertex)


def instrument_branches(program : Program):
    for control_flow_graph in program:
        data = ProgramPointData(control_flow_graph)
        for vertex in control_flow_graph:
            if vertex.number_of_successors() > 1:
                for succ_edge in vertex.successor_edge_iterator():
                    pass  # print(vertex.vertex_id, succ_edge.vertex_id)