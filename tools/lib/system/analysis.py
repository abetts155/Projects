import collections
import numpy
import random

from concurrent.futures import ThreadPoolExecutor

from lib.system.vertices import SubprogramVertex, is_basic_block
from lib.system.directed_graphs import (DepthFirstSearch,
                                        CallGraph,
                                        InstrumentationPointGraph)
from lib.system.vertices import ProgramPointVertex
from lib.utils import globals
from lib.utils import dot
from lib.utils.debug import verbose_message


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

    def is_instrumented(self, program_point):
        try:
            return self._instrumented[program_point]
        except KeyError:
            return False

    def get_all_instrumentation_points(self):
        return self._instrumented.keys()

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


class Program:
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like a call-context graph, may be included as well, but it depends on
    the purpose of the analysis.
    """

    def __init__(self):
        self._call_graph = CallGraph()
        self._control_flow_graphs = collections.OrderedDict()

    @property
    def call_graph(self):
        return self._call_graph

    @call_graph.setter
    def call_graph(self, value):
        self._call_graph = value

    def find_subprogram_with_program_point(self, program_point):
        for control_flow_graph in self:
            if control_flow_graph.has_vertex_for_program_point(program_point):
                return control_flow_graph.name
        assert False

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


def build_instrumentation_point_graphs(program : Program):
    instrumentation_point_graphs = {}
    depth_first_search_tree = DepthFirstSearch(program.call_graph,
                                               program.call_graph.root_function,
                                               False)
    # Traverse call graph in reverse topological order
    for call_vertex in depth_first_search_tree.post_order:
        verbose_message("Analysing function '{}'".format(call_vertex.name), __name__)
        caller_ipg = InstrumentationPointGraph.instrument_as_per_user_instruction\
            (program[call_vertex.name])
        instrumentation_point_graphs[call_vertex.name] = caller_ipg

        # Process every call made by this function
        for call_edge in call_vertex.successor_edge_iterator():
            callee_vertex = program.call_graph.get_vertex(call_edge.vertex_id)
            verbose_message("@ callee {}".format(callee_vertex.name), __name__)
            callee_ipg = instrumentation_point_graphs[callee_vertex.name]
            # Grab each instrumentation point from the callee that may be executed
            # first when the function begins
            entry_sign_posts = []
            if not callee_ipg.entry_vertex.abstract:
                entry_sign_posts.append(callee_ipg.entry_vertex)
            else:
                for succ_edge in callee_ipg.entry_vertex.successor_edge_iterator():
                    entry_sign_posts.append(callee_ipg.get_vertex(succ_edge.vertex_id))
            # Grab each instrumentation point from the callee that may be executed
            # last before the function returns
            exit_sign_posts = []
            if not callee_ipg.exit_vertex.abstract:
                exit_sign_posts.append(callee_ipg.exit_vertex)
            else:
                for pred_edge in callee_ipg.exit_vertex.predecessor_edge_iterator():
                    exit_sign_posts.append(callee_ipg.get_vertex(pred_edge.vertex_id))

            for call_site in call_edge.call_sites:
                verbose_message("@ call site {}".format(call_site), __name__)
                # Inline the sign posts to callee entry
                inlined_entry_program_points = set()
                for vertex in entry_sign_posts:
                    vertex_copy = ProgramPointVertex(caller_ipg.get_new_vertex_id(),
                                                     vertex.program_point,
                                                     abstract=True)
                    inlined_entry_program_points.add(vertex_copy)
                    caller_ipg.add_vertex(vertex_copy)
                # Inline the sign posts to callee exit
                inlined_exit_program_points = set()
                for vertex in exit_sign_posts:
                    vertex_copy = ProgramPointVertex(caller_ipg.get_new_vertex_id(),
                                                     vertex.program_point,
                                                     abstract=True)
                    inlined_exit_program_points.add(vertex_copy)
                    caller_ipg.add_vertex(vertex_copy)

                # Link the inlined instrumentation points
                for pred_vertex in inlined_entry_program_points:
                    for succ_vertex in inlined_exit_program_points:
                        caller_ipg.add_edge(pred_vertex, succ_vertex, None)

                # Relink the call site vertex
                edges_to_purge = set()
                call_site_vertex = caller_ipg.get_vertex_for_program_point(call_site)
                for pred_vertex in inlined_exit_program_points:
                    for redundant_succ_edge in call_site_vertex.successor_edge_iterator():
                        succ_vertex = caller_ipg.get_vertex(redundant_succ_edge.vertex_id)
                        caller_ipg.add_edge(pred_vertex, succ_vertex, None)
                        edges_to_purge.add((call_site_vertex, succ_vertex))
                        if redundant_succ_edge.backedge:
                            pred_edge = succ_vertex.get_predecessor_edge(pred_vertex.vertex_id)
                            succ_edge = pred_vertex.get_successor_edge(succ_vertex.vertex_id)
                            pred_edge.backedge = True
                            succ_edge.backedge = True

                for edge in edges_to_purge:
                    caller_ipg.remove_edge(*edge)

                for succ_vertex in inlined_entry_program_points:
                    caller_ipg.add_edge(call_site_vertex, succ_vertex, None)
        dot.make_file(caller_ipg)
    return instrumentation_point_graphs


def instrument_branches(program : Program):
    def do_instrumentation(args):
        control_flow_graph, call_vertex = args[0], args[1]
        call_sites = call_vertex.call_sites()
        for vertex in control_flow_graph:
            if vertex.number_of_successors() > 1:
                for succ_edge in vertex.successor_edge_iterator():
                    control_flow_graph.program_point_data.set_instrumented((vertex.vertex_id, succ_edge.vertex_id),
                                                                           True)
            if vertex.vertex_id in call_sites:
                control_flow_graph.program_point_data.set_instrumented(vertex.vertex_id, True)
            if vertex.vertex_id == control_flow_graph.exit_vertex.vertex_id:
                control_flow_graph.program_point_data.set_instrumented(vertex.vertex_id, True)
            if vertex.vertex_id == control_flow_graph.entry_vertex.vertex_id and call_vertex.number_of_predecessors() == 0:
                control_flow_graph.program_point_data.set_instrumented(vertex.vertex_id, True)

    work = [(control_flow_graph, program.call_graph.get_vertex_with_name(control_flow_graph.name)) for control_flow_graph in program]
    with ThreadPoolExecutor(min(len(program), 10)) as executor:
        result = executor.map(do_instrumentation, work)


