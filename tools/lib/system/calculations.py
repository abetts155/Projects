import abc
import os
import timeit
import subprocess
import decimal
import re
import sys

from lib.utils import globals
from lib.utils import debug
from lib.system.vertices import (RegularExpressionVertex,
                                 ProgramPointVertex,
                                 is_basic_block,
                                 is_direct_transition)
from lib.system import directed_graphs
from lib.system import analysis

EDGE_VARIABLE_PREFIX = 'E_'
VERTEX_VARIABLE_PREFIX = 'V_'
TRANSITION_VARIABLE_PREFIX = 'T_'
WCET_VARIABLE_PREFIX = 'W_'


def do_wcet_calculation_for_instrumentation_point_graph(instrumentation_point_graph : directed_graphs.InstrumentationPointGraph,
                                                        transition_execution_times,
                                                        call_execution_times):
    strong_components = directed_graphs.StronglyConnectedComponents(instrumentation_point_graph)
    if len(strong_components) == instrumentation_point_graph.number_of_vertices():
        # If every instrumentation point belongs to its own strong component, then the IPG is acyclic.
        # Then we can calculate a WCET estimate during a topological traversal of instrumentation points.
        depth_first_search_tree = directed_graphs.DepthFirstSearch(instrumentation_point_graph,
                                                                   instrumentation_point_graph.entry_vertex,
                                                                   False)
        calculated_times = {}
        for vertex in reversed(depth_first_search_tree.post_order):
            calculated_times[vertex] = 0
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = instrumentation_point_graph.get_vertex(pred_edge.vertex_id)
                key = (pred_vertex, vertex)
                transition_time = 0 if key not in transition_execution_times else max(transition_execution_times[key])
                calculated_times[vertex] = max(calculated_times[vertex],
                                               transition_time + calculated_times[pred_vertex])
            if not is_basic_block(vertex.program_point) and not is_direct_transition(vertex.program_point):
                # Call detected
                calculated_times[vertex] = calculated_times[vertex] + call_execution_times[vertex.program_point]
        # The exit vertex of the IPG is annotated with the WCET estimate of the function
        return calculated_times[instrumentation_point_graph.exit_vertex]
    else:
        assert False


def compute_and_compare_wcet_estimates_between_cfg_and_ipg(program: analysis.Program):
    for repetition in range(1, globals.args['repeat'] + 1):
        debug.verbose_message('Repetition {}'.format(repetition), __name__)
        for control_flow_graph in program:
            loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
            control_flow_graph.program_point_data.create_timing_data(True)

            ilp_for_control_flow_graph = \
                IntegerLinearProgramForControlFlowGraph \
                    (control_flow_graph,
                     loop_nesting_tree,
                     control_flow_graph.program_point_data)
            ilp_for_control_flow_graph.solve()

            ilp_for_instrumentation_point_graph = \
                IntegerLinearProgramForInstrumentationPointGraph \
                    (control_flow_graph.get_instrumentation_point_graph(),
                     loop_nesting_tree,
                     control_flow_graph.program_point_data)
            ilp_for_instrumentation_point_graph.solve()

            assert ilp_for_instrumentation_point_graph.wcet == ilp_for_control_flow_graph.wcet, \
                'IPG = {}, CFG = {}'.format(ilp_for_instrumentation_point_graph.wcet,
                                            ilp_for_control_flow_graph.wcet)

            print('>', control_flow_graph.name)
            print('control_flow_graph')
            print('WCET         = {}'.format(ilp_for_control_flow_graph.wcet))
            print('Variables    = {}'.format(len(ilp_for_control_flow_graph.variables)))
            print('Constraints  = {}'.format(len(ilp_for_control_flow_graph.constraints)))
            print('Construction = {}'.format(ilp_for_control_flow_graph.construction_time))
            print('Solve        = {}'.format(ilp_for_control_flow_graph.solve_time))
            print('-' * 25)
            print('instrumentation_point_graph')
            print('WCET         = {}'.format(ilp_for_instrumentation_point_graph.wcet))
            print('Variables    = {}'.format(len(ilp_for_instrumentation_point_graph.variables)))
            print('Constraints  = {}'.format(len(ilp_for_instrumentation_point_graph.constraints)))
            print('Construction = {}'.format(ilp_for_instrumentation_point_graph.construction_time))
            print('Solve        = {}'.format(ilp_for_instrumentation_point_graph.solve_time))


def calculate_wcet_using_integer_linear_programming(program):
    if globals.args['output']:
        log_file = open(os.path.abspath(globals.args['output']), 'w')
        old_stdout = sys.stdout
        sys.stdout = log_file
    try:
        for repetition in range(1, globals.args['repeat'] + 1):
            debug.verbose_message('Repetition {}'.format(repetition), __name__)
            for control_flow_graph in program:
                # The reason we grab the loop-nesting tree here and not inside the
                # constraint system is because we time how long it takes to 
                # construct the constraint system.  Since the loop-nesting tree is
                # built on the fly, it may happen that a portion of the time to
                # construct the constraint system is unfairly attributed to that
                # activity.
                loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
                control_flow_graph.program_point_data.create_timing_data(True)
                ilp_for_control_flow_graph = IntegerLinearProgramForControlFlowGraph \
                    (control_flow_graph,
                     loop_nesting_tree,
                     control_flow_graph.program_point_data)
                ilp_for_control_flow_graph.solve()

                ilp_for_super_block_graph = IntegerLinearProgramForSuperBlockGraph \
                    (control_flow_graph,
                     loop_nesting_tree,
                     control_flow_graph.program_point_data)
                ilp_for_super_block_graph.solve()

                assert ilp_for_super_block_graph.wcet == ilp_for_control_flow_graph.wcet

                print('>', control_flow_graph.name)
                print('control_flow_graph')
                print(ilp_for_control_flow_graph)
                print('super_blocks')
                print(ilp_for_super_block_graph)

                if globals.args['folded']:
                    ilp_with_folding = \
                        IntegerLinearProgramForSuperBlockGraphWithFolding \
                            (control_flow_graph,
                             loop_nesting_tree,
                             control_flow_graph.program_point_data)
                    ilp_with_folding.solve()

                    assert ilp_with_folding.wcet == ilp_for_control_flow_graph.wcet
                    print('super_blocks_folded')
                    print(ilp_with_folding)
    finally:
        if globals.args['output']:
            log_file.close()
            sys.stdout = old_stdout


def get_variable_for_program_point(program_point, variables):
    if is_basic_block(program_point):
        variable = '{}{}'.format(VERTEX_VARIABLE_PREFIX,
                                 program_point)
    else:
        variable = '{}{}_{}'.format(EDGE_VARIABLE_PREFIX,
                                    program_point[0],
                                    program_point[1])
    variables.add(variable)
    return variable


def get_variable_for_edge_between_program_points(program_point_one,
                                                 program_point_two,
                                                 variables):
    variable = TRANSITION_VARIABLE_PREFIX
    if is_basic_block(program_point_one):
        variable += '{}'.format(program_point_one)
    else:
        variable += '{}_{}'.format(program_point_one[0], program_point_one[1])
    variable += '.'
    if is_basic_block(program_point_two):
        variable += '{}'.format(program_point_two)
    else:
        variable += '{}_{}'.format(program_point_two[0], program_point_two[1])
    variables.add(variable)
    return variable


def get_vertex_wcet_variable(vertex_id):
    return '{}{}'.format(WCET_VARIABLE_PREFIX, vertex_id)


def get_new_line(num=1):
    return '\n' * num


class ConstraintSystem:
    """
    A constraint system that solves to a WCET estimate.
    """

    __metaclass__ = abc.ABCMeta

    def __init__(self):
        self._filename = None
        self._wcet = None
        self._solve_time = None
        self._construction_time = None
        self._variable_execution_counts = {}
        self._constraints = set()
        self._variables = set()

    @property
    def wcet(self):
        return self._wcet

    @property
    def solve_time(self):
        return self._solve_time

    @property
    def construction_time(self):
        return self._construction_time

    @property
    def variables(self):
        return self._variables

    @property
    def constraints(self):
        return self._constraints

    def __str__(self):
        return """WCET: {}
variables: {}
constraints: {}
construction: {}
solve: {}""".format(self._wcet,
                    len(self._variables),
                    len(self._constraints),
                    self._construction_time,
                    self._solve_time)


class SolverError(Exception):
    """
    Exception to catch when a call to a solver fails.
    """

    def __init__(self, message):
        Exception.__init__(self, message)


class IntegerLinearProgram(ConstraintSystem):
    """
    A constraint system that contains linear constraints only.
    """

    def _create_integer_constraint(self):
        self.int_constraint = 'int'
        counter = len(self._variables)
        for variable in self._variables:
            self.int_constraint += ' {}'.format(variable)
            if counter > 1:
                self.int_constraint += ','
            counter -= 1
        self.int_constraint += ';'

    def _write_to_file(self):
        with open(self._filename, 'w') as the_file:
            the_file.write(self.obj_function)
            the_file.write(get_new_line(2))
            for constraint in self._constraints:
                the_file.write(constraint)
                the_file.write(get_new_line())
            the_file.write(get_new_line())
            the_file.write(self.int_constraint)
            the_file.write(get_new_line())

    def solve(self):
        self._write_to_file()

        # Launch lp_solve with the created file
        command = 'lp_solve {}'.format(self._filename)
        start = timeit.default_timer()
        process = subprocess.Popen(command,
                                   shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
        stdout, _ = process.communicate()
        end = timeit.default_timer()
        self._solve_time = end - start

        if process.returncode != 0:
            raise SolverError('Running {} failed'.format(command))

        # Grab the WCET estimate and the execution counts of program
        # points in the control flow graph
        stdout = stdout.decode()
        for line in stdout.split(os.linesep):
            line = line.strip()
            # Process if the line is not whitespace and has digits in it
            if line and re.search('\d+', line):
                lexemes = re.split('\s+', line)
                if len(lexemes) == 2:
                    self._variable_execution_counts[lexemes[0]] = \
                        int(decimal.Decimal(lexemes[1]))
                else:
                    self._wcet = int(round(float(lexemes[-1])))

        # We only need the ILP file for debugging purposes        
        if not globals.args['debug'] and os.path.exists(self._filename):
            os.remove(self._filename)

    def output(self, function_name):
        print()
        print('wcet({})={}'.format(function_name, self._wcet))
        for variable in sorted(self._variable_execution_counts.keys()):
            if self._variable_execution_counts[variable] > 0:
                print('{} = {}'.format(variable,
                                       self._variable_execution_counts[variable]))


class IntegerLinearProgramForControlFlowGraph(IntegerLinearProgram):
    """
    The integer linear program derived from a control flow graph, execution 
    times of basic blocks and upper bounds on loop header execution counts.
    """

    def __init__(self,
                 control_flow_graph,
                 loop_nesting_tree,
                 program_point_data):
        ConstraintSystem.__init__(self)
        self._filename = '{}.{}.cfg.ilp'.format(globals.args['filename_prefix'],
                                                control_flow_graph.name)
        start = timeit.default_timer()
        self.__create_objective_function(control_flow_graph,
                                         program_point_data)
        self.__create_structural_constraints(control_flow_graph)
        self.__create_loop_bound_constraints(control_flow_graph,
                                             loop_nesting_tree,
                                             program_point_data)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self._construction_time = end - start

    def __create_objective_function(self,
                                    control_flow_graph,
                                    program_point_data):
        self.obj_function = 'max: '
        counter = len(control_flow_graph.basic_block_vertices)
        for vertex in control_flow_graph:
            if is_basic_block(vertex.program_point):
                vertex_variable = get_variable_for_program_point(vertex.program_point,
                                                                 self._variables)
                self.obj_function += '{} {}'.format \
                    (program_point_data.get_wcet(vertex.program_point),
                     vertex_variable)
                if counter > 1:
                    self.obj_function += ' + '
                counter -= 1
        self.obj_function += ';'

    def __create_structural_constraints(self, control_flow_graph):
        for vertex in control_flow_graph:
            if vertex.number_of_predecessors() == 1:
                pred_vertex = control_flow_graph.get_vertex \
                    (vertex.get_ith_predecessor_edge(0).vertex_id)
                if pred_vertex.number_of_successors() == 1:
                    constraint = get_variable_for_program_point \
                        (vertex.program_point, self._variables)
                    constraint += ' = '
                    constraint += get_variable_for_program_point \
                        (pred_vertex.program_point, self._variables)
                    constraint += ';'
                    self._constraints.add(constraint)
            else:
                vertex_variable = get_variable_for_program_point \
                    (vertex.program_point,
                     self._variables)
                constraint = vertex_variable
                constraint += ' = '
                counter = vertex.number_of_predecessors()
                for pred_edge in vertex.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph. \
                        get_vertex(pred_edge.vertex_id)
                    pred_vertex_variable = get_variable_for_program_point \
                        (pred_vertex.program_point,
                         self._variables)
                    constraint += pred_vertex_variable
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
                self._constraints.add(constraint)

            if vertex.number_of_successors() > 1:
                vertex_variable = get_variable_for_program_point \
                    (vertex.program_point,
                     self._variables)
                constraint = vertex_variable
                constraint += ' = '
                counter = vertex.number_of_successors()
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = control_flow_graph. \
                        get_vertex(succ_edge.vertex_id)
                    succ_vertex_variable = get_variable_for_program_point \
                        (succ_vertex.program_point,
                         self._variables)
                    constraint += succ_vertex_variable
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
                self._constraints.add(constraint)

    def __create_loop_bound_constraints(self,
                                        control_flow_graph,
                                        loop_nesting_tree,
                                        program_point_data):

        def create_local_loop_bound_constraint(abstract_vertex):
            constraint = get_variable_for_program_point(abstract_vertex.program_point,
                                                        self._variables)
            if (abstract_vertex.program_point ==
                    loop_nesting_tree.root_vertex.program_point):
                constraint += ' = {};'.format(max
                                              (program_point_data.get_loop_bound
                                               (abstract_vertex.program_point)))
            else:
                loop_body = loop_nesting_tree. \
                    get_loop_body_for_program_point(abstract_vertex.
                                                    program_point)
                header = control_flow_graph.get_vertex_for_program_point \
                    (abstract_vertex.program_point)
                loop_entry_predecessor_vertices = set()
                for pred_edge in header.predecessor_edge_iterator():
                    pred_tree_vertex = loop_nesting_tree.get_vertex(pred_edge.vertex_id)
                    if pred_tree_vertex not in loop_body:
                        loop_entry_predecessor_vertices.add(pred_tree_vertex)

                constraint += ' <= '
                counter = len(loop_entry_predecessor_vertices)
                for pred_vertex in loop_entry_predecessor_vertices:
                    constraint += '{} {}'.format(max
                                                 (program_point_data.get_loop_bound
                                                  (abstract_vertex.program_point)),
                                                 get_variable_for_program_point
                                                 (pred_vertex.program_point,
                                                  self._variables))
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
            self._constraints.add(constraint)

        def create_global_loop_bound_constraint(abstract_vertex):
            constraint = get_variable_for_program_point(abstract_vertex.program_point,
                                                        self._variables)
            constraint += ' <= '
            constraint += '{};'.format(sum
                                       (program_point_data.get_loop_bound
                                        (abstract_vertex.program_point)))
            self._constraints.add(constraint)

        for level, tree_vertices in loop_nesting_tree. \
                level_by_level_iterator \
                    (abstract_vertices_only=True):
            for abstract_vertex in tree_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    create_local_loop_bound_constraint(abstract_vertex)
                    if level > 1:
                        create_global_loop_bound_constraint(abstract_vertex)


class IntegerLinearProgramForSuperBlockGraph(IntegerLinearProgram):
    """
    The integer linear program derived from a super block graph.
    """

    def __init__(self,
                 control_flow_graph,
                 loop_nesting_tree,
                 program_point_data):
        ConstraintSystem.__init__(self)
        self._filename = '{}.{}.super.ilp'.format(globals.args['filename_prefix'],
                                                  control_flow_graph.name)
        self.__loop_exit_edges = set()
        start = timeit.default_timer()
        self.__create_objective_function(control_flow_graph,
                                         program_point_data)
        self.__create_constraints(control_flow_graph,
                                  loop_nesting_tree,
                                  program_point_data)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self._construction_time = end - start

    def __create_objective_function(self,
                                    control_flow_graph,
                                    program_point_data):
        self.obj_function = 'max: '
        counter = len(control_flow_graph.basic_block_vertices)

        for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
                level_by_level_iterator \
                    (abstract_vertices_only=True):
            for abstract_vertex in abstract_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    # Rebuild the super block graph for this loop irrespective of
                    # whether it has been built previously.  In the end we want to
                    # account properly for the time to construct the ILP, and this
                    # includes the overhead associated with super block construction.
                    subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex,
                                                                           redo=True)
                    for super_vertex in subgraph:
                        for induced_vertex in super_vertex.vertices:
                            if is_basic_block(induced_vertex.
                                                      program_point) \
                                    and not induced_vertex.abstract:
                                vertex_variable = get_variable_for_program_point \
                                    (induced_vertex.program_point,
                                     self._variables)
                                wcet = program_point_data.get_wcet(induced_vertex.
                                                                   program_point)
                                self.obj_function += '{} {}'.format(wcet,
                                                                    vertex_variable)
                                if counter > 1:
                                    self.obj_function += ' + '
                                counter -= 1
        self.obj_function += ';'

    def __create_constraints(self,
                             control_flow_graph,
                             loop_nesting_tree,
                             program_point_data):
        for level, tree_vertices in loop_nesting_tree.level_by_level_iterator \
                    (abstract_vertices_only=True):
            for abstract_vertex in tree_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    subgraph = control_flow_graph.get_super_block_subgraph \
                        (abstract_vertex)
                    for super_vertex in subgraph:
                        self.__create_intra_super_block_constraints(super_vertex)

                        if super_vertex.number_of_predecessors() > 1:
                            self.__create_predecessor_super_block_constraints \
                                (subgraph, super_vertex)

                        if super_vertex.number_of_successors() > 1 \
                                and not super_vertex.representative.abstract:
                            self.__create_successor_super_block_constraints \
                                (control_flow_graph, subgraph, super_vertex)

                    self.__create_loop_bound_constraints(control_flow_graph,
                                                         loop_nesting_tree,
                                                         level,
                                                         abstract_vertex,
                                                         program_point_data)

    def __create_intra_super_block_constraints(self,
                                               super_vertex):
        for induced_vertex in super_vertex.vertices:
            if induced_vertex != super_vertex.representative \
                    and not induced_vertex.abstract:
                if is_basic_block(induced_vertex.program_point) \
                        or induced_vertex.program_point in self.__loop_exit_edges:
                    constraint = get_variable_for_program_point \
                        (induced_vertex.program_point,
                         self._variables)
                    constraint += ' = '
                    constraint += get_variable_for_program_point \
                        (super_vertex.representative.program_point,
                         self._variables)
                    constraint += ';'
                    self._constraints.add(constraint)

    def __create_predecessor_super_block_constraints(self,
                                                     subgraph,
                                                     super_vertex):

        if super_vertex.representative.abstract:
            # An abstract vertex in its own super block
            constraint = ''
            counter = super_vertex.number_of_successors()
            for succ_edge in super_vertex.successor_edge_iterator():
                super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
                constraint += get_variable_for_program_point(super_succ_vertex.
                                                             representative.
                                                             program_point,
                                                             self._variables)
                if counter > 1:
                    constraint += ' + '
                counter -= 1
        else:
            constraint = get_variable_for_program_point(super_vertex.
                                                        representative.
                                                        program_point,
                                                        self._variables)

        constraint += ' = '
        counter = super_vertex.number_of_predecessors()
        for pred_edge in super_vertex.predecessor_edge_iterator():
            super_pred_vertex = subgraph.get_vertex \
                (pred_edge.vertex_id)
            constraint += get_variable_for_program_point(super_pred_vertex.
                                                         representative.
                                                         program_point,
                                                         self._variables)
            if counter > 1:
                constraint += ' + '
            counter -= 1
        constraint += ';'
        self._constraints.add(constraint)

    def __create_successor_super_block_constraints(self,
                                                   control_flow_graph,
                                                   subgraph,
                                                   super_vertex):
        for _, partition in super_vertex.successor_edge_partition_iterator():
            if len(partition) > 1:
                constraint = get_variable_for_program_point(super_vertex.
                                                            representative.
                                                            program_point,
                                                            self._variables)
                constraint += ' = '
                counter = len(partition)
                for succ_edge in partition:
                    super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
                    constraint += get_variable_for_program_point \
                        (super_succ_vertex.
                         representative.
                         program_point,
                         self._variables)
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
                self._constraints.add(constraint)

    def __create_loop_bound_constraints(self,
                                        control_flow_graph,
                                        loop_nesting_tree,
                                        level,
                                        abstract_vertex,
                                        program_point_data):

        def get_subgraph_of_outer_loop(subgraph, super_vertex):
            for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
                    level_by_level_iterator \
                        (abstract_vertices_only=True):
                for abstract_vertex in abstract_vertices:
                    if is_basic_block(abstract_vertex.program_point):
                        candidate_subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
                        if subgraph.root_vertex.vertex_id != \
                                candidate_subgraph.root_vertex.vertex_id \
                                and candidate_subgraph.has_vertex_for_program_point(super_vertex.
                                                                                            representative.
                                                                                            program_point):
                            return candidate_subgraph

        def create_local_loop_bound_constraint(abstract_vertex):
            loop_bound_tuple = program_point_data.get_loop_bound(abstract_vertex.
                                                                 program_point)
            constraint = get_variable_for_program_point(abstract_vertex.
                                                        program_point,
                                                        self._variables)
            if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
                constraint += ' = {};'.format(max(loop_bound_tuple))
            else:
                constraint += ' <= '
                subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
                loop_exit_super_blocks = [super_vertex for super_vertex in subgraph
                                          if super_vertex.is_loop_exit_edge]
                parent_subgraph = get_subgraph_of_outer_loop(subgraph,
                                                             loop_exit_super_blocks[0])

                counter = len(loop_exit_super_blocks)
                for succ_vertex in loop_exit_super_blocks:
                    succ_vertex_in_parent_subgraph = \
                        parent_subgraph.get_vertex_for_program_point \
                            (succ_vertex.representative.program_point)
                    constraint += '{} {}'.format(max(loop_bound_tuple),
                                                 get_variable_for_program_point
                                                 (succ_vertex_in_parent_subgraph.
                                                  representative.
                                                  program_point,
                                                  self._variables))
                    self.__loop_exit_edges.add(succ_vertex.
                                               representative.
                                               program_point)
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
            self._constraints.add(constraint)

        def create_global_loop_bound_constraint(abstract_vertex):
            loop_bound_tuple = program_point_data.get_loop_bound(abstract_vertex.
                                                                 program_point)
            constraint = get_variable_for_program_point(abstract_vertex.program_point,
                                                        self._variables)
            constraint += ' <= '
            constraint += '{};'.format(sum(loop_bound_tuple))
            self._constraints.add(constraint)

        create_local_loop_bound_constraint(abstract_vertex)
        if level > 1:
            create_global_loop_bound_constraint(abstract_vertex)


class IntegerLinearProgramForSuperBlockGraphWithFolding(IntegerLinearProgram):
    """
    The integer linear program derived after pre-solving execution times of
    super blocks.
    """

    def __init__(self,
                 control_flow_graph,
                 loop_nesting_tree,
                 program_point_data):
        ConstraintSystem.__init__(self)
        self._filename = '{}.{}.folded.super.ilp'. \
            format(globals.args['filename_prefix'],
                   control_flow_graph.name)
        start = timeit.default_timer()
        self.__create_objective_function(control_flow_graph,
                                         program_point_data)
        self.__create_constraints(control_flow_graph,
                                  loop_nesting_tree,
                                  program_point_data)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self._construction_time = end - start

    def __create_objective_function(self,
                                    control_flow_graph,
                                    program_point_data):
        self.obj_function = 'max: '
        for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
                level_by_level_iterator \
                    (abstract_vertices_only=True):
            for abstract_vertex in abstract_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex,
                                                                           redo=True)
                    for super_vertex in subgraph:
                        super_block_wcet = 0
                        for induced_vertex in super_vertex.vertices:
                            if is_basic_block(induced_vertex.program_point) \
                                    and not induced_vertex.abstract:
                                super_block_wcet += program_point_data.get_wcet \
                                    (induced_vertex.program_point)

                        vertex_variable = get_variable_for_program_point \
                            (super_vertex.representative.program_point,
                             self._variables)
                        self.obj_function += '{} {}'.format(super_block_wcet,
                                                            vertex_variable)
                        self.obj_function += ' + '
        strip_index = self.obj_function.rfind('+')
        self.obj_function = self.obj_function[:strip_index]
        self.obj_function += ';'

    def __create_constraints(self,
                             control_flow_graph,
                             loop_nesting_tree,
                             program_point_data):
        for level, tree_vertices in loop_nesting_tree.level_by_level_iterator \
                    (abstract_vertices_only=True):
            for abstract_vertex in tree_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    subgraph = control_flow_graph.get_super_block_subgraph \
                        (abstract_vertex)
                    for super_vertex in subgraph:
                        if super_vertex.number_of_predecessors() > 1:
                            self.__create_predecessor_super_block_constraints \
                                (subgraph, super_vertex)

                        if super_vertex.number_of_successors() > 1 \
                                and not super_vertex.representative.abstract:
                            self.__create_successor_super_block_constraints \
                                (control_flow_graph, subgraph, super_vertex)

                    self.__create_loop_bound_constraints(control_flow_graph,
                                                         loop_nesting_tree,
                                                         level,
                                                         abstract_vertex,
                                                         program_point_data)

    def __create_predecessor_super_block_constraints(self,
                                                     subgraph,
                                                     super_vertex):
        if super_vertex.representative.abstract:
            # An abstract vertex in its own super block
            constraint = ''
            counter = super_vertex.number_of_successors()
            for succ_edge in super_vertex.successor_edge_iterator():
                super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
                constraint += get_variable_for_program_point(super_succ_vertex.
                                                             representative.
                                                             program_point,
                                                             self._variables)
                if counter > 1:
                    constraint += ' + '
                counter -= 1
        else:
            constraint = get_variable_for_program_point(super_vertex.
                                                        representative.
                                                        program_point,
                                                        self._variables)

        constraint += ' = '
        counter = super_vertex.number_of_predecessors()
        for pred_edge in super_vertex.predecessor_edge_iterator():
            super_pred_vertex = subgraph.get_vertex \
                (pred_edge.vertex_id)
            constraint += get_variable_for_program_point(super_pred_vertex.
                                                         representative.
                                                         program_point,
                                                         self._variables)
            if counter > 1:
                constraint += ' + '
            counter -= 1
        constraint += ';'
        self._constraints.add(constraint)

    def __create_successor_super_block_constraints(self,
                                                   control_flow_graph,
                                                   subgraph,
                                                   super_vertex):
        for _, partition in super_vertex.successor_edge_partition_iterator():
            if len(partition) > 1:
                constraint = get_variable_for_program_point(super_vertex.
                                                            representative.
                                                            program_point,
                                                            self._variables)
                constraint += ' = '
                counter = len(partition)
                for succ_edge in partition:
                    super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
                    constraint += get_variable_for_program_point \
                        (super_succ_vertex.
                         representative.
                         program_point,
                         self._variables)
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
                self._constraints.add(constraint)

    def __create_loop_bound_constraints(self,
                                        control_flow_graph,
                                        loop_nesting_tree,
                                        level,
                                        abstract_vertex,
                                        program_point_data):

        def get_subgraph_of_outer_loop(subgraph, super_vertex):
            for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
                    level_by_level_iterator \
                        (abstract_vertices_only=True):
                for abstract_vertex in abstract_vertices:
                    if is_basic_block(abstract_vertex.program_point):
                        candidate_subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
                        if subgraph.root_vertex.vertex_id != \
                                candidate_subgraph.root_vertex.vertex_id \
                                and candidate_subgraph.has_vertex_for_program_point(super_vertex.
                                                                                            representative.
                                                                                            program_point):
                            return candidate_subgraph

        def create_local_loop_bound_constraint(abstract_vertex):
            loop_exit_edge_in_parent_subgraph = {}
            loop_bound_tuple = program_point_data.get_loop_bound \
                (abstract_vertex.program_point)
            subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
            super_vertex = subgraph.get_vertex_for_program_point(abstract_vertex.
                                                                 program_point)
            constraint = get_variable_for_program_point(super_vertex.
                                                        representative.
                                                        program_point,
                                                        self._variables)
            if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
                constraint += ' = {};'.format(max(loop_bound_tuple))
            else:
                constraint += ' <= '
                loop_exit_super_blocks = [super_vertex for super_vertex in subgraph
                                          if super_vertex.is_loop_exit_edge]
                parent_subgraph = get_subgraph_of_outer_loop(subgraph,
                                                             loop_exit_super_blocks[0])

                counter = len(loop_exit_super_blocks)
                for succ_vertex in loop_exit_super_blocks:
                    succ_vertex_in_parent_subgraph = \
                        parent_subgraph.get_vertex_for_program_point \
                            (succ_vertex.representative.program_point)
                    constraint += '{} {}'.format(max(loop_bound_tuple),
                                                 get_variable_for_program_point
                                                 (succ_vertex_in_parent_subgraph.
                                                  representative.
                                                  program_point,
                                                  self._variables))
                    loop_exit_edge_in_parent_subgraph[succ_vertex] = \
                        succ_vertex_in_parent_subgraph
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
            self._constraints.add(constraint)

            # We need intra super block constraints on loop-exit edges.
            for super_vertex_one, super_vertex_two in loop_exit_edge_in_parent_subgraph.items():
                constraint = get_variable_for_program_point(super_vertex_one.
                                                            representative.
                                                            program_point, self._variables)
                constraint += ' = '
                constraint += get_variable_for_program_point(super_vertex_two.
                                                             representative.
                                                             program_point, self._variables)
                constraint += ';'
                self._constraints.add(constraint)

        def create_global_loop_bound_constraint(abstract_vertex):
            loop_bound_tuple = program_point_data.get_loop_bound \
                (abstract_vertex.program_point)
            subgraph = control_flow_graph.get_super_block_subgraph \
                (abstract_vertex)
            super_vertex = subgraph.get_vertex_for_program_point \
                (abstract_vertex.program_point)
            constraint = get_variable_for_program_point(super_vertex.
                                                        representative.
                                                        program_point,
                                                        self._variables)
            constraint += ' <= '
            constraint += '{};'.format(sum(loop_bound_tuple))
            self._constraints.add(constraint)

        create_local_loop_bound_constraint(abstract_vertex)
        if level > 1:
            create_global_loop_bound_constraint(abstract_vertex)


class IntegerLinearProgramForInstrumentationPointGraph(IntegerLinearProgram):
    """
    The integer linear program derived from a control flow graph, execution 
    times of basic blocks and upper bounds on loop header execution counts.
    """

    def __init__(self,
                 instrumentation_point_graph,
                 loop_nesting_tree,
                 program_point_data):
        ConstraintSystem.__init__(self)
        self._filename = '{}.{}.ipg.ilp'.format(globals.args['filename_prefix'],
                                                instrumentation_point_graph.name)
        start = timeit.default_timer()
        self.__create_objective_function(instrumentation_point_graph,
                                         program_point_data)
        self.__create_structural_constraints(instrumentation_point_graph)
        self.__create_loop_bound_constraints(instrumentation_point_graph,
                                             loop_nesting_tree,
                                             program_point_data)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self._construction_time = end - start

    def __evaluate_wcet_of_edge(self,
                                instrumentation_point_graph,
                                program_point_data,
                                edge):
        depth_first_search = directed_graphs.DepthFirstSearch(edge.path_expression,
                                                              edge.path_expression.root_vertex,
                                                              False)
        evaluations = {}
        for path_expression_vertex in depth_first_search.post_order:
            if isinstance(path_expression_vertex, ProgramPointVertex):
                vertex = instrumentation_point_graph.get_vertex_for_program_point \
                    (path_expression_vertex.program_point)
                evaluations[path_expression_vertex] = program_point_data.get_wcet(vertex.program_point)
            else:
                if path_expression_vertex.operator == RegularExpressionVertex.SEQUENCE:
                    wcet = 0
                    for succ_edge in path_expression_vertex.successor_edge_iterator():
                        path_expression_succ_vertex = \
                            edge.path_expression.get_vertex(succ_edge.vertex_id)
                        wcet += evaluations[path_expression_succ_vertex]
                    evaluations[path_expression_vertex] = wcet
                elif path_expression_vertex.operator == RegularExpressionVertex.ALTERNATIVE:
                    wcet = 0
                    for succ_edge in path_expression_vertex.successor_edge_iterator():
                        path_expression_succ_vertex = \
                            edge.path_expression.get_vertex(succ_edge.vertex_id)
                        wcet = max(wcet, evaluations[path_expression_succ_vertex])
                    evaluations[path_expression_vertex] = wcet

        return evaluations[edge.path_expression.root_vertex]

    def __create_objective_function(self,
                                    instrumentation_point_graph,
                                    program_point_data):
        self.obj_function = 'max: '

        counter = instrumentation_point_graph.number_of_vertices()
        for vertex in instrumentation_point_graph:
            vertex_variable = get_variable_for_program_point(vertex.program_point,
                                                             self._variables)
            self.obj_function += '{} {}'.format(program_point_data.get_wcet(vertex.program_point),
                                                vertex_variable)
            if counter > 1:
                self.obj_function += ' + '
            counter -= 1

        self.obj_function += ' + '

        counter = instrumentation_point_graph.number_of_edges()
        for vertex in instrumentation_point_graph:
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = instrumentation_point_graph.get_vertex(succ_edge.vertex_id)
                wcet_of_edge = self.__evaluate_wcet_of_edge(instrumentation_point_graph,
                                                            program_point_data,
                                                            succ_edge)
                vertex_variable = get_variable_for_edge_between_program_points(vertex.program_point,
                                                                               succ_vertex.program_point,
                                                                               self._variables)
                self.obj_function += '{} {}'.format(wcet_of_edge, vertex_variable)
                if counter > 1:
                    self.obj_function += ' + '
                counter -= 1
        self.obj_function += ';'

    def __create_structural_constraints(self, instrumentation_point_graph):
        for vertex in instrumentation_point_graph:
            constraint = get_variable_for_program_point(vertex.program_point,
                                                        self._variables)
            constraint += ' = '
            counter = vertex.number_of_successors()
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = instrumentation_point_graph.get_vertex \
                    (succ_edge.vertex_id)
                constraint += get_variable_for_edge_between_program_points(vertex.program_point,
                                                                           succ_vertex.program_point,
                                                                           self._variables)
                if counter > 1:
                    constraint += ' + '
                counter -= 1
            constraint += ';'
            self._constraints.add(constraint)

            constraint = get_variable_for_program_point(vertex.program_point,
                                                        self._variables)
            constraint += ' = '
            counter = vertex.number_of_predecessors()
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = instrumentation_point_graph.get_vertex \
                    (pred_edge.vertex_id)
                constraint += get_variable_for_edge_between_program_points(pred_vertex.program_point,
                                                                           vertex.program_point,
                                                                           self._variables)
                if counter > 1:
                    constraint += ' + '
                counter -= 1
            constraint += ';'
            self._constraints.add(constraint)

    def __create_loop_bound_constraints(self,
                                        instrumentation_point_graph,
                                        loop_nesting_tree,
                                        program_point_data):

        def create_local_loop_bound_constraint(abstract_vertex):
            if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
                loop_entry_destinations = instrumentation_point_graph.get_loop_backedge_destinations(abstract_vertex)
                for vertex in loop_entry_destinations:
                    constraint = get_variable_for_program_point(vertex.program_point,
                                                                self._variables)
                    constraint += ' = 1;'
                    self._constraints.add(constraint)
            else:
                loop_entry_destinations = instrumentation_point_graph.get_loop_entry_destinations(abstract_vertex)
                for vertex in loop_entry_destinations:
                    constraint = get_variable_for_program_point(vertex.program_point,
                                                                self._variables)
                    loop_entry_predecessor_vertices = set()
                    for pred_edge in vertex.predecessor_edge_iterator():
                        pred_vertex = instrumentation_point_graph.get_vertex(pred_edge.vertex_id)
                        abstract_vertex_two = loop_nesting_tree.get_header_abstract_vertex_for_program_point(
                            pred_vertex.program_point)
                        if not loop_nesting_tree.is_ancestor(abstract_vertex, abstract_vertex_two):
                            loop_entry_predecessor_vertices.add(pred_vertex)

                    constraint += ' <= '
                    counter = len(loop_entry_predecessor_vertices)
                    for pred_vertex in loop_entry_predecessor_vertices:
                        constraint += '{} {}'.format(max
                                                     (program_point_data.get_loop_bound
                                                      (abstract_vertex.program_point)),
                                                     get_variable_for_program_point
                                                     (pred_vertex.program_point,
                                                      self._variables))
                        if counter > 1:
                            constraint += ' + '
                        counter -= 1
                    constraint += ';'
                self._constraints.add(constraint)

        def create_global_loop_bound_constraint(abstract_vertex):
            pass

        for level, tree_vertices in loop_nesting_tree. \
                level_by_level_iterator \
                    (abstract_vertices_only=True):
            for abstract_vertex in tree_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    create_local_loop_bound_constraint(abstract_vertex)
                    if level > 1:
                        create_global_loop_bound_constraint(abstract_vertex)
