import decimal
import os
import random
import re
import subprocess
import timeit

from graphs import (edges, graphs, vertices)
from system import database
from utils import messages


class ProgramPointVariable:
    def __init__(self, vertex: vertices.ProgramPointVertex):
        assert isinstance(vertex, vertices.ProgramPointVertex)
        self._vertex = vertex

    def __hash__(self):
        return hash(self._vertex.program_point)

    def __eq__(self, other):
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        if isinstance(self._vertex.program_point, vertices.Vertex):
            return 'v__{}'.format(self._vertex.program_point)
        else:
            return 'e__{}_{}'.format(self._vertex.program_point.predecessor(), self._vertex.program_point.successor())


class TransitionVariable:
    def __init__(self, transition: edges.TransitionEdge):
        assert isinstance(transition, edges.TransitionEdge)
        self._transition = transition

    def __hash__(self):
        return hash(self._transition)

    def __eq__(self, other):
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        if isinstance(self._transition.predecessor().program_point, vertices.Vertex):
            source = '{}'.format(self._transition.predecessor().program_point)
        else:
            source = '{}_{}'.format(self._transition.predecessor().program_point.predecessor(),
                                    self._transition.predecessor().program_point.successor())

        if isinstance(self._transition.successor().program_point, vertices.Vertex):
            destination = '{}'.format(self._transition.successor().program_point)
        else:
            destination = '{}_{}'.format(self._transition.successor().program_point.predecessor(),
                                         self._transition.successor().program_point.successor())

        return 't__{}__{}'.format(source, destination)


class Term:
    def __init__(self, coefficient, variable):
        self._coefficient = coefficient
        self._variable = variable

    def __str__(self):
        return '{} {}'.format(self._coefficient, self._variable)


class LinearExpr(list):
    def __str__(self):
        return ' + '.join(str(term) for term in self)


class Constraint:
    EQUALITY = '='
    LESS_OR_EQUAL = '<='

    def __init__(self, left, right, relation):
        self._left = left
        self._right = right
        self._relation = relation

    def __str__(self):
        return '{} {} {}'.format(self._left, self._relation, self._right)


class ConstraintSystem:
    """
    A constraint system that solves to a WCET estimate.
    """

    def __init__(self):
        self._wcet = 0
        self._solve_time = 0
        self._construction_time = []
        self._variable_execution_counts = {}
        self._objective = LinearExpr()
        self._constraints = []
        self._variables = set()

    @property
    def wcet(self):
        return self._wcet

    @property
    def solve_time(self):
        return self._solve_time

    def number_of_constraints(self):
        return len(self._constraints)

    def number_of_variables(self):
        return len(self._variables)

    @property
    def construction_time(self):
        return sum(self._construction_time)

    def add_to_construction_time(self, time):
        self._construction_time.append(time)

    def add_to_objective(self, term: Term):
        self._objective.append(term)

    def add_variable(self, variable: ProgramPointVariable):
        self._variables.add(variable)

    def add_constraint(self, constraint: Constraint):
        self._constraints.append(constraint)

    def __str__(self):
        return """
WCET:         {}
variables:    {}
constraints:  {}
construction: {}
solve:        {}
total:        {}""".format(self.wcet,
                           len(self._variables),
                           len(self._constraints),
                           self.construction_time,
                           self.solve_time,
                           self.construction_time + self.solve_time)


class IntegerLinearProgram(ConstraintSystem):
    """
    A constraint system that contains linear constraints only.
    """

    def solve(self, filename):
        def get_new_line(num=1):
            return '\n' * num

        random.shuffle(self._objective)
        random.shuffle(self._constraints)

        with open(filename, 'w') as wd:
            wd.write('max:\n{};'.format(' +\n'.join(str(term) for term in self._objective)))
            wd.write(get_new_line(2))
            for constraint in self._constraints:
                wd.write('{};'.format(constraint))
                wd.write(get_new_line())
            wd.write(get_new_line(2))
            wd.write('int\n{};'.format(',\n'.join(str(variable) for variable in self._variables)))
            wd.write(get_new_line())

        # Launch lp_solve with the created file
        args = ['lp_solve', filename]
        start = timeit.default_timer()
        process = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, _ = process.communicate()
        end = timeit.default_timer()
        self._solve_time = end - start

        if process.returncode != 0:
            messages.error_message("Running '{}' failed with {}".format(' '.join(args), process.returncode))

        # Grab the WCET estimate and the execution counts of variables.
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

    def output(self, function_name):
        print()
        print('wcet({})={}'.format(function_name, self._wcet))
        for variable in sorted(self._variable_execution_counts.keys()):
            if self._variable_execution_counts[variable] > 0:
                print('{} = {}'.format(variable,
                                       self._variable_execution_counts[variable]))


def create_ilp_for_program_point_graph(ppg: graphs.ProgramPointGraph, lnt: graphs.LoopNests, db: database.Database):
    def create_objective_function():
        for v in ppg:
            variable = ProgramPointVariable(v)
            ilp.add_variable(variable)
            term = Term(db.get_wcet(v), variable)
            ilp.add_to_objective(term)

    def create_structural_constraints():
        for v in ppg:
            if isinstance(v.program_point, vertices.Vertex):
                flow_in_lhs = LinearExpr()
                for predecessor_edge in ppg.predecessors(v):
                    variable = ProgramPointVariable(predecessor_edge.predecessor())
                    flow_in_lhs.append(variable)

                flow_in_rhs = LinearExpr()
                flow_in_rhs.append(ProgramPointVariable(v))

                flow_in_constraint = Constraint(flow_in_lhs, flow_in_rhs, Constraint.EQUALITY)
                ilp.add_constraint(flow_in_constraint)

                flow_out_lhs = LinearExpr()
                for predecessor_edge in ppg.predecessors(v):
                    variable = ProgramPointVariable(predecessor_edge.predecessor())
                    flow_out_lhs.append(variable)

                flow_out_rhs = LinearExpr()
                for successor_edge in ppg.successors(v):
                    variable = ProgramPointVariable(successor_edge.successor())
                    flow_out_rhs.append(variable)

                flow_out_constraint = Constraint(flow_out_lhs, flow_out_rhs, Constraint.EQUALITY)
                ilp.add_constraint(flow_out_constraint)

    def create_loop_bound_constraints():
        for loop in lnt:
            lhs = LinearExpr()
            lhs.append(ProgramPointVariable(loop.header))

            rhs = LinearExpr()
            if lnt.is_outermost_loop(loop):
                coefficient = db.get_global_wfreq(loop.header)
                rhs.append(str(coefficient))
                constraint = Constraint(lhs, rhs, Constraint.EQUALITY)
            else:
                coefficient = db.get_local_wfreq(loop.header)
                (loop_transition,) = [predecessor_edge for predecessor_edge in lnt.predecessors(loop)
                                      if predecessor_edge.direction == edges.LoopTransition.Direction.ENTRY]
                for transition in loop_transition:
                    rhs.append(Term(str(coefficient), ProgramPointVariable(transition.predecessor())))
                constraint = Constraint(lhs, rhs, Constraint.LESS_OR_EQUAL)

            ilp.add_constraint(constraint)

    ilp = IntegerLinearProgram()
    start = timeit.default_timer()
    create_objective_function()
    create_structural_constraints()
    create_loop_bound_constraints()
    end = timeit.default_timer()
    ilp.add_to_construction_time(end - start)
    return ilp


def create_ilp_for_super_block_graph(ppg:                    graphs.ProgramPointGraph,
                                     lnt:                    graphs.LoopNests,
                                     db:                     database.Database,
                                     fold_optimisation:      bool,
                                     dominator_optimisation: bool):

    def create_objective_function(super_graph):
        for super_block in super_graph.super_blocks():
            ilp.add_variable(ProgramPointVariable(super_block.representative))
            if fold_optimisation:
                wcet = 0
                for v in super_block:
                    if isinstance(v.program_point, vertices.Vertex):
                        wcet += db.get_wcet(v)
                term = Term(wcet, ProgramPointVariable(super_block.representative))
                ilp.add_to_objective(term)
            else:
                for v in super_block:
                    if isinstance(v.program_point, vertices.Vertex):
                        variable = ProgramPointVariable(v)
                        ilp.add_variable(variable)
                        term = Term(db.get_wcet(v), variable)
                        ilp.add_to_objective(term)

    def create_intra_super_block_constraints(super_graph):
        if not fold_optimisation:
            for super_block in super_graph.super_blocks():
                for v in super_block:
                    if isinstance(v.program_point, vertices.Vertex):
                        if v != super_block.representative:
                            lhs = LinearExpr()
                            lhs.append(ProgramPointVariable(v))
                            rhs = LinearExpr()
                            rhs.append(ProgramPointVariable(super_block.representative))
                            constraint = Constraint(lhs, rhs, Constraint.EQUALITY)
                            ilp.add_constraint(constraint)

    def create_fork_constraints(super_graph):
        for fork in super_graph.forks():
            lhs = LinearExpr()
            (super_block,) = [predecessor_edge.predecessor() for predecessor_edge in super_graph.predecessors(fork)]
            lhs.append(ProgramPointVariable(super_block.representative))
            rhs = LinearExpr()
            for successor_edge in super_graph.successors(fork):
                rhs.append(ProgramPointVariable(successor_edge.successor().representative))
            constraint = Constraint(lhs, rhs, Constraint.EQUALITY)
            ilp.add_constraint(constraint)

    def create_merge_constraints(super_graph):
        for merge in super_graph.merges():
            redundant_constraint = False
            if dominator_optimisation and not lnt.is_header(merge.program_point):
                (parent_edge,) = super_graph.pre_dominator_tree.predecessors(merge.program_point)
                immediate_pre_dominator = parent_edge.predecessor()
                (parent_edge,) = super_graph.post_dominator_tree.predecessors(immediate_pre_dominator)
                immediate_post_dominator = parent_edge.predecessor()
                # This merge immediately post-dominates the branch.
                redundant_constraint = immediate_post_dominator == merge.program_point

            if not redundant_constraint:
                lhs = LinearExpr()
                (super_block,) = [successor_edge.successor() for successor_edge in super_graph.successors(merge)]
                lhs.append(ProgramPointVariable(super_block.representative))
                rhs = LinearExpr()
                for predecessor_edge in super_graph.predecessors(merge):
                    rhs.append(ProgramPointVariable(predecessor_edge.predecessor().representative))
                constraint = Constraint(lhs, rhs, Constraint.EQUALITY)
                ilp.add_constraint(constraint)

    def create_loop_bound_constraints(super_graph):
        for loop in lnt:
            lhs = LinearExpr()
            super_block = super_graph[loop.header]
            lhs.append(ProgramPointVariable(super_block.representative))

            rhs = LinearExpr()
            if lnt.is_outermost_loop(loop):
                coefficient = db.get_global_wfreq(loop.header)
                rhs.append(str(coefficient))
                constraint = Constraint(lhs, rhs, Constraint.EQUALITY)
            else:
                coefficient = db.get_local_wfreq(loop.header)
                (loop_transition,) = [successor_edge for successor_edge in lnt.successors(loop)
                                      if successor_edge.direction == edges.LoopTransition.Direction.EXIT]
                for transition in loop_transition:
                    super_block = super_graph[transition.successor()]
                    rhs.append(Term(str(coefficient), ProgramPointVariable(super_block.representative)))
                constraint = Constraint(lhs, rhs, Constraint.LESS_OR_EQUAL)

            ilp.add_constraint(constraint)

    ilp = IntegerLinearProgram()
    start = timeit.default_timer()
    super_graph = graphs.SuperBlockGraph(ppg, lnt)
    create_objective_function(super_graph)
    create_intra_super_block_constraints(super_graph)
    create_fork_constraints(super_graph)
    create_merge_constraints(super_graph)
    create_loop_bound_constraints(super_graph)
    end = timeit.default_timer()
    ilp.add_to_construction_time(end - start)
    super_graph.dotify()
    return ilp


def create_ilp_for_instrumentation_point_graph(ipg: graphs.InstrumentationPointGraph,
                                               lnt: graphs.LoopNests,
                                               db: database.Database):
    def add_variables():
        for v in ipg:
            ilp.add_variable(ProgramPointVariable(v))
            for successor_edge in ipg.successors(v):
                ilp.add_variable(TransitionVariable(successor_edge))

    def create_objective_function():
        # Bring data into memory from the database so that this stage is not memory bound.
        wcets = {}
        for v in ipg:
            wcets[v] = db.get_wcet(v)
            for successor_edge in ipg.successors(v):
                for w in successor_edge:
                    if w not in wcets:
                        wcets[w] = db.get_wcet(w)

        for v in ipg:
            term = Term(wcets[v], ProgramPointVariable(v))
            ilp.add_to_objective(term)
            for successor_edge in ipg.successors(v):
                wcet = sum([wcets[w] for w in successor_edge])
                term = Term(wcet, TransitionVariable(successor_edge))
                ilp.add_to_objective(term)

    def create_structural_constraints():
        for v in ipg:
            flow_in_lhs = LinearExpr()
            for predecessor_edge in ipg.predecessors(v):
                variable = TransitionVariable(predecessor_edge)
                flow_in_lhs.append(variable)

            flow_in_rhs = LinearExpr()
            flow_in_rhs.append(ProgramPointVariable(v))

            flow_in_constraint = Constraint(flow_in_lhs, flow_in_rhs, Constraint.EQUALITY)
            ilp.add_constraint(flow_in_constraint)

            flow_out_lhs = LinearExpr()
            for predecessor_edge in ipg.predecessors(v):
                variable = TransitionVariable(predecessor_edge)
                flow_out_lhs.append(variable)

            flow_out_rhs = LinearExpr()
            for successor_edge in ipg.successors(v):
                variable = TransitionVariable(successor_edge)
                flow_out_rhs.append(variable)

            flow_out_constraint = Constraint(flow_out_lhs, flow_out_rhs, Constraint.EQUALITY)
            ilp.add_constraint(flow_out_constraint)

    def create_loop_bound_constraints(ipg, lnt, db):
        program_point_mapping = {}
        for loop in lnt:
            program_point_mapping[loop.header] = set()
            if not lnt.is_outermost_loop(loop):
                (loop_transition,) = [predecessor_edge for predecessor_edge in lnt.predecessors(loop)
                                      if predecessor_edge.direction == edges.LoopTransition.Direction.ENTRY]
                for transition in loop_transition:
                    program_point_mapping[transition.predecessor()] = set()

        for v in ipg:
            if v in program_point_mapping:
                program_point_mapping[v].add(v)
            for successor_edge in ipg.successors(v):
                for w in successor_edge:
                    if w in program_point_mapping:
                        program_point_mapping[w].add(successor_edge)

        for loop in lnt:
            lhs = LinearExpr()
            for program_point in program_point_mapping[loop.header]:
                if isinstance(program_point, edges.TransitionEdge):
                    lhs.append(TransitionVariable(program_point))
                else:
                    lhs.append(ProgramPointVariable(program_point))

            rhs = LinearExpr()
            if lnt.is_outermost_loop(loop):
                coefficient = db.get_global_wfreq(loop.header)
                rhs.append(str(coefficient))
                constraint = Constraint(lhs, rhs, Constraint.EQUALITY)
            else:
                coefficient = db.get_local_wfreq(loop.header)
                (loop_transition,) = [predecessor_edge for predecessor_edge in lnt.predecessors(loop)
                                      if predecessor_edge.direction == edges.LoopTransition.Direction.ENTRY]
                for transition in loop_transition:
                    for program_point in program_point_mapping[transition.predecessor()]:
                        if isinstance(program_point, edges.TransitionEdge):
                            rhs.append(Term(str(coefficient), TransitionVariable(program_point)))
                        else:
                            rhs.append(Term(str(coefficient), ProgramPointVariable(program_point)))

                constraint = Constraint(lhs, rhs, Constraint.LESS_OR_EQUAL)

            ilp.add_constraint(constraint)

    ilp = IntegerLinearProgram()
    start = timeit.default_timer()
    add_variables()
    create_objective_function()
    create_structural_constraints()
    create_loop_bound_constraints(ipg, lnt, db)
    end = timeit.default_timer()
    ilp.add_to_construction_time(end - start)
    return ilp



# def do_wcet_calculation_for_instrumentation_point_graph(program: analysis.Program,
#                                                         instrumentation_point_graph : directed_graphs.InstrumentationPointGraph,
#                                                         transition_execution_times,
#                                                         transition_max_freqs,
#                                                         call_execution_times):
#     strong_components = directed_graphs.StronglyConnectedComponents(instrumentation_point_graph)
#     if len(strong_components) == instrumentation_point_graph.number_of_vertices():
#         # If every instrumentation point belongs to its own strong component, then the IPG is acyclic.
#         # We can calculate a WCET estimate during a topological traversal of instrumentation points.
#         depth_first_search_tree = directed_graphs.DepthFirstSearch(instrumentation_point_graph,
#                                                                    instrumentation_point_graph.entry_vertex,
#                                                                    False)
#         calculated_times = {}
#         for vertex in reversed(depth_first_search_tree.post_order):
#             if vertex.number_of_predecessors():
#                 transition_times = set()
#                 for pred_edge in vertex.predecessor_edge_iterator():
#                     pred_vertex = instrumentation_point_graph.get_vertex(pred_edge.vertex_id)
#                     transition_time = 0
#                     if pred_vertex.abstract and vertex.abstract:
#                         callee = program.find_subprogram_with_program_point(vertex.program_point)
#                         assert callee == program.find_subprogram_with_program_point(pred_vertex.program_point)
#                         transition_time += call_execution_times[callee]
#                     else:
#                         if (pred_vertex, vertex) in transition_execution_times:
#                             transition_time = max(transition_execution_times[(pred_vertex, vertex)])
#                     transition_times.add(transition_time + calculated_times[pred_vertex])
#                 calculated_times[vertex] = max(transition_times)
#             else:
#                 calculated_times[vertex] = 0
#         # The exit vertex of the IPG is annotated with the WCET estimate of the function
#         return calculated_times[instrumentation_point_graph.exit_vertex]
#     else:
#         optimisation_problem = IntegerLinearProgramForInstrumentationPointGraphFromMeasurements(program,
#                                                                                                 instrumentation_point_graph,
#                                                                                                 transition_execution_times,
#                                                                                                 transition_max_freqs,
#                                                                                                 call_execution_times)
#         optimisation_problem.solve()
#         return optimisation_problem.wcet
#
#
# def compute_and_compare_wcet_estimates_between_cfg_and_ipg(program: analysis.Program):
#     for repetition in range(1, globals.args['repeat'] + 1):
#         debug.verbose_message('Repetition {}'.format(repetition), __name__)
#         for control_flow_graph in program:
#             loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
#             control_flow_graph.program_point_data.create_timing_data(True)
#
#             ilp_for_control_flow_graph = \
#                 IntegerLinearProgramForControlFlowGraph \
#                     (control_flow_graph,
#                      loop_nesting_tree,
#                      control_flow_graph.program_point_data)
#             ilp_for_control_flow_graph.solve()
#
#             ilp_for_instrumentation_point_graph = \
#                 IntegerLinearProgramForInstrumentationPointGraph \
#                     (control_flow_graph.get_instrumentation_point_graph(),
#                      loop_nesting_tree,
#                      control_flow_graph.program_point_data)
#             ilp_for_instrumentation_point_graph.solve()
#
#             assert ilp_for_instrumentation_point_graph.wcet == ilp_for_control_flow_graph.wcet, \
#                 'IPG = {}, CFG = {}'.format(ilp_for_instrumentation_point_graph.wcet,
#                                             ilp_for_control_flow_graph.wcet)
#
#             print('>', control_flow_graph.name)
#             print('control_flow_graph')
#             print('WCET         = {}'.format(ilp_for_control_flow_graph.wcet))
#             print('Variables    = {}'.format(len(ilp_for_control_flow_graph.variables)))
#             print('Constraints  = {}'.format(len(ilp_for_control_flow_graph.constraints)))
#             print('Construction = {}'.format(ilp_for_control_flow_graph.construction_time))
#             print('Solve        = {}'.format(ilp_for_control_flow_graph.solve_time))
#             print('-' * 25)
#             print('instrumentation_point_graph')
#             print('WCET         = {}'.format(ilp_for_instrumentation_point_graph.wcet))
#             print('Variables    = {}'.format(len(ilp_for_instrumentation_point_graph.variables)))
#             print('Constraints  = {}'.format(len(ilp_for_instrumentation_point_graph.constraints)))
#             print('Construction = {}'.format(ilp_for_instrumentation_point_graph.construction_time))
#             print('Solve        = {}'.format(ilp_for_instrumentation_point_graph.solve_time))
#
#
# def calculate_wcet_using_integer_linear_programming(program):
#     if globals.args['output']:
#         log_file = open(os.path.abspath(globals.args['output']), 'w')
#         old_stdout = sys.stdout
#         sys.stdout = log_file
#     try:
#         for repetition in range(1, globals.args['repeat'] + 1):
#             debug.verbose_message('Repetition {}'.format(repetition), __name__)
#             for control_flow_graph in program:
#                 # The reason we grab the loop-nesting tree here and not inside the
#                 # constraint system is because we time how long it takes to
#                 # construct the constraint system.  Since the loop-nesting tree is
#                 # built on the fly, it may happen that a portion of the time to
#                 # construct the constraint system is unfairly attributed to that
#                 # activity.
#                 loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
#                 control_flow_graph.program_point_data.create_timing_data(True)
#                 ilp_for_control_flow_graph = IntegerLinearProgramForControlFlowGraph \
#                     (control_flow_graph,
#                      loop_nesting_tree,
#                      control_flow_graph.program_point_data)
#                 ilp_for_control_flow_graph.solve()
#
#                 ilp_for_super_block_graph = IntegerLinearProgramForSuperBlockGraph \
#                     (control_flow_graph,
#                      loop_nesting_tree,
#                      control_flow_graph.program_point_data)
#                 ilp_for_super_block_graph.solve()
#
#                 assert ilp_for_super_block_graph.wcet == ilp_for_control_flow_graph.wcet
#
#                 print('>', control_flow_graph.name)
#                 print('control_flow_graph')
#                 print(ilp_for_control_flow_graph)
#                 print('super_blocks')
#                 print(ilp_for_super_block_graph)
#
#                 if globals.args['folded']:
#                     ilp_with_folding = \
#                         IntegerLinearProgramForSuperBlockGraphWithFolding \
#                             (control_flow_graph,
#                              loop_nesting_tree,
#                              control_flow_graph.program_point_data)
#                     ilp_with_folding.solve()
#
#                     assert ilp_with_folding.wcet == ilp_for_control_flow_graph.wcet
#                     print('super_blocks_folded')
#                     print(ilp_with_folding)
#     finally:
#         if globals.args['output']:
#             log_file.close()
#             sys.stdout = old_stdout
#
#
#
#
#
# def get_variable_for_edge_between_program_points(program_point_one,
#                                                  program_point_two,
#                                                  variables):
#     variable = TRANSITION_VARIABLE_PREFIX
#     if is_basic_block(program_point_one):
#         variable += '{}'.format(program_point_one)
#     else:
#         variable += '{}_{}'.format(program_point_one[0], program_point_one[1])
#     variable += '.'
#     if is_basic_block(program_point_two):
#         variable += '{}'.format(program_point_two)
#     else:
#         variable += '{}_{}'.format(program_point_two[0], program_point_two[1])
#     variables.add(variable)
#     return variable
#
#
# def get_vertex_wcet_variable(vertex_id):
#     return '{}{}'.format(WCET_VARIABLE_PREFIX, vertex_id)
#
#
#
#
#
#
#
# class IntegerLinearProgramForInstrumentationPointGraphFromMeasurements(IntegerLinearProgram):
#     """
#     The integer linear program derived from an instrumentation point graph, execution
#     times of transitions and upper bounds on transition execution counts.
#     """
#
#     def __init__(self,
#                  program,
#                  instrumentation_point_graph,
#                  transition_execution_times,
#                  transition_max_freqs,
#                  call_execution_times):
#         ConstraintSystem.__init__(self)
#         self._filename = '{}.{}.ipg.ilp'.format(globals.args['filename_prefix'],
#                                                 instrumentation_point_graph.name)
#         start = timeit.default_timer()
#         self.__create_objective_function(program,
#                                          instrumentation_point_graph,
#                                          transition_execution_times,
#                                          call_execution_times)
#         self.__create_structural_constraints(instrumentation_point_graph)
#         self.__create_capacity_constraints(instrumentation_point_graph,
#                                            transition_max_freqs)
#         self._create_integer_constraint()
#         end = timeit.default_timer()
#         self._construction_time = end - start
#
#     def __create_objective_function(self,
#                                     program,
#                                     instrumentation_point_graph,
#                                     transition_execution_times,
#                                     call_execution_times):
#         self.obj_function = 'max: '
#
#         counter = instrumentation_point_graph.number_of_edges()
#         for vertex in instrumentation_point_graph:
#             for succ_edge in vertex.successor_edge_iterator():
#                 succ_vertex = instrumentation_point_graph.get_vertex(succ_edge.vertex_id)
#                 if vertex.abstract and succ_vertex.abstract:
#                     callee = program.find_subprogram_with_program_point(vertex.program_point)
#                     assert callee == program.find_subprogram_with_program_point(succ_vertex.program_point)
#                     transition_time = call_execution_times[callee]
#                 elif (vertex, succ_vertex) in transition_execution_times:
#                     transition_time = max(transition_execution_times[(vertex, succ_vertex)])
#                 else:
#                     transition_time = 0
#                 self.obj_function += '{} {}'.format(transition_time,
#                                                     get_variable_for_edge_between_program_points(vertex.vertex_id,
#                                                                                                  succ_vertex.vertex_id,
#                                                                                                  self._variables))
#                 if counter > 1:
#                     self.obj_function += ' + '
#                 counter -= 1
#         self.obj_function += ';'
#
#     def __create_structural_constraints(self, instrumentation_point_graph):
#         for vertex in instrumentation_point_graph:
#             constraint = get_variable_for_program_point(vertex.vertex_id,
#                                                         self._variables)
#             constraint += ' = '
#             counter = vertex.number_of_successors()
#             for succ_edge in vertex.successor_edge_iterator():
#                 succ_vertex = instrumentation_point_graph.get_vertex \
#                     (succ_edge.vertex_id)
#                 constraint += get_variable_for_edge_between_program_points(vertex.vertex_id,
#                                                                            succ_vertex.vertex_id,
#                                                                            self._variables)
#                 if counter > 1:
#                     constraint += ' + '
#                 counter -= 1
#             constraint += ';'
#             self._constraints.append(constraint)
#
#             constraint = get_variable_for_program_point(vertex.vertex_id,
#                                                         self._variables)
#             constraint += ' = '
#             counter = vertex.number_of_predecessors()
#             for pred_edge in vertex.predecessor_edge_iterator():
#                 pred_vertex = instrumentation_point_graph.get_vertex \
#                     (pred_edge.vertex_id)
#                 constraint += get_variable_for_edge_between_program_points(pred_vertex.vertex_id,
#                                                                            vertex.vertex_id,
#                                                                            self._variables)
#                 if counter > 1:
#                     constraint += ' + '
#                 counter -= 1
#             constraint += ';'
#             self._constraints.append(constraint)
#
#     def __create_capacity_constraints(self,
#                                       instrumentation_point_graph,
#                                       transition_max_freqs):
#         for vertex in instrumentation_point_graph:
#             for succ_edge in vertex.successor_edge_iterator():
#                 succ_vertex = instrumentation_point_graph.get_vertex(succ_edge.vertex_id)
#                 if (vertex, succ_vertex) in transition_max_freqs:
#                     constraint = '{} {} {};'.format(get_variable_for_edge_between_program_points(vertex.vertex_id,
#                                                                                                  succ_vertex.vertex_id,
#                                                                                                  self._variables),
#                                                     '<=',
#                                                     transition_max_freqs[(vertex, succ_vertex)])
#                     self._constraints.append(constraint)
#                 elif succ_edge.backedge:
#                     capacity = (1 if vertex == instrumentation_point_graph.exit_vertex and
#                                      succ_vertex == instrumentation_point_graph.entry_vertex
#                                 else 0)
#                     constraint = '{} = {};'.format(get_variable_for_edge_between_program_points(vertex.vertex_id,
#                                                                                                 succ_vertex.vertex_id,
#                                                                                                 self._variables),
#                                                    capacity)
#                     self._constraints.append(constraint)
#
#
#
#
#
#
# class IntegerLinearProgramForSuperBlockGraph(IntegerLinearProgram):
#     """
#     The integer linear program derived from a super block graph.
#     """
#
#     def __init__(self,
#                  control_flow_graph,
#                  loop_nesting_tree,
#                  program_point_data):
#         ConstraintSystem.__init__(self)
#         self._filename = '{}.{}.super.ilp'.format(globals.args['filename_prefix'],
#                                                   control_flow_graph.name)
#         self.__loop_exit_edges = set()
#         start = timeit.default_timer()
#         self.__create_objective_function(control_flow_graph,
#                                          program_point_data)
#         self.__create_constraints(control_flow_graph,
#                                   loop_nesting_tree,
#                                   program_point_data)
#         self._create_integer_constraint()
#         end = timeit.default_timer()
#         self._construction_time = end - start
#
#     def __create_objective_function(self,
#                                     control_flow_graph,
#                                     program_point_data):
#         self.obj_function = 'max: '
#         counter = len(control_flow_graph.basic_block_vertices)
#
#         for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
#                 level_by_level_iterator \
#                     (abstract_vertices_only=True):
#             for abstract_vertex in abstract_vertices:
#                 if is_basic_block(abstract_vertex.program_point):
#                     # Rebuild the super block graph for this loop irrespective of
#                     # whether it has been built previously.  In the end we want to
#                     # account properly for the time to construct the ILP, and this
#                     # includes the overhead associated with super block construction.
#                     subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex,
#                                                                            redo=True)
#                     for super_vertex in subgraph:
#                         for induced_vertex in super_vertex.vertices:
#                             if is_basic_block(induced_vertex.
#                                                       program_point) \
#                                     and not induced_vertex.abstract:
#                                 vertex_variable = get_variable_for_program_point \
#                                     (induced_vertex.program_point,
#                                      self._variables)
#                                 wcet = program_point_data.get_wcet(induced_vertex.
#                                                                    program_point)
#                                 self.obj_function += '{} {}'.format(wcet,
#                                                                     vertex_variable)
#                                 if counter > 1:
#                                     self.obj_function += ' + '
#                                 counter -= 1
#         self.obj_function += ';'
#
#     def __create_constraints(self,
#                              control_flow_graph,
#                              loop_nesting_tree,
#                              program_point_data):
#         for level, tree_vertices in loop_nesting_tree.level_by_level_iterator \
#                     (abstract_vertices_only=True):
#             for abstract_vertex in tree_vertices:
#                 if is_basic_block(abstract_vertex.program_point):
#                     subgraph = control_flow_graph.get_super_block_subgraph \
#                         (abstract_vertex)
#                     for super_vertex in subgraph:
#                         self.__create_intra_super_block_constraints(super_vertex)
#
#                         if super_vertex.number_of_predecessors() > 1:
#                             self.__create_predecessor_super_block_constraints \
#                                 (subgraph, super_vertex)
#
#                         if super_vertex.number_of_successors() > 1 \
#                                 and not super_vertex.representative.abstract:
#                             self.__create_successor_super_block_constraints \
#                                 (control_flow_graph, subgraph, super_vertex)
#
#                     self.__create_loop_bound_constraints(control_flow_graph,
#                                                          loop_nesting_tree,
#                                                          level,
#                                                          abstract_vertex,
#                                                          program_point_data)
#
#     def __create_intra_super_block_constraints(self,
#                                                super_vertex):
#         for induced_vertex in super_vertex.vertices:
#             if induced_vertex != super_vertex.representative \
#                     and not induced_vertex.abstract:
#                 if is_basic_block(induced_vertex.program_point) \
#                         or induced_vertex.program_point in self.__loop_exit_edges:
#                     constraint = get_variable_for_program_point \
#                         (induced_vertex.program_point,
#                          self._variables)
#                     constraint += ' = '
#                     constraint += get_variable_for_program_point \
#                         (super_vertex.representative.program_point,
#                          self._variables)
#                     constraint += ';'
#                     self._constraints.append(constraint)
#
#     def __create_predecessor_super_block_constraints(self,
#                                                      subgraph,
#                                                      super_vertex):
#
#         if super_vertex.representative.abstract:
#             # An abstract vertex in its own super block
#             constraint = ''
#             counter = super_vertex.number_of_successors()
#             for succ_edge in super_vertex.successor_edge_iterator():
#                 super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
#                 constraint += get_variable_for_program_point(super_succ_vertex.
#                                                              representative.
#                                                              program_point,
#                                                              self._variables)
#                 if counter > 1:
#                     constraint += ' + '
#                 counter -= 1
#         else:
#             constraint = get_variable_for_program_point(super_vertex.
#                                                         representative.
#                                                         program_point,
#                                                         self._variables)
#
#         constraint += ' = '
#         counter = super_vertex.number_of_predecessors()
#         for pred_edge in super_vertex.predecessor_edge_iterator():
#             super_pred_vertex = subgraph.get_vertex \
#                 (pred_edge.vertex_id)
#             constraint += get_variable_for_program_point(super_pred_vertex.
#                                                          representative.
#                                                          program_point,
#                                                          self._variables)
#             if counter > 1:
#                 constraint += ' + '
#             counter -= 1
#         constraint += ';'
#         self._constraints.append(constraint)
#
#     def __create_successor_super_block_constraints(self,
#                                                    control_flow_graph,
#                                                    subgraph,
#                                                    super_vertex):
#         for _, partition in super_vertex.successor_edge_partition_iterator():
#             if len(partition) > 1:
#                 constraint = get_variable_for_program_point(super_vertex.
#                                                             representative.
#                                                             program_point,
#                                                             self._variables)
#                 constraint += ' = '
#                 counter = len(partition)
#                 for succ_edge in partition:
#                     super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
#                     constraint += get_variable_for_program_point \
#                         (super_succ_vertex.
#                          representative.
#                          program_point,
#                          self._variables)
#                     if counter > 1:
#                         constraint += ' + '
#                     counter -= 1
#                 constraint += ';'
#                 self._constraints.append(constraint)
#
#     def __create_loop_bound_constraints(self,
#                                         control_flow_graph,
#                                         loop_nesting_tree,
#                                         level,
#                                         abstract_vertex,
#                                         program_point_data):
#
#         def get_subgraph_of_outer_loop(subgraph, super_vertex):
#             for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
#                     level_by_level_iterator \
#                         (abstract_vertices_only=True):
#                 for abstract_vertex in abstract_vertices:
#                     if is_basic_block(abstract_vertex.program_point):
#                         candidate_subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
#                         if subgraph.root_vertex.vertex_id != \
#                                 candidate_subgraph.root_vertex.vertex_id \
#                                 and candidate_subgraph.has_vertex_for_program_point(super_vertex.
#                                                                                             representative.
#                                                                                             program_point):
#                             return candidate_subgraph
#
#         def create_local_loop_bound_constraint(abstract_vertex):
#             loop_bound_tuple = program_point_data.get_loop_bound(abstract_vertex.
#                                                                  program_point)
#             constraint = get_variable_for_program_point(abstract_vertex.
#                                                         program_point,
#                                                         self._variables)
#             if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
#                 constraint += ' = {};'.format(max(loop_bound_tuple))
#             else:
#                 constraint += ' <= '
#                 subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
#                 loop_exit_super_blocks = [super_vertex for super_vertex in subgraph
#                                           if super_vertex.is_loop_exit_edge]
#                 parent_subgraph = get_subgraph_of_outer_loop(subgraph,
#                                                              loop_exit_super_blocks[0])
#
#                 counter = len(loop_exit_super_blocks)
#                 for succ_vertex in loop_exit_super_blocks:
#                     succ_vertex_in_parent_subgraph = \
#                         parent_subgraph.get_vertex_for_program_point \
#                             (succ_vertex.representative.program_point)
#                     constraint += '{} {}'.format(max(loop_bound_tuple),
#                                                  get_variable_for_program_point
#                                                  (succ_vertex_in_parent_subgraph.
#                                                   representative.
#                                                   program_point,
#                                                   self._variables))
#                     self.__loop_exit_edges.add(succ_vertex.
#                                                representative.
#                                                program_point)
#                     if counter > 1:
#                         constraint += ' + '
#                     counter -= 1
#                 constraint += ';'
#             self._constraints.append(constraint)
#
#         def create_global_loop_bound_constraint(abstract_vertex):
#             loop_bound_tuple = program_point_data.get_loop_bound(abstract_vertex.
#                                                                  program_point)
#             constraint = get_variable_for_program_point(abstract_vertex.program_point,
#                                                         self._variables)
#             constraint += ' <= '
#             constraint += '{};'.format(sum(loop_bound_tuple))
#             self._constraints.append(constraint)
#
#         create_local_loop_bound_constraint(abstract_vertex)
#         if level > 1:
#             create_global_loop_bound_constraint(abstract_vertex)
#
#
# class IntegerLinearProgramForSuperBlockGraphWithFolding(IntegerLinearProgram):
#     """
#     The integer linear program derived after pre-solving execution times of
#     super blocks.
#     """
#
#     def __init__(self,
#                  control_flow_graph,
#                  loop_nesting_tree,
#                  program_point_data):
#         ConstraintSystem.__init__(self)
#         self._filename = '{}.{}.folded.super.ilp'. \
#             format(globals.args['filename_prefix'],
#                    control_flow_graph.name)
#         start = timeit.default_timer()
#         self.__create_objective_function(control_flow_graph,
#                                          program_point_data)
#         self.__create_constraints(control_flow_graph,
#                                   loop_nesting_tree,
#                                   program_point_data)
#         self._create_integer_constraint()
#         end = timeit.default_timer()
#         self._construction_time = end - start
#
#     def __create_objective_function(self,
#                                     control_flow_graph,
#                                     program_point_data):
#         self.obj_function = 'max: '
#         for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
#                 level_by_level_iterator \
#                     (abstract_vertices_only=True):
#             for abstract_vertex in abstract_vertices:
#                 if is_basic_block(abstract_vertex.program_point):
#                     subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex,
#                                                                            redo=True)
#                     for super_vertex in subgraph:
#                         super_block_wcet = 0
#                         for induced_vertex in super_vertex.vertices:
#                             if is_basic_block(induced_vertex.program_point) \
#                                     and not induced_vertex.abstract:
#                                 super_block_wcet += program_point_data.get_wcet \
#                                     (induced_vertex.program_point)
#
#                         vertex_variable = get_variable_for_program_point \
#                             (super_vertex.representative.program_point,
#                              self._variables)
#                         self.obj_function += '{} {}'.format(super_block_wcet,
#                                                             vertex_variable)
#                         self.obj_function += ' + '
#         strip_index = self.obj_function.rfind('+')
#         self.obj_function = self.obj_function[:strip_index]
#         self.obj_function += ';'
#
#     def __create_constraints(self,
#                              control_flow_graph,
#                              loop_nesting_tree,
#                              program_point_data):
#         for level, tree_vertices in loop_nesting_tree.level_by_level_iterator \
#                     (abstract_vertices_only=True):
#             for abstract_vertex in tree_vertices:
#                 if is_basic_block(abstract_vertex.program_point):
#                     subgraph = control_flow_graph.get_super_block_subgraph \
#                         (abstract_vertex)
#                     for super_vertex in subgraph:
#                         if super_vertex.number_of_predecessors() > 1:
#                             self.__create_predecessor_super_block_constraints \
#                                 (subgraph, super_vertex)
#
#                         if super_vertex.number_of_successors() > 1 \
#                                 and not super_vertex.representative.abstract:
#                             self.__create_successor_super_block_constraints \
#                                 (control_flow_graph, subgraph, super_vertex)
#
#                     self.__create_loop_bound_constraints(control_flow_graph,
#                                                          loop_nesting_tree,
#                                                          level,
#                                                          abstract_vertex,
#                                                          program_point_data)
#
#     def __create_predecessor_super_block_constraints(self,
#                                                      subgraph,
#                                                      super_vertex):
#         if super_vertex.representative.abstract:
#             # An abstract vertex in its own super block
#             constraint = ''
#             counter = super_vertex.number_of_successors()
#             for succ_edge in super_vertex.successor_edge_iterator():
#                 super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
#                 constraint += get_variable_for_program_point(super_succ_vertex.
#                                                              representative.
#                                                              program_point,
#                                                              self._variables)
#                 if counter > 1:
#                     constraint += ' + '
#                 counter -= 1
#         else:
#             constraint = get_variable_for_program_point(super_vertex.
#                                                         representative.
#                                                         program_point,
#                                                         self._variables)
#
#         constraint += ' = '
#         counter = super_vertex.number_of_predecessors()
#         for pred_edge in super_vertex.predecessor_edge_iterator():
#             super_pred_vertex = subgraph.get_vertex \
#                 (pred_edge.vertex_id)
#             constraint += get_variable_for_program_point(super_pred_vertex.
#                                                          representative.
#                                                          program_point,
#                                                          self._variables)
#             if counter > 1:
#                 constraint += ' + '
#             counter -= 1
#         constraint += ';'
#         self._constraints.append(constraint)
#
#     def __create_successor_super_block_constraints(self,
#                                                    control_flow_graph,
#                                                    subgraph,
#                                                    super_vertex):
#         for _, partition in super_vertex.successor_edge_partition_iterator():
#             if len(partition) > 1:
#                 constraint = get_variable_for_program_point(super_vertex.
#                                                             representative.
#                                                             program_point,
#                                                             self._variables)
#                 constraint += ' = '
#                 counter = len(partition)
#                 for succ_edge in partition:
#                     super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
#                     constraint += get_variable_for_program_point \
#                         (super_succ_vertex.
#                          representative.
#                          program_point,
#                          self._variables)
#                     if counter > 1:
#                         constraint += ' + '
#                     counter -= 1
#                 constraint += ';'
#                 self._constraints.append(constraint)
#
#     def __create_loop_bound_constraints(self,
#                                         control_flow_graph,
#                                         loop_nesting_tree,
#                                         level,
#                                         abstract_vertex,
#                                         program_point_data):
#
#         def get_subgraph_of_outer_loop(subgraph, super_vertex):
#             for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree(). \
#                     level_by_level_iterator \
#                         (abstract_vertices_only=True):
#                 for abstract_vertex in abstract_vertices:
#                     if is_basic_block(abstract_vertex.program_point):
#                         candidate_subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
#                         if subgraph.root_vertex.vertex_id != \
#                                 candidate_subgraph.root_vertex.vertex_id \
#                                 and candidate_subgraph.has_vertex_for_program_point(super_vertex.
#                                                                                             representative.
#                                                                                             program_point):
#                             return candidate_subgraph
#
#         def create_local_loop_bound_constraint(abstract_vertex):
#             loop_exit_edge_in_parent_subgraph = {}
#             loop_bound_tuple = program_point_data.get_loop_bound \
#                 (abstract_vertex.program_point)
#             subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
#             super_vertex = subgraph.get_vertex_for_program_point(abstract_vertex.
#                                                                  program_point)
#             constraint = get_variable_for_program_point(super_vertex.
#                                                         representative.
#                                                         program_point,
#                                                         self._variables)
#             if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
#                 constraint += ' = {};'.format(max(loop_bound_tuple))
#             else:
#                 constraint += ' <= '
#                 loop_exit_super_blocks = [super_vertex for super_vertex in subgraph
#                                           if super_vertex.is_loop_exit_edge]
#                 parent_subgraph = get_subgraph_of_outer_loop(subgraph,
#                                                              loop_exit_super_blocks[0])
#
#                 counter = len(loop_exit_super_blocks)
#                 for succ_vertex in loop_exit_super_blocks:
#                     succ_vertex_in_parent_subgraph = \
#                         parent_subgraph.get_vertex_for_program_point \
#                             (succ_vertex.representative.program_point)
#                     constraint += '{} {}'.format(max(loop_bound_tuple),
#                                                  get_variable_for_program_point
#                                                  (succ_vertex_in_parent_subgraph.
#                                                   representative.
#                                                   program_point,
#                                                   self._variables))
#                     loop_exit_edge_in_parent_subgraph[succ_vertex] = \
#                         succ_vertex_in_parent_subgraph
#                     if counter > 1:
#                         constraint += ' + '
#                     counter -= 1
#                 constraint += ';'
#             self._constraints.append(constraint)
#
#             # We need intra super block constraints on loop-exit edges.
#             for super_vertex_one, super_vertex_two in loop_exit_edge_in_parent_subgraph.items():
#                 constraint = get_variable_for_program_point(super_vertex_one.
#                                                             representative.
#                                                             program_point, self._variables)
#                 constraint += ' = '
#                 constraint += get_variable_for_program_point(super_vertex_two.
#                                                              representative.
#                                                              program_point, self._variables)
#                 constraint += ';'
#                 self._constraints.append(constraint)
#
#         def create_global_loop_bound_constraint(abstract_vertex):
#             loop_bound_tuple = program_point_data.get_loop_bound \
#                 (abstract_vertex.program_point)
#             subgraph = control_flow_graph.get_super_block_subgraph \
#                 (abstract_vertex)
#             super_vertex = subgraph.get_vertex_for_program_point \
#                 (abstract_vertex.program_point)
#             constraint = get_variable_for_program_point(super_vertex.
#                                                         representative.
#                                                         program_point,
#                                                         self._variables)
#             constraint += ' <= '
#             constraint += '{};'.format(sum(loop_bound_tuple))
#             self._constraints.append(constraint)
#
#         create_local_loop_bound_constraint(abstract_vertex)
#         if level > 1:
#             create_global_loop_bound_constraint(abstract_vertex)
#
#
# class IntegerLinearProgramForInstrumentationPointGraph(IntegerLinearProgram):
#     """
#     The integer linear program derived from a control flow graph, execution
#     times of basic blocks and upper bounds on loop header execution counts.
#     """
#
#     def __init__(self,
#                  instrumentation_point_graph,
#                  loop_nesting_tree,
#                  program_point_data):
#         ConstraintSystem.__init__(self)
#         self._filename = '{}.{}.ipg.ilp'.format(globals.args['filename_prefix'],
#                                                 instrumentation_point_graph.name)
#         start = timeit.default_timer()
#         self.__create_objective_function(instrumentation_point_graph,
#                                          program_point_data)
#         self.__create_structural_constraints(instrumentation_point_graph)
#         self.__create_loop_bound_constraints(instrumentation_point_graph,
#                                              loop_nesting_tree,
#                                              program_point_data)
#         self._create_integer_constraint()
#         end = timeit.default_timer()
#         self._construction_time = end - start
#
#     def __evaluate_wcet_of_edge(self,
#                                 instrumentation_point_graph,
#                                 program_point_data,
#                                 edge):
#         depth_first_search = directed_graphs.DepthFirstSearch(edge.path_expression,
#                                                               edge.path_expression.root_vertex,
#                                                               False)
#         evaluations = {}
#         for path_expression_vertex in depth_first_search.post_order:
#             if isinstance(path_expression_vertex, ProgramPointVertex):
#                 vertex = instrumentation_point_graph.get_vertex_for_program_point \
#                     (path_expression_vertex.program_point)
#                 evaluations[path_expression_vertex] = program_point_data.get_wcet(vertex.program_point)
#             else:
#                 if path_expression_vertex.operator == RegularExpressionVertex.SEQUENCE:
#                     wcet = 0
#                     for succ_edge in path_expression_vertex.successor_edge_iterator():
#                         path_expression_succ_vertex = \
#                             edge.path_expression.get_vertex(succ_edge.vertex_id)
#                         wcet += evaluations[path_expression_succ_vertex]
#                     evaluations[path_expression_vertex] = wcet
#                 elif path_expression_vertex.operator == RegularExpressionVertex.ALTERNATIVE:
#                     wcet = 0
#                     for succ_edge in path_expression_vertex.successor_edge_iterator():
#                         path_expression_succ_vertex = \
#                             edge.path_expression.get_vertex(succ_edge.vertex_id)
#                         wcet = max(wcet, evaluations[path_expression_succ_vertex])
#                     evaluations[path_expression_vertex] = wcet
#
#         return evaluations[edge.path_expression.root_vertex]
#
#     def __create_objective_function(self,
#                                     instrumentation_point_graph,
#                                     program_point_data):
#         self.obj_function = 'max: '
#
#         counter = instrumentation_point_graph.number_of_vertices()
#         for vertex in instrumentation_point_graph:
#             vertex_variable = get_variable_for_program_point(vertex.program_point,
#                                                              self._variables)
#             self.obj_function += '{} {}'.format(program_point_data.get_wcet(vertex.program_point),
#                                                 vertex_variable)
#             if counter > 1:
#                 self.obj_function += ' + '
#             counter -= 1
#
#         self.obj_function += ' + '
#
#         counter = instrumentation_point_graph.number_of_edges()
#         for vertex in instrumentation_point_graph:
#             for succ_edge in vertex.successor_edge_iterator():
#                 succ_vertex = instrumentation_point_graph.get_vertex(succ_edge.vertex_id)
#                 wcet_of_edge = self.__evaluate_wcet_of_edge(instrumentation_point_graph,
#                                                             program_point_data,
#                                                             succ_edge)
#                 vertex_variable = get_variable_for_edge_between_program_points(vertex.program_point,
#                                                                                succ_vertex.program_point,
#                                                                                self._variables)
#                 self.obj_function += '{} {}'.format(wcet_of_edge, vertex_variable)
#                 if counter > 1:
#                     self.obj_function += ' + '
#                 counter -= 1
#         self.obj_function += ';'
#
#     def __create_structural_constraints(self, instrumentation_point_graph):
#         for vertex in instrumentation_point_graph:
#             constraint = get_variable_for_program_point(vertex.program_point,
#                                                         self._variables)
#             constraint += ' = '
#             counter = vertex.number_of_successors()
#             for succ_edge in vertex.successor_edge_iterator():
#                 succ_vertex = instrumentation_point_graph.get_vertex \
#                     (succ_edge.vertex_id)
#                 constraint += get_variable_for_edge_between_program_points(vertex.program_point,
#                                                                            succ_vertex.program_point,
#                                                                            self._variables)
#                 if counter > 1:
#                     constraint += ' + '
#                 counter -= 1
#             constraint += ';'
#             self._constraints.append(constraint)
#
#             constraint = get_variable_for_program_point(vertex.program_point,
#                                                         self._variables)
#             constraint += ' = '
#             counter = vertex.number_of_predecessors()
#             for pred_edge in vertex.predecessor_edge_iterator():
#                 pred_vertex = instrumentation_point_graph.get_vertex \
#                     (pred_edge.vertex_id)
#                 constraint += get_variable_for_edge_between_program_points(pred_vertex.program_point,
#                                                                            vertex.program_point,
#                                                                            self._variables)
#                 if counter > 1:
#                     constraint += ' + '
#                 counter -= 1
#             constraint += ';'
#             self._constraints.append(constraint)
#
#     def __create_loop_bound_constraints(self,
#                                         instrumentation_point_graph,
#                                         loop_nesting_tree,
#                                         program_point_data):
#
#         def create_local_loop_bound_constraint(abstract_vertex):
#             if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
#                 loop_entry_destinations = instrumentation_point_graph.get_loop_backedge_destinations(abstract_vertex)
#                 for vertex in loop_entry_destinations:
#                     constraint = get_variable_for_program_point(vertex.program_point,
#                                                                 self._variables)
#                     constraint += ' = 1;'
#                     self._constraints.append(constraint)
#             else:
#                 loop_entry_destinations = instrumentation_point_graph.get_loop_entry_destinations(abstract_vertex)
#                 for vertex in loop_entry_destinations:
#                     constraint = get_variable_for_program_point(vertex.program_point,
#                                                                 self._variables)
#                     loop_entry_predecessor_vertices = set()
#                     for pred_edge in vertex.predecessor_edge_iterator():
#                         pred_vertex = instrumentation_point_graph.get_vertex(pred_edge.vertex_id)
#                         abstract_vertex_two = loop_nesting_tree.get_header_abstract_vertex_for_program_point(
#                             pred_vertex.program_point)
#                         if not loop_nesting_tree.is_ancestor(abstract_vertex, abstract_vertex_two):
#                             loop_entry_predecessor_vertices.add(pred_vertex)
#
#                     constraint += ' <= '
#                     counter = len(loop_entry_predecessor_vertices)
#                     for pred_vertex in loop_entry_predecessor_vertices:
#                         constraint += '{} {}'.format(max
#                                                      (program_point_data.get_loop_bound
#                                                       (abstract_vertex.program_point)),
#                                                      get_variable_for_program_point
#                                                      (pred_vertex.program_point,
#                                                       self._variables))
#                         if counter > 1:
#                             constraint += ' + '
#                         counter -= 1
#                     constraint += ';'
#                 self._constraints.append(constraint)
#
#         def create_global_loop_bound_constraint(abstract_vertex):
#             pass
#
#         for level, tree_vertices in loop_nesting_tree. \
#                 level_by_level_iterator \
#                     (abstract_vertices_only=True):
#             for abstract_vertex in tree_vertices:
#                 if is_basic_block(abstract_vertex.program_point):
#                     create_local_loop_bound_constraint(abstract_vertex)
#                     if level > 1:
#                         create_global_loop_bound_constraint(abstract_vertex)
