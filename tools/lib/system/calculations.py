
import abc
import os
import timeit
import subprocess
import decimal
import re

from tools.lib.utils import config
from tools.lib.utils import debug
from tools.lib.system.vertices import ProgramPointVertex


def calculate_wcet_using_integer_linear_programming(program,
                                                    repeat=1):
    
    for repetition in range(1, repeat+1):
        debug.verbose_message('Repetition {}'.format(repetition), __name__)
        for control_flow_graph in program.control_flow_graph_iterator():
            # The reason we grab the loop-nesting tree here and not inside the
            # constraint system is because we time how long it takes to 
            # construct the constraint system.  Since the loop-nesting tree is
            # built on the fly, it may happen that a portion of the time to
            # construct the constraint system is unfairly attributed to that
            # activity.
            loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
            control_flow_graph.create_timing_data_if_required()
            ilp_for_control_flow_graph = IntegerLinearProgramForControlFlowGraph\
                                            (control_flow_graph,
                                             loop_nesting_tree)
            ilp_for_control_flow_graph.solve()
            debug.verbose_message('wcet({})={}'.
                                  format(control_flow_graph.name,
                                         ilp_for_control_flow_graph.wcet), 
                                  __name__)
              
            ilp_for_super_block_graph = IntegerLinearProgramForSuperBlockGraph\
                                            (control_flow_graph,
                                             loop_nesting_tree)
            ilp_for_super_block_graph.solve()
            debug.verbose_message('wcet({})={}'.
                                  format(control_flow_graph.name,
                                         ilp_for_super_block_graph.wcet), 
                                  __name__)
             

edge_variable_prefix = 'E_'
vertex_variable_prefix = 'V_'
def get_execution_count_variable(program_point):
    if ProgramPointVertex.is_basic_block(program_point):
        return '{}{}'.format(vertex_variable_prefix,
                         program_point)
    return '{}{}_{}'.format(edge_variable_prefix, 
                            program_point[0],
                            program_point[1])
    


wcet_variable_prefix   = 'W_'
def get_vertex_wcet_variable(vertex_id):
    return '{}{}'.format(wcet_variable_prefix, vertex_id)


def get_new_line(num=1):
    return '\n' * num



class ConstraintSystem:
    
    """
    Any constraint system that solves to a WCET estimate must, at the least,
    generate an objective function, structural constraints and execution
    count constraints on program points in loops.
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
    
       
    @abc.abstractmethod
    def __create_objective_function(self):
        pass
    
    
    @abc.abstractmethod
    def __create_structural_constraints(self):
        pass
    
    
    @abc.abstractmethod
    def __create_loop_bound_constraints(self):
        pass

   

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
        stdout, _  = process.communicate()
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
                    self._variable_execution_counts[lexemes[0]] =\
                        int(decimal.Decimal(lexemes[1]))
                else:
                    self._wcet = int(round(float(lexemes[-1])))
    
        # We only need the ILP file for debugging purposes        
        if not config.Arguments.debug and os.path.exists(self._filename):
            os.remove(self._filename)

    
    
class IntegerLinearProgramForControlFlowGraph(IntegerLinearProgram):
    
    """
    The integer linear program derived from a control flow graph, execution 
    times of basic blocks and upper bounds on loop header execution counts.
    """
    
    def __init__(self, control_flow_graph, loop_nesting_tree):
        ConstraintSystem.__init__(self)
        self._filename = '{}.{}.cfg.ilp'.format(config.get_filename_prefix(), 
                                               control_flow_graph.name)
        start = timeit.default_timer()
        self.__create_objective_function(control_flow_graph)
        self.__create_structural_constraints(control_flow_graph)
        self.__create_loop_bound_constraints(control_flow_graph,
                                             loop_nesting_tree)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self._construction_time = end - start
        
        
    def __create_objective_function(self, 
                                    control_flow_graph):
        self.obj_function = 'max: '
        counter = control_flow_graph.number_of_basic_blocks()
        for vertex in control_flow_graph:
            if ProgramPointVertex.is_basic_block(vertex.program_point):
                vertex_variable = get_execution_count_variable(vertex.program_point) 
                self.obj_function += '{} {}'.format\
                                        (vertex.wcet, vertex_variable)
                if counter > 1:
                    self.obj_function += ' + '
                counter -= 1
        self.obj_function += ';'


    def __create_structural_constraints(self, control_flow_graph):
        handled_flow_in_constraint = set()
        for vertex in control_flow_graph:
            if vertex.number_of_predecessors() == 1:
                if vertex not in handled_flow_in_constraint:
                    vertex_variable = get_execution_count_variable\
                                        (vertex.program_point)
                    self._variables.add(vertex_variable)
                    constraint = vertex_variable
                    constraint += ' = '
                    pred_vertex = control_flow_graph.\
                                    get_vertex(vertex.
                                               get_ith_predecessor_edge(0).
                                               vertex_id)
                    pred_vertex_variable = get_execution_count_variable\
                                            (pred_vertex.program_point)
                    self._variables.add(pred_vertex_variable)
                    constraint += pred_vertex_variable
                    constraint += ';'
                    self._constraints.add(constraint)                    
            else:
                vertex_variable = get_execution_count_variable\
                                        (vertex.program_point)
                self._variables.add(vertex_variable)
                constraint = vertex_variable
                constraint += ' = '
                counter = vertex.number_of_predecessors()
                for pred_edge in vertex.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph.\
                                    get_vertex(pred_edge.vertex_id)
                    pred_vertex_variable = get_execution_count_variable\
                                            (pred_vertex.program_point)
                    self._variables.add(pred_vertex_variable)
                    constraint += pred_vertex_variable
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1 
                constraint += ';'
                self._constraints.add(constraint)
                
            if vertex.number_of_successors() > 1:
                vertex_variable = get_execution_count_variable\
                                        (vertex.program_point)
                self._variables.add(vertex_variable)
                constraint = vertex_variable
                constraint += ' = '
                counter = vertex.number_of_successors()
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = control_flow_graph.\
                                    get_vertex(succ_edge.vertex_id)
                    succ_vertex_variable = get_execution_count_variable\
                                            (succ_vertex.program_point)
                    self._variables.add(succ_vertex_variable)
                    constraint += succ_vertex_variable
                    handled_flow_in_constraint.add(succ_vertex)
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1 
                constraint += ';'
                self._constraints.add(constraint)     
            
            
    def __create_loop_bound_constraints(self, 
                                        control_flow_graph,
                                        loop_nesting_tree):
        
        def create_local_loop_bound_constraint(abstract_vertex):
            header = control_flow_graph.get_vertex_for_program_point\
                                (abstract_vertex.program_point)
            constraint = get_execution_count_variable(header.program_point)
            if header.program_point == loop_nesting_tree.root_vertex.program_point:
                constraint += ' = {};'.format(max(header.loop_bound))
            else:
                loop_body = loop_nesting_tree.\
                                get_loop_body_for_program_point(header.program_point)
                loop_entry_predecessor_vertices = set()
                for pred_edge in header.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph.get_vertex(pred_edge.vertex_id)
                    if pred_vertex not in loop_body:
                        loop_entry_predecessor_vertices.add(pred_vertex)
                       
                constraint += ' <= ' 
                counter = len(loop_entry_predecessor_vertices)
                for pred_vertex in loop_entry_predecessor_vertices:
                    constraint += '{} {}'.format(max(header.loop_bound),
                                                     get_execution_count_variable
                                                     (pred_vertex.program_point))
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1 
                constraint += ';'
            self._constraints.add(constraint)
            
        
        def create_global_loop_bound_constraint(abstract_vertex):
            header = control_flow_graph.get_vertex_for_program_point\
                                (abstract_vertex.program_point)
            constraint = get_execution_count_variable(header.program_point)
            constraint += ' <= '
            constraint += '{};'.format(sum(header.loop_bound))
            self._constraints.add(constraint)
            
        
        for _, tree_vertices in loop_nesting_tree.\
                                    level_by_level_iterator\
                                        (abstract_vertices_only=True):
            for abstract_vertex in tree_vertices:
                if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
                    create_local_loop_bound_constraint(abstract_vertex)
                    create_global_loop_bound_constraint(abstract_vertex)
                    
            
    

class IntegerLinearProgramForSuperBlockGraph(IntegerLinearProgram):
    
    """
    The integer linear program derived from a super block graph, execution 
    times of basic blocks and upper bounds on loop header execution counts.
    """
    
    def __init__(self, control_flow_graph, loop_nesting_tree):
        ConstraintSystem.__init__(self)
        self._filename = '{}.{}.super.ilp'.format(config.get_filename_prefix(), 
                                                  control_flow_graph.name)
        start = timeit.default_timer()
        self.__create_objective_function(control_flow_graph)
        self.__create_structural_constraints(control_flow_graph,
                                             loop_nesting_tree)
        self.__create_loop_bound_constraints(control_flow_graph,
                                             loop_nesting_tree)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self._construction_time = end - start
                
        
    def __create_objective_function(self, control_flow_graph):
        self.obj_function = 'max: '
        counter = control_flow_graph.number_of_basic_blocks()
        for _, subgraph in control_flow_graph.super_block_graph_iterator():
            for super_vertex in subgraph:
                for induced_vertex in super_vertex.vertices:
                    if ProgramPointVertex.is_basic_block(induced_vertex.program_point)\
                    and not induced_vertex.abstract:
                        vertex_variable = get_execution_count_variable\
                                            (induced_vertex.program_point)   
                        self._variables.add(vertex_variable) 
                        cfg_vertex = control_flow_graph.get_vertex_for_program_point\
                                        (induced_vertex.program_point)
                        self.obj_function += '{} {}'.format(cfg_vertex.wcet,
                                                            vertex_variable)
                        if counter > 1:
                            self.obj_function += ' + '
                        counter -= 1
        self.obj_function += ';'
        
        
    def __create_structural_constraints(self,
                                        control_flow_graph,
                                        loop_nesting_tree):
        for _, subgraph in control_flow_graph.super_block_graph_iterator():
            for super_vertex in subgraph:
                self.__create_intra_super_block_constraints(super_vertex)
                        
                if super_vertex.number_of_predecessors() > 1\
                and not super_vertex.representative.abstract:
                    self.__create_predecessor_super_block_constraints(subgraph,
                                                                      super_vertex)
                    
                if super_vertex.number_of_successors() > 1:
                    self.__create_successor_super_block_constraints(control_flow_graph,
                                                                    subgraph,
                                                                    super_vertex)
                    
                            
                            
    def __create_intra_super_block_constraints(self, 
                                               super_vertex):
        for induced_vertex in super_vertex.vertices:
            if induced_vertex != super_vertex.representative\
            and not induced_vertex.abstract\
            and ProgramPointVertex.is_basic_block(induced_vertex.program_point):
                constraint = get_execution_count_variable\
                                    (induced_vertex.program_point)
                constraint += ' = '
                constraint += get_execution_count_variable\
                                    (super_vertex.representative.program_point)
                constraint += ';'
                self._constraints.add(constraint)
                
                
    def __create_predecessor_super_block_constraints(self,
                                                     subgraph, 
                                                     super_vertex):
        constraint = get_execution_count_variable(super_vertex.
                                                      representative.
                                                      program_point)
        constraint += ' = '
        counter = super_vertex.number_of_predecessors()
        for pred_edge in super_vertex.predecessor_edge_iterator():
            super_pred_vertex = subgraph.get_vertex\
                                (pred_edge.vertex_id)
            constraint += get_execution_count_variable(super_pred_vertex.
                                                           representative.
                                                           program_point)
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
                constraint = get_execution_count_variable(super_vertex.
                                                          representative.
                                                          program_point)
                constraint += ' = '
                counter = len(partition)
                for succ_edge in partition:
                    super_succ_vertex = subgraph.get_vertex(succ_edge.vertex_id)
                    constraint += get_execution_count_variable\
                                        (super_succ_vertex.representative.program_point)
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1
                constraint += ';'
                self._constraints.add(constraint)
    
    
    def __create_loop_bound_constraints(self,
                                        control_flow_graph,
                                        loop_nesting_tree):
        
        def create_local_loop_bound_constraint(abstract_vertex):
            header = control_flow_graph.get_vertex_for_program_point\
                                            (abstract_vertex.program_point)
            subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
            constraint = get_execution_count_variable(abstract_vertex.program_point)
            if abstract_vertex.program_point == loop_nesting_tree.root_vertex.program_point:
                constraint += ' = {};'.format(max(header.loop_bound))
            else:
                constraint += ' <= ' 
                loop_exit_super_blocks = [super_vertex for super_vertex in subgraph
                                          if super_vertex.is_loop_exit_edge]
                
                counter = len(loop_exit_super_blocks)
                for succ_vertex in loop_exit_super_blocks:
                    constraint += '{} {}'.format(max(header.loop_bound),
                                                 get_execution_count_variable
                                                 (succ_vertex.representative.
                                                  program_point))
                    if counter > 1:
                        constraint += ' + '
                    counter -= 1 
                constraint += ';'
            self._constraints.add(constraint)
            
        
        def create_global_loop_bound_constraint(abstract_vertex):
            header = control_flow_graph.get_vertex_for_program_point\
                                            (abstract_vertex.program_point)
            constraint = get_execution_count_variable(abstract_vertex.program_point)
            constraint += ' <= '
            constraint += '{};'.format(sum(header.loop_bound))
            self._constraints.add(constraint)
        
        
        for _, tree_vertices in loop_nesting_tree.\
                                    level_by_level_iterator\
                                        (abstract_vertices_only=True):
            for abstract_vertex in tree_vertices:
                if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
                    create_local_loop_bound_constraint(abstract_vertex)
                    create_global_loop_bound_constraint(abstract_vertex) 
                    
        
    
    