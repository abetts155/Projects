

import random
import abc
import os
import timeit
import subprocess
import shlex
import decimal


from tools.lib.utils import config
from tools.lib.utils import debug
from tools.lib.system.vertices import ProgramPointVertex


def calculate_wcet_using_integer_linear_programming(program,
                                                    repeat=1):
    
    for repetition in range(1, repeat+1):
        debug.verbose_message('Repetition {}'.format(repetition), __name__)
        for control_flow_graph in program.control_flow_graph_iterator():
            debug.verbose_message(control_flow_graph.name, __name__)
            timing_data = program.get_timing_data_for_function\
                            (control_flow_graph.name)
            ilp = IntegerLinearProgramForControlFlowGraph(control_flow_graph,
                                                          timing_data)
            ilp.solve()
            

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
        self.filename = None
        self.wcet = None
        self.solve_time = 0.0
        self.construction_time = 0.0
        self.variable_execution_counts = {}
        self.the_constraints = []
        self.the_variables = set()
        
    
    def cleanup(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)
    
    
    def shuffle(self):
        random.shuffle(self.the_constraints)
    
        
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
        counter = len(self.the_variables)
        for variable in self.the_variables:
            self.int_constraint += ' {}'.format(variable)
            if counter > 1:
                self.int_constraint += ','
            counter -= 1
        self.int_constraint += ';'

      
    def write_to_file(self):
        with open(self.filename, 'w') as the_file:
            the_file.write(self.obj_function)
            the_file.write(get_new_line(2))
            for constraint in self.the_constraints:
                the_file.write(constraint)
                the_file.write(get_new_line())
            the_file.write(get_new_line())
            the_file.write(self.int_constraint)
            the_file.write(get_new_line())
            
    
    def solve(self):
        self.write_to_file()
        command = 'lp_solve {}'.format(self.filename) 
        start = timeit.default_timer()
        process = subprocess.Popen(command,
                                   shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
        stdout, _  = process.communicate()
        end = timeit.default_timer()
        self.solve_time = end - start
        if process.returncode != 0:
            raise SolverError('Running {} failed'.format(command))
        stdout = stdout.decode()
        for line in stdout.split(os.linesep):
            line = line.strip()
            lexemes = shlex.split(line)
            if line.startswith('Value of objective function'):
                self.wcet = int(round(float(lexemes[-1]))) 
            elif line.startswith(edge_variable_prefix) \
            or line.startswith(vertex_variable_prefix):
                assert len(lexemes) == 2
                self.variable_execution_counts[lexemes[0]] =\
                    int(decimal.Decimal(lexemes[1]))

    
    
class IntegerLinearProgramForControlFlowGraph(IntegerLinearProgram):
    
    def __init__(self, control_flow_graph, timing_analysis_data):
        ConstraintSystem.__init__(self)
        self.filename = '{}.{}.cfg.ilp'.format(config.get_filename_prefix(), 
                                               control_flow_graph.name)
        start = timeit.default_timer()
        self.__create_objective_function(control_flow_graph, 
                                         timing_analysis_data)
        self.__create_structural_constraints(control_flow_graph)
        self.__create_loop_bound_constraints(control_flow_graph,
                                             timing_analysis_data)
        self._create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
        
        
    def __create_objective_function (self, 
                                     control_flow_graph, 
                                     timing_analysis_data):
        self.obj_function = 'max: '
        counter = control_flow_graph.number_of_vertices()
        for vertex in control_flow_graph:
            vertex_variable = get_execution_count_variable(vertex.program_point)   
            self.the_variables.add(vertex_variable)    
            self.obj_function +=\
                '{} {}'.format(timing_analysis_data.get_wcet(vertex),
                               vertex_variable)
            if counter > 1:
                self.obj_function += ' + '
            counter -= 1
        self.obj_function += ';'


    def __create_structural_constraints (self, control_flow_graph):
        handled_flow_in_constraint = set()
        for vertex in control_flow_graph:
            if vertex.number_of_predecessors() == 1:
                if vertex not in handled_flow_in_constraint:
                    new_constraint = get_execution_count_variable\
                                        (vertex.program_point)
                    new_constraint += ' = '
                    pred_vertex = control_flow_graph.\
                                    get_vertex(vertex.
                                               get_ith_predecessor_edge(0).
                                               vertex_id)
                    new_constraint += get_execution_count_variable\
                                        (pred_vertex.program_point)
                    new_constraint += ';'
                    self.the_constraints.append(new_constraint)
            else:
                new_constraint = get_execution_count_variable\
                                    (vertex.program_point)
                new_constraint += ' = '
                counter = vertex.number_of_predecessors()
                for pred_edge in vertex.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph.\
                                    get_vertex(pred_edge.vertex_id)
                    new_constraint += get_execution_count_variable\
                                    (pred_vertex.program_point)
                    if counter > 1:
                        new_constraint += ' + '
                    counter -= 1 
                new_constraint += ';'
                self.the_constraints.append(new_constraint)
                
            if vertex.number_of_successors() > 1:
                new_constraint = get_execution_count_variable\
                                    (vertex.program_point)
                new_constraint += ' = '
                counter = vertex.number_of_successors()
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = control_flow_graph.\
                                    get_vertex(succ_edge.vertex_id)
                    handled_flow_in_constraint.add(succ_vertex)
                    new_constraint += get_execution_count_variable\
                                    (succ_vertex.program_point)
                    if counter > 1:
                        new_constraint += ' + '
                    counter -= 1 
                new_constraint += ';'
                self.the_constraints.append(new_constraint)     
            
            
    def __create_loop_bound_constraints(self, 
                                        control_flow_graph,
                                        timing_analysis_data):
        loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
        for header in loop_nesting_tree.header_iterator():
            new_constraint = get_execution_count_variable(header.program_point)
            
            if header.program_point == loop_nesting_tree.root_vertex.program_point:
                new_constraint += ' = 1;'
            else:
                loop_body = loop_nesting_tree.get_loop_body(header)
                loop_entry_predecessor_vertices = set()
                for pred_edge in header.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph.get_vertex(pred_edge.vertex_id)
                    if pred_vertex not in loop_body:
                        loop_entry_predecessor_vertices.add(pred_vertex)
                       
                new_constraint += ' <= ' 
                counter = len(loop_entry_predecessor_vertices)
                for pred_vertex in loop_entry_predecessor_vertices:
                    new_constraint += '{} {}'.format(timing_analysis_data.
                                                     get_loop_bound(header),
                                                     get_execution_count_variable
                                                     (pred_vertex.program_point))
                    if counter > 1:
                        new_constraint += ' + '
                    counter -= 1 
                new_constraint += ';'
            
            self.the_constraints.append(new_constraint)
    
    
    
    