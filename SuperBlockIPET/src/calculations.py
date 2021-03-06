import directed_graphs
import vertices
import config
import utils
import debug
import os
import timeit
import subprocess
import shlex
import abc
import random
import re
import decimal
import numpy
import collections

edge_variable_prefix   = "E_"
vertex_variable_prefix = "V_"
wcet_variable_prefix   = "W_"
    
def get_edge_execution_count_variable(predID, succID):
    return "%s%d_%d" % (edge_variable_prefix, predID, succID)

def get_vertex_execution_count_variable(vertexID):
    return "%s%d" % (vertex_variable_prefix, vertexID)

def get_vertex_WCET_variable(vertexID):
    return "%s%d" % (wcet_variable_prefix, vertexID)

def get_execution_count_variable_for_program_point(program_point):
    if isinstance(program_point, vertices.CFGVertex):
        return get_vertex_execution_count_variable(program_point.vertexID) 
    elif isinstance(program_point, vertices.CFGEdge):
        return get_edge_execution_count_variable(program_point.edge[0], program_point.edge[1])
    else:
        assert isinstance(program_point, vertices.HeaderVertex)
        return get_vertex_execution_count_variable(program_point.vertexID) 

def get_new_line (num=1):
    return "\n" * num

class LpSolve:
    comma      = ","
    equals     = " = "
    int_       = "int"
    lte        = " <= "
    max_       = "max: "
    plus       = " + "
    semi_colon = ";"
    
class ConstraintSystem:
    __metaclass__ = abc.ABCMeta
    
    def __init__(self, filename):
        self.filename = filename
        self.wcet = -1
        self.solve_time = 0.0
        self.construction_time = 0.0
        self.variable_execution_counts = {}
        self.the_constraints = []
        self.the_variables = set()
    
    def clean(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)
    
    def shuffle(self):
        random.shuffle(self.the_constraints)
        
    @abc.abstractmethod
    def create_objective_function(self):
        pass
    
    @abc.abstractmethod
    def create_structural_constraints(self):
        pass
    
    @abc.abstractmethod
    def create_loop_bound_constraints(self):
        pass
        
class ILP(ConstraintSystem):        
    def create_integer_constraint(self):
        self.int_constraint = "%s " % LpSolve.int_
        counter = len(self.the_variables)
        for variable in self.the_variables:
            self.int_constraint += variable
            if counter > 1:
                self.int_constraint += LpSolve.comma
            counter -= 1
        self.int_constraint += LpSolve.semi_colon
      
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
        debug.debug_message("Solving ILP", __name__, 10)
        self.write_to_file()
        cmd             = "lp_solve %s" % self.filename 
        start           = timeit.default_timer()
        proc            = subprocess.Popen(cmd, 
                                           shell=True,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.PIPE)
        stdout, stderr  = proc.communicate()
        end             = timeit.default_timer()
        self.solve_time = end - start
        if proc.returncode != 0:
            debug.exit_message("Running '%s' failed" % cmd)
        for line in stdout.split(os.linesep):
            line = line.strip()
            if line.startswith("Value of objective function"):
                lexemes   = shlex.split(line)
                self.wcet = long(round(float(lexemes[-1]))) 
            elif line.startswith(edge_variable_prefix) or line.startswith(vertex_variable_prefix):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Incorrectly detected variable execution count line '%s'" % line
                self.variable_execution_counts[lexemes[0]] = long(decimal.Decimal(lexemes[1])) 

class CreateCFGILP(ILP):
    def __init__ (self, data, cfg, lnt):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, cfg.name, "cfg")
        ILP.__init__(self, filename)
        start = timeit.default_timer()
        self.create_objective_function(data, cfg)
        self.create_structural_constraints(cfg)
        self.create_loop_bound_constraints(data, lnt, cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start

    def create_objective_function (self, data, cfg):
        self.obj_function = LpSolve.max_
        counter = cfg.number_of_vertices()
        for v in cfg:        
            wcet = data.get_basic_block_wcet(v.vertexID)
            self.obj_function += "%d %s" % (wcet, get_vertex_execution_count_variable(v.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, cfg):
        for v in cfg:
            self.the_variables.add(get_vertex_execution_count_variable(v.vertexID))
            new_constraint = ""
            new_constraint += get_vertex_execution_count_variable(v.vertexID)
            new_constraint += LpSolve.equals
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():    
                self.the_variables.add(get_edge_execution_count_variable(predID, v.vertexID))
                new_constraint += get_edge_execution_count_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1    
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)
            new_constraint = ""
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():
                new_constraint += get_edge_execution_count_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1
            new_constraint += LpSolve.equals
            counter = v.number_of_successors()
            for succID in v.successors.keys():
                new_constraint += get_edge_execution_count_variable(v.vertexID, succID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)       
            
    def create_loop_bound_constraints(self, data, lnt, cfg):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    upper_bound_list   = data.get_upper_bound_on_header(treev.headerID)
                    global_upper_bound = numpy.sum(upper_bound_list)
                    if upper_bound_list:
                        local_upper_bound = numpy.max(upper_bound_list) 
                    else:
                        local_upper_bound = 0 
                    if treev.level == 0:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(cfg.get_entryID())
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % global_upper_bound
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += LpSolve.lte
                        incoming_edges = lnt.get_loop_entry_edges(treev.headerID)
                        counter        = len(incoming_edges)
                        for predID, succID in incoming_edges:
                            new_constraint += "%d %s" % (local_upper_bound, 
                                                         get_edge_execution_count_variable(predID, succID))
                            if counter > 1:
                                new_constraint += LpSolve.plus 
                            counter -= 1
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                        if treev.level > 1:
                            new_constraint = ""
                            new_constraint += get_vertex_execution_count_variable(treev.headerID)
                            new_constraint += LpSolve.lte
                            new_constraint += "%d" % global_upper_bound
                            new_constraint += LpSolve.semi_colon
                            self.the_constraints.append(new_constraint)
        
class CreateSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "superg")
        ILP.__init__(self, filename)
        self.header_to_exit_super_blocks = {}
        self.supervs = set()
        start = timeit.default_timer()
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    subgraph = super_block_cfg.whole_body_subgraphs[treev.headerID]
                    self.create_intra_super_block_constraints(subgraph)
                    self.create_structural_constraints(lnt, super_block_cfg, subgraph)
                    self.create_loop_bound_constraints(data, treev)
        self.create_objective_function(data, cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
                        
    def create_objective_function (self, data, cfg):
        self.obj_function = LpSolve.max_
        counter = cfg.number_of_vertices()
        for superv in self.supervs:
            self.the_variables.add(get_execution_count_variable_for_program_point(superv.representative))
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    wcet     = data.get_basic_block_wcet(program_point.vertexID)
                    variable = get_vertex_execution_count_variable(program_point.vertexID)
                    self.obj_function += "%d %s" % (wcet, variable)
                    self.the_variables.add(variable)
                    if counter > 1:
                        self.obj_function += LpSolve.plus
                    counter -= 1
        self.obj_function += LpSolve.semi_colon
        
    def create_intra_super_block_constraints(self, subgraph):
        for superv in subgraph:
            self.supervs.add(superv)
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex) and program_point != superv.representative:
                    new_constraint = ""
                    new_constraint += get_vertex_execution_count_variable(program_point.vertexID)
                    new_constraint += LpSolve.equals
                    new_constraint += get_vertex_execution_count_variable(superv.representative.vertexID)
                    new_constraint += LpSolve.semi_colon
                    self.the_constraints.append(new_constraint)
            
    def create_structural_constraints(self, lnt, super_block_cfg, subgraph):
        for superv in subgraph:
            if superv.number_of_predecessors() > 1:
                new_constraint = ""
                new_constraint += get_execution_count_variable_for_program_point(superv.representative)
                new_constraint += LpSolve.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    super_predv = subgraph.getVertex(predID)
                    new_constraint += get_execution_count_variable_for_program_point(super_predv.representative)
                    if counter > 1:
                        new_constraint += LpSolve.plus
                    counter -= 1
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    if len(partition) > 1:
                        new_constraint = ""
                        new_constraint += get_execution_count_variable_for_program_point(superv.representative)
                        new_constraint += LpSolve.equals
                        counter = len(partition)
                        for succID in partition:
                            super_succv = subgraph.getVertex(succID)
                            if super_succv.exit_edge:
                                headerv         = lnt.getVertex(lnt.getVertex(super_succv.headerID).parentID)
                                parent_headerv  = lnt.getVertex(headerv.parentID)
                                parent_subgraph = super_block_cfg.whole_body_subgraphs[parent_headerv.headerID]
                                super_succv     = parent_subgraph.program_point_to_superv[super_succv.representative.edge]
                                if headerv.headerID not in self.header_to_exit_super_blocks:
                                    self.header_to_exit_super_blocks[headerv.headerID] = set()
                                self.header_to_exit_super_blocks[headerv.headerID].add(super_succv)
                            new_constraint += get_execution_count_variable_for_program_point(super_succv.representative)
                            if counter > 1:
                                new_constraint += LpSolve.plus
                            counter -= 1
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
            
    def create_loop_bound_constraints(self, data, treev):
        upper_bound_list   = data.get_upper_bound_on_header(treev.headerID)
        global_upper_bound = numpy.sum(upper_bound_list)
        if upper_bound_list:
            local_upper_bound = numpy.max(upper_bound_list) 
        else:
            local_upper_bound = 0 
        if treev.level == 0:
            new_constraint = ""
            new_constraint += get_vertex_execution_count_variable(treev.headerID)
            new_constraint += LpSolve.equals
            new_constraint += "%d" % global_upper_bound
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)
        else:
            new_constraint  = ""
            new_constraint += get_vertex_execution_count_variable(treev.headerID)
            new_constraint += LpSolve.lte
            counter         = len(self.header_to_exit_super_blocks[treev.headerID])
            for succ_superv in self.header_to_exit_super_blocks[treev.headerID]:
                new_constraint += "%d %s" % (local_upper_bound,
                                             get_execution_count_variable_for_program_point(succ_superv.representative))
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)
            if treev.level > 1:
                new_constraint  = ""
                new_constraint += get_vertex_execution_count_variable(treev.headerID)
                new_constraint += LpSolve.lte
                new_constraint += "%d" % global_upper_bound
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)
        
class CreateFoldedSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.folded.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "superg")
        ILP.__init__(self, filename)
        self.header_to_exit_super_blocks = {}
        self.superv_to_wcet = {}
        start = timeit.default_timer()
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    subgraph = super_block_cfg.whole_body_subgraphs[treev.headerID]
                    self.compute_wcet_of_super_blocks(data, subgraph)
                    self.create_structural_constraints(lnt, super_block_cfg, subgraph)
                    self.create_loop_bound_constraints(data, treev, subgraph)
        self.create_objective_function()
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
    
    def compute_wcet_of_super_blocks(self, data, subgraph):
        for superv in subgraph:
            wcet = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    wcet += data.get_basic_block_wcet(program_point.vertexID)
            self.superv_to_wcet[superv] = wcet
    
    def create_objective_function (self):
        self.obj_function = LpSolve.max_
        counter = len(self.superv_to_wcet)
        for superv, wcet in self.superv_to_wcet.iteritems():
            self.the_variables.add(get_vertex_execution_count_variable(superv.vertexID))
            self.obj_function += "%d %s" % (wcet, get_vertex_execution_count_variable(superv.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, lnt, super_block_cfg, subgraph):
        for superv in subgraph:
            if superv.number_of_predecessors() > 1:
                new_constraint = ""
                new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                new_constraint += LpSolve.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    new_constraint += get_vertex_execution_count_variable(predID)
                    if counter > 1:
                        new_constraint += LpSolve.plus
                    counter -= 1
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    if len(partition) > 1:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += LpSolve.equals
                        counter = len(partition)
                        for succID in partition:
                            super_succv = subgraph.getVertex(succID)
                            if super_succv.exit_edge:
                                headerv         = lnt.getVertex(lnt.getVertex(super_succv.headerID).parentID)
                                parent_headerv  = lnt.getVertex(headerv.parentID)
                                parent_subgraph = super_block_cfg.whole_body_subgraphs[parent_headerv.headerID]
                                super_succv     = parent_subgraph.program_point_to_superv[super_succv.representative.edge]
                                if headerv.headerID not in self.header_to_exit_super_blocks:
                                    self.header_to_exit_super_blocks[headerv.headerID] = set()
                                self.header_to_exit_super_blocks[headerv.headerID].add(super_succv)
                            new_constraint += get_vertex_execution_count_variable(super_succv.vertexID)
                            if counter > 1:
                                new_constraint += LpSolve.plus
                            counter -= 1
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)                      
                
    def create_loop_bound_constraints(self, data, treev, subgraph):
        upper_bound_list   = data.get_upper_bound_on_header(treev.headerID)
        global_upper_bound = numpy.sum(upper_bound_list)
        if upper_bound_list:
            local_upper_bound = numpy.max(upper_bound_list) 
        else:
            local_upper_bound = 0
        if treev.level == 0:
            new_constraint = ""
            new_constraint += get_vertex_execution_count_variable(subgraph.rootv.vertexID)
            new_constraint += LpSolve.equals
            new_constraint += "%d" % global_upper_bound
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)
        else:
            new_constraint = ""
            new_constraint += get_vertex_execution_count_variable(subgraph.rootv.vertexID)
            new_constraint += LpSolve.lte
            counter         = len(self.header_to_exit_super_blocks[treev.headerID])
            for succ_superv in self.header_to_exit_super_blocks[treev.headerID]:
                new_constraint += "%d %s" % (local_upper_bound,
                                             get_vertex_execution_count_variable(succ_superv.vertexID))
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)
            if treev.level > 1:
                new_constraint = ""
                new_constraint += get_vertex_execution_count_variable(subgraph.rootv.vertexID)
                new_constraint += LpSolve.lte
                new_constraint += "%d" % global_upper_bound
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)

class ECLIPSE:
    conjunct         = "," + get_new_line()
    clause_separator = ":-"
    domain_separator = " #:: "
    equals           = " #= "
    lib              = "lib"
    lt               = " #< "
    lte              = " #=< "
    multiply         = "*"
    plus             = " + "
    terminator       = "."
    
    @staticmethod
    def get_temp_list(suffix):
        return "VARS%d" % suffix

class CLP(ConstraintSystem):
    WCET = "WCET"
    PWCET = "PWCET"
    VERTEX_TIMES = "VERTEX_TIMES"
    VERTEX_COUNTS = "VERTEX_COUNTS"
    EDGE_COUNTS = "EDGE_COUNTS"
    OUTPUT_PREDICATE_HEAD = "print_results"
    
    def __init__ (self, filename):
        ConstraintSystem.__init__(self, filename)
        self.results_filename = filename + ".res"
        self.goal = "solve(%s)" % CLP.WCET
        self.variable_lists = []
        
    def clean(self):
        ConstraintSystem.clean(self)
        if os.path.exists(self.results_filename):
            os.remove(self.results_filename)
        
    def write_to_file(self):
        with open(self.filename, 'w') as the_file:
            self.add_required_packages(the_file)
            the_file.write(get_new_line())
            the_file.write("%s%s" % (self.goal,
                                     ECLIPSE.clause_separator))
            the_file.write(get_new_line())
            the_file.write(self.obj_function)
            the_file.write(get_new_line())
            for a_list in self.variable_lists:
                the_file.write(a_list)
            the_file.write(get_new_line())
            for constraint in self.the_constraints:
                the_file.write(constraint)
            the_file.write(get_new_line())
            self.add_epilogue(the_file)
            the_file.write(get_new_line(2))
            self.add_output_predicates(the_file)
            the_file.write(get_new_line(2))
            
    def add_required_packages(self, the_file):
        for lib in ['ic', 'branch_and_bound', 'lists', 'util']:
            the_file.write("%s%s(%s)%s%s" % (ECLIPSE.clause_separator,
                                             ECLIPSE.lib,
                                             lib,
                                             ECLIPSE.terminator,
                                             get_new_line()))
    
    def add_epilogue(self, the_file):
        the_file.write("%s%s%d%s%s%s" % (CLP.PWCET,
                                         ECLIPSE.equals,
                                         - 1,
                                         ECLIPSE.multiply,
                                         CLP.WCET,
                                         ECLIPSE.conjunct))
        the_file.write("append(%s,%s,%s)%s" % (CLP.EDGE_COUNTS,
                                               CLP.VERTEX_COUNTS,
                                               ECLIPSE.get_temp_list(0),
                                               ECLIPSE.conjunct))
        the_file.write("append(%s,%s,%s)%s" % (CLP.VERTEX_TIMES,
                                               ECLIPSE.get_temp_list(0),
                                               ECLIPSE.get_temp_list(1),
                                               ECLIPSE.conjunct))
        the_file.write("time(bb_min(search(%s,0,input_order,indomain_max,complete,[]),%s,bb_options{}))%s" % (ECLIPSE.get_temp_list(1),
                                                                                                              CLP.PWCET,
                                                                                                              ECLIPSE.conjunct))
        the_file.write("%s(%s, %s, %s)%s" % (CLP.OUTPUT_PREDICATE_HEAD,
                                             CLP.VERTEX_COUNTS,
                                             CLP.EDGE_COUNTS,
                                             CLP.WCET,
                                             ECLIPSE.terminator))               
    
    def add_output_predicates(self, the_file):
        file_handle = "F"        
        the_file.write('%s(%s,%s,%s) %s' % (CLP.OUTPUT_PREDICATE_HEAD,
                                            CLP.VERTEX_COUNTS,
                                            CLP.EDGE_COUNTS,
                                            CLP.WCET,
                                            ECLIPSE.clause_separator))
        the_file.write(get_new_line())
        the_file.write('open("%s",%s,%s)%s' % (self.results_filename,
                                               "write",
                                               file_handle,
                                               ECLIPSE.conjunct))
        the_file.write('print_list(%s,"%s: ",%s)%s' % (file_handle,
                                                       CLP.VERTEX_COUNTS,
                                                       CLP.VERTEX_COUNTS,
                                                       ECLIPSE.conjunct))
        the_file.write('print_list(%s,"%s: ",%s)%s' % (file_handle,
                                                       CLP.EDGE_COUNTS,
                                                       CLP.EDGE_COUNTS,
                                                       ECLIPSE.conjunct))
        the_file.write('write(%s, "%s: ")%s' % (file_handle,
                                                CLP.WCET,
                                                ECLIPSE.conjunct))
        the_file.write('write(%s,%s)%s' % (file_handle,
                                           CLP.WCET,
                                           ECLIPSE.conjunct))
        the_file.write('nl(%s)%s' % (file_handle,
                                     ECLIPSE.conjunct))
        the_file.write('close(%s)%s' % (file_handle,
                                        ECLIPSE.terminator))
        the_file.write(get_new_line(2))
        the_file.write('print_list(_,_,[])%s' % (ECLIPSE.terminator))
        the_file.write(get_new_line())
        the_file.write('print_list(%s,Name,[H|T])%s' % (file_handle,
                                                        ECLIPSE.clause_separator))
        the_file.write(get_new_line())
        the_file.write('write(%s,Name)%s' % (file_handle,
                                             ECLIPSE.conjunct))
        the_file.write('write(%s,H)%s' % (file_handle,
                                          ECLIPSE.conjunct))
        the_file.write('print_list1(%s,T)%s' % (file_handle,
                                                ECLIPSE.conjunct))
        the_file.write('nl(%s)%s' % (file_handle,
                                    ECLIPSE.terminator))
        the_file.write(get_new_line(2))        
        the_file.write('print_list1(_,[])%s' % (ECLIPSE.terminator))
        the_file.write(get_new_line())
        the_file.write('print_list1(%s,[H|T])%s' % (file_handle,
                                                    ECLIPSE.clause_separator))
        the_file.write(get_new_line())
        the_file.write('write(%s,",")%s' % (file_handle,
                                            ECLIPSE.conjunct))
        the_file.write('write(%s,H)%s' % (file_handle,
                                          ECLIPSE.conjunct))
        the_file.write('print_list1(%s,T)%s' % (file_handle,
                                                ECLIPSE.terminator))
        
    def solve(self):
        debug.debug_message("Solving CLP", __name__, 10)
        self.write_to_file()        
        cmd             = 'jeclipse -b %s -e "%s."' % (self.filename, self.goal) 
        start           = timeit.default_timer()
        proc            = subprocess.Popen(cmd,
                                           shell=True,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.PIPE)
        stdout, stderr  = proc.communicate()
        end             = timeit.default_timer()
        self.solve_time = end - start
        if proc.returncode != 0:
            debug.exit_message("Running '%s' failed" % cmd)
        with open(self.results_filename) as the_file:
            for line in the_file:
                if re.match(r'%s' % CLP.WCET, line):
                    values = re.findall(r'[0-9]+', line)
                    assert len(values) == 1, "Unable to find WCET value in CLP output file"
                    self.wcet = long(values[0])

class CreateCFGCLP(CLP):
    def __init__ (self, data, cfg, lnt):
        filename = "%s.%s.%s.ecl" % (config.Arguments.basepath + os.sep + config.Arguments.basename, cfg.name, "cfg")
        CLP.__init__(self, filename)
        start = timeit.default_timer()
        self.add_variables(cfg)
        self.create_objective_function(cfg)
        self.create_execution_time_domains(data, cfg)
        self.create_structural_constraints(cfg)
        self.create_loop_bound_constraints(data, cfg, lnt)
        end = timeit.default_timer()
        self.construction_time = end - start

    def add_variables(self, cfg):
        bb_execution_count_variables = []
        edge_execution_count_variables = []
        bb_execution_time_variables = []
        for v in cfg:
            bb_execution_count_variables.append(get_vertex_execution_count_variable(v.vertexID))
            bb_execution_time_variables.append(get_vertex_WCET_variable(v.vertexID))
            self.the_variables.add(get_vertex_execution_count_variable(v.vertexID))
            self.the_variables.add(get_vertex_WCET_variable(v.vertexID))
            for succID in v.successors.keys():
                edge_execution_count_variables.append(get_edge_execution_count_variable(v.vertexID, succID))
                self.the_variables.add(get_edge_execution_count_variable(v.vertexID, succID))
        self.variable_lists.append("%s = [%s]%s" % (CLP.VERTEX_COUNTS,
                                                    ','.join(v for v in bb_execution_count_variables),
                                                    ECLIPSE.conjunct))
        self.variable_lists.append("%s = [%s]%s" % (CLP.EDGE_COUNTS,
                                                    ','.join(v for v in edge_execution_count_variables),
                                                    ECLIPSE.conjunct))
        self.variable_lists.append("%s = [%s]%s" % (CLP.VERTEX_TIMES,
                                                    ','.join(v for v in bb_execution_time_variables),
                                                    ECLIPSE.conjunct))
    
    def create_objective_function(self, cfg):
        self.obj_function = CLP.WCET
        self.obj_function += ECLIPSE.equals
        counter = cfg.number_of_vertices()
        for v in cfg:
            self.obj_function += "%s%s%s" % (get_vertex_execution_count_variable(v.vertexID),
                                             ECLIPSE.multiply,
                                             get_vertex_WCET_variable(v.vertexID))
            if counter > 1:
                self.obj_function += ECLIPSE.plus
            counter -= 1
        self.obj_function += ECLIPSE.conjunct
        
    def create_execution_time_domains(self, data, cfg):
        for v in cfg:
            self.the_constraints.append("%s%s%d%s" % (get_vertex_WCET_variable(v.vertexID),
                                                      ECLIPSE.equals,
                                                      data.get_basic_block_wcet(v.vertexID),
                                                      ECLIPSE.conjunct))
        
    def create_structural_constraints(self, cfg):
        for v in cfg:
            new_constraint = get_vertex_execution_count_variable(v.vertexID)
            new_constraint += ECLIPSE.equals
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():
                new_constraint += get_edge_execution_count_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += ECLIPSE.plus
                counter -= 1
            new_constraint += ECLIPSE.conjunct
            self.the_constraints.append(new_constraint)
            new_constraint = ""
            counter = v.number_of_successors()
            for succID in v.successors.keys():
                new_constraint += get_edge_execution_count_variable(v.vertexID, succID)
                if counter > 1:
                    new_constraint += ECLIPSE.plus
                counter -= 1
            new_constraint += ECLIPSE.equals
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():
                new_constraint += get_edge_execution_count_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += ECLIPSE.plus
                counter -= 1
            new_constraint += ECLIPSE.conjunct
            self.the_constraints.append(new_constraint)
    
    def create_loop_bound_constraints(self, data, cfg, lnt):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(cfg.get_entryID())
                        new_constraint += ECLIPSE.equals
                        new_constraint += "%d" % data.get_upper_bound_on_header(cfg.get_entryID())
                        new_constraint += ECLIPSE.conjunct
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += ECLIPSE.lte
                        incoming_edges = lnt.get_loop_entry_edges(treev.headerID)
                        counter        = len(incoming_edges)
                        for predID, succID in incoming_edges:
                            new_constraint += "%d %s %s" % (data.get_upper_bound_on_header(treev.headerID),
                                                            ECLIPSE.multiply, 
                                                            get_edge_execution_count_variable(predID, succID))
                            if counter > 1:
                                new_constraint += ECLIPSE.plus 
                            counter -= 1
                        new_constraint += ECLIPSE.conjunct
                        self.the_constraints.append(new_constraint)        
        for v in cfg:            
            headerv = lnt.getVertex(lnt.getVertex(v.vertexID).parentID)
            lower_bound = 0
            upper_bound = data.get_upper_bound_on_header(headerv.headerID)
            new_constraint = "%s%s[%d..%d]%s" % (get_vertex_execution_count_variable(v.vertexID),
                                              ECLIPSE.domain_separator,
                                              lower_bound,
                                              upper_bound,
                                              ECLIPSE.conjunct)
            self.the_constraints.append(new_constraint) 
        for v in cfg:            
            headerv = lnt.getVertex(lnt.getVertex(v.vertexID).parentID)
            lower_bound = 0
            upper_bound = data.get_upper_bound_on_header(headerv.headerID)
            for succID in v.successors.keys(): 
                new_constraint = "%s%s[%d..%d]%s" % (get_edge_execution_count_variable(v.vertexID, succID),
                                           ECLIPSE.domain_separator,
                                           lower_bound,
                                           upper_bound,
                                           ECLIPSE.conjunct)
                self.the_constraints.append(new_constraint)
                
class CreateSuperBlockCFGCLP(CLP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.ecl" % (config.Arguments.basepath + os.sep + config.Arguments.basename, cfg.name, "superg")
        CLP.__init__(self, filename)
        start = timeit.default_timer()
        self.add_variables(super_block_cfg)
        self.create_objective_function(cfg)
        self.create_execution_time_domains(data, cfg)
        self.create_intra_super_block_constraints(super_block_cfg)
        self.create_structural_constraints(lnt, super_block_cfg)
        self.create_loop_bound_constraints(data, super_block_cfg, lnt)
        end = timeit.default_timer()
        self.construction_time = end - start
        
    def add_variables(self, super_block_cfg):
        bb_execution_count_variables = []
        edge_execution_count_variables = []
        bb_execution_time_variables = []
        for superv in super_block_cfg:
            if isinstance(superv.representative, vertices.CFGEdge):
                edge_execution_count_variables.append(get_execution_count_variable_for_program_point(superv.representative))
                self.the_variables.add(get_execution_count_variable_for_program_point(superv.representative))
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    bb_execution_count_variables.append(get_vertex_execution_count_variable(program_point.vertexID))
                    bb_execution_time_variables.append(get_vertex_WCET_variable(program_point.vertexID))
                    self.the_variables.add(get_vertex_execution_count_variable(program_point.vertexID))
                    self.the_variables.add(get_vertex_WCET_variable(program_point.vertexID))
        self.variable_lists.append("%s = [%s]%s" % (CLP.VERTEX_COUNTS,
                                                    ','.join(v for v in bb_execution_count_variables),
                                                    ECLIPSE.conjunct))
        self.variable_lists.append("%s = [%s]%s" % (CLP.EDGE_COUNTS,
                                                    ','.join(v for v in edge_execution_count_variables),
                                                    ECLIPSE.conjunct))
        self.variable_lists.append("%s = [%s]%s" % (CLP.VERTEX_TIMES,
                                                    ','.join(v for v in bb_execution_time_variables),
                                                    ECLIPSE.conjunct))
    
    def create_objective_function(self, cfg):
        self.obj_function = CLP.WCET
        self.obj_function += ECLIPSE.equals
        counter = cfg.number_of_vertices()
        for v in cfg:
            self.obj_function += "%s%s%s" % (get_vertex_execution_count_variable(v.vertexID),
                                             ECLIPSE.multiply,
                                             get_vertex_WCET_variable(v.vertexID))
            if counter > 1:
                self.obj_function += ECLIPSE.plus
            counter -= 1
        self.obj_function += ECLIPSE.conjunct
        
    def create_execution_time_domains(self, data, cfg):
        for v in cfg:
            self.the_constraints.append("%s%s%d%s" % (get_vertex_WCET_variable(v.vertexID),
                                                      ECLIPSE.equals,
                                                      data.get_basic_block_wcet(v.vertexID),
                                                      ECLIPSE.conjunct))
            
    def create_intra_super_block_constraints(self, super_block_cfg):
        for superv in super_block_cfg:
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex) and program_point != superv.representative:
                    new_constraint = ""
                    new_constraint += get_vertex_execution_count_variable(program_point.vertexID)
                    new_constraint += ECLIPSE.equals
                    new_constraint += get_vertex_execution_count_variable(superv.representative.vertexID)
                    new_constraint += ECLIPSE.conjunct
                    self.the_constraints.append(new_constraint)
        
    def create_structural_constraints(self, lnt, super_block_cfg):
        for superv in super_block_cfg:
            contains_loop_header = False
            for program_point in superv.program_points:
                if lnt.is_loop_header(program_point.vertexID):
                    contains_loop_header = True
            if superv.number_of_predecessors() > 1 and not contains_loop_header:
                new_constraint = ""
                new_constraint += get_execution_count_variable_for_program_point(superv.representative)
                new_constraint += ECLIPSE.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    super_predv = super_block_cfg.getVertex(predID)
                    new_constraint += get_execution_count_variable_for_program_point(super_predv.representative)
                    if counter > 1:
                        new_constraint += ECLIPSE.plus
                    counter -= 1
                new_constraint += ECLIPSE.conjunct
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    new_constraint = ""
                    new_constraint += get_execution_count_variable_for_program_point(superv.representative)
                    new_constraint += ECLIPSE.equals
                    counter = len(partition)
                    for succID in partition:
                        super_succv = super_block_cfg.getVertex(succID)
                        new_constraint += get_execution_count_variable_for_program_point(super_succv.representative)
                        if counter > 1:
                            new_constraint += ECLIPSE.plus
                        counter -= 1
                    new_constraint += ECLIPSE.conjunct
                    self.the_constraints.append(new_constraint)
    
    def create_loop_bound_constraints(self, data, super_block_cfg, lnt):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += ECLIPSE.equals
                        new_constraint += "%d" % data.get_upper_bound_on_header(treev.headerID)
                        new_constraint += ECLIPSE.conjunct
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += ECLIPSE.equals
                        pred_super_blocks = get_loop_entry_super_blocks(lnt, super_block_cfg, treev.headerID)
                        counter           = len(pred_super_blocks)
                        for pred_superv in pred_super_blocks:
                            new_constraint += "%d %s %s" % (data.get_upper_bound_on_header(treev.headerID),
                                                            ECLIPSE.multiply,
                                                            get_execution_count_variable_for_program_point(pred_superv.representative))
                            if counter > 1:
                                new_constraint += ECLIPSE.plus
                            counter -= 1
                        new_constraint += ECLIPSE.conjunct
                        self.the_constraints.append(new_constraint)        
        for superv in super_block_cfg:
            if isinstance(superv.representative, vertices.CFGEdge):
                headerv = lnt.getVertex(lnt.getVertex(superv.representative.edge[0]).parentID)
                lower_bound = 0
                upper_bound = data.get_upper_bound_on_header(headerv.headerID)
                new_constraint = "%s%s[%d..%d]%s" % (get_execution_count_variable_for_program_point(superv.representative),
                                                     ECLIPSE.domain_separator,
                                                     lower_bound,
                                                     upper_bound,
                                                     ECLIPSE.conjunct)
                self.the_constraints.append(new_constraint)
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    headerv = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    lower_bound = 0
                    upper_bound = data.get_upper_bound_on_header(headerv.headerID)
                    new_constraint = "%s%s[%d..%d]%s" % (get_execution_count_variable_for_program_point(program_point),
                                                         ECLIPSE.domain_separator,
                                                         lower_bound,
                                                         upper_bound,
                                                         ECLIPSE.conjunct)
                    self.the_constraints.append(new_constraint)
                    
class CreateFoldedSuperBlockCFGCLP(CLP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.folded.ecl" % (config.Arguments.basepath + os.sep + config.Arguments.basename, cfg.name, "superg")
        CLP.__init__(self, filename)
        start = timeit.default_timer()
        self.add_variables(super_block_cfg)
        self.create_objective_function(data, super_block_cfg)
        self.create_execution_time_domains(data, super_block_cfg)
        self.create_structural_constraints(super_block_cfg)
        self.create_loop_bound_constraints(data, super_block_cfg, lnt)
        end = timeit.default_timer()
        self.construction_time = end - start
        
    def add_variables(self, super_block_cfg):
        superv_execution_count_variables = []
        superv_execution_time_variables = []
        for superv in super_block_cfg:
            superv_execution_count_variables.append(get_vertex_execution_count_variable(superv.vertexID))
            superv_execution_time_variables.append(get_vertex_WCET_variable(superv.vertexID))
            self.the_variables.add(get_vertex_execution_count_variable(superv.vertexID))
            self.the_variables.add(get_vertex_WCET_variable(superv.vertexID))
        self.variable_lists.append("%s = [%s]%s" % (CLP.VERTEX_COUNTS,
                                                    ','.join(v for v in superv_execution_count_variables),
                                                    ECLIPSE.conjunct))
        self.variable_lists.append("%s = [%s]%s" % (CLP.VERTEX_TIMES,
                                                    ','.join(v for v in superv_execution_time_variables),
                                                    ECLIPSE.conjunct))
    
    def create_objective_function (self, data, super_block_cfg):
        self.obj_function = CLP.WCET
        self.obj_function += ECLIPSE.equals
        counter = super_block_cfg.number_of_vertices()
        for superv in super_block_cfg:
            wcet = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    wcet += data.get_basic_block_wcet(program_point.vertexID)
            self.the_variables.add(get_vertex_execution_count_variable(superv.vertexID))
            self.obj_function += "%s%s%s" % (get_vertex_execution_count_variable(superv.vertexID),
                                             ECLIPSE.multiply,
                                             get_vertex_WCET_variable(superv.vertexID))
            if counter > 1:
                self.obj_function += ECLIPSE.plus
            counter -= 1
        self.obj_function += ECLIPSE.conjunct
        
    def create_execution_time_domains(self, data, super_block_cfg):
        for superv in super_block_cfg:
            wcet = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    wcet += data.get_basic_block_wcet(program_point.vertexID)
            self.the_constraints.append("%s%s%d%s" % (get_vertex_WCET_variable(superv.vertexID),
                                                      ECLIPSE.equals,
                                                      wcet,
                                                      ECLIPSE.conjunct))
            
    def create_structural_constraints (self, super_block_cfg):
        for superv in super_block_cfg:
            if superv.number_of_predecessors() > 1:
                new_constraint = ""
                new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                new_constraint += ECLIPSE.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    new_constraint += get_vertex_execution_count_variable(predID)
                    if counter > 1:
                        new_constraint += ECLIPSE.plus
                    counter -= 1
                new_constraint += ECLIPSE.conjunct
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    new_constraint = ""
                    new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                    new_constraint += ECLIPSE.equals
                    counter = len(partition)
                    for succID in partition:
                        new_constraint += get_vertex_execution_count_variable(succID)
                        if counter > 1:
                            new_constraint += ECLIPSE.plus
                        counter -= 1
                    new_constraint += ECLIPSE.conjunct
                    self.the_constraints.append(new_constraint)
    
    def create_loop_bound_constraints(self, data, super_block_cfg, lnt):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    superv = super_block_cfg.find_super_block_for_header(treev.headerID)
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += ECLIPSE.equals
                        new_constraint += "%d" % data.get_upper_bound_on_header(treev.headerID)
                        new_constraint += ECLIPSE.conjunct
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += ECLIPSE.equals
                        pred_super_blocks = get_loop_entry_super_blocks(lnt, super_block_cfg, treev.headerID)
                        counter           = len(pred_super_blocks)
                        for pred_superv in pred_super_blocks:
                            new_constraint += "%d %s %s" % (data.get_upper_bound_on_header(treev.headerID),
                                                            ECLIPSE.multiply,
                                                            get_vertex_execution_count_variable(pred_superv.vertexID))
                            if counter > 1:
                                new_constraint += ECLIPSE.plus
                            counter -= 1
                        new_constraint += ECLIPSE.conjunct
                        self.the_constraints.append(new_constraint)
        
        for superv in super_block_cfg:
            lower_bound = 0
            upper_bound = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGEdge):
                    headerv = lnt.getVertex(lnt.getVertex(program_point.edge[0]).parentID)
                    upper_bound = max(upper_bound, data.get_upper_bound_on_header(headerv.headerID))
                else:
                    headerv = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    upper_bound = max(upper_bound, data.get_upper_bound_on_header(headerv.headerID))
            new_constraint = "%s%s[%d..%d]%s" % (get_vertex_execution_count_variable(superv.vertexID),
                                                 ECLIPSE.domain_separator,
                                                 lower_bound,
                                                 upper_bound,
                                                 ECLIPSE.conjunct)
            self.the_constraints.append(new_constraint)

REGION = utils.enum('CONTINUATIONS', 'EXITS')

class RegionalCalculation:
    def __init__(self, data, lnt, super_block_cfg):
        self.loop_wcets                    = {}
        self.upper_bound_for_continuations = {}
        self.upper_bound_for_exits         = {}
        start           = timeit.default_timer()
        self.calculate_continuation_and_exit_upper_bounds(data, lnt)
        self.wcet       = self.do_computation(data, lnt, super_block_cfg)
        end             = timeit.default_timer()
        self.solve_time = end - start  
    
    def calculate_continuation_and_exit_upper_bounds(self, data, lnt):
        # Compute the upper bounds when the loop continues iterating and when the loop exits
        for headerv in lnt.get_header_vertices():
            upper_bound_list              = data.get_upper_bound_on_header(headerv.headerID)
            upper_bound_for_continuations = []
            upper_bound_for_exits         = []  
            for value in upper_bound_list:
                if value > 0:
                    if not lnt.is_do_while_loop(headerv.headerID):
                        upper_bound_for_continuations.append(value-1)
                        upper_bound_for_exits.append(1)
                    else:
                        upper_bound_for_continuations.append(value)
            debug.debug_message("header %d: original = %s, continuations = %s, exits = %s" % (headerv.headerID, 
                                                                                              upper_bound_list,
                                                                                              upper_bound_for_continuations, 
                                                                                              upper_bound_for_exits),
                                __name__, 1)  
            self.upper_bound_for_continuations[headerv.headerID] = upper_bound_for_continuations
            self.upper_bound_for_exits[headerv.headerID]         = upper_bound_for_exits
            
    def split_continuation_and_exit_values(self, parentv, reduced_value):
        continuation_value = []
        exit_value         = []
        first_index        = 0
        second_index       = 0
        third_index        = 0
        for upper_bound in self.upper_bound_for_continuations[parentv.headerID]:
            second_index = first_index + upper_bound
            third_index  = second_index + 1
            continuation_value.extend(reduced_value[first_index:second_index])
            exit_value.extend(reduced_value[second_index:third_index])
            first_index  = third_index
        return continuation_value, exit_value

class RegionalCalculationEnhancedCFG(RegionalCalculation):
    def do_computation(self, data, lnt, super_block_cfg):
        # Go through each loop bottom up and calculate the WCET inside continuation subgraph
        # and, if needed, the WCET inside exit subgraph 
        for the_vertices in lnt.level_by_level_iterator(True):
            for headerv in [v for v in the_vertices if isinstance(v, vertices.HeaderVertex)]:
                self.calculate_wcet_inside_whole_subgraph(data, lnt, super_block_cfg, headerv)
        rootv = lnt.getVertex(lnt.rootID)
        return self.loop_wcets[(rootv.headerID, REGION.CONTINUATIONS)]  
    
    def calculate_wcet_inside_whole_subgraph(self, data, lnt, super_block_cfg, headerv):
        debug.debug_message("Doing calculation in continuation region of loop with header %d" % headerv.headerID, __name__, 1)
        subgraph = super_block_cfg.whole_body_enhanced_CFGs[headerv.headerID]
        
        self.do_calculation_on_subgraph(data, 
                                        lnt, 
                                        headerv, 
                                        subgraph,
                                        self.upper_bound_for_continuations[headerv.headerID], 
                                        self.upper_bound_for_exits[headerv.headerID])
        
        reduced_value = self.reduce_loop_wcet(headerv, 
                                              (subgraph.exitID, REGION.CONTINUATIONS),
                                              self.upper_bound_for_continuations[headerv.headerID])
        debug.debug_message("WCET(header %d) = %s" % (headerv.headerID, reduced_value), __name__, 10)
        
        if headerv.level > 0:
            parentv = lnt.getVertex(headerv.parentID) 
            if not lnt.is_do_while_loop(parentv.headerID) \
            and super_block_cfg.exits_only_enhanced_CFGs[parentv.headerID].hasVertex(headerv.vertexID):
                continuation_value, exit_value = self.split_continuation_and_exit_values(parentv, reduced_value)
                self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)] = continuation_value
                self.loop_wcets[(headerv.headerID, REGION.EXITS)]         = exit_value
            else:
                self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)] = reduced_value
                self.loop_wcets[(headerv.headerID, REGION.EXITS)]         = 0
                
            debug.debug_message("WCET(header %s, CONTINUATIONS) = %s" % (headerv.headerID, 
                                                                         self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)]), 
                                                                         __name__, 10)
            debug.debug_message("WCET(header %s, EXITS) = %s" % (headerv.headerID, 
                                                                 self.loop_wcets[(headerv.headerID, REGION.EXITS)]), 
                                                                 __name__, 10)
                
            for exit_edge in lnt.get_loop_exit_edges(headerv.headerID):
                exitv         = subgraph.getVertexForEdge(exit_edge[0], exit_edge[1])
                reduced_value = self.reduce_loop_wcet(headerv, 
                                                      (exitv.vertexID, REGION.EXITS),
                                                      self.upper_bound_for_exits[headerv.headerID])
                if not lnt.is_do_while_loop(parentv.headerID) \
                and super_block_cfg.exits_only_enhanced_CFGs[parentv.headerID].hasVertex(headerv.vertexID):
                    continuation_value, exit_value = self.split_continuation_and_exit_values(parentv, reduced_value)
                    self.loop_wcets[(exit_edge, REGION.CONTINUATIONS)] = continuation_value
                    self.loop_wcets[(exit_edge, REGION.EXITS)]         = exit_value
                else:
                    self.loop_wcets[(exit_edge, REGION.CONTINUATIONS)] = reduced_value
                    self.loop_wcets[(exit_edge, REGION.EXITS)]         = 0
                
                debug.debug_message("WCET(exit %s, CONTINUATIONS) = %s" % (exit_edge, 
                                                                           self.loop_wcets[(exit_edge, REGION.CONTINUATIONS)]), 
                                                                           __name__, 10)
                debug.debug_message("WCET(exit %s, EXITS) = %s" % (exit_edge, 
                                                                   self.loop_wcets[(exit_edge, REGION.EXITS)]), 
                                                                   __name__, 10)
        else:
            self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)] = reduced_value
    
    def do_calculation_on_subgraph(self, data, lnt, headerv, subgraph, upper_bound_for_continuations, upper_bound_for_exits):
        self.v_wcets = {}                    
        dfs = directed_graphs.DepthFirstSearch(subgraph, subgraph.entryID)
        for vertexID in reversed(dfs.post_order):
            v = subgraph.getVertex(vertexID)
            if isinstance(v, vertices.CFGVertex): 
                if not v.dummy:
                    self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = numpy.array([data.get_basic_block_wcet(v.vertexID)] * numpy.sum(upper_bound_for_continuations))
                    self.v_wcets[(v.vertexID, REGION.EXITS)]         = numpy.array([data.get_basic_block_wcet(v.vertexID)] * numpy.sum(upper_bound_for_exits))
                else:
                    self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = 0
                    self.v_wcets[(v.vertexID, REGION.EXITS)]         = 0
            elif isinstance(v, vertices.HeaderVertex):
                self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = self.loop_wcets[(v.headerID, REGION.CONTINUATIONS)]
                self.v_wcets[(v.vertexID, REGION.EXITS)]         = self.loop_wcets[(v.headerID, REGION.EXITS)]
            else:
                inner_headerID = lnt.is_loop_exit_edge(v.edge[0], v.edge[1])
                if inner_headerID and inner_headerID != headerv.headerID and not lnt.is_do_while_loop(inner_headerID):
                    self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = self.loop_wcets[(v.edge, REGION.CONTINUATIONS)]
                    self.v_wcets[(v.vertexID, REGION.EXITS)]         = self.loop_wcets[(v.edge, REGION.EXITS)]
                else:
                    self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = 0
                    self.v_wcets[(v.vertexID, REGION.EXITS)]         = 0
            if v.number_of_predecessors() > 1:
                maximum_of_continuations = 0
                maximum_of_exits         = 0
                for predID in v.predecessors.keys():
                    maximum_of_continuations = numpy.maximum(maximum_of_continuations, self.v_wcets[(predID, REGION.CONTINUATIONS)])
                    maximum_of_exits         = numpy.maximum(maximum_of_exits, self.v_wcets[(predID, REGION.EXITS)])
                self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = numpy.add(maximum_of_continuations, self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)])
                self.v_wcets[(v.vertexID, REGION.EXITS)]         = numpy.add(maximum_of_exits,         self.v_wcets[(v.vertexID, REGION.EXITS)])
            elif v.number_of_predecessors() == 1:
                predID = v.predecessors.keys()[0]
                self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)] = numpy.add(self.v_wcets[(predID, REGION.CONTINUATIONS)], 
                                                                             self.v_wcets[(v.vertexID, REGION.CONTINUATIONS)])
                self.v_wcets[(v.vertexID, REGION.EXITS)]         = numpy.add(self.v_wcets[(predID, REGION.EXITS)], 
                                                                             self.v_wcets[(v.vertexID, REGION.EXITS)])
             
    def reduce_loop_wcet(self, headerv, key, upper_bound_list):
        if headerv.level > 0:
            the_reduction = []
            first_index   = 0
            for upper_bound in upper_bound_list:
                second_index = first_index + upper_bound
                the_reduction.append(numpy.sum(self.v_wcets[key][first_index:second_index]))
                second_index = first_index
            return numpy.array(the_reduction)
        else:
            return numpy.sum(self.v_wcets[key])

class RegionalCalculationSuperBlockCFG(RegionalCalculation):
    def do_computation(self, data, lnt, super_block_cfg):
        # Go through each loop bottom up and calculate the WCET inside continuation subgraph
        # and, if needed, the WCET inside exit subgraph 
        for the_vertices in lnt.level_by_level_iterator(True):
            for headerv in [v for v in the_vertices if isinstance(v, vertices.HeaderVertex)]:
                self.calculate_wcet_inside_continuation_subgraph(data, lnt, super_block_cfg, headerv)
                if not lnt.is_do_while_loop(headerv.headerID):
                    self.calculate_wcet_inside_exit_subgraph(data, lnt, super_block_cfg, headerv)
        rootv = lnt.getVertex(lnt.rootID)
        return self.loop_wcets[(rootv.headerID, REGION.CONTINUATIONS)]  
    
    def calculate_wcet_inside_continuation_subgraph(self, data, lnt, super_block_cfg, headerv):
        debug.debug_message("Doing calculation in continuation region of loop with header %d" % headerv.headerID, __name__, 1)
        subgraph = super_block_cfg.tails_only_subgraphs[headerv.headerID]
        self.do_calculation_on_subgraph(data, 
                                        lnt, 
                                        headerv, 
                                        subgraph,
                                        self.upper_bound_for_continuations[headerv.headerID], 
                                        REGION.CONTINUATIONS)
        
        reduced_value = self.reduce_loop_wcet(headerv, 
                                              subgraph.rootv, 
                                              headerv.headerID, 
                                              self.upper_bound_for_continuations[headerv.headerID])
        
        if headerv.level > 0:
            parentv = lnt.getVertex(headerv.parentID)
            if not lnt.is_do_while_loop(parentv.headerID) and super_block_cfg.exits_only_enhanced_CFGs[parentv.headerID].hasVertex(headerv.vertexID):
                continuation_value, exit_value = self.split_continuation_and_exit_values(parentv, reduced_value)
                self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)] = continuation_value
                self.loop_wcets[(headerv.headerID, REGION.EXITS)]         = exit_value
            else:
                self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)] = reduced_value
                self.loop_wcets[(headerv.headerID, REGION.EXITS)]         = 0
                
            debug.debug_message("WCET(header %s, CONTINUATIONS) = %s" % (headerv.headerID, 
                                                                         self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)]), 
                                                                         __name__, 10)
            debug.debug_message("WCET(header %s, EXITS) = %s" % (headerv.headerID, 
                                                                 self.loop_wcets[(headerv.headerID, REGION.EXITS)]), 
                                                                 __name__, 10)
        else:
            self.loop_wcets[(headerv.headerID, REGION.CONTINUATIONS)] = reduced_value
                
    def calculate_wcet_inside_exit_subgraph(self, data, lnt, super_block_cfg, headerv):
        debug.debug_message("Doing calculation in exit region of loop with header %d" % headerv.headerID, __name__, 1)
        subgraph = super_block_cfg.exits_only_subgraphs[headerv.headerID]
        self.do_calculation_on_subgraph(data, 
                                        lnt, 
                                        headerv, 
                                        subgraph, 
                                        self.upper_bound_for_exits[headerv.headerID],
                                        REGION.EXITS)
        for key in self.v_wcets[subgraph.rootv]:
            if key != headerv.headerID:
                reduced_value = self.reduce_loop_wcet(headerv, 
                                                      subgraph.rootv, 
                                                      key, 
                                                      self.upper_bound_for_exits[headerv.headerID])
                self.loop_wcets[(key, REGION.CONTINUATIONS)] = reduced_value
                if headerv.level > 0:
                    parentv = lnt.getVertex(headerv.parentID)
                    if not lnt.is_do_while_loop(parentv.headerID) and super_block_cfg.exits_only_enhanced_CFGs[parentv.headerID].hasVertex(headerv.vertexID):
                        continuation_value, exit_value = self.split_continuation_and_exit_values(parentv, reduced_value)
                        self.loop_wcets[(key, REGION.CONTINUATIONS)] = continuation_value
                        self.loop_wcets[(key, REGION.EXITS)]         = exit_value
    
    def do_calculation_on_subgraph(self, data, lnt, headerv, subgraph, upper_bound_list, region):
        self.v_wcets = {}
        for superv in subgraph:
            self.v_wcets[superv] = {}                    
        dfs = directed_graphs.DepthFirstSearch(subgraph, subgraph.rootv.vertexID)
        for vertexID in dfs.post_order:
            superv = subgraph.getVertex(vertexID)
            intra_superv_wcet = self.compute_execution_time_within_super_block(data, lnt, superv, headerv, upper_bound_list, region)
            self.compute_execution_time_from_successors(data, lnt, subgraph, superv, headerv, intra_superv_wcet)
            if superv.exit_edge:                    
                self.v_wcets[superv][superv.exit_edge] = self.v_wcets[superv][headerv.headerID]
    
    def compute_execution_time_within_super_block(self, data, lnt, superv, headerv, upper_bound_list, region):
        upper_bound             = numpy.sum(upper_bound_list)
        inner_loop_contribution = 0
        this_loop_contribution  = 0
        for program_point in superv.program_points:
            if isinstance(program_point, vertices.CFGVertex):
                this_loop_contribution = numpy.add(this_loop_contribution, data.get_basic_block_wcet(program_point.vertexID))
            else:
                if isinstance(program_point, vertices.CFGEdge):
                    the_edge       = program_point.edge
                    inner_headerID = lnt.is_loop_exit_edge(the_edge[0], the_edge[1])
                    if inner_headerID and inner_headerID != headerv.headerID and not lnt.is_do_while_loop(inner_headerID):
                        key = ((the_edge[0], the_edge[1]), region)
                        inner_loop_contribution = numpy.add(inner_loop_contribution, self.loop_wcets[key])
                if isinstance(program_point, vertices.HeaderVertex):
                    key = (program_point.headerID, region)
                    inner_loop_contribution = numpy.add(inner_loop_contribution, self.loop_wcets[key])
        this_loop_contribution = numpy.array(upper_bound * [this_loop_contribution])
        loop_body_wcet = numpy.add(inner_loop_contribution, this_loop_contribution)
        return loop_body_wcet
    
    def compute_execution_time_from_successors(self, data, lnt, subgraph, superv, headerv, intra_superv_wcet):
        if superv.number_of_successors() == 0:
            self.v_wcets[superv][headerv.headerID] = intra_superv_wcet
        else:
            wcets_at_superv = collections.OrderedDict()
            wcet_for_header = 0
            for the_partition in superv.successor_partitions.values():
                wcets_within_partition = self.compute_wcet_within_partition(lnt, 
                                                                            subgraph, 
                                                                            headerv, 
                                                                            wcets_at_superv, 
                                                                            the_partition)
                for key in wcets_within_partition.keys():
                    if key != headerv.headerID:
                        wcets_within_partition[key] = numpy.add(wcet_for_header, wcets_within_partition[key])
                for key, wcet in wcets_within_partition.iteritems():
                    if key not in wcets_at_superv:
                        wcets_at_superv[key] = wcet
                    else:
                        wcets_at_superv[key] = numpy.add(wcets_at_superv[key], wcet)
                    if key == headerv.headerID:
                        wcet_for_header = numpy.add(wcet, wcet_for_header)
            for key, wcet in wcets_at_superv.iteritems():
                self.v_wcets[superv][key] = numpy.add(wcet, intra_superv_wcet)
            
    def compute_wcet_within_partition(self, lnt, subgraph, headerv, wcets_at_superv, the_partition):  
        wcets_within_partition = collections.OrderedDict()
        for succID in the_partition:
            succ_superv = subgraph.getVertex(succID)
            for key in self.v_wcets[succ_superv]:
                if key not in wcets_within_partition:
                    # We have not yet encountered this key in the partition
                    wcets_within_partition[key] = self.v_wcets[succ_superv][key]
                else:
                    # Take the maximum value among the elements in the partition
                    wcets_within_partition[key] = numpy.maximum(wcets_within_partition[key], self.v_wcets[succ_superv][key])
        return wcets_within_partition

    def reduce_loop_wcet(self, headerv, v, key, upper_bound_list):
        if headerv.level > 0:
            the_reduction = []
            first_index   = 0
            for upper_bound in upper_bound_list:
                second_index = first_index + upper_bound
                the_reduction.append(numpy.sum(self.v_wcets[v][key][first_index:second_index]))
                second_index = first_index
            return numpy.array(the_reduction)
        else:
            return numpy.sum(self.v_wcets[v][key])
