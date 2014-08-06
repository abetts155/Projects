import vertices
import config
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
import trees
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
                    upper_bound_list = data.get_upper_bound_on_header(treev.headerID)
                    upper_bound      = numpy.sum(upper_bound_list)    
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(cfg.get_entryID())
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % upper_bound
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += LpSolve.lte
                        incoming_edges = lnt.get_loop_entry_edges(treev.headerID)
                        counter        = len(incoming_edges)
                        for predID, succID in incoming_edges:
                            new_constraint += "%d %s" % (upper_bound, 
                                                         get_edge_execution_count_variable(predID, succID))
                            if counter > 1:
                                new_constraint += LpSolve.plus 
                            counter -= 1
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
        
class CreateSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "superg")
        ILP.__init__(self, filename)
        self.header_to_exit_super_blocks = {}
        start = timeit.default_timer()
        self.create_objective_function(data, cfg, lnt, super_block_cfg)
        self.create_intra_super_block_constraints(super_block_cfg)
        self.create_structural_constraints(lnt, super_block_cfg)
        self.create_loop_bound_constraints(data, lnt, super_block_cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
                        
    def create_objective_function (self, data, cfg, lnt, super_block_cfg):
        self.obj_function = LpSolve.max_
        counter = cfg.number_of_vertices()
        for superv in super_block_cfg:
            self.the_variables.add(get_execution_count_variable_for_program_point(superv.representative))
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    parent_headerv = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    if parent_headerv.headerID == superv.headerID:
                        wcet = data.get_basic_block_wcet(program_point.vertexID)
                        variable = get_vertex_execution_count_variable(program_point.vertexID)
                        self.obj_function += "%d %s" % (wcet, variable)
                        self.the_variables.add(variable)
                        if counter > 1:
                            self.obj_function += LpSolve.plus
                        counter -= 1
        self.obj_function += LpSolve.semi_colon
        
    def create_intra_super_block_constraints(self, super_block_cfg):
        for superv in super_block_cfg:
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex) and program_point != superv.representative:
                    new_constraint = ""
                    new_constraint += get_vertex_execution_count_variable(program_point.vertexID)
                    new_constraint += LpSolve.equals
                    new_constraint += get_vertex_execution_count_variable(superv.representative.vertexID)
                    new_constraint += LpSolve.semi_colon
                    self.the_constraints.append(new_constraint)
            
    def create_structural_constraints (self, lnt, super_block_cfg):
        for superv in super_block_cfg:
            if superv.number_of_predecessors() > 1:
                new_constraint = ""
                new_constraint += get_execution_count_variable_for_program_point(superv.representative)
                new_constraint += LpSolve.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    super_predv = super_block_cfg.getVertex(predID)
                    new_constraint += get_execution_count_variable_for_program_point(super_predv.representative)
                    if counter > 1:
                        new_constraint += LpSolve.plus
                    counter -= 1
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    new_constraint = ""
                    new_constraint += get_execution_count_variable_for_program_point(superv.representative)
                    new_constraint += LpSolve.equals
                    counter = len(partition)
                    for succID in partition:
                        super_succv = super_block_cfg.getVertex(succID)
                        if super_succv.exit_edge:
                            headerv         = lnt.getVertex(lnt.getVertex(super_succv.headerID).parentID)
                            parent_headerv  = lnt.getVertex(headerv.parentID)
                            parent_subgraph = super_block_cfg.per_loop_subgraphs[parent_headerv.headerID]
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
            
    def create_loop_bound_constraints(self, data, lnt, super_block_cfg):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    upper_bound_list = data.get_upper_bound_on_header(treev.headerID)
                    upper_bound      = numpy.sum(upper_bound_list)
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % upper_bound
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint  = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += LpSolve.lte
                        counter         = len(self.header_to_exit_super_blocks[treev.headerID])
                        for succ_superv in self.header_to_exit_super_blocks[treev.headerID]:
                            new_constraint += "%d %s" % (upper_bound,
                                                         get_execution_count_variable_for_program_point(succ_superv.representative))
                            if counter > 1:
                                new_constraint += LpSolve.plus
                            counter -= 1
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
        
class CreateFoldedSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.folded.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "superg")
        ILP.__init__(self, filename)
        self.header_to_exit_super_blocks = {}
        start = timeit.default_timer()
        self.create_objective_function(data, lnt, super_block_cfg)
        self.create_structural_constraints(lnt, super_block_cfg)
        self.create_loop_bound_constraints(data, lnt, super_block_cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
    
    def create_objective_function (self, data, lnt, super_block_cfg):
        self.obj_function = LpSolve.max_
        counter = super_block_cfg.number_of_vertices()
        for superv in super_block_cfg:
            wcet = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    parent_headerv = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    if parent_headerv.headerID == superv.headerID:
                        wcet += data.get_basic_block_wcet(program_point.vertexID)
            self.the_variables.add(get_vertex_execution_count_variable(superv.vertexID))
            self.obj_function += "%d %s" % (wcet, get_vertex_execution_count_variable(superv.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, lnt, super_block_cfg):
        for superv in super_block_cfg:
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
                    new_constraint = ""
                    new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                    new_constraint += LpSolve.equals
                    counter = len(partition)
                    for succID in partition:
                        super_succv = super_block_cfg.getVertex(succID)
                        if super_succv.exit_edge:
                            headerv         = lnt.getVertex(lnt.getVertex(super_succv.headerID).parentID)
                            parent_headerv  = lnt.getVertex(headerv.parentID)
                            parent_subgraph = super_block_cfg.per_loop_subgraphs[parent_headerv.headerID]
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
            
    def create_loop_bound_constraints(self, data, lnt, super_block_cfg):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    upper_bound_list = data.get_upper_bound_on_header(treev.headerID)
                    upper_bound      = numpy.sum(upper_bound_list)    
                    superv           = super_block_cfg.find_super_block_for_header(treev.headerID)
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % upper_bound
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += LpSolve.lte
                        counter         = len(self.header_to_exit_super_blocks[treev.headerID])
                        for succ_superv in self.header_to_exit_super_blocks[treev.headerID]:
                            new_constraint += "%d %s" % (upper_bound,
                                                         get_vertex_execution_count_variable(succ_superv.vertexID))
                            if counter > 1:
                                new_constraint += LpSolve.plus
                            counter -= 1
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
            
class TreeBasedCalculation:
    def __init__(self, data, lnt, super_block_cfg):
        self.superv_wcets = {}
        self.loop_wcets   = {}
        start             = timeit.default_timer()
        self.wcet         = self.do_computation(data, lnt, super_block_cfg)
        end               = timeit.default_timer()
        self.solve_time   = end - start
        
    def do_computation(self, data, lnt, super_block_cfg):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    debug.debug_message("Doing calculation for loop with header %d" % treev.headerID, __name__, 1)
                    subgraph = super_block_cfg.per_loop_subgraphs[treev.headerID]
                    for superv in subgraph:
                        self.superv_wcets[superv] = {}
                    root_superv = super_block_cfg.find_super_block_for_header(treev.headerID)
                    dfs         = trees.DepthFirstSearch(subgraph, root_superv.vertexID)
                    for vertexID in dfs.post_order:
                        superv = super_block_cfg.getVertex(vertexID)
                        self.compute_wcet_of_super_block(data, lnt, super_block_cfg, superv, treev)
                    if treev.level > 0:
                        for key in self.superv_wcets[root_superv]:
                            if key == treev.headerID:
                                upper_bound_list = data.get_upper_bound_on_header(treev.headerID)
                                upper_bound      = numpy.sum(upper_bound_list) 
                                if not lnt.is_do_while_loop(treev.headerID):
                                    upper_bound -= 1   
                                self.loop_wcets[treev.headerID] = self.superv_wcets[root_superv][treev.headerID] * upper_bound
                            else:
                                self.loop_wcets[key] = self.superv_wcets[root_superv][key]
                    else:
                        self.loop_wcets[treev.headerID] = self.superv_wcets[root_superv][treev.headerID]
                    debug.debug_message("WCET(header %d) = %s" % (treev.headerID, self.loop_wcets[treev.headerID]), __name__, 10)
        rootv = lnt.getVertex(lnt.rootID)
        return self.loop_wcets[rootv.headerID]
    
    def compute_wcet_of_super_block(self, data, lnt, super_block_cfg, superv, treev):
        # Sum up the contribution of each basic block in the super block
        intra_superv_wcet = self.compute_execution_time_within_super_block(data, lnt, superv, treev)
        if superv.number_of_successors() == 0:
            self.superv_wcets[superv][treev.headerID] = intra_superv_wcet
        else:
            # Add in the contribution caused by branching control flow
            if superv.number_of_successors() > 1:
                self.compute_max_of_branches(data, super_block_cfg, superv, intra_superv_wcet)
            # Add in the contribution caused by merging of control flow
            for succID in superv.merge_super_blocks:
                succ_superv = super_block_cfg.getVertex(succID)
                for key in self.superv_wcets[succ_superv]:
                    if key in self.superv_wcets[superv]:
                        self.superv_wcets[superv][key] = numpy.add(self.superv_wcets[succ_superv][key], self.superv_wcets[superv][key]) 
                    else:
                        self.superv_wcets[superv][key] = numpy.add(self.superv_wcets[succ_superv][key], intra_superv_wcet) 
        for key in self.superv_wcets[superv]:
            pass
            #print "superv = %d, key = %s, wcet = %d" % (superv.vertexID, key, self.superv_wcets[superv][key])
    
    def compute_execution_time_within_super_block(self, data, lnt, superv, treev):
        wcet = 0
        for program_point in superv.program_points:
            if isinstance(program_point, vertices.CFGVertex):
                wcet = numpy.add(wcet, data.get_basic_block_wcet(program_point.vertexID))
            elif isinstance(program_point, vertices.CFGEdge):
                the_edge       = program_point.edge
                inner_headerID = lnt.is_loop_exit_edge(the_edge[0], the_edge[1])
                if inner_headerID and inner_headerID != treev.headerID and not lnt.is_do_while_loop(inner_headerID):
                    wcet = numpy.add(wcet, self.loop_wcets[(the_edge[0], the_edge[1])])
            else:
                wcet = numpy.add(wcet, self.loop_wcets[program_point.headerID])
        return wcet
    
    def compute_max_of_branches(self, data, super_block_cfg, superv, intra_superv_wcet):
        wcets = collections.OrderedDict()
        for the_partition in superv.successor_partitions.values():
            partition_wcets = collections.OrderedDict()
            for succID in the_partition:
                succ_superv = super_block_cfg.getVertex(succID)
                for key in self.superv_wcets[succ_superv]:
                    if key not in partition_wcets:
                        partition_wcets[key] = self.superv_wcets[succ_superv][key]
                    else:
                        partition_wcets[key] = numpy.maximum(partition_wcets[key], self.superv_wcets[succ_superv][key])
                if succ_superv.exit_edge:                    
                    the_edge = succ_superv.representative.edge
                    partition_wcets[(the_edge[0], the_edge[1])] = 0
            for key, wcet in partition_wcets.iteritems():
                if key not in wcets:
                    wcets[key] = wcet
                else:
                    wcets[key] = numpy.add(wcets[key], wcet)
        for key, wcet in wcets.iteritems():
            self.superv_wcets[superv][key] = numpy.add(wcet, intra_superv_wcet)
        
