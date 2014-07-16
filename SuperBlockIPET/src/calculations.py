import vertices
import config
import debug
import os
import timeit
import subprocess
import shlex
import decimal
import abc
import random
import re

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
    else:
        return get_edge_execution_count_variable(program_point.edge[0], program_point.edge[1])

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
        self.filename                  = filename
        self.wcet                      = -1
        self.solve_time                = 0.0
        self.construction_time         = 0.0
        self.variable_execution_counts = {}
        self.the_constraints           = []
        self.the_variables             = set()
    
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
        cmd            = "lp_solve %s" % self.filename 
        start          = timeit.default_timer()
        proc           = subprocess.Popen(cmd, 
                                          shell=True, 
                                          stdout=subprocess.PIPE, 
                                          stderr=subprocess.PIPE)
        return_code     = proc.wait()
        end             = timeit.default_timer()
        self.solve_time = end - start
        if return_code != 0:
            debug.exit_message("Running '%s' failed" % cmd)
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes   = shlex.split(line)
                self.wcet = long(decimal.Decimal(lexemes[-1])) 
            elif line.startswith(edge_variable_prefix) or line.startswith(vertex_variable_prefix):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Incorrectly detected variable execution count line '%s'" % line
                self.variable_execution_counts[lexemes[0]] = int(lexemes[1]) 

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
                    # Get iteration edges for this header
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(cfg.get_entryID())
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(cfg.get_entryID())
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        self.create_constraints_for_loop(data, cfg, lnt, treev)
                        
    def create_constraints_for_loop(self, data, cfg, lnt, treev):
        bound          = data.get_loop_bound(treev.headerID)
        incoming_edges = self.get_loop_entry_edges(cfg, lnt, treev.headerID)
        new_constraint = ""
        new_constraint += get_vertex_execution_count_variable(treev.headerID)
        new_constraint += LpSolve.lte
        counter = len(incoming_edges)
        for predID, succID in incoming_edges:
            new_constraint += "%d %s" % (bound, get_edge_execution_count_variable(predID, succID))
            if counter > 1:
                new_constraint += LpSolve.plus 
            counter -= 1
        new_constraint += LpSolve.semi_colon
        self.the_constraints.append(new_constraint)
            
    def get_loop_entry_edges(self, cfg, lnt, headerID):
        edgeIDs = []
        v       = cfg.getVertex(headerID)
        for predID in v.predecessors.keys():
            if not lnt.is_loop_back_edge(predID, headerID):
                edgeIDs.append((predID, headerID))
        assert edgeIDs, "Unable to find loop-entry edges into loop with header %d" % headerID
        return edgeIDs
        
class CreateSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "super_block_cfg")
        ILP.__init__(self, filename)
        start = timeit.default_timer()
        self.create_objective_function(data, cfg, super_block_cfg)
        self.create_intra_super_block_constraints(super_block_cfg)
        self.create_structural_constraints(lnt, super_block_cfg)
        self.create_loop_bound_constraints(data, lnt, super_block_cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
                        
    def create_objective_function (self, data, cfg, super_block_cfg):
        self.obj_function = LpSolve.max_
        counter = cfg.number_of_vertices()
        for superv in super_block_cfg:
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
            contains_loop_header = True
            for program_point in superv.program_points:
                if lnt.is_loop_header(program_point.vertexID):
                    contains_loop_header = True
            if superv.number_of_predecessors() > 1 and not contains_loop_header:
                # Ensure the merge was created from forward control flow
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
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(treev.headerID)
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        parentv             = lnt.getVertex(treev.parentID)
                        outer_subgraph      = super_block_cfg.per_loop_subgraphs[parentv.headerID]
                        outer_header_superv = outer_subgraph.program_point_to_superv[parentv.headerID]
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(treev.headerID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d %s" % (data.get_loop_bound(treev.headerID), 
                                                     get_execution_count_variable_for_program_point(outer_header_superv.representative))
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
        
class CreateFoldedSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.folded.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "super_block_cfg")
        ILP.__init__(self, filename)
        start = timeit.default_timer()
        self.create_objective_function(data, super_block_cfg)
        self.create_structural_constraints(lnt, super_block_cfg)
        self.create_loop_bound_constraints(data, lnt, super_block_cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
    
    def create_objective_function (self, data, super_block_cfg):
        self.obj_function = LpSolve.max_
        counter = super_block_cfg.number_of_vertices()
        for superv in super_block_cfg:
            wcet = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    wcet += data.get_basic_block_wcet(program_point.vertexID)
            self.the_variables.add(get_vertex_execution_count_variable(superv.vertexID))
            self.obj_function += "%d %s" % (wcet, get_vertex_execution_count_variable(superv.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, lnt, super_block_cfg):
        for superv in super_block_cfg:
            contains_loop_header = True
            for program_point in superv.program_points:
                if lnt.is_loop_header(program_point.vertexID):
                    contains_loop_header = True
            if superv.number_of_predecessors() > 1 and not contains_loop_header:
                # Ensure the merge was created from forward control flow
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
                        new_constraint += get_vertex_execution_count_variable(succID)
                        if counter > 1:
                            new_constraint += LpSolve.plus
                        counter -= 1
                    new_constraint += LpSolve.semi_colon
                    self.the_constraints.append(new_constraint)                      
            
    def create_loop_bound_constraints(self, data, lnt, super_block_cfg):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    superv = super_block_cfg.find_super_block_for_header(treev.headerID)
                    if treev.vertexID == lnt.rootID:
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(treev.headerID)
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                    else:
                        parentv             = lnt.getVertex(treev.parentID)
                        outer_subgraph      = super_block_cfg.per_loop_subgraphs[parentv.headerID]
                        outer_header_superv = outer_subgraph.program_point_to_superv[parentv.headerID]
                        new_constraint = ""
                        new_constraint += get_vertex_execution_count_variable(superv.vertexID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d %s" % (data.get_loop_bound(treev.headerID), 
                                                     get_vertex_execution_count_variable(outer_header_superv.vertexID))
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
    WCET                  = "WCET"
    PWCET                 = "PWCET"
    VERTEX_TIMES          = "VERTEX_TIMES"
    VERTEX_COUNTS         = "VERTEX_COUNTS"
    EDGE_COUNTS           = "EDGE_COUNTS"
    OUTPUT_PREDICATE_HEAD = "print_results"
    
    def __init__ (self, filename):
        ConstraintSystem.__init__(self, filename)
        self.results_filename = filename + ".res"
        self.goal             = "solve(%s)" % CLP.WCET
        self.variable_lists   = []
        
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
                                         -1, 
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
        return_code     = proc.wait()
        end             = timeit.default_timer()
        self.solve_time = end - start
        if return_code != 0:
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
        bb_execution_count_variables   = []
        edge_execution_count_variables = []
        bb_execution_time_variables    = []
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
        self.obj_function  = CLP.WCET
        self.obj_function += ECLIPSE.equals
        counter            = cfg.number_of_vertices()
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
            new_constraint  = get_vertex_execution_count_variable(v.vertexID)
            new_constraint += ECLIPSE.equals
            counter         = v.number_of_predecessors()
            for predID in v.predecessors.keys():
                new_constraint += get_edge_execution_count_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += ECLIPSE.plus
                counter -= 1
            new_constraint += ECLIPSE.conjunct
            self.the_constraints.append(new_constraint)
            new_constraint = ""
            counter        = v.number_of_successors()
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
        for v in cfg:            
            headerv        = lnt.getVertex(lnt.getVertex(v.vertexID).parentID)
            lower_bound    = 0
            upper_bound    = data.get_loop_bound(headerv.headerID)
            new_constraint = "%s%s[%d..%d]%s" % (get_vertex_execution_count_variable(v.vertexID), 
                                              ECLIPSE.domain_separator, 
                                              lower_bound, 
                                              upper_bound, 
                                              ECLIPSE.conjunct)
            self.the_constraints.append(new_constraint) 
        for v in cfg:            
            headerv     = lnt.getVertex(lnt.getVertex(v.vertexID).parentID)
            lower_bound = 0
            upper_bound = data.get_loop_bound(headerv.headerID)
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
        bb_execution_count_variables   = []
        edge_execution_count_variables = []
        bb_execution_time_variables    = []
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
        self.obj_function  = CLP.WCET
        self.obj_function += ECLIPSE.equals
        counter            = cfg.number_of_vertices()
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
            contains_loop_header = True
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
        for superv in super_block_cfg:
            if isinstance(superv.representative, vertices.CFGEdge):
                headerv     = lnt.getVertex(lnt.getVertex(superv.representative.edge[0]).parentID)
                lower_bound = 0
                upper_bound = data.get_loop_bound(headerv.headerID)
                new_constraint = "%s%s[%d..%d]%s" % (get_execution_count_variable_for_program_point(superv.representative), 
                                                     ECLIPSE.domain_separator, 
                                                     lower_bound, 
                                                     upper_bound, 
                                                     ECLIPSE.conjunct)
                self.the_constraints.append(new_constraint)
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    headerv        = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    lower_bound    = 0
                    upper_bound    = data.get_loop_bound(headerv.headerID)
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
        superv_execution_time_variables  = []
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
        self.obj_function  = CLP.WCET
        self.obj_function += ECLIPSE.equals
        counter            = super_block_cfg.number_of_vertices()
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
        for superv in super_block_cfg:
            lower_bound = 0
            upper_bound = 0
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGEdge):
                    headerv     = lnt.getVertex(lnt.getVertex(program_point.edge[0]).parentID)
                    upper_bound = max(upper_bound, data.get_loop_bound(headerv.headerID))
                else:
                    headerv     = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    upper_bound = max(upper_bound, data.get_loop_bound(headerv.headerID))
            new_constraint = "%s%s[%d..%d]%s" % (get_vertex_execution_count_variable(superv.vertexID), 
                                                 ECLIPSE.domain_separator, 
                                                 lower_bound, 
                                                 upper_bound, 
                                                 ECLIPSE.conjunct)
            self.the_constraints.append(new_constraint)
        