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

class LpSolve:
    comma         = ","
    edge_prefix   = "e_"
    equals        = " = "
    int_          = "int"
    lte           = " <= "
    max_          = "max: "
    plus          = " + "
    semi_colon    = ";"
    vertex_prefix = "v_"
    
    @staticmethod
    def get_edge_variable(predID, succID):
        return "%s%d_%d" % (LpSolve.edge_prefix, predID, succID)
    
    @staticmethod
    def get_vertex_variable(vertexID):
        return "%s%d" % (LpSolve.vertex_prefix, vertexID)
    
    @staticmethod
    def get_comment (comment):
        return "// " + comment + "\n"
    
    @staticmethod
    def get_new_line (num=1):
        return "\n" * num  
    
class ILP():
    __metaclass__ = abc.ABCMeta
    
    def __init__(self, filename):
        self.filename                  = filename
        self.wcet                      = -1
        self.variable_execution_counts = {}
        self.the_constraints           = []
        self.the_variables             = set()
        
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
            the_file.write(LpSolve.get_new_line(2))
            for constraint in self.the_constraints:
                the_file.write(constraint)
                the_file.write(LpSolve.get_new_line())
            the_file.write(LpSolve.get_new_line())
            the_file.write(self.int_constraint)
            the_file.write(LpSolve.get_new_line())
            
    def shuffle(self):
        random.shuffle(self.the_constraints)
      
    def solve(self):
        debug.debug_message("Solving ILP", __name__, 10)
        if config.Arguments.shuffle_constraints:
            self.shuffle()
        self.write_to_file()
        cmd        = "lp_solve %s" % self.filename 
        start      = timeit.default_timer()
        proc       = subprocess.Popen(cmd, 
                                      shell=True, 
                                      stdout=subprocess.PIPE, 
                                      stderr=subprocess.PIPE)
        return_code = proc.wait()
        end         = timeit.default_timer()
        self.solve_time = end - start
        if return_code != 0:
            debug.exit_message("Running '%s' failed" % cmd)
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes   = shlex.split(line)
                self.wcet = long(decimal.Decimal(lexemes[-1])) 
            elif line.startswith(LpSolve.edge_prefix) or line.startswith(LpSolve.vertex_prefix):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Incorrectly detected variable execution count line '%s'" % line
                self.variable_execution_counts[lexemes[0]] = int(lexemes[1]) 

    @abc.abstractmethod
    def create_objective_function(self):
        pass
    
    @abc.abstractmethod
    def create_structural_constraints(self):
        pass
    
    @abc.abstractmethod
    def create_loop_bound_constraints(self):
        pass

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
            self.obj_function += "%d %s" % (wcet, LpSolve.get_vertex_variable(v.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, cfg):
        for v in cfg:
            self.the_variables.add(LpSolve.get_vertex_variable(v.vertexID))
            # Flow in, flow out w.r.t to vertex and its predecessor edges
            new_constraint = ""
            new_constraint += LpSolve.get_vertex_variable(v.vertexID)
            new_constraint += LpSolve.equals
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():    
                self.the_variables.add(LpSolve.get_edge_variable(predID, v.vertexID))
                new_constraint += LpSolve.get_edge_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1    
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)
            # Flow in, flow out w.r.t to predecessor edges and successor edges
            new_constraint = ""
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():
                new_constraint += LpSolve.get_edge_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1
            new_constraint += LpSolve.equals
            counter = v.number_of_successors()
            for succID in v.successors.keys():
                new_constraint += LpSolve.get_edge_variable(v.vertexID, succID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1
            new_constraint += LpSolve.semi_colon
            self.the_constraints.append(new_constraint)       
            
    def create_loop_bound_constraints(self, data, lnt, cfg):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if level > 0:
                        self.create_constraints_for_loop(data, cfg, lnt, treev)
                    else:
                        new_constraint = ""
                        new_constraint += LpSolve.get_vertex_variable(cfg.get_entryID())
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(cfg.get_entryID())
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                        
    def create_constraints_for_loop(self, data, cfg, lnt, treev):
        bound          = data.get_loop_bound(treev.headerID)
        incoming_edges = self.get_loop_entry_edges(cfg, lnt, treev.headerID)
        new_constraint = ""
        new_constraint += LpSolve.get_vertex_variable(treev.headerID)
        new_constraint += LpSolve.lte
        counter = len(incoming_edges)
        for predID, succID in incoming_edges:
            new_constraint += "%d %s" % (bound, LpSolve.get_edge_variable(predID, succID))
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
        self.create_structural_constraints(super_block_cfg)
        self.create_loop_bound_constraints(data, lnt, super_block_cfg)
        self.create_integer_constraint()
        end = timeit.default_timer()
        self.construction_time = end - start
        
    def get_variable_for_program_point(self, program_point):
        if isinstance(program_point, vertices.CFGVertex):
            variable = LpSolve.get_vertex_variable(program_point.vertexID) 
            self.the_variables.add(variable)
            return variable
        else:
            variable = LpSolve.get_edge_variable(program_point.edge[0], program_point.edge[1])
            self.the_variables.add(variable)
            return variable 
                        
    def create_objective_function (self, data, cfg, super_block_cfg):
        self.obj_function = LpSolve.max_
        counter = cfg.number_of_vertices()
        for superv in super_block_cfg:
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex):
                    wcet = data.get_basic_block_wcet(program_point.vertexID)
                    self.obj_function += "%d %s" % (wcet, self.get_variable_for_program_point(program_point))
                    if counter > 1:
                        self.obj_function += LpSolve.plus
                    counter -= 1
        self.obj_function += LpSolve.semi_colon
        
    def create_intra_super_block_constraints(self, super_block_cfg):
        for superv in super_block_cfg:
            for program_point in superv.program_points:
                if isinstance(program_point, vertices.CFGVertex) and program_point != superv.representative:
                    new_constraint = ""
                    new_constraint += self.get_variable_for_program_point(program_point)
                    new_constraint += LpSolve.equals
                    new_constraint += self.get_variable_for_program_point(superv.representative)
                    new_constraint += LpSolve.semi_colon
                    self.the_constraints.append(new_constraint)
            
    def create_structural_constraints (self, super_block_cfg):
        for superv in super_block_cfg:
            if superv.number_of_predecessors() > 1:
                new_constraint = ""
                new_constraint += self.get_variable_for_program_point(superv.representative)
                new_constraint += LpSolve.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    super_predv = super_block_cfg.getVertex(predID)
                    new_constraint += self.get_variable_for_program_point(super_predv.representative)
                    if counter > 1:
                        new_constraint += LpSolve.plus
                    counter -= 1
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    new_constraint = ""
                    new_constraint += self.get_variable_for_program_point(superv.representative)
                    new_constraint += LpSolve.equals
                    counter = len(partition)
                    for succID in partition:
                        super_succv = super_block_cfg.getVertex(succID)
                        new_constraint += self.get_variable_for_program_point(super_succv.representative)
                        if counter > 1:
                            new_constraint += LpSolve.plus
                        counter -= 1
                    new_constraint += LpSolve.semi_colon
                    self.the_constraints.append(new_constraint)                      
            
    def create_loop_bound_constraints(self, data, lnt, super_block_cfg):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if level > 0:
                        pass
                    else:
                        new_constraint = ""
                        new_constraint += LpSolve.get_vertex_variable(treev.headerID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(treev.headerID)
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
        
class CreateCompressedSuperBlockCFGILP(ILP):
    def __init__ (self, data, cfg, lnt, super_block_cfg):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, super_block_cfg.name, "super_block_cfg")
        ILP.__init__(self, filename)
        start = timeit.default_timer()
        self.create_objective_function(data, super_block_cfg)
        self.create_structural_constraints(super_block_cfg)
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
            self.the_variables.add(LpSolve.get_vertex_variable(superv.vertexID))
            self.obj_function += "%d %s" % (wcet, LpSolve.get_vertex_variable(superv.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, super_block_cfg):
        for superv in super_block_cfg:
            if superv.number_of_predecessors() > 1:
                new_constraint = ""
                new_constraint += LpSolve.get_vertex_variable(superv.vertexID)
                new_constraint += LpSolve.equals
                counter = superv.number_of_predecessors()
                for predID in superv.predecessors.keys():
                    new_constraint += LpSolve.get_vertex_variable(predID)
                    if counter > 1:
                        new_constraint += LpSolve.plus
                    counter -= 1
                new_constraint += LpSolve.semi_colon
                self.the_constraints.append(new_constraint)
            if superv.number_of_successors() > 1:
                for partition in superv.successor_partitions.values():
                    new_constraint = ""
                    new_constraint += LpSolve.get_vertex_variable(superv.vertexID)
                    new_constraint += LpSolve.equals
                    counter = len(partition)
                    for succID in partition:
                        new_constraint += LpSolve.get_vertex_variable(succID)
                        if counter > 1:
                            new_constraint += LpSolve.plus
                        counter -= 1
                    new_constraint += LpSolve.semi_colon
                    self.the_constraints.append(new_constraint)                      
            
    def create_loop_bound_constraints(self, data, lnt, super_block_cfg):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    superv = super_block_cfg.find_super_block_for_header(treev.headerID)
                    if level > 0:
                        pass
                    else:
                        new_constraint = ""
                        new_constraint += LpSolve.get_vertex_variable(superv.vertexID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(treev.headerID)
                        new_constraint += LpSolve.semi_colon
                        self.the_constraints.append(new_constraint)
                        
