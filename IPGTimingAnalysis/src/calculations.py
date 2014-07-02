from __future__ import print_function

import vertices
import debug
import config
import os
import subprocess
import shlex
import decimal
import abc
import sys

def compare_execution_counts(icfg_ilp_execution_counts, ipg_ilp_execution_counts):
    for key, count in icfg_ilp_execution_counts.iteritems():
        if key in ipg_ilp_execution_counts and ipg_ilp_execution_counts[key] == count:
            print("Equality on %s: %d" % (key, count))
    for key, count in icfg_ilp_execution_counts.iteritems():
        if key in ipg_ilp_execution_counts and ipg_ilp_execution_counts[key] != count:
            print("Inequality on %s: %d and %d" % (key, count, ipg_ilp_execution_counts[key]))
        elif count > 0 and key not in ipg_ilp_execution_counts:
            print("Inequality on %s: %d and %d" % (key, count, 0))

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
        self.constraints               = []
      
    def write_to_file(self):
        with open(self.filename, 'w') as the_file:
            the_file.write(self.obj_function)
            the_file.write(LpSolve.get_new_line(2))
            for constraint in self.constraints:
                the_file.write(constraint)
                the_file.write(LpSolve.get_new_line())
            the_file.write(LpSolve.get_new_line())
            the_file.write(self.int_constraint)
            the_file.write(LpSolve.get_new_line())
      
    def solve(self):
        debug.debug_message("Solving ILP", __name__, 10)
        self.write_to_file()
        cmd  = "lp_solve %s" % self.filename 
        proc = subprocess.Popen(cmd, 
                                shell=True, 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE)
        if proc.wait() != 0:
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
    
    @abc.abstractmethod
    def create_integer_constraint(self):
        pass
        
class CreateIPGILP (ILP):
    def __init__ (self, data, ipg, lnt, ipg_loop_info):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, ipg.name, "ipg")
        ILP.__init__(self, filename)
        self.create_objective_function(data, ipg)
        self.create_structural_constraints(ipg)
        self.create_loop_bound_constraints(data, ipg, lnt, ipg_loop_info)
        self.create_integer_constraint(ipg)

    def create_objective_function (self, data, ipg):
        self.obj_function = LpSolve.max_
        counter = ipg.number_of_edges()
        for v in ipg:
            for succID in v.successors.keys():      
                wcet  = data.get_ipg_edge_wcet(v.vertexID, succID)
                self.obj_function += "%d %s" % (wcet, LpSolve.get_edge_variable(v.vertexID, succID))
                if counter > 1:
                    self.obj_function += LpSolve.plus
                counter -= 1
        self.obj_function += LpSolve.semi_colon

    def create_structural_constraints (self, ipg):
        for v in ipg:
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
            self.constraints.append(new_constraint)
            
    def create_loop_bound_constraints (self, data, ipg, lnt, ipg_loop_info):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if level > 0:
                        self.create_constraints_for_loop(data, ipg, lnt, ipg_loop_info, treev)
                    else:
                        iteration_edges = ipg_loop_info.iteration_edges[treev.headerID]
                        assert len(iteration_edges) == 1, \
                        "There should be exactly 1 iteration edge for the entry vertex %d but there are %d" % (treev.headerID, len(iteration_edges))
                        new_constraint = ""
                        predID, succID = list(iteration_edges)[0]
                        new_constraint += LpSolve.get_edge_variable(predID, succID)
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(treev.headerID)
                        new_constraint += LpSolve.semi_colon
                        self.constraints.append(new_constraint)
                        
    def create_constraints_for_loop (self, data, ipg, lnt, ipg_loop_info, headerv):
        iteration_edges = ipg_loop_info.iteration_edges[headerv.headerID]
        bound           = sys.maxint
        for predID, succID in iteration_edges:
            predv = ipg.getVertex(predID)
            succe = predv.get_successor_edge(succID)
            for icfgv in succe.edge_label:
                bound = min(data.get_loop_bound(icfgv.vertexID), bound)

        new_constraint  = ""
        counter = len(iteration_edges)
        for predID, succID in iteration_edges:
            new_constraint += LpSolve.get_edge_variable(predID, succID)
            if counter > 1:
                new_constraint += LpSolve.plus
            counter -= 1
        new_constraint += LpSolve.lte
        
        relative_edges = None
        if ipg_loop_info.loop_entry_edges[headerv.headerID]:
            relative_edges = ipg_loop_info.loop_entry_edges[headerv.headerID]
        else:
            relative_edges = ipg_loop_info.loop_exit_edges[headerv.headerID]
        assert relative_edges, "Unable to find loop-entry or loop-exit edges for header %d" % headerv.headerID
        
        counter = len(relative_edges)
        for predID, succID in relative_edges:
            new_constraint += "%d %s" % (bound, LpSolve.get_edge_variable(predID, succID))
            if counter > 1:
                new_constraint += LpSolve.plus
            counter -= 1

        new_constraint += LpSolve.semi_colon
        self.constraints.append(new_constraint)
            
    def create_integer_constraint(self, ipg):
        self.int_constraint = LpSolve.int_
        counter = ipg.number_of_edges()
        for v in ipg:
            for succID in v.successors.keys():
                self.int_constraint += " %s" % LpSolve.get_edge_variable(v.vertexID, succID)
                if counter > 1:
                    self.int_constraint += LpSolve.comma
                counter -= 1
        self.int_constraint += LpSolve.semi_colon
        
    def compute_execution_counts(self, icfg, ipg):
        execution_counts = {}
        for v in ipg:
            for succID in v.successors.keys():                
                succe    = v.get_successor_edge(succID)
                variable = LpSolve.get_edge_variable(v.vertexID, succID)
                if self.variable_execution_counts[variable]:
                    for program_point in succe.edge_label:
                        program_pointv = icfg.getVertex(program_point.vertexID)
                        if isinstance(program_pointv, vertices.CFGVertex):
                            key = LpSolve.get_vertex_variable(program_point.vertexID)
                        else:
                            predID, succID = program_pointv.edge
                            key = LpSolve.get_edge_variable(predID, succID)
                        if key not in execution_counts:
                            execution_counts[key] = 0
                        execution_counts[key] += self.variable_execution_counts[variable]
        return execution_counts
                
class CreateICFGILP (ILP):
    def __init__ (self, data, icfg, lnt):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, icfg.name, "icfg")
        ILP.__init__(self, filename)
        self.create_objective_function(data, icfg)
        self.create_structural_constraints(icfg)
        self.create_loop_bound_constraints(data, lnt, icfg)
        self.create_integer_constraint(icfg)

    def create_objective_function (self, data, icfg):
        self.obj_function = LpSolve.max_
        counter = icfg.number_of_vertices()
        for v in icfg:        
            wcet = data.get_basic_block_wcet(v.vertexID)
            self.obj_function += "%d %s" % (wcet, LpSolve.get_vertex_variable(v.vertexID))
            if counter > 1:
                self.obj_function += LpSolve.plus
            counter -= 1
        self.obj_function += LpSolve.semi_colon
            
    def create_structural_constraints (self, icfg):
        for v in icfg:
            # Flow in, flow out w.r.t to vertex and its predecessor edges
            new_constraint = ""
            new_constraint += LpSolve.get_vertex_variable(v.vertexID)
            new_constraint += LpSolve.equals
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():    
                new_constraint += LpSolve.get_edge_variable(predID, v.vertexID)
                if counter > 1:
                    new_constraint += LpSolve.plus
                counter -= 1    
            new_constraint += LpSolve.semi_colon
            self.constraints.append(new_constraint)
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
            self.constraints.append(new_constraint)       
            
    def create_loop_bound_constraints(self, data, lnt, icfg):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if level > 0:
                        self.create_constraints_for_loop(data, icfg, lnt, treev)
                    else:
                        new_constraint = ""
                        new_constraint += LpSolve.get_vertex_variable(icfg.get_entryID())
                        new_constraint += LpSolve.equals
                        new_constraint += "%d" % data.get_loop_bound(icfg.get_entryID())
                        new_constraint += LpSolve.semi_colon
                        self.constraints.append(new_constraint)
                        
    def create_constraints_for_loop(self, data, icfg, lnt, treev):
        bound          = data.get_loop_bound(treev.headerID)
        incoming_edges = self.get_loop_entry_edges(icfg, lnt, treev.headerID)
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
        self.constraints.append(new_constraint)
            
    def get_loop_entry_edges(self, icfg, lnt, headerID):
        edgeIDs = []
        v       = icfg.getVertex(headerID)
        for predID in v.predecessors.keys():
            if not lnt.isLoopBackEdge(predID, headerID):
                edgeIDs.append((predID, headerID))
        assert edgeIDs, "Unable to find loop-entry edges into loop with header %d" % headerID
        return edgeIDs
    
    def create_integer_constraint(self, icfg):
        self.int_constraint = LpSolve.int_
        counter = icfg.number_of_vertices() + icfg.number_of_edges()
        for v in icfg:
            self.int_constraint += " %s" % LpSolve.get_vertex_variable(v.vertexID)
            if counter > 1:
                self.int_constraint += LpSolve.comma
            counter -= 1
            for succID in v.successors.keys():
                succe = v.get_successor_edge(succID)
                self.int_constraint += " %s" % LpSolve.get_edge_variable(v.vertexID, succID)
                if counter > 1:
                    self.int_constraint += LpSolve.comma
                counter -= 1
        self.int_constraint += LpSolve.semi_colon

    