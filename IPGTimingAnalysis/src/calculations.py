import vertices
import debug
import config
import os
import subprocess
import shlex
import decimal
import abc

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
    def get_edge_variable (edgeID):
        return "%s%d" % (LpSolve.edge_prefix, edgeID)
    
    @staticmethod
    def get_vertex_variable (vertexID):
        return "%s%d" % (LpSolve.vertex_prefix, vertexID)
    
    @staticmethod
    def get_comment (comment):
        return "// " + comment + "\n"
    
    @staticmethod
    def get_new_line (num=1):
        return "\n" * num  
    
class ILP():
    __metaclass__ = abc.ABCMeta
    
    def __init__(self, filename, variable_prefix):
        self.filename                  = filename
        self.variable_prefix           = variable_prefix
        self.wcet                      = -1
        self.variable_execution_counts = {}
        
    def solve(self):
        debug.debug_message("Solving ILP", __name__, 10)
        cmd  = "lp_solve %s" % self.filename 
        proc = subprocess.Popen(cmd, 
                                shell=True, 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE)
        if proc.wait() != 0:
            debug.exitMessage("Running '%s' failed" % cmd)
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes   = shlex.split(line)
                self.wcet = long(decimal.Decimal(lexemes[-1])) 
            elif line.startswith(LpSolve.edge_prefix) or line.startswith(LpSolve.vertex_prefix):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Incorrectly detected variable execution count line '%s'" % line
                prefix = lexemes[0][:2]
                if prefix == self.variable_prefix:
                    variable = int(lexemes[0][2:])
                    count    = int(lexemes[1]) 
                    self.variable_execution_counts[variable] = count
                    
    @abc.abstractmethod
    def print_execution_counts(self):
        pass
    
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
    def create_integer_constraints(self):
        pass
        
class CreateIPGILP (ILP):
    def __init__ (self, data, ipg, lnt, miniIPGs):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, ipg.name, "ipg")
        ILP.__init__(self, filename, LpSolve.edge_prefix)
        with open(filename, 'w') as self.the_file:
            self.create_objective_function(data, ipg)
            self.create_structural_constraints(ipg)
            self.create_loop_bound_constraints(data, lnt, miniIPGs)
            self.create_integer_constraints(ipg)
        
    def print_execution_counts (self, ipg):
        for edgeID, count in self.variable_execution_counts.iteritems():
            debug.verbose_message("Execution count of variable %s = %d" % (LpSolve.get_edge_variable(edgeID), count), __name__)
        basic_block_execution_counts = {}
        for v in ipg:
            for succID in v.successors.keys():                
                succe  = v.get_successor_edge(succID)
                edgeID = succe.get_edgeID() 
                if self.variable_execution_counts[edgeID]:
                    for bbID in succe.edge_label:
                        if bbID not in basic_block_execution_counts:
                            basic_block_execution_counts[bbID] = 0
                        basic_block_execution_counts[bbID] += self.variable_execution_counts[edgeID]
        for vertexID, count in basic_block_execution_counts.iteritems():
            debug.verbose_message("Execution count of variable %s = %d" % (LpSolve.get_vertex_variable(vertexID), count), __name__)

    def create_objective_function (self, data, ipg):
        self.the_file.write(LpSolve.max_)
        counter = ipg.number_of_edges()
        for v in ipg:
            for succID in v.successors.keys():                
                succe          = v.get_successor_edge(succID)
                edgeID         = succe.get_edgeID()
                transitionWCET = data.get_ipg_edge_wcet(v.vertexID, succID)
                self.the_file.write("%d %s" % (transitionWCET, LpSolve.get_edge_variable(edgeID)))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
        self.the_file.write(LpSolve.semi_colon)
        self.the_file.write(LpSolve.get_new_line(2))

    def create_structural_constraints (self, ipg):
        for v in ipg:
            self.the_file.write(LpSolve.get_comment("Vertex %d" % v.vertexID))
            # Analyse the predecessors
            self.the_file.write(LpSolve.get_vertex_variable(v.vertexID))
            self.the_file.write(LpSolve.equals)
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():                    
                prede  = v.get_predecessor_edge(predID)
                edgeID = prede.get_edgeID()
                self.the_file.write(LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            self.the_file.write(LpSolve.semi_colon)
            self.the_file.write(LpSolve.get_new_line())  
            # Flow in, flow out w.r.t to predecessors and successors
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():                    
                prede  = v.get_predecessor_edge(predID)
                edgeID = prede.get_edgeID()
                self.the_file.write(LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            self.the_file.write(LpSolve.equals)   
            # Analyse the successors
            counter = v.number_of_successors()
            for succID in v.successors.keys():
                succe  = v.get_successor_edge(succID)
                edgeID = succe.get_edgeID()
                self.the_file.write(LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            self.the_file.write(LpSolve.semi_colon)
            self.the_file.write(LpSolve.get_new_line(2))
            
    def create_loop_bound_constraints (self, data, lnt, miniIPGs):
        for level, the_vertices in lnt.levelIterator(True):
            for v in the_vertices:
                if isinstance(v, vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if level > 0:
                        self.create_constraints_for_loop(data, lnt, miniIPGs, v)
                    else:
                        headerID = v.headerID
                        self.the_file.write(LpSolve.get_comment("Relative capacity constraint for entry vertex %d" % (headerID)))
                        iterationEdgeIDs = miniIPGs.getMiniIPG(headerID).getIterationEdgeIDs()
                        assert len(iterationEdgeIDs) == 1, "There should be exactly one iteration edge for the entry vertex %d. There are %d." % (headerID, len(iterationEdgeIDs))
                        edgeID = iter(iterationEdgeIDs).next()
                        self.the_file.write(LpSolve.get_edge_variable(edgeID))
                        self.the_file.write(LpSolve.equals)
                        self.the_file.write("1")
                        self.the_file.write(LpSolve.semi_colon)
                        self.the_file.write(LpSolve.get_new_line(2))
                        
    def create_constraints_for_loop (self, data, lnt, miniIPGs, v):
        debug.debug_message("Analysing header %d" % v.headerID, __name__, 1)
        headerID       = v.headerID
        miniIPG        = miniIPGs.getMiniIPG(headerID)
        entryIpoints   = set([])
        ieDestinations = miniIPG.getIterationEdgeDestinations()
        decrementBound = {}
        for succID in v.successors.keys():
            if succID in ieDestinations:
                entryIpoints.add(succID)
                if self.in_loop_exit(succID, miniIPG, lnt, headerID):
                    decrementBound[succID] = False
                else:
                    decrementBound[succID] = True
        for ancestorv in lnt.getAllProperAncestors(v.vertexID):
            # Get the loop bound w.r.t. the ancestor loop
            ancestorHeaderID = ancestorv.headerID
            self.the_file.write(LpSolve.get_comment("Relative capacity constraint for header %d w.r.t to header %d" % (headerID, ancestorHeaderID)))
            bound = data.get_loop_bound(headerID, ancestorHeaderID)
            # Write out the relative edges
            outerMiniIPG        = miniIPGs.getMiniIPG(ancestorHeaderID)
            outerIpoints        = set([])
            ieOuterDestinations = outerMiniIPG.getIterationEdgeDestinations()
            for succID in ancestorv.successors.keys():
                if succID in ieOuterDestinations:
                    outerIpoints.add(succID)
            if len(outerIpoints) == 0:
                debug.debug_message("Loop with header %d does not have Ipoints at its nesting level which are at entry Ipoints" % (ancestorHeaderID), __name__, 1)
                ieOuterSources = outerMiniIPG.getIterationEdgeSources()
                for succID in ancestorv.successors.keys():
                    if succID in ieOuterSources:
                        outerIpoints.add(succID)
            # Write out the entry Ipoints
            for entryID in entryIpoints:
                self.the_file.write(LpSolve.get_vertex_variable(entryID))
                self.the_file.write(LpSolve.lte)
                counter = len(outerIpoints)
                for vertexID in outerIpoints:
                    if decrementBound[entryID] and not lnt.isDoWhileLoop(headerID):
                        self.the_file.write("%d %s" % (bound - 1, LpSolve.get_vertex_variable(vertexID)))
                    else:
                        self.the_file.write("%d %s" % (bound, LpSolve.get_vertex_variable(vertexID)))
                    if counter > 1:
                        self.the_file.write(LpSolve.plus)
                    counter -= 1
                self.the_file.write(LpSolve.semi_colon)
                self.the_file.write(LpSolve.get_new_line())
            self.the_file.write(LpSolve.get_new_line())
    
    def in_loop_exit(self, ipointID, miniIPG, lnt, headerID):
        for exitID in lnt.getLoopExits(headerID):
            if ipointID in miniIPG.getReachableSet(exitID):
                return True
        return False
            
    def create_integer_constraints (self, ipg):
        self.the_file.write(LpSolve.int_)
        counter = ipg.number_of_edges()
        for v in ipg:
            for succID in v.successors.keys():                
                succe  = v.get_successor_edge(succID)
                edgeID = succe.get_edgeID()
                self.the_file.write(" %s" % LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.comma)
                counter -= 1
        self.the_file.write(LpSolve.semi_colon)
        self.the_file.write(LpSolve.get_new_line())
                
class CreateICFGILP (ILP):
    def __init__ (self, data, icfg, lnt):
        filename = "%s.%s.%s.ilp" % (config.Arguments.basepath + os.sep + config.Arguments.basename, icfg.name, "icfg")
        ILP.__init__(self, filename, LpSolve.vertex_prefix)
        self.__vertexIDToExecutionCount = {}
        with open(filename, 'w') as self.the_file:
            self.create_objective_function(data, icfg)
            self.create_structural_constraints(icfg)
            self.create_loop_bound_constraints(data, lnt, icfg)
            self.create_integer_constraints(icfg)

    def print_execution_counts (self, icfg):
        for vertexID, count in self.variable_execution_counts.iteritems():
            if not icfg.isIpoint(vertexID):
                debug.verbose_message("Execution count of variable %s = %d" % (LpSolve.get_vertex_variable(vertexID), count), __name__)

    def create_objective_function (self, data, icfg):
        self.the_file.write(LpSolve.max_)        
        counter = icfg.number_of_vertices()
        for v in icfg:        
            vertexWCET = data.get_basic_block_wcet(v.vertexID)
            self.the_file.write("%d %s" % (vertexWCET, LpSolve.get_vertex_variable(v.vertexID)))
            if counter > 1:
                self.the_file.write(LpSolve.plus)
            counter -= 1
        self.the_file.write(LpSolve.semi_colon)
        self.the_file.write(LpSolve.get_new_line(2))
            
    def create_structural_constraints (self, icfg):
        for v in icfg:
            self.the_file.write(LpSolve.get_comment("Vertex %d" % v.vertexID))
            # Flow into vertex w.r.t. predecessors
            self.the_file.write(LpSolve.get_vertex_variable(v.vertexID))
            self.the_file.write(LpSolve.equals)
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():                    
                prede  = v.get_predecessor_edge(predID)
                edgeID = prede.get_edgeID()
                self.the_file.write(LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            self.the_file.write(LpSolve.semi_colon)
            self.the_file.write(LpSolve.get_new_line())     
            # Flow in, flow out w.r.t to predecessors and successors
            counter = v.number_of_predecessors()
            for predID in v.predecessors.keys():                    
                prede  = v.get_predecessor_edge(predID)
                edgeID = prede.get_edgeID()
                self.the_file.write(LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            self.the_file.write(LpSolve.equals)
            counter = v.number_of_successors()
            for succID in v.successors.keys():
                succe  = v.get_successor_edge(succID)
                edgeID = succe.get_edgeID()
                self.the_file.write(LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            self.the_file.write(LpSolve.semi_colon)
            self.the_file.write(LpSolve.get_new_line(2))       
            
    def create_loop_bound_constraints(self, data, lnt, icfg):
        for level, the_vertices in lnt.levelIterator(True):
            for v in the_vertices:
                if isinstance(v, vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if level > 0:
                        self.create_constraints_for_loop(data, icfg, lnt, v)
                    else:
                        headerID = v.headerID
                        self.the_file.write(LpSolve.get_comment("Relative capacity constraint for entry vertex %d" % (headerID)))
                        self.the_file.write(LpSolve.get_vertex_variable(icfg.get_entryID()))
                        self.the_file.write(LpSolve.equals)
                        self.the_file.write("1")
                        self.the_file.write(LpSolve.semi_colon)
                        self.the_file.write(LpSolve.get_new_line(2))
                        
    def create_constraints_for_loop(self, data, icfg, lnt, v):
        debug.debug_message("Analysing header %d" % v.headerID, __name__, 1)
        headerID         = v.headerID
        parentv          = lnt.getVertex(v.get_parentID())
        relativeHeaderID = headerID
        for ancestorv in lnt.getAllProperAncestors(v.vertexID):
            # Get the loop bound w.r.t. the ancestor loop
            ancestorHeaderID = ancestorv.headerID
            bound = data.get_loop_bound(headerID, ancestorHeaderID)
            self.the_file.write(LpSolve.get_comment("Relative capacity constraint for header %d w.r.t to header %d" % (headerID, ancestorHeaderID)))
            self.the_file.write(LpSolve.get_comment("Bound = %d" % bound))
            self.the_file.write(LpSolve.get_vertex_variable(headerID))
            self.the_file.write(LpSolve.lte)
            # Write out the relative edges
            relativeEdgeIDs = self.get_loop_entry_edges(icfg, lnt, relativeHeaderID)
            counter         = len(relativeEdgeIDs)
            for edgeID in relativeEdgeIDs:
                self.the_file.write("%d %s" % (bound, LpSolve.get_edge_variable(edgeID)))
                if counter > 1:
                    self.the_file.write(LpSolve.plus)
                counter -= 1
            parentv          = lnt.getVertex(v.get_parentID())
            relativeHeaderID = parentv.headerID
            self.the_file.write(LpSolve.semi_colon)
            self.the_file.write(LpSolve.get_new_line(2))
            
    def get_loop_entry_edges(self, icfg, lnt, headerID):
        edgeIDs = []
        v       = icfg.getVertex(headerID)
        for predID in v.predecessors.keys():
            if not lnt.isLoopBackEdge(predID, headerID):
                prede = v.get_predecessor_edge(predID)
                edgeIDs.append(prede.get_edgeID())
        assert edgeIDs, "Unable to find loop-entry edges into loop with header %d" % headerID
        return edgeIDs
    
    def create_integer_constraints(self, icfg):
        self.the_file.write(LpSolve.int_)
        counter = icfg.number_of_vertices() + icfg.number_of_edges()
        for v in icfg:
            self.the_file.write(" %s" % LpSolve.get_vertex_variable(v.vertexID))
            if counter > 1:
                self.the_file.write(LpSolve.comma)
            counter -= 1
            for succID in v.successors.keys():
                succe  = v.get_successor_edge(succID)
                edgeID = succe.get_edgeID()
                self.the_file.write(" %s" % LpSolve.get_edge_variable(edgeID))
                if counter > 1:
                    self.the_file.write(LpSolve.comma)
                counter -= 1
        self.the_file.write(LpSolve.semi_colon)
        self.the_file.write(LpSolve.get_new_line())
    