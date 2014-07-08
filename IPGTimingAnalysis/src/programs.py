from __future__ import print_function

import config
import trees
import cfgs
import ipgs
import udraw
import database
import calculations
import vertices
import debug

class LoopByLoopInformation():    
    def __init__ (self, enhanced_icfg, enhanced_lnt, lnt, ipg):
        self.enhanced_lnt            = enhanced_lnt
        self.enhanced_icfgs_per_loop = {}
        self.ipgs_per_loop           = {}
        self.loop_back_edges         = {}
        self.loop_entry_edges        = {}
        self.loop_exit_edges         = {}
        self.loop_exit_regions       = {}
        self.initialise()
        self.construct_loop_info(enhanced_icfg, lnt, ipg)
        self.check_IPG_edges(ipg)
    
    def initialise(self):
        for treev in self.enhanced_lnt:
            if isinstance(treev, vertices.HeaderVertex):
                self.loop_back_edges[treev.headerID] = set()
                self.loop_entry_edges[treev.headerID] = set()
                self.loop_exit_edges[treev.headerID]  = set()
                
    def construct_loop_info(self, enhanced_icfg, lnt, ipg):
        for v in ipg:
            for succID in v.successors.keys():
                succe = v.get_successor_edge(succID)
                for program_point in succe.edge_label:
                    if isinstance(program_point, vertices.CFGEdge):
                        the_edge = program_point.edge
                        if lnt.is_loop_back_edge(the_edge[0], the_edge[1]):
                            self.loop_back_edges[the_edge[1]].add((v.vertexID, succID))
        
        for treev in self.enhanced_lnt:
            if isinstance(treev, vertices.HeaderVertex):
                debug.debug_message("Analysing header %d" % treev.headerID, __name__, 1)
                enhanced_icfg_of_loop = self.enhanced_lnt.induce_subgraph(treev)
                self.enhanced_icfgs_per_loop[treev.headerID] = enhanced_icfg_of_loop
                self.loop_exit_regions[treev.headerID] = self.compute_reachable_program_points_from_loop_exits(treev)
                udraw.make_file(enhanced_icfg_of_loop, "%s.header_%d.%s" % (enhanced_icfg.name, treev.headerID, "icfg"))
                self.ipgs_per_loop[treev.headerID] = ipgs.IPGLoopInformation(self, 
                                                                             treev.headerID, 
                                                                             enhanced_icfg_of_loop, 
                                                                             self.enhanced_lnt, 
                                                                             ipg)

    def compute_reachable_program_points_from_loop_exits(self, treev):
        enhanced_ICFG = self.enhanced_icfgs_per_loop[treev.headerID]
        reverse_ICFG  = enhanced_ICFG.get_reverse_graph()
        reachable     = set()
        if treev.vertexID == self.enhanced_lnt.get_rootID():
            for v in reverse_ICFG:
                reachable.add(v.vertexID)
        else:
            for exitID in self.enhanced_lnt.get_loop_exits_for_header(treev.headerID):
                stack = []
                stack.append(exitID)
                while stack:
                    poppedID = stack.pop()
                    reachable.add(poppedID)
                    poppedv  = reverse_ICFG.getVertex(poppedID)
                    for succID in poppedv.successors.keys():
                        stack.append(succID)
        return reachable

    def check_IPG_edges(self, ipg):
        edges_in_ipg = set()
        for v in ipg:
            for succID in v.successors.keys():
                edges_in_ipg.add((v.vertexID, succID))
        for ipg_loop_info in self.ipgs_per_loop.values():
            for (predID, succID) in ipg_loop_info.edges_added:
                if (predID, succID) not in edges_in_ipg:
                    debug.warning_message("IPG edge (%d, %d) should NOT be in the IPG" % (predID, succID))
                else:
                    edges_in_ipg.remove((predID, succID))
        if edges_in_ipg:
            for (predID, succID) in edges_in_ipg:
                debug.warning_message("IPG edgee (%d, %d) should be in the IPG" % (predID, succID))
    
    def get_loop_ICFG(self, headerID):
        assert headerID in self.loopICFGs, "Unable to find the loop ICFG for header %d" % (headerID)
        return self.loopICFGs[headerID]
    
    def get_loop_IPG(self, headerID):
        assert headerID in self.loopIPGs, "Unable to find the loop IPG for header %d" % (headerID)
        return self.loopIPGs[headerID]
    
    def is_in_loop_exit_region(self, headerID, vertexID):
        return vertexID in self.loop_exit_regions[headerID]
    
class Program():
    def __init__(self):
        self.icfgs             = {}
        self.enhanced_icfgs    = {}
        self.lnts              = {}
        self.enhanced_lnts     = {}
        self.ipgs              = {}
        self.loop_by_loop_info = {}
        
    def add_ICFG(self, icfg):
        assert icfg.name
        self.icfgs[icfg.name] = icfg
        udraw.make_file(icfg, "%s.icfg" % (icfg.name))
    
    def create_enhanced_ICFGs(self):
        for name, icfg in self.icfgs.iteritems():
            self.enhanced_icfgs[name] = cfgs.EnhancedICFG(icfg)
            udraw.make_file(self.enhanced_icfgs[icfg.name], "%s.enhanced_icfg" % (icfg.name))
    
    def create_LNTs(self):
        for name, icfg in self.icfgs.iteritems():
            self.lnts[name] = trees.LoopNests(icfg, icfg.get_entryID())
            self.enhanced_lnts[name] = trees.LoopNests(self.enhanced_icfgs[name], 
                                                       self.enhanced_icfgs[name].get_entryID())
            udraw.make_file(self.lnts[name], "%s.lnt" % (name))
            udraw.make_file(self.enhanced_lnts[name], "%s.enhanced_lnt" % (name))
    
    def create_IPGs(self):
        for name, enhanced_icfg in self.enhanced_icfgs.iteritems():
            self.ipgs[name] = ipgs.IPG(enhanced_icfg)
            udraw.make_file(self.ipgs[name], "%s.ipg" % (name))
            
    def create_loop_by_loop_information(self):
        for name, enhanced_icfg in self.enhanced_icfgs.iteritems():
            self.loop_by_loop_info[name] = LoopByLoopInformation(enhanced_icfg, 
                                                                 self.enhanced_lnts[name], 
                                                                 self.lnts[name],
                                                                 self.ipgs[name])
            udraw.make_file(self.ipgs[name], "%s.ipg" % (name))
            
    def do_wcet_calculation(self):
        self.icfg_ilps = {}
        self.ipg_ilps  = {}
        for icfg in self.icfgs.values():
            # ICFG calculation
            data     = database.CreateWCETData(icfg, 
                                               self.enhanced_lnts[icfg.name], 
                                               self.ipgs[icfg.name], 
                                               self.loop_by_loop_info[icfg.name])
            icfg_ilp = calculations.CreateICFGILP(data, 
                                                  icfg, 
                                                  self.lnts[icfg.name])
            icfg_ilp.solve()
            self.icfg_ilps[icfg.name] = icfg_ilp            
            print("ICFG:: WCET(%s) = %d (solve time = %f)" % (icfg.name, icfg_ilp.wcet, icfg_ilp.solve_time))
            # IPG calculation
            ipg_loop_info = self.loop_by_loop_info[icfg.name]
            ipg_ilp       = calculations.CreateIPGILP(data, 
                                                      self.ipgs[icfg.name], 
                                                      self.lnts[icfg.name],
                                                      self.enhanced_lnts[icfg.name], 
                                                      ipg_loop_info)
            ipg_ilp.solve()
            self.ipg_ilps[icfg.name] = ipg_ilp
            print("IPG::  WCET(%s) = %d (solve time = %f)" % (icfg.name, ipg_ilp.wcet, ipg_ilp.solve_time))
    
    def instrument(self):
        for icfg in self.icfgs.values():
            if config.Arguments.add_path_reconstructible_instrumentation:
                icfg.instrument_using_depth_first_spanning_tree()
            else:
                icfg.add_edges_between_ipoints()
            udraw.make_file(icfg, "%s.icfg" % (icfg.name))
    
    def do_analysis(self):
        self.create_enhanced_ICFGs()
        self.create_LNTs()
        self.create_IPGs()
        self.create_loop_by_loop_information()
        self.do_wcet_calculation()
        