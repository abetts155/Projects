from __future__ import print_function

import trees
import cfgs
import ipgs
import udraw
import database
import calculations
import vertices
import debug

class LoopByLoopInformation():    
    def __init__ (self, enhanced_icfg, enhanced_lnt, ipg):
        self.enhanced_icfgs_per_loop = {}
        self.ipgs_per_loop           = {}
        self.iteration_edges         = {}
        self.loop_entry_edges        = {}
        self.loop_exit_edges         = {}
        self.initialise(enhanced_lnt)
        self.construct_loop_info(enhanced_icfg, enhanced_lnt, ipg)
        self.check(ipg)
    
    def initialise(self, enhanced_lnt):
        for treev in enhanced_lnt:
            if isinstance(treev, vertices.HeaderVertex):
                self.iteration_edges[treev.headerID]  = set()
                self.loop_entry_edges[treev.headerID] = set()
                self.loop_exit_edges[treev.headerID]  = set()
                
    def construct_loop_info(self, enhanced_icfg, enhanced_lnt, ipg):
        for treev in enhanced_lnt:
            if isinstance(treev, vertices.HeaderVertex):
                debug.debug_message("Analysing header %d" % treev.headerID, __name__, 1)
                enhanced_icfg_of_loop = enhanced_lnt.induce_subgraph(treev)
                self.enhanced_icfgs_per_loop[treev.headerID] = enhanced_icfg_of_loop
                udraw.make_file(enhanced_icfg_of_loop, "%s.header_%d.%s" % (enhanced_icfg.name, treev.headerID, "icfg"))
                self.ipgs_per_loop[treev.headerID] = ipgs.IPGLoopInformation(self, 
                                                                             treev.headerID, 
                                                                             enhanced_icfg_of_loop, 
                                                                             enhanced_lnt, 
                                                                             ipg)

    def check(self, ipg):
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
        self.enhanced_icfgs[icfg.name] = cfgs.EnhancedCFG(icfg)
        udraw.make_file(icfg, "%s.icfg" % (icfg.name))
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
    
    def do_analysis(self):
        self.create_LNTs()
        self.create_IPGs()
        self.create_loop_by_loop_information()
        self.do_wcet_calculation()
        