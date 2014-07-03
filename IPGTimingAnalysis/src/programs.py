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
    def __init__ (self, icfg, lnt, ipg):
        self.loop_ICFGs       = {}
        self.loop_IPGs        = {}
        self.iteration_edges  = {}
        self.loop_entry_edges = {}
        self.loop_exit_edges  = {}
        self.initialise(lnt)
        self.construct_loop_info(icfg, lnt, ipg)
        self.check(ipg)
    
    def initialise(self, lnt):
        for treev in lnt:
            if isinstance(treev, vertices.HeaderVertex):
                self.iteration_edges[treev.headerID]  = set()
                self.loop_entry_edges[treev.headerID] = set()
                self.loop_exit_edges[treev.headerID]  = set()
                
    def construct_loop_info(self, icfg, lnt, ipg):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    debug.debug_message("Analysing header %d" % treev.headerID, __name__, 1)
                    loopICFG  = lnt.induce_subgraph(treev)
                    self.loop_ICFGs[treev.headerID] = loopICFG
                    loopICFG.set_edgeIDs()
                    udraw.make_file(loopICFG, "%s.header_%d.%s" % (icfg.name, treev.headerID, "icfg"))
                    self.loop_IPGs[treev.headerID] = ipgs.LoopIPG(self, treev.headerID, loopICFG, lnt, ipg)
    
    def check(self, ipg):
        edges_in_ipg = set()
        for v in ipg:
            for succID in v.successors.keys():
                edges_in_ipg.add((v.vertexID, succID))
        for ipg_loop_info in self.loop_IPGs.values():
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
        udraw.make_file(icfg, "%s.icfg" % (icfg.name))
    
    def create_IPGs(self):
        for icfg in self.icfgs.values():
            enhanced_icfg = cfgs.EnhancedCFG(icfg)
            enhanced_icfg.set_edgeIDs()
            self.enhanced_icfgs[icfg.name] = enhanced_icfg
            udraw.make_file(enhanced_icfg, "%s.enhanced_icfg" % (icfg.name))
            enhanced_lnt = trees.LoopNests(enhanced_icfg, enhanced_icfg.get_entryID())
            self.enhanced_lnts[icfg.name] = enhanced_lnt
            udraw.make_file(enhanced_lnt, "%s.enhanced_lnt" % (icfg.name))
            ipg = ipgs.IPG(enhanced_icfg)
            assert ipg.name
            self.ipgs[ipg.name] = ipg
            udraw.make_file(ipg, "%s.ipg" % (ipg.name))
            
    def create_loop_by_loop_IPG_information(self):
        for icfg in self.icfgs.values():
            lnt = self.lnts[icfg.name]
            ipg = self.ipgs[icfg.name]
            self.loop_by_loop_info[icfg.name] = LoopByLoopInformation(icfg, lnt, ipg)
            udraw.make_file(ipg, "%s.ipg" % (ipg.name))
    
    def create_LNTs(self):
        for icfg in self.icfgs.values():
            lnt = trees.LoopNests(icfg, icfg.get_entryID())
            assert lnt.name
            self.lnts[lnt.name] = lnt
            udraw.make_file(lnt, "%s.lnt" % (lnt.name))
            
    def do_wcet_calculation(self):
        self.icfg_ilps = {}
        self.ipg_ilps  = {}
        for icfg in self.icfgs.values():
            # ICFG calculation
            data     = database.CreateWCETData(icfg, self.enhanced_lnts[icfg.name], self.ipgs[icfg.name], self.loop_by_loop_info[icfg.name])
            icfg_ilp = calculations.CreateICFGILP(data, icfg, self.lnts[icfg.name])
            icfg_ilp.solve()
            self.icfg_ilps[icfg.name] = icfg_ilp            
            print("ICFG:: WCET(%s) = %d" % (icfg.name, icfg_ilp.wcet))
            # IPG calculation
            ipg_loop_info = self.loop_by_loop_info[icfg.name]
            ipg_ilp       = calculations.CreateIPGILP(data, self.ipgs[icfg.name], self.lnts[icfg.name], ipg_loop_info)
            ipg_ilp.solve()
            self.ipg_ilps[icfg.name] = ipg_ilp
            print("IPG:: WCET(%s) = %d" % (icfg.name, ipg_ilp.wcet))
            # Do comparison
            #calculations.compare_execution_counts(icfg_ilp.variable_execution_counts,
            #                                      ipg_ilp.compute_execution_counts(self.enhanced_icfgs[icfg.name], self.ipgs[icfg.name]))
            
            