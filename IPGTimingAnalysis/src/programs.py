from __future__ import print_function

import trees
import cfgs
import ipgs
import udraw
import database
import calculations
    
class Program():
    def __init__(self):
        self.icfgs             = {}
        self.enhanced_icfgs    = {}
        self.ipgs              = {}
        self.lnts              = {}
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
            udraw.make_file(enhanced_icfg, "%s.icfg" % (icfg.name))
            ipg = ipgs.IPG(enhanced_icfg)
            assert ipg.name
            self.ipgs[ipg.name] = ipg
            udraw.make_file(ipg, "%s.ipg" % (ipg.name))
            
    def create_loop_by_loop_IPG_information(self):
        for icfg in self.icfgs.values():
            lnt = self.lnts[icfg.name]
            ipg = self.ipgs[icfg.name]
            self.loop_by_loop_info[icfg.name] = ipgs.LoopByLoopIPGInformation(icfg, lnt, ipg)
    
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
            lnt           = self.lnts[icfg.name]
            ipg           = self.ipgs[icfg.name]
            ipg_loop_info = self.loop_by_loop_info[icfg.name]
            data          = database.CreateWCETData(icfg, lnt, ipg)
            icfg_ilp      = calculations.CreateICFGILP(data, icfg, lnt)
            icfg_ilp.solve()
            print("ICFG:: WCET(%s) = %d" % (icfg.name, icfg_ilp.wcet))
            self.icfg_ilps[icfg.name] = icfg_ilp
            ipg_ilp      = calculations.CreateIPGILP(data, ipg, lnt, ipg_loop_info)
            ipg_ilp.solve()
            print("IPG:: WCET(%s) = %d" % (icfg.name, ipg_ilp.wcet))
            self.ipg_ilps[icfg.name] = ipg_ilp
            calculations.compare_execution_counts(icfg_ilp.variable_execution_counts,
                                                  ipg_ilp.compute_execution_counts(self.enhanced_icfgs[icfg.name], ipg))
            
            