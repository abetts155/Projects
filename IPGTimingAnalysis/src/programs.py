import trees
import ipgs
import udraw
import database
import calculations
    
class Program():
    def __init__(self):
        self.icfgs             = {}
        self.ipgs              = {}
        self.lnts              = {}
        self.loop_by_loop_ipgs = {}
        
    def addICFG(self, icfg):
        assert icfg.name
        self.icfgs[icfg.name] = icfg
        udraw.make_file(icfg, "%s.icfg" % (icfg.name))
    
    def create_ipgs(self):
        for icfg in self.icfgs.values():
            ipg = ipgs.IPG(icfg)
            assert ipg.name
            self.ipgs[ipg.name] = ipg
            udraw.make_file(ipg, "%s.ipg" % (ipg.name))
            
    def create_loop_by_loop_ipgs(self):
        for icfg in self.icfgs.values():
            lnt = self.lnts[icfg.name]
            ipg = self.ipgs[icfg.name]
            self.loop_by_loop_ipgs[icfg.name] = ipgs.LoopByLoopIPGs(icfg, lnt, ipg)
    
    def create_lnts(self):
        for icfg in self.icfgs.values():
            lnt = trees.LoopNests(icfg, icfg.getEntryID())
            assert lnt.name
            self.lnts[lnt.name] = lnt
            udraw.make_file(lnt, "%s.lnt" % (lnt.name))
            
    def do_wcet_calculation(self):
        self.icfg_ilps = {}
        self.ipg_ilps  = {}
        for icfg in self.icfgs.values():
            lnt                       = self.lnts[icfg.name]
            ipg                       = self.ipgs[icfg.name]
            ipg_per_loop              = self.loop_by_loop_ipgs[icfg.name]
            data                      = database.CreateWCETData(icfg, lnt, ipg)
            self.icfg_ilps[icfg.name] = calculations.CreateICFGILP(data, icfg, lnt)
            self.ipg_ilps[icfg.name]  = calculations.CreateIPGILP(data, ipg, lnt, ipg_per_loop)
            self.icfg_ilps[icfg.name].solve()
            self.ipg_ilps[icfg.name].solve()