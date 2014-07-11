import trees
import super_block_graphs
import udraw
import database
import calculations
    
class Program():
    def __init__(self):
        self.cfgs             = {}
        self.lnts             = {}
        self.super_block_cfgs = {}
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg
        udraw.make_file(cfg, "%s.cfg" % (cfg.name))
        
    def create_LNTs(self):
        for name, cfg in self.cfgs.iteritems():
            self.lnts[name] = trees.LoopNests(cfg, cfg.get_entryID())
            udraw.make_file(self.lnts[name], "%s.lnt" % (name))
            
    def create_super_block_CFGs(self):
        for name, cfg in self.cfgs.iteritems():
            self.super_block_cfgs[name] = super_block_graphs.SuperBlockCFG(cfg, self.lnts[name])
            udraw.make_file(self.super_block_cfgs[name], "%s.superg" % (name))
            
    def do_wcet_calculation(self):
        self.cfg_ilps             = {}
        self.super_block_cfg_ilps = {}
        for cfg in self.cfgs.values():
            # CFG calculation
            data    = database.CreateWCETData(cfg, self.lnts[cfg.name])
            cfg_ilp = calculations.CreateCFGILP(data, cfg, self.lnts[cfg.name])
            cfg_ilp.solve()
            self.cfg_ilps[cfg.name] = cfg_ilp            
            print("CFG::             WCET(%s) = %d (solve time = %f) (construction time = %f) (total time = %f)" % (cfg.name, 
                                                                                                        cfg_ilp.wcet, 
                                                                                                        cfg_ilp.solve_time,
                                                                                                        cfg_ilp.construction_time,
                                                                                                        cfg_ilp.solve_time + cfg_ilp.construction_time))
            
            super_block_cfg_ilp = calculations.CreateSuperBlockCFGILP(data, cfg, self.lnts[cfg.name], self.super_block_cfgs[cfg.name])
            super_block_cfg_ilp.solve()
            self.super_block_cfg_ilps[cfg.name] = super_block_cfg_ilp            
            print("Super block CFG:: WCET(%s) = %d (solve time = %f) (construction time = %f) (total time = %f)" % (cfg.name, 
                                                                                                                    super_block_cfg_ilp.wcet, 
                                                                                                                    super_block_cfg_ilp.solve_time,
                                                                                                                    super_block_cfg_ilp.construction_time,
                                                                                                                    super_block_cfg_ilp.solve_time + super_block_cfg_ilp.construction_time))