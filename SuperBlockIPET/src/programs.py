import trees
import super_block_graphs
import udraw
import database
import calculations
import config
import sys
import os
    
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
        self.cfg_ilps                           = {}
        self.super_block_cfg_ilps               = {}
        total_cfg_solve_time                    = 0.0
        total_cfg_construction_time             = 0.0
        total_super_block_cfg_solve_time        = 0.0
        total_super_block_cfg_construction_time = 0.0
        
        if config.Arguments.log_to_file:
            filename   = "%s.output.txt" % (config.Arguments.basepath + os.sep + config.Arguments.basename)
            log_file   = open(filename, 'w')
            old_stdout = sys.stdout
            sys.stdout = log_file
        try:
            for cfg in self.cfgs.values():
                data                                     = database.CreateWCETData(cfg, self.lnts[cfg.name])
                cfg_ilp                                  = calculations.CreateCFGILP(data, cfg, self.lnts[cfg.name])
                self.cfg_ilps[cfg.name]                  = cfg_ilp
                total_cfg_construction_time             += cfg_ilp.construction_time
                super_block_cfg_ilp                      = calculations.CreateSuperBlockCFGILP(data, cfg, self.lnts[cfg.name], self.super_block_cfgs[cfg.name])
                self.super_block_cfg_ilps[cfg.name]      = super_block_cfg_ilp 
                total_super_block_cfg_construction_time += super_block_cfg_ilp.construction_time  
                    
                for i in range(1, config.Arguments.repeat_calculation + 1):
                    print("===== Repetition %d =====" % i)
                    cfg_ilp.solve()     
                    total_cfg_solve_time += cfg_ilp.solve_time
                    print("CFG::             WCET(%s) = %d" % (cfg.name, cfg_ilp.wcet))
                    
                    super_block_cfg_ilp.solve()
                    total_super_block_cfg_solve_time += super_block_cfg_ilp.solve_time         
                    print("Super block CFG:: WCET(%s) = %d" % (cfg.name, super_block_cfg_ilp.wcet))
                    
                    assert cfg_ilp.wcet == super_block_cfg_ilp.wcet, "Disparity in WCETs: (%f, %f)" % (cfg_ilp.wcet, super_block_cfg_ilp.wcet)
                   
                print("CFG::             %s (solve time = %f) (construction time = %f) (total time = %f)" % (cfg.name, 
                                                                                                             total_cfg_solve_time/config.Arguments.repeat_calculation,
                                                                                                             total_cfg_construction_time,
                                                                                                             total_cfg_solve_time/config.Arguments.repeat_calculation + total_cfg_construction_time))
                        
                print("Super block CFG:: %s (solve time = %f) (construction time = %f) (total time = %f)" % (cfg.name, 
                                                                                                             total_super_block_cfg_solve_time/config.Arguments.repeat_calculation,
                                                                                                             total_super_block_cfg_construction_time,
                                                                                                             total_super_block_cfg_solve_time/config.Arguments.repeat_calculation + total_super_block_cfg_construction_time))
        finally:
            if config.Arguments.log_to_file:
                log_file.close()
                sys.stdout = old_stdout