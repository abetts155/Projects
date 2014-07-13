import trees
import super_block_graphs
import udraw
import database
import calculations
import config
import sys
import os
import numpy

class CalculationInformation:
    def __init__(self, ilp):
        self.ilp         = ilp
        self.solve_times = []
    
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
            
    def print_results(self, cfg):                 
        print("""
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
len(self.cfg_calculations[cfg.name].ilp.the_variables),
len(self.cfg_calculations[cfg.name].ilp.the_constraints),
numpy.amin(self.cfg_calculations[cfg.name].solve_times),
numpy.amax(self.cfg_calculations[cfg.name].solve_times), 
numpy.average(self.cfg_calculations[cfg.name].solve_times)))

        print("""
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
len(self.super_block_cfg_calculations[cfg.name].ilp.the_variables),
len(self.super_block_cfg_calculations[cfg.name].ilp.the_constraints),
numpy.amin(self.super_block_cfg_calculations[cfg.name].solve_times),
numpy.amax(self.super_block_cfg_calculations[cfg.name].solve_times), 
numpy.average(self.super_block_cfg_calculations[cfg.name].solve_times)))

    def repeat_calculation(self, cfg, cfg_ilp, super_block_cfg_ilp):
        for i in range(1, config.Arguments.repeat_calculation + 1):
            print("===== Repetition %d =====" % i)
            if config.Arguments.shuffle_constraints:
                cfg_ilp.shuffle()
                super_block_cfg_ilp.shuffle()
            cfg_ilp.solve() 
            self.cfg_calculations[cfg.name].solve_times.append(cfg_ilp.solve_time)    
            print("CFG::             WCET(%s) = %d" % (cfg.name, cfg_ilp.wcet))
            super_block_cfg_ilp.solve()
            self.super_block_cfg_calculations[cfg.name].solve_times.append(super_block_cfg_ilp.solve_time)    
            print("Super block CFG:: WCET(%s) = %d" % (cfg.name, super_block_cfg_ilp.wcet))
            assert cfg_ilp.wcet == super_block_cfg_ilp.wcet, "Disparity in WCETs: (%f, %f)" % (cfg_ilp.wcet, super_block_cfg_ilp.wcet)      

    def do_wcet_calculation(self):
        self.cfg_calculations             = {}
        self.super_block_cfg_calculations = {}
        if config.Arguments.log_to_file:
            filename   = os.path.abspath(config.Arguments.log_to_file)
            log_file   = open(filename, 'w')
            old_stdout = sys.stdout
            sys.stdout = log_file
        try:
            for cfg in self.cfgs.values():
                data                            = database.CreateWCETData(cfg, self.lnts[cfg.name])
                cfg_ilp                         = calculations.CreateCFGILP(data, cfg, self.lnts[cfg.name])
                self.cfg_calculations[cfg.name] = CalculationInformation(cfg_ilp)
                if config.Arguments.fold_wcets_of_super_blocks:
                    super_block_cfg_ilp = calculations.CreateCompressedSuperBlockCFGILP(data, cfg, self.lnts[cfg.name], self.super_block_cfgs[cfg.name])
                else:
                    super_block_cfg_ilp = calculations.CreateSuperBlockCFGILP(data, cfg, self.lnts[cfg.name], self.super_block_cfgs[cfg.name])
                self.super_block_cfg_calculations[cfg.name] = CalculationInformation(super_block_cfg_ilp) 
                    
                self.repeat_calculation(cfg, cfg_ilp, super_block_cfg_ilp)
                cfg_ilp.clean()
                super_block_cfg_ilp.clean()
                self.print_results(cfg)               
        finally:
            if config.Arguments.log_to_file:
                log_file.close()
                sys.stdout = old_stdout
                