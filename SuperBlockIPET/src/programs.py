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
*** CFG data ***
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
len(self.cfg_ilp_calculations[cfg.name].ilp.the_variables),
len(self.cfg_ilp_calculations[cfg.name].ilp.the_constraints),
numpy.amin(self.cfg_ilp_calculations[cfg.name].solve_times),
numpy.amax(self.cfg_ilp_calculations[cfg.name].solve_times), 
numpy.average(self.cfg_ilp_calculations[cfg.name].solve_times)))

        print("""
*** Super block CFG data ***
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
len(self.super_block_cfg_ilp_calculations[cfg.name].ilp.the_variables),
len(self.super_block_cfg_ilp_calculations[cfg.name].ilp.the_constraints),
numpy.amin(self.super_block_cfg_ilp_calculations[cfg.name].solve_times),
numpy.amax(self.super_block_cfg_ilp_calculations[cfg.name].solve_times), 
numpy.average(self.super_block_cfg_ilp_calculations[cfg.name].solve_times)))
        
        print("""
*** Super block CFG (folded) data ***
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
len(self.super_block_cfg_folded_ilp_calculations[cfg.name].ilp.the_variables),
len(self.super_block_cfg_folded_ilp_calculations[cfg.name].ilp.the_constraints),
numpy.amin(self.super_block_cfg_folded_ilp_calculations[cfg.name].solve_times),
numpy.amax(self.super_block_cfg_folded_ilp_calculations[cfg.name].solve_times), 
numpy.average(self.super_block_cfg_folded_ilp_calculations[cfg.name].solve_times)))

    def repeat_calculation(self, cfg, cfg_calculation, super_block_cfg_calculation, super_block_cfg_folded_calculation):
        for i in range(1, config.Arguments.repeat_calculation + 1):
            print("===== Repetition %d =====" % i)
            cfg_calculation.solve() 
            self.cfg_ilp_calculations[cfg.name].solve_times.append(cfg_calculation.solve_time)    
            print("CFG::                      WCET(%s) = %d" % (cfg.name, cfg_calculation.wcet))
            super_block_cfg_calculation.solve()
            self.super_block_cfg_ilp_calculations[cfg.name].solve_times.append(super_block_cfg_calculation.solve_time)    
            print("Super block CFG::          WCET(%s) = %d" % (cfg.name, super_block_cfg_calculation.wcet))
            assert cfg_calculation.wcet == super_block_cfg_calculation.wcet, "Disparity in WCETs: (%f, %f)" % (cfg_calculation.wcet, super_block_cfg_calculation.wcet) 
            super_block_cfg_folded_calculation.solve()
            self.super_block_cfg_folded_ilp_calculations[cfg.name].solve_times.append(super_block_cfg_folded_calculation.solve_time)
            print("Super block CFG (folded):: WCET(%s) = %d" % (cfg.name, super_block_cfg_folded_calculation.wcet))
            assert cfg_calculation.wcet == super_block_cfg_folded_calculation.wcet, "Disparity in WCETs: (%f, %f)" % (cfg_calculation.wcet, super_block_cfg_folded_calculation.wcet)

    def do_wcet_calculation(self):
        self.cfg_ilp_calculations                    = {}
        self.super_block_cfg_ilp_calculations        = {}
        self.super_block_cfg_folded_ilp_calculations = {}
        if config.Arguments.log_to_file:
            filename   = os.path.abspath(config.Arguments.log_to_file)
            log_file   = open(filename, 'w')
            old_stdout = sys.stdout
            sys.stdout = log_file
        try:
            for cfg in self.cfgs.values():
                lnt                                                    = self.lnts[cfg.name]
                data                                                   = database.CreateWCETData(cfg, lnt)
                if config.Arguments.use_clp:
                    cfg_clp_calculation = calculations.CreateCFGCLP(data, cfg, lnt)
                    cfg_clp_calculation.solve()
                cfg_calculation                                        = calculations.CreateCFGILP(data, cfg, lnt)
                self.cfg_ilp_calculations[cfg.name]                    = CalculationInformation(cfg_calculation)
                super_block_cfg_calculation                            = calculations.CreateSuperBlockCFGILP(data, cfg, lnt, self.super_block_cfgs[cfg.name])
                self.super_block_cfg_ilp_calculations[cfg.name]        = CalculationInformation(super_block_cfg_calculation) 
                super_block_cfg_folded_calculation                     = calculations.CreateCompressedSuperBlockCFGILP(data, cfg, lnt, self.super_block_cfgs[cfg.name])
                self.super_block_cfg_folded_ilp_calculations[cfg.name] = CalculationInformation(super_block_cfg_folded_calculation)   
                self.repeat_calculation(cfg, cfg_calculation, super_block_cfg_calculation, super_block_cfg_folded_calculation)
                cfg_calculation.clean()
                super_block_cfg_calculation.clean()
                super_block_cfg_folded_calculation.clean()
                self.print_results(cfg)               
        finally:
            if config.Arguments.log_to_file:
                log_file.close()
                sys.stdout = old_stdout
                