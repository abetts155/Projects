from __future__ import print_function

import trees
import super_block_graphs
import udraw
import calculations
import config
import sys
import os
import numpy
    
class Program():
    def __init__(self):
        self.cfgs                         = {}
        self.lnts                         = {}
        self.super_block_cfgs             = {}
        self.super_block_cfg_calculations = {}
        
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
    
    def do_wcet_calculation(self, data):
        for cfg in self.cfgs.values():
            if config.Arguments.randomise_WCET_data:
                data.assign_wcets_to_basic_blocks(cfg)
                data.assign_loop_bounds(self.lnts[cfg.name])
            superg = self.super_block_cfgs[cfg.name]
            self.super_block_cfg_calculations[cfg.name] = set()
            for i in range(1, config.Arguments.repeat_calculation + 1):
                print("===== Repetition %d =====" % i)
                tree_based_calculation = calculations.TreeBasedCalculation(data, superg)
                self.super_block_cfg_calculations[cfg.name].add(tree_based_calculation)
                print("Super block CFG:: WCET(%s) = %d" % (cfg.name, tree_based_calculation.wcet))
                
    def print_results_of_individual_cfg(self, cfg, tree_calculations):
        solve_times = [calculation.solve_time for calculation in tree_calculations]
        print("""
***** Super block CFG *****
Function     = %s 
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
numpy.amin(solve_times),
numpy.amax(solve_times), 
numpy.average(solve_times)))

    def print_results(self):
        if config.Arguments.log_to_file:
            filename   = os.path.abspath(config.Arguments.log_to_file)
            log_file   = open(filename, 'w')
            old_stdout = sys.stdout
            sys.stdout = log_file
        try:
            for cfg in self.cfgs.values():
                self.print_results_of_individual_cfg(cfg, self.super_block_cfg_calculations[cfg.name])
        finally:
            if config.Arguments.log_to_file:
                log_file.close()
                sys.stdout = old_stdout
        
                