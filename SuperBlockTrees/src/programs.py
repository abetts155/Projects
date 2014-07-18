import trees
import super_block_graphs
import udraw
import database
import calculations
import config
import sys
import os
import numpy

class SolverInformation:
    def __init__(self, constraint_system):
        self.constraint_system  = constraint_system
        self.solve_times        = []
        
class CalculationInformation:
    def __init__(self, name):
        self.name                                = name
        self.cfg_calculations                    = {}
        self.super_block_cfg_calculations        = {}
        self.super_block_cfg_folded_calculations = {}
    
class Program():
    def __init__(self):
        self.cfgs             = {}
        self.lnts             = {}
        self.super_block_cfgs = {}
        self.ilps             = CalculationInformation("ILP")
        self.clps             = CalculationInformation("CLP")
        
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

    def repeat_calculation(self, cfg, cfg_calculation, super_block_cfg_calculation, super_block_cfg_folded_calculation):
        cfg_times                    = []
        super_block_cfg_times        = []
        super_block_cfg_folded_times = []
        for i in range(1, config.Arguments.repeat_calculation + 1):
            print("===== Repetition %d =====" % i)
            cfg_calculation.solve() 
            cfg_times.append(cfg_calculation.solve_time)  
            print("CFG::                      WCET(%s) = %d" % (cfg.name, cfg_calculation.wcet))
            super_block_cfg_calculation.solve()
            super_block_cfg_times.append(super_block_cfg_calculation.solve_time)  
            print("Super block CFG::          WCET(%s) = %d" % (cfg.name, super_block_cfg_calculation.wcet))
            super_block_cfg_folded_calculation.solve()
            super_block_cfg_folded_times.append(super_block_cfg_folded_calculation.solve_time)
            print("Super block CFG (folded):: WCET(%s) = %d" % (cfg.name, super_block_cfg_folded_calculation.wcet))
            assert cfg_calculation.wcet == super_block_cfg_calculation.wcet, "Disparity in WCETs: (%f, %f)" % (cfg_calculation.wcet, super_block_cfg_calculation.wcet) 
            assert cfg_calculation.wcet == super_block_cfg_folded_calculation.wcet, "Disparity in WCETs: (%f, %f)" % (cfg_calculation.wcet, super_block_cfg_folded_calculation.wcet)
        return cfg_times, super_block_cfg_times, super_block_cfg_folded_times
    
    def do_wcet_calculation(self):
        for cfg in self.cfgs.values():
            lnt    = self.lnts[cfg.name]
            superg = self.super_block_cfgs[cfg.name]
            data   = database.CreateWCETData(cfg, lnt)
            if config.Arguments.use_ilp:
                cfg_ilp_calculation                                     = calculations.CreateCFGILP(data, cfg, lnt)
                self.ilps.cfg_calculations[cfg.name]                    = SolverInformation(cfg_ilp_calculation)
                super_block_cfg_ilp_calculation                         = calculations.CreateSuperBlockCFGILP(data, cfg, lnt, superg)
                self.ilps.super_block_cfg_calculations[cfg.name]        = SolverInformation(super_block_cfg_ilp_calculation)
                super_block_cfg_ilp_folded_calculation                  = calculations.CreateFoldedSuperBlockCFGILP(data, cfg, lnt, superg)
                self.ilps.super_block_cfg_folded_calculations[cfg.name] = SolverInformation(super_block_cfg_ilp_folded_calculation)  
                cfg_times, super_block_cfg_times, super_block_cfg_folded_times = self.repeat_calculation(cfg, 
                                                                                                         self.ilps.cfg_calculations[cfg.name].constraint_system, 
                                                                                                         self.ilps.super_block_cfg_calculations[cfg.name].constraint_system,
                                                                                                         self.ilps.super_block_cfg_folded_calculations[cfg.name].constraint_system) 
                self.ilps.cfg_calculations[cfg.name].solve_times.extend(cfg_times)
                self.ilps.super_block_cfg_calculations[cfg.name].solve_times.extend(super_block_cfg_times)
                self.ilps.super_block_cfg_folded_calculations[cfg.name].solve_times.extend(super_block_cfg_folded_times)
                if not config.Arguments.keep_temps:
                    self.ilps.cfg_calculations[cfg.name].constraint_system.clean()
                    self.ilps.super_block_cfg_calculations[cfg.name].constraint_system.clean()
                    self.ilps.super_block_cfg_folded_calculations[cfg.name].constraint_system.clean()
            if config.Arguments.use_clp:
                cfg_clp_calculation                                     = calculations.CreateCFGCLP(data, cfg, lnt)
                self.clps.cfg_calculations[cfg.name]                    = SolverInformation(cfg_clp_calculation)
                super_block_cfg_clp_calculation                         = calculations.CreateSuperBlockCFGCLP(data, cfg, lnt, superg)
                self.clps.super_block_cfg_calculations[cfg.name]        = SolverInformation(super_block_cfg_clp_calculation)
                super_block_cfg_clp_folded_calculation                  = calculations.CreateFoldedSuperBlockCFGCLP(data, cfg, lnt, superg)
                self.clps.super_block_cfg_folded_calculations[cfg.name] = SolverInformation(super_block_cfg_clp_folded_calculation)
                cfg_times, super_block_cfg_times, super_block_cfg_folded_times = self.repeat_calculation(cfg, 
                                                                                                         self.clps.cfg_calculations[cfg.name].constraint_system, 
                                                                                                         self.clps.super_block_cfg_calculations[cfg.name].constraint_system,
                                                                                                         self.clps.super_block_cfg_folded_calculations[cfg.name].constraint_system)
                
                self.clps.cfg_calculations[cfg.name].solve_times.extend(cfg_times)
                self.clps.super_block_cfg_calculations[cfg.name].solve_times.extend(super_block_cfg_times)
                self.clps.super_block_cfg_folded_calculations[cfg.name].solve_times.extend(super_block_cfg_folded_times)
                if not config.Arguments.keep_temps:
                    self.clps.cfg_calculations[cfg.name].constraint_system.clean()
                    self.clps.super_block_cfg_calculations[cfg.name].constraint_system.clean()
                    self.clps.super_block_cfg_folded_calculations[cfg.name].constraint_system.clean()               
     
                
    def print_results_of_individual_cfg(self, cfg, calculation_information):                 
        print("""
***** %s: CFG *****
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(calculation_information.name,
cfg.name, 
len(calculation_information.cfg_calculations[cfg.name].constraint_system.the_variables),
len(calculation_information.cfg_calculations[cfg.name].constraint_system.the_constraints),
numpy.amin(calculation_information.cfg_calculations[cfg.name].solve_times),
numpy.amax(calculation_information.cfg_calculations[cfg.name].solve_times), 
numpy.average(calculation_information.cfg_calculations[cfg.name].solve_times)))

        print("""
***** %s: Super block CFG *****
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(calculation_information.name,
cfg.name, 
len(calculation_information.super_block_cfg_calculations[cfg.name].constraint_system.the_variables),
len(calculation_information.super_block_cfg_calculations[cfg.name].constraint_system.the_constraints),
numpy.amin(calculation_information.super_block_cfg_calculations[cfg.name].solve_times),
numpy.amax(calculation_information.super_block_cfg_calculations[cfg.name].solve_times), 
numpy.average(calculation_information.super_block_cfg_calculations[cfg.name].solve_times)))
        
        print("""
***** %s: Super block CFG with folding *****
Function     = %s 
variables    = %d
constraints  = %d
min time     = %f
max time     = %f
average time = %f""" % \
(calculation_information.name,
cfg.name, 
len(calculation_information.super_block_cfg_folded_calculations[cfg.name].constraint_system.the_variables),
len(calculation_information.super_block_cfg_folded_calculations[cfg.name].constraint_system.the_constraints),
numpy.amin(calculation_information.super_block_cfg_folded_calculations[cfg.name].solve_times),
numpy.amax(calculation_information.super_block_cfg_folded_calculations[cfg.name].solve_times), 
numpy.average(calculation_information.super_block_cfg_folded_calculations[cfg.name].solve_times))) 
       
    def print_results(self):
        if config.Arguments.log_to_file:
            filename   = os.path.abspath(config.Arguments.log_to_file)
            log_file   = open(filename, 'w')
            old_stdout = sys.stdout
            sys.stdout = log_file
        try:
            for cfg in self.cfgs.values():
                if config.Arguments.use_ilp:
                    self.print_results_of_individual_cfg(cfg, self.ilps)
                if config.Arguments.use_clp:
                    self.print_results_of_individual_cfg(cfg, self.clps)
        finally:
            if config.Arguments.log_to_file:
                log_file.close()
                sys.stdout = old_stdout
        
                