from __future__ import print_function

import directed_graphs
import vertices
import debug
import calculations
import config
import sys
import os
import numpy

class SolverInformation:
    def __init__(self, constraint_system):
        self.constraint_system  = constraint_system
        self.solve_times        = []
        
class ConstraintBasedCalculationInformation:
    def __init__(self, name):
        self.name                                = name
        self.cfg_calculations                    = {}
        self.super_block_cfg_calculations        = {}
        self.super_block_cfg_folded_calculations = {}
    
class Program:
    def __init__(self):
        self.callg = directed_graphs.CallGraph()
        self.cfgs  = {}
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg
        callv = vertices.CallGraphVertex(self.callg.get_next_vertexID(), cfg.name)
        self.callg.addVertex(callv)

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
            assert cfg_calculation.wcet == super_block_cfg_calculation.wcet, "Disparity in WCETs for %s: (%f, %f)" % (cfg.name, cfg_calculation.wcet, super_block_cfg_calculation.wcet) 
            assert cfg_calculation.wcet == super_block_cfg_folded_calculation.wcet, "Disparity in WCETs for %s: (%f, %f)" % (cfg.name, cfg_calculation.wcet, super_block_cfg_folded_calculation.wcet)
        return cfg_times, super_block_cfg_times, super_block_cfg_folded_times
    
    def do_CLP_calculation(self, data):
        self.clps = ConstraintBasedCalculationInformation("CLP")
        for cfg in self.cfgs.values():
            if config.Arguments.function is None or config.Arguments.function == cfg.name:
                function_data = data.function_data[cfg.name]
                
                cfg_clp_calculation                                     = calculations.CreateCFGCLP(function_data, cfg, cfg.get_LNT())
                self.clps.cfg_calculations[cfg.name]                    = SolverInformation(cfg_clp_calculation)
                super_block_cfg_clp_calculation                         = calculations.CreateSuperBlockCFGCLP(function_data, cfg, cfg.get_LNT(), cfg.get_super_block_cfg())
                self.clps.super_block_cfg_calculations[cfg.name]        = SolverInformation(super_block_cfg_clp_calculation)
                super_block_cfg_clp_folded_calculation                  = calculations.CreateFoldedSuperBlockCFGCLP(function_data, cfg, cfg.get_LNT(), cfg.get_super_block_cfg())
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
    
    def do_ILP_calculation(self, data):
        self.ilps = ConstraintBasedCalculationInformation("ILP")
        for cfg in self.cfgs.values():
            if config.Arguments.function is None or config.Arguments.function == cfg.name:
                function_data = data.function_data[cfg.name]
                cfg_ilp_calculation                                     = calculations.CreateCFGILP(function_data, cfg, cfg.get_LNT())
                self.ilps.cfg_calculations[cfg.name]                    = SolverInformation(cfg_ilp_calculation)
                super_block_cfg_ilp_calculation                         = calculations.CreateSuperBlockCFGILP(function_data, cfg, cfg.get_LNT(), cfg.get_super_block_cfg())
                self.ilps.super_block_cfg_calculations[cfg.name]        = SolverInformation(super_block_cfg_ilp_calculation)
                super_block_cfg_ilp_folded_calculation                  = calculations.CreateFoldedSuperBlockCFGILP(function_data, cfg, cfg.get_LNT(), cfg.get_super_block_cfg())
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
    
    def do_region_based_calculation(self, data):        
        self.region_based_calculations                 = {}
        self.region_based_calculations_super_block_CFG = {}
        for cfg in self.cfgs.values(): 
            if config.Arguments.function is None or config.Arguments.function == cfg.name:
                function_data = data.function_data[cfg.name]
                if config.Arguments.region_based:
                    self.region_based_calculations[cfg.name] = []
                if config.Arguments.region_based_super_block_CFG:
                    self.region_based_calculations_super_block_CFG[cfg.name] = []
                for i in range(1, config.Arguments.repeat_calculation + 1):
                    print("===== Repetition %d =====" % i)
                    if config.Arguments.region_based:
                        region_based_calculation = calculations.RegionalCalculationEnhancedCFG(function_data, cfg.get_LNT(), cfg.get_super_block_cfg())
                        self.region_based_calculations[cfg.name].append(region_based_calculation)
                    if config.Arguments.region_based_super_block_CFG:
                        region_based_calculation = calculations.RegionalCalculationSuperBlockCFG(function_data, cfg.get_LNT(), cfg.get_super_block_cfg())
                        self.region_based_calculations_super_block_CFG[cfg.name].append(region_based_calculation)
                    print("Region::                   WCET(%s) = %d" % (cfg.name, region_based_calculation.wcet))
                    if config.Arguments.ilp:
                        cfg_calculation = self.ilps.cfg_calculations[cfg.name].constraint_system
                        if cfg_calculation.wcet != region_based_calculation.wcet:
                            debug.warning_message("Disparity in WCETs for %s: (%f, %f)" % (cfg.name, 
                                                                                           cfg_calculation.wcet, 
                                                                                           region_based_calculation.wcet))
    def do_wcet_calculation(self, data):
        if config.Arguments.randomise_WCET_data:
            for cfg in self.cfgs.values():
                if config.Arguments.function is None or config.Arguments.function == cfg.name:
                    function_data = data.function_data[cfg.name]
                    function_data.assign_wcets_to_basic_blocks(cfg)
                    function_data.assign_loop_bounds(self.lnts[cfg.name])
        if config.Arguments.ilp:
            self.do_ILP_calculation(data)
        if config.Arguments.clp:
            self.do_CLP_calculation(data)
        if config.Arguments.region_based:
            self.do_region_based_calculation(data)
        if config.Arguments.region_based_super_block_CFG:
            self.do_region_based_calculation(data)
    
    def print_results_of_individual_region_based_calculation(self, cfg, calculation_information):
        solve_times = [calculation.solve_time for calculation in calculation_information[cfg.name]]
        print("""
Region-based
Function     = %s 
min time     = %f
max time     = %f
average time = %f""" % \
(cfg.name, 
numpy.amin(solve_times),
numpy.amax(solve_times), 
numpy.average(solve_times)))
                
    def print_results_of_individual_cfg(self, cfg, calculation_information):                 
        print("""
%s: CFG
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
%s: Super block CFG
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
%s: Super block CFG with folding
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
                if config.Arguments.function is None or config.Arguments.function == cfg.name:
                    if config.Arguments.ilp:
                        self.print_results_of_individual_cfg(cfg, self.ilps)
                    if config.Arguments.clp:
                        self.print_results_of_individual_cfg(cfg, self.clps)
                    if config.Arguments.region_based:
                        self.print_results_of_individual_region_based_calculation(cfg, self.region_based_calculations)
                    if config.Arguments.region_based_super_block_CFG:
                        self.print_results_of_individual_region_based_calculation(cfg, self.region_based_calculations_super_block_CFG)
        finally:
            if config.Arguments.log_to_file:
                log_file.close()
                sys.stdout = old_stdout
        
                