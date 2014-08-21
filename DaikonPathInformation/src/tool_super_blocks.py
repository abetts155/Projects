#!/usr/bin/env python

from __future__ import print_function

import program_input_output
import debug
import traces
import calculations
import config
import argparse
import os

def count_program_points(program):
    superg_program_points = 0
    cfg_program_points    = 0
    for cfg in program.cfgs.values():
        pathg = cfg.get_super_block_cfg()
        superg_program_points += len(pathg.getMonitoredProgramPoints())
        cfg_program_points    += min(cfg.number_of_vertices(), cfg.number_of_edges())
    print(config.Arguments.program, "[Super block program points = %d" % superg_program_points, "CFG program points = %d]" % cfg_program_points)
    return program

def the_command_line():
    parser = argparse.ArgumentParser(description="Compute super block cfgs")
    
    parser.add_argument("program_file",
                        help="a file containing program information (with '.txt' extension)")
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        help="debug mode",
                        default=0)
    
    parser.add_argument("-u",
                        "--udraw",
                        action="store_true",
                        help="generate uDrawGraph files",
                        default=False)

    parser.add_argument("--generate-traces",
                        type=int,
                        help="generate traces for the given program",
                        default=0,
                        metavar="<INT>")
    
    parser.add_argument("--parse-trace",
                        help="parse this trace file",
                        metavar="<FILE>")
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    config.Arguments.basename = os.path.splitext(os.path.basename(config.Arguments.program_file))[0]
    config.Arguments.basepath = os.path.abspath(os.path.dirname(config.Arguments.program_file))
    
    config.Arguments.program_file = os.path.abspath(config.Arguments.program_file)
    if not config.Arguments.program_file.endswith(".txt"):
        debug.exit_message("Please pass a program file with a '%s' suffix" % ".txt")
        
    if config.Arguments.parse_trace is not None:
        config.Arguments.parse_trace = os.path.abspath(config.Arguments.parse_trace)
        assert os.path.exists(config.Arguments.parse_trace), "Trace file '%s' does not exist" % config.Arguments.parse_trace
        assert os.path.getmtime(config.Arguments.program_file) <= os.path.getmtime(config.Arguments.parse_trace), "Program file modified AFTER trace file generation"
        
if __name__ == "__main__":
    the_command_line()
    program = program_input_output.read_file(config.Arguments.program_file)
    count_program_points(program)
    if config.Arguments.generate_traces > 0:
        traces.Generatetraces(program, config.Arguments.traces)
    elif config.Arguments.parse_trace:
        data = traces.Parsetraces(config.Arguments.tracefile, program)
        program.output()
        calculations.WCETCalculation(program, data)
