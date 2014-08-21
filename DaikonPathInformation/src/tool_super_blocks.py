#!/usr/bin/env python

from __future__ import print_function

import program_input_output
import directed_graphs
import debug
import traces
import calculations
import config
import argparse
import os

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

    parser.add_argument("-t",
                        "--traces",
                        type=int,
                        help="generate dummy traces for the given program",
                        default=0,
                        metavar="<INT>")
    
    parser.add_argument("-T",
                        dest="tracefile",
                        help="parse this trace file",
                        metavar="<FILE>")
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    setattr(config.Arguments, "basename", os.path.splitext(os.path.basename(config.Arguments.program_file))[0])
    setattr(config.Arguments, "basepath", os.path.abspath(os.path.dirname(config.Arguments.program_file)))
    
    assert config.Arguments.program.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
        
if __name__ == "__main__":
    the_command_line()
    program  = program_input_output.read_file(config.Arguments.program)
    totalSuperBlockProgramPoints = 0
    totalNaiveProgramPoints      = 0
    for cfg in program.getcfgs():
        functionName = cfg.getName()
        lnt = directed_graphs.LoopNests(cfg, cfg.getEntryID())
        program.addLNT(lnt, functionName)
        pathg = program.getPathInfoGraph(functionName)
        totalSuperBlockProgramPoints += len(pathg.getMonitoredProgramPoints())
        totalNaiveProgramPoints += min(cfg.numOfvertices(), cfg.numOfedges())
    print(config.Arguments.program, "[Super block program points = %d" % totalSuperBlockProgramPoints, "CFG program points = %d]" % totalNaiveProgramPoints)
    program.generateAllUDrawFiles()
    if config.Arguments.traces:
        debug.verbose_message("Generating dummy traces", __name__)
        traces.Generatetraces(program, config.Arguments.traces)
    elif config.Arguments.tracefile:
        config.Arguments.tracefile = os.path.abspath(config.Arguments.tracefile)
        assert os.path.exists(config.Arguments.tracefile), "Trace file '%s' does not exist" % config.Arguments.tracefile
        assert os.path.getmtime(config.Arguments.program) <= os.path.getmtime(config.Arguments.tracefile), "Program file modified AFTER trace file generation"
        data = traces.Parsetraces(config.Arguments.tracefile, program)
        program.output()
        program.generateAllUDrawFiles()
        calculations.WCETCalculation(program, data)
        
        
