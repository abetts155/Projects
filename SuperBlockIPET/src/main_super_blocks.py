#!/usr/bin/env python

import argparse
import os
import sys
import threading
import config
import debug
import program_input_output

def the_command_line (): 
    ilp                          = "--ilp"
    clp                          = "--clp"
    region_based                 = "--region-based"
    region_based_super_block_CFG = "--region-based-super-block-CFG"
    all_calculations             = [ilp, clp, region_based, region_based_super_block_CFG]
    
    parser = argparse.ArgumentParser(description="Compute WCET using implicit path enumeration defined on the super block CFG")
    
    parser.add_argument("program_file",
                        help="a file containing program information (with '.txt' extension)")
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        help="debug mode",
                        default=0)
    
    parser.add_argument("--function",
                        help="do a WCET calculation for this function only",
                        default=None)
    
    parser.add_argument("--keep-temps",
                        action="store_true",
                        help="keep any files generated during the analysis",
                        default=False)
    
    parser.add_argument("--log-to-file",
                        help="log output to this file")
    
    parser.add_argument("--randomise-WCET-data",
                        action="store_true",
                        help="randomise data used in WCET calculation, overriding any data supplied in the program file",
                        default=False)
    
    parser.add_argument("--repeat-calculation",
                        type=int,
                        help="repeat the calculation this many times",
                        default=1,
                        metavar="<INT>")
    
    parser.add_argument("--shuffle-constraints",
                        action="store_true",
                        help="before repeating the solve stage of a constraint-based WCET calculation, shuffle the constraints",
                        default=False)
        
    parser.add_argument("-u",
                        "--udraw",
                        action="store_true",
                        help="generate uDraw files to visualise graphs",
                        default=False)
    
    parser.add_argument(clp,
                        action="store_true",
                        help="use constraint logic programming to solve WCET estimation constraint system",
                        default=False)
    
    parser.add_argument(ilp,
                        action="store_true",
                        help="use integer linear programming to solve WCET estimation constraint system",
                        default=False)
    
    parser.add_argument(region_based,
                        action="store_true",
                        help="use region-based calculation on CFG to compute a WCET estimate",
                        default=False)
    
    parser.add_argument(region_based_super_block_CFG,
                        action="store_true",
                        help="use region-based calculation on super block CFG to compute a WCET estimate",
                        default=False)
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    if not (config.Arguments.ilp or 
            config.Arguments.clp or
            config.Arguments.region_based or
            config.Arguments.region_based_super_block_CFG):
        debug.exit_message("You must calculate specify how to calculate a WCET estimate with one or more of the following: %s" %
                           ','.join(all_calculations))
    
    config.Arguments.basename = os.path.splitext(os.path.basename(config.Arguments.program_file))[0]
    config.Arguments.basepath = os.path.abspath(os.path.dirname(config.Arguments.program_file))
        
if __name__ == "__main__": 
    threading.stack_size(67108864) # 64MB stack
    sys.setrecursionlimit(2**20)
    the_command_line()
    data, program = program_input_output.read_file(config.Arguments.program_file)
    program.do_wcet_calculation(data)
    program.print_results()
    
    