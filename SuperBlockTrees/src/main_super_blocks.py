#!/usr/bin/env python

import argparse
import os
import sys
import threading
import config
import program_input_output

def the_command_line (): 
    parser = argparse.ArgumentParser(description="Compute WCET using implicit path enumeration defined on the super block CFG")
    
    parser.add_argument("program_file",
                        help="a file containing program information (with '.txt' extension)")
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        help="debug mode",
                        default=0)
    
    parser.add_argument("--log-to-file",
                        help="log output to this file")
    
    parser.add_argument("--repeat-calculation",
                        type=int,
                        help="repeat the calculation this many times",
                        default=1,
                        metavar="<INT>")
    
    parser.add_argument("--randomise-WCET-data",
                        action="store_true",
                        help="randomise data used in WCET calculation, overriding any data supplied in the program file",
                        default=False)
        
    parser.add_argument("-u",
                        "--udraw",
                        action="store_true",
                        help="generate uDraw files to visualise graphs",
                        default=False)
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    setattr(config.Arguments, "basename", os.path.splitext(os.path.basename(config.Arguments.program_file))[0])
    setattr(config.Arguments, "basepath", os.path.abspath(os.path.dirname(config.Arguments.program_file)))
        
if __name__ == "__main__": 
    threading.stack_size(67108864) # 64MB stack
    sys.setrecursionlimit(2 ** 20)
    the_command_line()
    data, program = program_input_output.read_file(config.Arguments.program_file)
    program.create_LNTs()
    program.create_super_block_CFGs()
    program.do_wcet_calculation(data)   
    program.print_results()
    
    