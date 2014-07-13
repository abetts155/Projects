#!/usr/bin/env python

import argparse
import os
import sys
import threading
import config
import debug
import program_input_output

def the_command_line (): 
    def clean ():
        for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
            for filename in files:
                if filename.endswith(".udraw") or filename.endswith(".ilp"):
                    full_path = os.path.join(paths, filename)
                    debug.verbose_message("Removing '%s'" % full_path, __name__)
                    os.remove(full_path)
       
    # The command-line parser and its options
    parser = argparse.ArgumentParser(description="Compute WCET using implicit path enumeration defined on the super block CFG")
    
    parser.add_argument("program_file",
                        help="a file containing program information (with '.txt' extension)")
    
    parser.add_argument("-c",
                        "--clean",
                        metavar="",
                        type=clean,
                        help="clean files from previous runs",
                        default=False)
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        help="debug mode",
                        default=0)
    
    parser.add_argument("--fold-wcets-of-super-blocks",
                        action="store_true",
                        help="pre-compute WCETs of super blocks, in effect assuming basic block execution times are constant",
                        default=False)
    
    parser.add_argument("--repeat-calculation",
                        type=int,
                        help="repeat the calculation this many times",
                        default=1,
                        metavar="<INT>")
    
    parser.add_argument("--log-to-file",
                        help="log output to this file")
    
    parser.add_argument("--shuffle-constraints",
                        action="store_true",
                        help="before repeating the solve stage of the WCET calculation, shuffle the constraints",
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
    program = program_input_output.read_file(config.Arguments.program_file)
    program.create_LNTs()
    program.create_super_block_CFGs()
    program.do_wcet_calculation()
    
    