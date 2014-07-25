#!/usr/bin/env python

import os
import argparse
import config
import program_input_output
import programs
import regular_expressions
import re
import ast

def parse_input_from_user(message):
    the_input = raw_input("-> %s: " % message).lower()
    the_input = re.sub(r'\s+', '', the_input)
    if re.match(r'\d+', the_input):
        return ast.literal_eval(the_input)
    return the_input

def do_analysis(program):
    for cfg in program.cfgs.values():
        cfg.create_per_loop_reachability_info()
    
    try:    
        while True:
            cfg_name    = parse_input_from_user("Enter CFG name")
            start       = parse_input_from_user("Enter start program point")
            end         = parse_input_from_user("Enter end program point")
            induced_CFG = programs.create_induced_subgraph(program, cfg_name, start, end)
            regular_expressions.PathExpression(induced_CFG)
    except KeyboardInterrupt:
        pass

def the_command_line (): 
    parser = argparse.ArgumentParser(description="Compute path expressions from a CFG")
    
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
    the_command_line()
    program = program_input_output.read_file(config.Arguments.program_file)
    do_analysis(program)