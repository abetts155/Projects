#!/usr/bin/env python

import os
import argparse
import config
import program_input_output
import regular_expressions
import debug
import re
import ast

def create_path_expressions_for_all_loops(program):
    for cfg in program.cfgs.values():
        regular_expressions.create_path_expression_for_all_loops(cfg)

def create_path_expression_between_two_vertices(program):
    prompt_prefix = "->"
    
    def parse_input_from_user(message):
        the_input = raw_input("%s %s: " % (prompt_prefix, message)).lower()
        the_input = re.sub(r'\s+', '', the_input)
        if re.match(r'\d+', the_input):
            return ast.literal_eval(the_input)
        return the_input
    
    try:    
        while True:
            cfg = None
            if len(program.cfgs) == 1:
                cfg = program.cfgs.itervalues().next()
            else:
                cfg_name = parse_input_from_user("Enter CFG name")
                if cfg_name not in program.cfgs:
                    debug.warning_message("Program does not have CFG %s" % cfg_name)
                else:
                    cfg = program.cfgs[cfg_name]
            if cfg is not None:
                print("%s Vertices = {%s}" % (prompt_prefix, ','.join(str(v.vertexID) for v in cfg)))
                startID = parse_input_from_user("Enter start vertex")
                if not cfg.hasVertex(startID):
                    debug.warning_message("CFG does not have vertex: %d" % startID)
                else:
                    endID = parse_input_from_user("Enter end vertex")
                    if not cfg.hasVertex(endID):
                        debug.warning_message("CFG does not have vertex: %d" % endID)
                    else:
                        regular_expressions.create_path_expression(cfg, startID, endID)
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
    
    parser.add_argument("-l",
                        "--loops",
                        action="store_true",
                        help="generate path expressions for loop bodies only",
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
    
    config.Arguments.basename = os.path.splitext(os.path.basename(config.Arguments.program_file))[0]
    config.Arguments.basepath = os.path.abspath(os.path.dirname(config.Arguments.program_file))

if __name__ == "__main__": 
    the_command_line()
    program = program_input_output.read_file(config.Arguments.program_file)
    if config.Arguments.loops:
        create_path_expressions_for_all_loops(program)
    else:
        create_path_expression_between_two_vertices(program)
    