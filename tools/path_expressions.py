#!/usr/bin/env python

import os
import argparse
import ast

from tools.lib.utils import config
from tools.lib.utils import debug
from tools.lib.system import program



def create_path_expression_between_two_program_points(the_program):
    prompt_prefix = "->"
    
    def parse_string_input_from_user(message):
        the_input = input("%s %s: " % (prompt_prefix, message)).lower()
        return ''.join(the_input)
    
    def parse_integer_input_from_user(message):
        the_input = input("%s %s: " % (prompt_prefix, message)).lower()
        return ast.literal_eval(''.join(the_input))
    
    try:    
        while True:
            print("%s Functions = {%s}" % 
                  (prompt_prefix, 
                   ','.join(control_flow_graph.name
                            for control_flow_graph in 
                            the_program.control_flow_graph_iterator())))
            chosen_name = parse_string_input_from_user("Enter function name")
            if not the_program.has_function(chosen_name):
                debug.warning_message(" The program does not a function called '%s'" 
                                      % chosen_name)
            else:
                control_flow_graph = the_program.get_control_flow_graph(chosen_name)
                print("%s Vertices = %s" % 
                      (prompt_prefix, 
                       ' '.join(str(basic_block.vertex_id) 
                                for basic_block in control_flow_graph)))
                
                try:
                    start_vertex = control_flow_graph.get_vertex\
                        (parse_integer_input_from_user("Enter start vertex"))
                    end_vertex   = control_flow_graph.get_vertex\
                        (parse_integer_input_from_user("Enter end vertex"))
                    the_query = ((start_vertex,), (end_vertex,))
                except KeyError:
                    debug.warning_message("The control flow graph" 
                                          " does not have that vertex")
    except KeyboardInterrupt:
        pass


def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     'Compute path expressions from a CFG')
    
    parser.add_argument('program_file',
                        help='a file containing program information'
                        ' (with .txt extension)')
    
    parser.add_argument('-d',
                        '--debug',
                        type=int,
                        help='debug mode',
                        default=0)
    
    parser.add_argument('--dot',
                        action='store_true',
                        help='produce Graphviz dot files of graphs produced'
                        ' during the analysis',
                        default=False)

    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='be verbose',
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    config.set_filename_prefix()
    config.purge_png_files()


if __name__ == "__main__": 
    parse_the_command_line()
    the_program = program.read_program_information_from_file(config.Arguments.program_file)
    #create_path_expression_between_two_program_points(the_program)
    