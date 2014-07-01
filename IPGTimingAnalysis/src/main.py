#!/usr/bin/env python

import os
import argparse
import config
import debug
import parse_program_file

def analyse():
    program = parse_program_file.parse_file()
    program.create_lnts()
    program.create_ipgs()
    program.create_mini_ipgs()
        
def the_command_line():
    def clean():
        for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
            files.sort()
            for filename in files:
                if filename.endswith('.udraw') or filename.endswith('.ilp'):
                    full_path = os.path.join(paths, filename)
                    debug.verbose_message("Removing '%s'" % full_path, __name__)
                    os.remove(full_path)
    
    parser = argparse.ArgumentParser(description="Calculate WCET estimates using the instrumentation point graph and implicit path enumeration")
    
    parser.add_argument(dest="program_file",
                        metavar="<FILE>",
                        help="the file detailing the structure of the program and the positions of instrumentation points")
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        metavar="<INT>",
                        help="debug mode",
                        default=0)
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.add_argument("-c",
                        "--clean",
                        metavar="",
                        type=clean,
                        help="clean files from previous runs",
                        default=False)
    
    parser.add_argument("-u",
                      "--udraw",
                      action="store_true",
                      help="generate uDrawGraph files",
                      default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    setattr(config.Arguments, "basename", os.path.splitext(os.path.basename(config.Arguments.program_file))[0])
    setattr(config.Arguments, "basepath", os.path.abspath(os.path.dirname(config.Arguments.program_file)))
                
if __name__ == "__main__":
    the_command_line()
    analyse()
