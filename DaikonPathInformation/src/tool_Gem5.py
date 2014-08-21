#!/usr/bin/env python

import config
import debug
import timing
import calculations
import traces
import testing
import arm
import program_input_output
import argparse
import subprocess
import sys
import os
import re
import distutils.spawn

def do_analysis(program):   
    time1 = timing.log("TRACE PARSING RUN #1 (NO INLINING)")
    data = traces.Gem5Parser(program, config.Arguments.gem5_traces)
    debug.verbose_message("HWMT = %d" % data.getLongestTime(), __name__)   
    calculations.WCETCalculation(program, data)
    program.output()
    program.generateAllUDrawFiles()

    if program.getCallGraph().numOfvertices() > 1 and config.Arguments.inline:
        program.inlineCalls()
        time2 = timing.log("TRACE PARSING RUN #2 (INLINED PROGRAM)")
        data = traces.Gem5Parser(program, config.Arguments.gem5_traces)
        debug.verbose_message("HWMT = %d" % data.getLongestTime(), __name__)
        calculations.WCETCalculation(program, data)
        program.output()
        program.generateAllUDrawFiles("inlined")
        
def set_gem5_variables():
    # Need a gem5 environment variable 
    gem5_home = "GEM5_HOME"
    try:
        config.Arguments.gem5_basepath = os.environ[gem5_home]
        if not os.path.exists(os.path.abspath(config.Arguments.gem5_basepath)):
            debug.exit_message("Your gem5 base directory '%s' does not exist" % config.Arguments.gem5_basepath)
        config.Arguments.gem5_simulator = config.Arguments.gem5_basepath + os.sep + "build" + os.sep + "ARM" + os.sep + "gem5.opt"
        if not os.path.exists(config.Arguments.gem5_simulator):
            debug.exit_message(
"""Unable to find '%s' in your gem5 distribution, which is the optimised arm 
configuration of gem5. Ensure that you have built this version using 
'scons arm/build/gem5.opt' in '%s'""" \
% (config.Arguments.gem5_simulator, config.Arguments.gem5_basepath))
        config.Arguments.gem5_config = config.Arguments.gem5_basepath + os.sep + "configs" + os.sep + "example" + os.sep + "se.py"
        if not os.path.exists(config.Arguments.gem5_config):
            debug.exit_message("The gem5 configuration file '%s' does not exist" % config.Arguments.gem5_config)
    except KeyError:
        debug.exit_message ("You need to set environment variable '%s' to simulate the program using gem5" % gem5_home)
        
def check_trace_files():
    files = []
    for trace_file in config.Arguments.gem5_traces:
        trace_file = os.path.abspath(trace_file)
        if not re.match(r'.*%s\.trace\.[0-9]+' % config.Arguments.basename, trace_file):
            debug.exitMessage("The file '%s' is not a valid gem5 trace for '%s'" % (trace_file, config.Arguments.basename)) 
        if not os.path.isfile(trace_file):
            debug.exitMessage("The argument '%s' is not a valid file" % trace_file) 
        files.append(trace_file)
    config.Arguments.gem5_traces = files

def disassemble_program(binary):
    debug.verbose_message("Disassembling program", __name__)
    disassembly_filename = binary + ".dis"
    with open(disassembly_filename, 'w') as disassembly:
        cmd        = "%s %s -d" % (config.Arguments.objdump, binary)
        proc       = subprocess.Popen(cmd, 
                                      shell=True, 
                                      stdout=disassembly, 
                                      stderr=sys.stderr)    
        returncode = proc.wait()
        if returncode:
            debug.exit_message("Disassembling '%s' failed" % binary) 
    return disassembly_filename
        
def compile_program(program):
    debug.verbose_message("Compiling program", __name__)
    optimisation = ""
    extraFlags   = ""
    if config.Arguments.flags:
        for flag in config.Arguments.flags:
            extraFlags += "-%s " % flag
            if re.match(r'O[0-3]+', flag):
                optimisation = flag
    binary     = program[:-2] + optimisation
    cmd        = "%s -fno-stack-protector -static %s %s -o %s" % (config.Arguments.GCC, extraFlags, program, binary)
    debug.debug_message("Compiling with command '%s'" % cmd, 1)
    proc       = subprocess.Popen(cmd, 
                                  shell=True, 
                                  stdout=sys.stdout, 
                                  stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        debug.exit_message("Compiling '%s' with '%s' failed" % (program, cmd))
    return binary
        
def get_binary_and_program():
    file_ext = os.path.splitext(config.Arguments.program_file)[1]
    if file_ext:
        if file_ext == '.c':
            if not distutils.spawn.find_executable(config.Arguments.GCC):
                debug.exit_message("Unable to find arm GCC cross compiler '%s' on your path" % config.Arguments.GCC)
            if not distutils.spawn.find_executable(config.Arguments.objdump):
                debug.exit_message("Unable to find arm GCC object dump facility '%s' on your path" % config.Arguments.objdump)
            if not config.Arguments.root:
                debug.exit_message("To compile and analyse a C file, you must supply a root function via -r.")
            binary      = compile_program(config.Arguments.program_file)
            disassembly = disassemble_program(binary)
            program     = arm.Disassembler(disassembly, config.Arguments.root).program
            program_input_output.write_disassembled_program_to_file(program, disassembly)
            return binary, program
        else:
            debug.exit_message("Unable to compile '%s' because its extension is not '%s'" % (config.Arguments.program_file, '.c'))
    else:
        binary                        = config.Arguments.program_file
        config.Arguments.program_file = binary + ".txt"
        if not os.access(binary, os.X_OK):
            debug.exit_message("The argument '%s' does not have execute permissions" % binary)
        if not os.path.exists(config.Arguments.program_file):
            debug.exit_message("Expected to find file with program information in '%s' but it is not there" % config.Arguments.program_file)
        program = program_input_output.read_file(config.Arguments.program_file)
        return binary, program

def the_command_line():
    def comma_separated_list(the_list):
        try:
            return the_list.split(',')
        except:
            raise argparse.ArgumentTypeError("Invalid compiler flags")
    
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter,
                                      description="Run C programs on gem5 and analyse traces")
    
    parser.add_argument("program_file",
                        help="either a program to compile (with '.c.' extension) or a pre-compiled binary")
    
    parser.add_argument("gem5_traces", 
                        nargs='*',
                        help="previous gem5 runs")
    
    parser.add_argument("-C",
                        "--compile",
                        action="store_true",
                        help="only compile program",
                        default=False)

    parser.add_argument("--compiler-flags",
                        type=comma_separated_list,
                        help="flags to be passed to the compiler",
                        dest="flags",
                        metavar="<FLAGS>")
    
    parser.add_argument("-d",
                         "--debug",
                         action="store",
                         type=int,
                         help="debug mode",
                         metavar="<INT>",
                         default=0)
    
    parser.add_argument("--exclusive-size",
                         action="store",
                         type=int,
                         help="size of subsets of mutually exclusive basic blocks to compute",
                         metavar="<INT>")
    
    parser.add_argument("-G",
                        "--ga",
                        action="store_true",
                        help="use a genetic algorithm to generate test vectors",
                        default=False)
    
    parser.add_argument("-I",
                        "--inline",
                        action="store_true",
                        help="do analysis with fully inlined program where applicable",
                        default=False)
    
    parser.add_argument("-r",
                         "--root",
                         action="store",
                         help="the function that is the entry point of the analysis. [This should not be 'main']",
                         metavar="<FUNCTION>")
    
    parser.add_argument("-T",
                         "--number-of-tests",
                         action="store",
                         type=int,
                         dest="tests",
                         help="the number of times to run the application",
                         metavar="<INT>",
                         default=1)
    
    parser.add_argument("-u",
                        "--udraw",
                        action="store_true",
                        help="generate uDrawGraph files",
                        default=False)
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    config.Arguments.basename = os.path.splitext(os.path.basename(config.Arguments.program_file))[0]
    config.Arguments.basepath = os.path.abspath(os.path.dirname(config.Arguments.program_file))
    
    config.Arguments.program_file = os.path.abspath(config.Arguments.program_file)
    if not os.path.exists(config.Arguments.program_file):
        debug.exit_message("The first command-line argument must be a file: '%s' does not exist." % config.Arguments.program_file)
    elif not os.path.isfile(config.Arguments.program_file):
        debug.exit_message("The first command-line argument must be a file: '%s' is not a file" % config.Arguments.program_file)
        
    config.Arguments.test_specification_file = os.path.splitext(config.Arguments.program_file)[0] + '.test'
    if not os.path.exists(config.Arguments.test_specification_file ):
        debug.exit_message("Expected to find the test specification file '%s' but it is not there" % config.Arguments.test_specification_file)

if __name__ == "__main__":   
    the_command_line()
    debug.verbose_message("%s Analysing program '%s' %s" % ('*' * 10, config.Arguments.program_file, '*' * 10), __name__)
    time1 = timing.log("COMPILING BEGIN")
    binary, program = get_binary_and_program()
    time2 = timing.log("COMPILING END")
    if config.Arguments.compile:
        debug.exit_message("DONE")
    if config.Arguments.gem5_traces:
        check_trace_files()
    else:
        set_gem5_variables()
        if config.Arguments.ga:
            debug.verbose_message("Using GA to generate test vectors", __name__)
            config.Arguments.gem5_traces.extend(testing.runGAGem5(binary))
        else:
            debug.verbose_message("Running program on gem5 with %d tests" % config.Arguments.tests, __name__)
            config.Arguments.gem5_traces.extend(testing.run_gem5(binary))
    do_analysis(program)
