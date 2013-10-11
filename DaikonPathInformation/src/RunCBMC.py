#!/usr/bin/python2.7
import sys, os

def runCBMC (cbmc, program):
    from subprocess import Popen, PIPE
    cmd = '%s -DCBMC %s' % (cbmc, program)
    Debug.verboseMessage("Running '%s'" % cmd, 1)
    proc = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Running '%s' failed" % cmd)
    Debug.verboseMessage("All conjectures in '%s' verify!" % program)

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Verify dynamic analysis conjectures using CBMC")
    
    cmdline.add_argument("program",
                         help="a '.c' file with CBMC instrumentation and assertions")
    
    cmdline.add_argument("-d",
                         "--debug",
                         type=int,
                         help="debug mode",
                         default=0)
 
    cmdline.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    return cmdline.parse_args()

if __name__ == "__main__":
    import Debug
    
    cbmc = "/home/adam/CBMC/cbmc"
    assert os.path.exists(cbmc), "The CBMC path '%s' does not exist" % cbmc
    assert os.path.isfile(cbmc), "'%s' is not a file" % cbmc
    
    args          = commandLine()
    Debug.verbose = args.verbose
    Debug.debug   = args.debug    
    assert args.program.endswith('.c'), "Please pass a program file with a '.c' suffix"
    assert args.program.exists('.c'), "The program file '%s' does not exist" % args.program
    assert os.path.isfile(cbmc), "The program '%s' is not a file" % args.program
    args.program  = os.path.abspath(args.program)
    runCBMC(cbmc, args.program)
    
    
    