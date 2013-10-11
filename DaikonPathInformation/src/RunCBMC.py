#!/usr/bin/python2.7
import sys, os

def runCBMC (cbmc, program):
    from subprocess import Popen, PIPE
    cmd = '%s -DCBMC %s' % (cbmc, program)
    Debug.verboseMessage("Running '%s'" % cmd)
    proc = Popen(cmd, shell=True, stdout=PIPE, stderr=sys.stdout)    
    proc.wait()
    success = False
    for line in proc.stdout.readlines():
        if line.startswith("VERIFICATION SUCCESSFUL"):
            success = True
            break
    if success:
        Debug.verboseMessage("All conjectures in '%s' verify!" % program)
    else:
        Debug.verboseMessage("The following fail in '%s'!" % program)

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Verify dynamic analysis conjectures using CBMC")
    
    cmdline.add_argument("program",
                         help="a '.c' file with CBMC instrumentation and assertions")
    
    cmdline.add_argument("--CBMC",
                         dest="cbmc",
                         help="path to CBMC")
    
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
    args          = commandLine()
    Debug.verbose = args.verbose
    Debug.debug   = args.debug    
    
    assert args.cbmc, "You need to pass the path to CBMC"
    assert os.path.exists(args.cbmc), "The CBMC path '%s' does not exist" % args.cbmc
    assert os.path.isfile(args.cbmc), "'%s' is not a file" % args.cbmc
    args.cbmc = os.path.abspath(args.cbmc)
    
    assert args.program.endswith('.c'), "Please pass a program file with a '.c' suffix"
    assert os.path.exists(args.program), "The program file '%s' does not exist" % args.program
    assert os.path.isfile(args.program), "The program '%s' is not a file" % args.program
    args.program = os.path.abspath(args.program)
    runCBMC(args.cbmc, args.program)
    
    
    