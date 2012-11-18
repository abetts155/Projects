#!/usr/bin/python2.6

import sys, optparse
import Debug

# The command-line parser and its options
parser = optparse.OptionParser(add_help_option=False)

parser.add_option("-d",
                  "--debug",
                  action="store",
                  dest="debug",
                  type="int",
                  help="Debug mode.",
                  default=False)

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

parser.add_option("--float",
                 action="store_true",
                 dest="float",
                 help="Generate floating-point data.",
                 default=False)

parser.add_option("--non-negative",
                 action="store_true",
                 dest="nonNegative",
                 help="Generate non-negative data.",
                 default=False)

parser.add_option("-T",
                  "--tests",
                  action="store",
                  dest="tests",
                  type="int",
                  help="Number of test vectors to generate.",
                  default=1)

(opts, args) = parser.parse_args(sys.argv[1:])
Debug.verbose = opts.verbose
Debug.debug = opts.debug

# Settings regarding range
upperBound = (2**31)-1
lowerBound = -(2**31)

def checkCommandLine ():
    # Check that the user has passed the correct options
    if len(args) != 2:
        Debug.exitMessage("You need to specify the name of the CUDA binary and how many arguments the kernel expects")
        
    tvLength   = None
    cudaBinary = None
    if args[0].isdigit():
        tvLength = int(args[0])
        cudaBinary = args[1]
    elif args[1].isdigit():
        tvLength = int(args[1])
        cudaBinary = args[0]
    return tvLength, cudaBinary
            
def generateTestVector (tvLength):
    import random
    tv = []    
    for i in range(0, tvLength):
        if opts.float:
            num = random.uniform(lowerBound, upperBound)
        else:
            num = random.randint(lowerBound, upperBound)
        tv.append(num)
    return tv

def runProgram (tv, cudaBinary, f):
    from subprocess import Popen, PIPE
    commandLine = ' '.join(str(elem) for elem in tv)
    command     = "./%s %s" % (cudaBinary, commandLine)
    proc = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
    stdoutdata, stderrdata = proc.communicate()
    
    f.write(stdoutdata)
    
    if proc.returncode != 0:
        Debug.exitMessage("Running '%s' failed" % command)
      
if __name__ == "__main__":    
    tvLength, cudaBinary = checkCommandLine ()
    print tvLength, cudaBinary
    if opts.nonNegative:
        lowerBound = 0
    with open(cudaBinary + ".gpgpusim", 'w') as f:
        for i in range(1, opts.tests+1):
            Debug.debugMessage("Test vector #%d" % i, 1)
            tv = generateTestVector(tvLength)
            runProgram(tv, cudaBinary, f)
    