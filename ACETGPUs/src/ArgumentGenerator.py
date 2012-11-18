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

parser.add_option("-o",
                 "--out",
                 action="store",
                 type="string",
                 dest="outFile",
                 help="Specify output file.",
                 default="arguments.txt")

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
    if len(args) == 0:
        Debug.exitMessage("You need to specify how many arguments should be generated")
    elif len(args) > 1:
        Debug.exitMessage("Too many arguments given")
    try:
        tvLength = int(args[0])
        return tvLength
    except:
        Debug.exitMessage("The argument must be an integer")
            
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
          
if __name__ == "__main__":    
    tvLength = checkCommandLine ()
    if opts.nonNegative:
        lowerBound = 0
    with open(opts.outFile, 'w') as f:
        for i in range(1, opts.tests+1):
            Debug.debugMessage("Test vector #%d" % i, 1)
            tv   = generateTestVector(tvLength)
            text = ','.join(str(elem) for elem in tv)
            f.write(text)
            f.write("\n")
    