#!/usr/bin/python2.6

import sys, optparse
import ParseProgramFile, Debug

# The command-line parser and its options
cmdline = optparse.OptionParser(add_help_option=False)

cmdline.add_option("-d",
                  "--debug",
                  action="store",
                  dest="debug",
                  type="int",
                  help="Debug mode.",
                  default=False)

cmdline.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

cmdline.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

cmdline.add_option("-u",
                 "--udraw",
                 action="store_true",
                 dest="udraw",
                 help="Generate uDrawGraph files.",
                 default=False)

(opts, args) = cmdline.parse_args(sys.argv[1:])
Debug.verbose = opts.verbose
Debug.debug = opts.debug
                
if __name__ == "__main__":
    if len(args) == 1:
        outfile = args[0]
        assert outfile.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
        # Create the CFGs
        program = ParseProgramFile.createProgram(outfile)
        for icfg in program.getICFGs():
            if icfg.numberOfIpoints() == 2:
                RegularExpressions.RegularExpressionsWithoutIpoints(icfg)
            else:
                RegularExpressions.RegularExpressions(icfg)
    else:
        Debug.exitMessage("You need to specify the name of a file containing CFGs")
