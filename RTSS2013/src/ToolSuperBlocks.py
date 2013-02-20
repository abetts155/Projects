#!/usr/bin/python2.6

import sys, optparse, os
import ParseProgramFile, Debug, Trees, Traces, UDrawGraph, SuperBlocks

# The command-line parser and its options
cmdline = optparse.OptionParser(add_help_option=False)

cmdline.add_option("-d",
                  "--debug",
                  action="store",
                  dest="debug",
                  type="int",
                  help="Debug mode.",
                  default=0)

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

cmdline.add_option("-G",
                  "--generate-traces",
                  action="store",
                  type="int",
                  dest="traces",
                  help="Generate dummy traces for the given program. [Default is %default].",
                  default=0,
                  metavar="<INT>")

cmdline.add_option("-C",
                  "--deep-clean",
                  action="store_true",
                  dest="deepclean",
                  help="Clean previous runs of GPGPU-sim and uDrawgraph files.",
                  default=False)

cmdline.add_option("-u",
                 "--udraw",
                 action="store_true",
                 dest="udraw",
                 help="Generate uDrawGraph files.",
                 default=False)

(opts, args)       = cmdline.parse_args(sys.argv[1:])
Debug.verbose      = opts.verbose
Debug.debug        = opts.debug
UDrawGraph.enabled = opts.udraw

def removeFile (fullPath):
    Debug.verboseMessage("Removing '%s'" % fullPath)
    os.remove(fullPath)

def preClean (abspath):
    for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
        files.sort()
        for filename in files:
            if filename.endswith('.udraw'):
                removeFile(os.path.join(paths, filename))
                
if __name__ == "__main__":
    if opts.deepclean:
        preClean(os.path.abspath(os.curdir))
    if len(args) == 1:
        infile = args[0]
        assert infile.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
        # Create the CFGs
        filename = os.path.basename(infile)
        basepath = os.path.abspath(os.path.dirname(infile))
        basename = os.path.splitext(filename)[0]
        program = ParseProgramFile.createProgram(infile)
        UDrawGraph.makeUdrawFile(program.getCallGraph(), "%s.%s" % (basename, "callg"))
        for icfg in program.getICFGs():
            functionName = icfg.getName()
            UDrawGraph.makeUdrawFile(icfg, "%s.%s.%s" % (basename, functionName, "icfg"))
            lnt = Trees.LoopNests(icfg, icfg.getEntryID())
            program.addLNT(lnt, functionName)
            UDrawGraph.makeUdrawFile(lnt, "%s.%s.%s" % (basename, functionName, "lnt"))
            superg = SuperBlocks.SuperBlockGraph(icfg, lnt)
            UDrawGraph.makeUdrawFile(superg, "%s.%s.%s" % (basename, functionName, "superg"))
        if opts.traces:
            Traces.GenerateTraces(basepath, basename, program, opts.traces)
    else:
        Debug.exitMessage("You need to specify the name of a file containing CFGs")
