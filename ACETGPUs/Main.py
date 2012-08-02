#!/usr/bin/python2.7

import sys
import ICFGs, CFGs, ParseCFG, Debug, Trees, GraphVisualisation
from optparse import OptionParser

# The command-line parser and its options
parser = OptionParser(add_help_option=False)

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

(opts, args) = parser.parse_args(sys.argv[1:])
Debug.verbose = opts.verbose
Debug.debug = opts.debug

def checkCommandLine ():
    # Check that the user has passed the correct options
    if len(args) == 0:
        Debug.exitMessage("You need to pass a file containing output from GPGPU-sim")
            
def createCFGs ():
    cfg = CFGs.CFG()
    cfgInput = False
    f = open(args[0])
    for line in f:
        if "*** CFG ***" in line:
            if not cfgInput:
                cfgInput = True
            else:
                cfgInput = False
        else:
            if cfgInput:
                ParseCFG.parseLine(line, cfg)
    f.close()
    return cfg 

def doAnalysis (cfg):
    icfg = ICFGs.ICFG(cfg)
    icfg.setEntryID()
    icfg.setExitID()
    icfg.addExitEntryEdge()
    GraphVisualisation.makeUdrawFile (icfg, "icfg")
    lnt = Trees.LoopNests(icfg, icfg.entryID)
    GraphVisualisation.makeUdrawFile (lnt, "lnt")
        
if __name__ == "__main__":
    checkCommandLine ()
    cfg = createCFGs()
    doAnalysis(cfg)
