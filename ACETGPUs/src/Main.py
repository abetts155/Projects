#!/usr/bin/python2.6

import sys, shlex, optparse
import ICFGs, CFGs, ParseCFG, Debug, Trees, GraphVisualisation, Traces
import IPGs

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
    with open(args[0], 'r') as f:
        for line in f:
            if "*** CFG ***" in line:
                if not cfgInput:
                    cfgInput = True
                else:
                    cfgInput = False
                    break
            else:
                if cfgInput:
                    ParseCFG.parseLine(line, cfg)
    return cfg 

def doAnalysis (cfg):
    icfg = ICFGs.ICFG(cfg)
    icfg.setEntryID()
    icfg.setExitID()
    icfg.addExitEntryEdge()
    GraphVisualisation.makeUdrawFile (icfg, "icfg")
    lnt = Trees.LoopNests(icfg, icfg.getEntryID())
    GraphVisualisation.makeUdrawFile (lnt, "lnt")
    ipg = IPGs.IPG(icfg, lnt)
    
def getLineOfTimingTrace (line):
    lexemes                 = shlex.split(line)
    expectedNumberOfLexemes = 13
    PCIndex     = 3 
    warpIndex   = 6
    SMIndex     = 9
    cycleIndex  = 12
    if len(lexemes) != expectedNumberOfLexemes:
        return False, None, None
    else:
        SMAndWarp   = int(lexemes[SMIndex]), int(lexemes[warpIndex])
        timingTuple = lexemes[PCIndex], int(lexemes[cycleIndex])
        return True, SMAndWarp, timingTuple

def getWarp (allWarps, SMAndWarp):
    SMID   = SMAndWarp[0]
    warpID = SMAndWarp[1]
    for w in allWarps:
        if w.getMultiprocessorID() == SMID and w.getWarpID() == warpID:
            return w
    
    w = Traces.Warp(SMID, warpID)
    allWarps.append(w)
    return w

def splitTraces ():
    traceDetected = False
    allWarps = []
    with open(args[0], 'r') as f:
        for line in f:
            if not traceDetected:
                if "Issued" in line and "PC" in line and "Warp" in line and "SM" in line and "cycle" in line:
                    traceDetected = True
            if traceDetected:
                outcome, SMAndWarp, timingTuple = getLineOfTimingTrace(line)
                if outcome:
                    w = getWarp (allWarps, SMAndWarp)
                    w.appendToTrace(timingTuple)
                       
    for w in allWarps:
        Debug.debugMessage("\nSM %s, WARP %s" % (w.getMultiprocessorID(), w.getWarpID()), 10)
        for t in w.getTrace():
            Debug.debugMessage("\n\t %s %s" % (t[0], t[1]), 10)
        
if __name__ == "__main__":
    checkCommandLine ()
    cfg = createCFGs()
    doAnalysis(cfg)
    splitTraces ()
