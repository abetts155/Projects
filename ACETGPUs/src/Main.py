#!/usr/bin/python2.6

import sys, shlex, optparse
import ICFGs, CFGs, ParseCFGs, Debug, Trees, GraphVisualisations, Traces
import IPGs, WCET

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
    if len(args) != 2:
        Debug.exitMessage("You need to specify the name of the CUDA binary and how many test vectors to generate")
        
    numOfTVs   = None
    cudaBinary = None
    if args[0].isdigit():
        numOfTVs = int(args[0])
        cudaBinary = args[1]
    elif args[1].isdigit():
        numOfTVs = int(args[1])
        cudaBinary = args[0]
        
    assert numOfTVs > 0, "The number of test vectors has to be a non-negative integer"
    return numOfTVs, cudaBinary
            
def createCFGs (outfile):
    cfg = CFGs.CFG()
    cfgInput = False
    with open(outfile, 'r') as f:
        for line in f:
            if "*** CFG ***" in line:
                if not cfgInput:
                    cfgInput = True
                else:
                    cfgInput = False
                    break
            else:
                if cfgInput:
                    ParseCFGs.parseLine(line, cfg)
    return cfg 

def doAnalysis (cfg):
    icfg = ICFGs.ICFG(cfg)
    icfg.setEntryID()
    icfg.setExitID()
    icfg.addExitEntryEdge()
    GraphVisualisations.makeUdrawFile (icfg, "icfg")
    lnt = Trees.LoopNests(icfg, icfg.getEntryID())
    GraphVisualisations.makeUdrawFile (lnt, "lnt")
    ipg = IPGs.IPG(icfg, lnt)
    GraphVisualisations.makeUdrawFile (ipg, "ipg")
    return ipg
    
def getLineOfTimingTrace (line):
    lexemes     = shlex.split(line)
    PCIndex     = 3 
    warpIndex   = 6
    SMIndex     = 9
    cycleIndex  = 12
    SMAndWarp   = int(lexemes[SMIndex]), int(lexemes[warpIndex])
    timingTuple = lexemes[PCIndex], int(lexemes[cycleIndex])
    return SMAndWarp, timingTuple

def getWarp (allWarpTraces, SMAndWarp):
    SMID   = SMAndWarp[0]
    warpID = SMAndWarp[1]
    for w in allWarpTraces:
        if w.getMultiprocessorID() == SMID and w.getWarpID() == warpID:
            return w
    
    w = Traces.WarpTrace(SMID, warpID)
    allWarpTraces.append(w)
    return w

def splitTraces (ipg, outfile):
    allWarpTraces = []
    with open(outfile, 'r') as f:
        for line in f:
            if "Issued" in line and "PC" in line and "Warp" in line and "SM" in line and "cycle" in line:
                SMAndWarp, timingTuple = getLineOfTimingTrace(line)
                w = getWarp (allWarpTraces, SMAndWarp)
                w.appendToTrace(timingTuple)
    return allWarpTraces

def runProgram (numOfTVs, cudaBinary):
    from subprocess import Popen, PIPE
    outputFilename = cudaBinary + ".gpgpusim"
    with open(outputFilename, 'w') as outfile:
        command = "%s %d" % (cudaBinary, numOfTVs)
        Debug.debugMessage("Running '%s'" % command, 1)
        proc = Popen(command, shell=True, executable="/bin/bash", stdout=outfile, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.exitMessage("Running '%s' failed" % command)
        outfile.flush()
    return outputFilename
    
if __name__ == "__main__":
    numOfTVs, cudaBinary = checkCommandLine ()
    outfile = runProgram(numOfTVs, cudaBinary)
    cfg = createCFGs(outfile)
    ipg = doAnalysis(cfg)
    allWarpTraces = splitTraces (ipg, outfile)
    allData = Traces.TraceData(allWarpTraces, ipg)
    allData.output()
    WCET.LinearProgram(ipg, allData, outfile)
