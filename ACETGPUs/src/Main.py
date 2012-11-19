#!/usr/bin/python2.6

import sys, shlex, optparse, os
import ICFGs, CFGs, ParseCFGs, Debug, Trees, GraphVisualisations, Traces
import IPGs, WCET

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

def doAnalysis (cfg, basename):
    icfg = ICFGs.ICFG(cfg)
    icfg.setEntryID()
    icfg.setExitID()
    icfg.addExitEntryEdge()
    if opts.udraw:
        GraphVisualisations.makeUdrawFile (icfg, "%s.%s" % (basename, "icfg"))
    lnt = Trees.LoopNests(icfg, icfg.getEntryID())
    if opts.udraw:
        GraphVisualisations.makeUdrawFile (lnt, "%s.%s" % (basename, "lnt"))
    ipg = IPGs.IPG(icfg, lnt)
    if opts.udraw:
        GraphVisualisations.makeUdrawFile (ipg, "%s.%s" % (basename, "ipg"))
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
    outfilename = cudaBinary + ".gpgpusim"
    Debug.debugMessage("Creating file '%s' for GPGPU-sim output" % outfilename, 1)
    with open(outfilename, 'w') as outfile:
        command = "%s %d" % (cudaBinary, numOfTVs)
        Debug.debugMessage("Running '%s'" % command, 1)
        proc = Popen(command, shell=True, executable="/bin/bash", stdout=outfile, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.exitMessage("Running '%s' failed" % command)
        outfile.flush()
    return outfilename
    
def cleanUp (abspath):
    for paths, dirs, files in os.walk(abspath):
        files.sort()
        for filename in files:
            if filename.startswith('_cuobjdump_') or filename.startswith('_ptxplus_'):
                fullPath = os.path.join(paths, filename)
                Debug.debugMessage("Removing '%s'" % fullPath, 5)
                os.remove(fullPath)
                
if __name__ == "__main__":
    # Remove files created from previous runs
    cleanUp (os.path.abspath(os.curdir))
    # Get the CUDA binary name and number of test vectors
    numOfTVs, cudaBinary = checkCommandLine ()
    # Get the filename of the binary without the path
    basename = os.path.basename(cudaBinary)
    # Run the program on GPGPU-sim and get generated output
    outfile = runProgram(numOfTVs, cudaBinary)
    # Create the CFGs
    cfg = createCFGs(outfile)
    # Create the IPG
    ipg = doAnalysis(cfg, basename)
    # Split into warp-specific traces
    allWarpTraces = splitTraces (ipg, outfile)
    allData = Traces.TraceData(allWarpTraces, ipg)
    allData.output()
    # Create an ILP from the IPG and the parsed data
    WCET.LinearProgram(ipg, allData, outfile)
