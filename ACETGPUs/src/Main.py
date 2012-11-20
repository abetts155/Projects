#!/usr/bin/python2.6

import sys, optparse, os
import ICFGs, CFGs, ParseCFGs, Debug, Trees, UDrawGraph, Traces
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
gpgpuFileExt = '.gpgpusim'

def createGraphs (program, basename):
    Debug.debugMessage("Creating data structures", 1)
    for cfg in program.getCFGs():
        functionName = cfg.getName()
        icfg = ICFGs.ICFG(cfg)
        icfg.setEntryID()
        icfg.setExitID()
        icfg.addExitEntryEdge()
        program.addICFG(icfg)
        if opts.udraw:
            UDrawGraph.makeUdrawFile (icfg, "%s.%s.%s" % (basename, functionName, "icfg"))
        lnt = Trees.LoopNests(icfg, icfg.getEntryID())
        program.addLNT(lnt)
        if opts.udraw:
            UDrawGraph.makeUdrawFile (lnt, "%s.%s.%s" % (basename, functionName, "lnt"))
        ipg = IPGs.IPG(icfg, lnt)
        program.addIPG(ipg)
        if opts.udraw:
            UDrawGraph.makeUdrawFile (ipg, "%s.%s.%s" % (basename, functionName, "ipg"))
    
def getLineOfTimingTrace (line):
    import shlex
    tokenizer = shlex.shlex(line, posix=True)
    tokenizer.whitespace += ','
    lexemes = list(tokenizer)
    PCIndex     = 0 
    warpIndex   = 1
    SMIndex     = 2
    cycleIndex  = 3
    SMAndWarp   = int(lexemes[SMIndex]), int(lexemes[warpIndex])
    timingTuple = lexemes[PCIndex], int(lexemes[cycleIndex])
    return SMAndWarp, timingTuple

def getWarp (allWarpTraces, SMAndWarp):
    key = (SMAndWarp[0], SMAndWarp[1])
    if key in allWarpTraces:
        return allWarpTraces[key]
        
    w = Traces.WarpTrace(SMAndWarp[0], SMAndWarp[1])
    allWarpTraces[key] = w
    return w

def splitTraces (outfile):
    Debug.debugMessage("Splitting traces", 1)
    allWarpTraces = {}
    with open(outfile, 'r') as f:
        for line in f:
            if line.startswith('0x'):
                SMAndWarp, timingTuple = getLineOfTimingTrace(line)
                w = getWarp (allWarpTraces, SMAndWarp)
                w.appendToTrace(timingTuple)
    return allWarpTraces

def writeTraces (allWarpTraces, basename):
    traceFileName = basename + ".warps.trace"
    with open(traceFileName, 'w') as f:
        for w in allWarpTraces.values(): 
            f.write("\n%s\n" % ('=' * 20))
            f.write("SM = %d WARP = %d\n" % (w.getMultiprocessorID(), w.getWarpID()))
            f.write("%s\n" % ('=' * 20))
            for t in w.getTrace():
                ipointID = int(t[0], 0)
                time     = long(t[1])
                f.write("0x%04X %d\n" % (ipointID, time))

def doAnalysis (outfile, basename): 
    # Create the CFGs
    program = createCFGs(outfile)
    # Create the IPG
    createGraphs(program, basename)
    # Split into warp-specific traces
    allWarpTraces = splitTraces (outfile)
    if Debug.debug >= 5:
        writeTraces(allWarpTraces, basename)
    allData = Traces.TraceData(allWarpTraces, program)
    allData.output()
    # Create an ILP from the IPG and the parsed data
    for ipg in program.getIPGs():
        WCET.LinearProgram(ipg, allData, outfile)

def checkCommandLineForAction ():
    # Check that the user has passed the correct options
    if len(args) == 2:
        numOfTVs   = None
        cudaBinary = None
        if args[0].isdigit():
            numOfTVs = int(args[0])
            cudaBinary = args[1]
        elif args[1].isdigit():
            numOfTVs = int(args[1])
            cudaBinary = args[0]
        assert numOfTVs > 0, "The number of test vectors has to be a non-negative integer"
        # Get the filename of the binary without the path
        basename = os.path.basename(cudaBinary)
        # Run the program on GPGPU-sim and get generated output
        outfile = runProgram(numOfTVs, cudaBinary)
        doAnalysis(outfile, basename)
    elif len(args) == 1:
        outfile = args[0]
        file
        assert outfile.endswith(gpgpuFileExt), "Please pass a file with a '%s' suffix" % (gpgpuFileExt)
        basename = os.path.splitext(os.path.basename(outfile))[0]
        doAnalysis(outfile, basename)
    else:
        Debug.exitMessage("You need to specify the name of the CUDA binary and how many test vectors to generate")
            
def createCFGs (outfile):
    import shlex
    program = CFGs.Program()
    cfg = None
    cfgInput = False
    with open(outfile, 'r') as f:
        for line in f:
            if line.startswith('0x'):
                break
            if "*** CFG ***" in line:
                if not cfgInput:
                    Debug.debugMessage("New CFG found", 1)
                    cfg = CFGs.CFG()
                    program.addCFG(cfg)
                    cfgInput = True
                else:
                    cfgInput = False
            else:
                if cfgInput:
                    if line.startswith("Printing basic blocks for function"):
                        lexemes = shlex.split(line)
                        functionName = lexemes[-1][:-1]
                        cfg.setName(functionName)
                        Debug.debugMessage("Setting CFG name to '%s'" % functionName, 1)
                    else:
                        ParseCFGs.parseLine(line, cfg)
    return program 

def runProgram (numOfTVs, cudaBinary):
    from subprocess import Popen, PIPE
    outfilename = cudaBinary + gpgpuFileExt
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
    # What to do depends on which parameters were parameters on the command line
    checkCommandLineForAction ()
