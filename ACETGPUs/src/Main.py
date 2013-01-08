#!/usr/bin/python2.6

import sys, optparse, os
import Debug

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

cmdline.add_option("-T",
                  "--number-of-tests",
                  action="store",
                  type="int",
                  dest="tests",
                  help="The number of times to run the kernel. [Default is %default].",
                  default=0,
                  metavar="<INT>")

cmdline.add_option("--no-parsing",
                 action="store_true",
                 dest="noParse",
                 help="Do not parse traces.",
                 default=False)

(opts, args) = cmdline.parse_args(sys.argv[1:])
Debug.verbose = opts.verbose
Debug.debug = opts.debug
gpgpuFileExt = '.gpgpusim'

def createGraphs (program, basename):
    import UDrawGraph, ICFGs, Trees, IPGs
    Debug.debugMessage("Creating data structures", 1)
    for cfg in program.getCFGs():
        functionName = cfg.getName()
        UDrawGraph.makeUdrawFile (cfg, "%s.%s.%s" % (basename, functionName, "cfg"))
        predomTree  = Trees.Dominators(cfg, cfg.getEntryID())
        reverseg    = cfg.getReverseCFG()
        postdomTree = Trees.Dominators(reverseg, reverseg.getEntryID())
        UDrawGraph.makeUdrawFile (predomTree, "%s.%s.%s" % (basename, functionName, "pre"))
        UDrawGraph.makeUdrawFile (postdomTree, "%s.%s.%s" % (basename, functionName, "post"))
        icfg = ICFGs.ICFG(cfg)
        icfg.setEntryID()
        icfg.setExitID()
        icfg.addExitEntryEdge()
        program.addICFG(icfg)
        UDrawGraph.makeUdrawFile (icfg, "%s.%s.%s" % (basename, functionName, "icfg"))
        lnt = Trees.LoopNests(icfg, icfg.getEntryID())
        program.addLNT(lnt)
        UDrawGraph.makeUdrawFile (lnt, "%s.%s.%s" % (basename, functionName, "lnt"))
        ipg = IPGs.IPG(icfg, lnt)
        program.addIPG(ipg)
        icfg.addBranchDivergenceEdges(lnt)
        ipg.updateWithBranchDivergentPaths()
        UDrawGraph.makeUdrawFile (icfg, "%s.%s.%s" % (basename, functionName, "icfg"))
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
    import Traces
    key = (SMAndWarp[0], SMAndWarp[1])
    if key in allWarpTraces:
        return allWarpTraces[key]
    w = Traces.WarpTrace(SMAndWarp[0], SMAndWarp[1])
    allWarpTraces[key] = w
    return w

def splitTraces (generatedFiles):
    Debug.debugMessage("Splitting traces", 1)
    allWarpTraces = {}
    traceFound = False
    for outfile in generatedFiles:
        Debug.debugMessage("Analysing file '%s'" % outfile, 1)
        with open(outfile, 'r') as f:
            for line in f:
                if not traceFound:
                    if line.startswith('0x'):
                        traceFound = True
                if traceFound:
                    try:
                        SMAndWarp, timingTuple = getLineOfTimingTrace(line)
                        w = getWarp (allWarpTraces, SMAndWarp)
                        w.appendToTrace(timingTuple)
                    except ValueError:
                        # Line found which is not a trace tuple;
                        # therefore finished processing this file
                        traceFound = False
                        break
    return allWarpTraces

def writeTraces (allWarpTraces, basename, basepath):
    traceFileName = basepath + os.sep + basename + ".warps.trace"
    with open(traceFileName, 'w') as f:
        for w in allWarpTraces.values(): 
            f.write("\n%s\n" % ('=' * 20))
            f.write("SM = %d WARP = %d\n" % (w.getMultiprocessorID(), w.getWarpID()))
            f.write("%s\n" % ('=' * 20))
            for t in w.getTrace():
                ipointID = int(t[0], 0)
                time     = long(t[1])
                f.write("0x%04X %d\n" % (ipointID, time))

def doAnalysis (generatedFiles, basename, basepath): 
    import Traces, WCET
    # Create the CFGs
    program = createCFGs(generatedFiles[0])
    # Create the IPG
    createGraphs(program, basename)
    if not opts.noParse:
        # Split into warp-specific traces
        allWarpTraces = splitTraces (generatedFiles)
        if Debug.debug >= 5:
            writeTraces(allWarpTraces, basename, basepath)
        traceData = Traces.TraceData(allWarpTraces, program)
        traceData.output()
    
        print "%s End-to-end timing data %s" % ("=" * 11, "=" * 11)
        for ipg in program.getIPGs():
            functionName = ipg.getName()
            print "ACET(%s) = %ld" % (functionName, traceData.getACET(functionName))
            print "HWMT(%s) = %ld" % (functionName, traceData.getHWMT(functionName))
            # Create an ILP from the IPG and the parsed data
            ilp = WCET.LinearProgram(ipg, traceData, basename, basepath)
            print "WCET(%s) = %ld" % (ipg.getName(), ilp.getWCET())

def checkCommandLineForAction ():
    from threading import Thread
    # Check that the user has passed the correct options
    if opts.tests > 0:
        cudaBinary = args[0]
        if not os.path.exists(cudaBinary):
            Debug.exitMessage("The argument '%s' does not exist" % cudaBinary)
        elif not os.path.isfile(cudaBinary):
            Debug.exitMessage("The argument '%s' is not a file" % cudaBinary)
        elif not os.access(cudaBinary, os.X_OK):
            Debug.exitMessage("The argument '%s' does not have execute permission" % cudaBinary)
        # Get the filename of the binary without the path
        basename = os.path.basename(cudaBinary)
        basepath = os.path.abspath(os.path.dirname(cudaBinary))
        # Run the program on GPGPU-sim and get generated output
        theThreads     = []
        generatedFiles = []
        for i in range(0, opts.tests):
            outfilename = cudaBinary + gpgpuFileExt + str(i)
            generatedFiles.append(outfilename)
            t = Thread(runProgram(outfilename, cudaBinary))
            theThreads.append(t)
            t.start()
        for t in theThreads:
            t.join()
        doAnalysis(generatedFiles, basename, basepath)
    elif len(args) == 1:
        outfile = args[0]
        assert outfile.endswith(gpgpuFileExt), "Please pass a file with a '%s' suffix" % (gpgpuFileExt)
        basename = os.path.splitext(os.path.basename(outfile))[0]
        doAnalysis(outfile, basename)
    else:
        Debug.exitMessage("You need to specify the name of the CUDA binary and how many test vectors to generate")
            
def createCFGs (outfile):
    import ParseCFGs
    cfgLines = []
    cfgInput = False
    with open(outfile, 'r') as f:
        for line in f:
            if line.startswith('0x'):
                break
            if "*** CFG ***" in line:
                if not cfgInput:
                    cfgInput = True
                else:
                    cfgInput = False
            if cfgInput:
                cfgLines.append(line)
    program = ParseCFGs.createProgram(cfgLines)
    return program 

def runProgram (outfilename, cudaBinary):
    from subprocess import Popen, PIPE
    
    Debug.debugMessage("Creating file '%s' for GPGPU-sim output" % outfilename, 1)
    with open(outfilename, 'w') as outfile:
        Debug.debugMessage("Running '%s'" % cudaBinary, 1)
        proc = Popen(cudaBinary, shell=True, executable="/bin/bash", stdout=outfile, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.exitMessage("Running '%s' failed" % cudaBinary)
        outfile.flush()
    
def cleanUp (abspath):
    for paths, dirs, files in os.walk(abspath):
        files.sort()
        for filename in files:
            if filename.startswith('_cuobjdump_') or filename.startswith('_ptxplus_'):
                fullPath = os.path.join(paths, filename)
                Debug.debugMessage("Removing '%s'" % fullPath, 5)
                os.remove(fullPath)
                
if __name__ == "__main__":
    import multiprocessing
    print multiprocessing.cpu_count()
    # What to do depends on which parameters were parameters on the command line
    checkCommandLineForAction ()
    # Remove temporarily generated files
    cleanUp (os.path.abspath(os.curdir))
