#!/usr/bin/python2.6

import sys, optparse, os, time
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

cmdline.add_option("-C",
                  "--deep-clean",
                  action="store_true",
                  dest="deepclean",
                  help="Clean previous runs of GPGPU-sim and uDrawgraph files.",
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

opts, args    = cmdline.parse_args(sys.argv[1:])
Debug.verbose = opts.verbose
Debug.debug   = opts.debug
gpgpuFileExt  = '.gpgpusim'
runPrefix     = 'run'

def createGraphs (program, basepath):
    import UDrawGraph, ICFGs, Trees, IPGs
    Debug.debugMessage("Creating data structures", 1)
    for cfg in program.getCFGs():
        functionName = cfg.getName()
        UDrawGraph.makeUdrawFile (cfg, basepath, "%s.%s" % (functionName, "cfg"))
        predomTree  = Trees.Dominators(cfg, cfg.getEntryID())
        reverseg    = cfg.getReverseCFG()
        postdomTree = Trees.Dominators(reverseg, reverseg.getEntryID())
        UDrawGraph.makeUdrawFile (predomTree, basepath, "%s.%s" % (functionName, "pre"))
        UDrawGraph.makeUdrawFile (postdomTree, basepath, "%s.%s" % (functionName, "post"))
        icfg = ICFGs.ICFG(cfg)
        icfg.setEntryID()
        icfg.setExitID()
        icfg.addExitEntryEdge()
        program.addICFG(icfg)
        UDrawGraph.makeUdrawFile (icfg, basepath, "%s.%s" % (functionName, "icfg"))
        lnt = Trees.LoopNests(icfg, icfg.getEntryID())
        program.addLNT(lnt)
        UDrawGraph.makeUdrawFile (lnt, basepath, "%s.%s" % (functionName, "lnt"))
        ipg = IPGs.IPG(icfg, lnt)
        program.addIPG(ipg)
        icfg.addBranchDivergenceEdges(lnt)
        ipg.updateWithBranchDivergentPaths()
        UDrawGraph.makeUdrawFile (icfg, basepath, "%s.%s" % (functionName, "icfg"))
        UDrawGraph.makeUdrawFile (ipg, basepath, "%s.%s" % (functionName, "ipg"))
        
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
    for outfile in generatedFiles:
        traceFound = False
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
    createGraphs(program, basepath)
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
            ilp = WCET.LinearProgram(ipg, traceData, functionName, basepath)
            print "WCET(%s) = %ld" % (ipg.getName(), ilp.getWCET())
            
def runCUDAKernel (basepath):
        from subprocess import Popen, PIPE
        import signal
        import re
        
        cmd = args[0]
        Debug.debugMessage("CUDA application command '%s'" % cmd, 1)
        
        run = 0
        for filename in os.listdir(basepath):
            match = re.search(r'%s[0-9]+%s' % (runPrefix, gpgpuFileExt), filename)
            if match:
                index = filename.find(gpgpuFileExt)
                num   = int(filename[len(runPrefix):index])
                if num > run:
                    run = num
        run += 1
        
        # Run the program on GPGPU-sim and get generated output
        outfiles       = []
        generatedFiles = []
        for i in xrange(run, opts.tests + run):
            outfilename = basepath + os.sep + runPrefix + str(i) + gpgpuFileExt
            generatedFiles.append(outfilename)
            outfiles.append(outfilename)
        assert generatedFiles, "No output files were generated"
        processes        = []
        pidToOutFilename = {}
        pidToOutFile     = {}
        while True:
            while outfiles and len(processes) < multiprocessing.cpu_count()/2:
                outfilename = outfiles.pop()
                outfile = open(outfilename, 'w')
                Debug.debugMessage("Spawning new process for '%s'" % outfilename, 1)
                proc = Popen(cmd, shell=True, stdout=outfile)
                processes.append(proc)
                pidToOutFile[proc.pid]     = outfile
                pidToOutFilename[proc.pid] = outfilename
            for p in processes:
                if p.poll() is not None:
                    processes.remove(p)
                    pidToOutFile[p.pid].close()
                    if p.returncode != 0:
                        Debug.debugMessage("Process failed for output file '%s' with return code %d" % (pidToOutFilename[p.pid], p.returncode), 1)
                        outfiles.append(pidToOutFilename[p.pid])
            if not processes and not outfiles:
                break
            else:
                time.sleep(1)
                p = Popen(['ps', '-A'], stdout=PIPE)
                out, err = p.communicate()
                for line in out.splitlines():
                    if 'cuobjdump_to_pt' in line:
                        pid = int(line.split(None, 1)[0])
                        os.kill(pid, signal.SIGKILL)
                        Debug.debugMessage("Killing stray CUDA-to-PTXPlus process", 1)
        return generatedFiles

def checkCommandLineForAction ():    
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
        generatedFiles = runCUDAKernel(basepath)
        doAnalysis(generatedFiles, basename, basepath)
    elif len(args) > 0:
        for arg in args:
            if not arg.endswith(gpgpuFileExt):
                Debug.exitMessage("Each file must end with a '%s' suffix. You passed '%s'." % (gpgpuFileExt, arg))
        basename = os.path.splitext(os.path.basename(args[0]))[0]
        basepath = os.path.abspath(os.path.dirname(args[0]))
        doAnalysis(args, basename, basepath)
    else:
        Debug.exitMessage("""There are two ways to run this script:
    1) Either pass a CUDA binary as an argument and the number of times you want to run the kernel with the -T option; or
    2) Pass a number of files that were generated by GPGPU-sim from a previous testing run""")
            
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

def removeFile (fullPath):
    print "Removing '%s'" % fullPath
    os.remove(fullPath)
    
def postClean (abspath):
    # The following file is generated by GPGPU-sim on every execution.
    # It includes stats about each instruction execution
    gpgpuInstrStats = abspath + os.sep + 'gpgpu_inst_stats.txt'
    if os.path.exists(gpgpuInstrStats):
        os.remove(gpgpuInstrStats)
    for paths, dirs, files in os.walk(abspath):
        files.sort()
        for filename in files:
            if filename.startswith('_cuobjdump_') or filename.startswith('_ptxplus_'):
                removeFile(os.path.join(paths, filename))
                
def preClean (abspath):
    import re
    for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
        files.sort()
        for filename in files:
            match = re.search(r'%s[0-9]+%s' % (runPrefix, gpgpuFileExt), filename)
            if match:
                removeFile(os.path.join(paths, filename))
            elif filename.endswith('.udraw') or filename.endswith('.ilp'):
                removeFile(os.path.join(paths, filename))
            elif filename == 'benchmark.warps.trace':
                removeFile(os.path.join(paths, filename))
                
if __name__ == "__main__":
    import multiprocessing
    Debug.verboseMessage("You have %d CPUs on your system" % multiprocessing.cpu_count())
    if opts.deepclean:
        preClean(os.path.abspath(os.curdir))
    try:
        # What to do depends on which parameters were parameters on the command line
        checkCommandLineForAction ()
    finally:
        # Remove temporarily generated files
        postClean (os.path.abspath(os.curdir))
