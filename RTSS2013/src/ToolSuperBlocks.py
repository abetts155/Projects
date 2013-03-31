#!/usr/bin/python2.7

def parseGem5Traces (filestem, program):
    import ParseGem5Trace
    import re    
    gem5TraceDirectory = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory
    assert os.path.exists(gem5TraceDirectory), "Unable to find directory '%s' which should contain the gem5 traces" % gem5TraceDirectory
    gem5Traces = []
    for filename in os.listdir(gem5TraceDirectory):
        match = re.match(r'%s' % os.path.basename(filestem), filename)
        if match:
            tracefile = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory + os.sep + filename
            gem5Traces.append(tracefile)
    ParseGem5Trace.parse(program, gem5Traces)

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Compute super block CFGs")
    
    cmdline.add_argument("program",
                         help="a file containing program information (with '.txt' extension)")
    
    cmdline.add_argument("--clean",
                         action="store_true",
                         help="clean out temporary files",
                         default=False)
    
    cmdline.add_argument("-d",
                         "--debug",
                         type=int,
                         help="debug mode",
                         default=0)
    
    cmdline.add_argument("-u",
                         "--udraw",
                         action="store_true",
                         help="generate uDrawGraph files",
                         default=False)
    
    cmdline.add_argument("--gem5-traces",
                         dest="gem5traces",
                         action="store_true",
                         help="parse gem5 traces",
                         default=False)
    
    cmdline.add_argument("--calculation",
                         action="store_true",
                         help="do WCET calculation",
                         default=False)
    
    cmdline.add_argument("-I",
                         "--inlining-capacity",
                         dest="inliningCapacity",
                         type=int,
                         help="inlining capacity beyond which the size of each CFG will not exceed",
                         metavar="<INT>")
    
    cmdline.add_argument("-t",
                         "--traces",
                         type=int,
                         help="generate dummy traces for the given program",
                         default=0,
                         metavar="<INT>")
    
    cmdline.add_argument("-T",
                         dest="tracefile",
                         help="parse this trace file",
                         metavar="<FILE>")
    
    cmdline.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    return cmdline.parse_args()
        
if __name__ == "__main__":
    import os
    import ParseProgramFile, Debug, Trees, Traces, UDrawGraph, SuperBlocks, Vertices, Utils, Calculations
    
    args               = commandLine()
    Debug.verbose      = args.verbose
    Debug.debug        = args.debug
    UDrawGraph.enabled = args.udraw
    
    if args.inliningCapacity:
        assert args.inliningCapacity > 0, "The inlining capacity must be a positive integer"            
    if args.clean:
        Utils.clean()
    assert args.program.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
    # Create the CFGs
    Debug.verboseMessage("Reading in program file to do super block CFG analysis")
    filename = os.path.basename(args.program)
    basepath = os.path.abspath(os.path.dirname(args.program))
    basename = os.path.splitext(filename)[0]
    program  = ParseProgramFile.createProgram(args.program)
    UDrawGraph.makeUdrawFile(program.getCallGraph(), "%s.%s" % (basename, "callg"))
    program.inlineCalls(args.inliningCapacity)
    Debug.verboseMessage("Analysing CFGs")
    for icfg in program.getICFGs():
        functionName = icfg.getName()
        if icfg.getExitID() != Vertices.dummyVertexID:
            UDrawGraph.makeUdrawFile(icfg, "%s.%s.%s" % (basename, functionName, "icfg"))
            lnt = Trees.LoopNests(icfg, icfg.getEntryID())
            program.addLNT(lnt, functionName)
            UDrawGraph.makeUdrawFile(lnt, "%s.%s.%s" % (basename, functionName, "lnt"))
            superg = SuperBlocks.SuperBlockGraph(icfg, lnt)
            UDrawGraph.makeUdrawFile(superg, "%s.%s.%s" % (basename, functionName, "superg"))
            program.addSuperBlockCFG(superg, functionName)
        else:
            Debug.warningMessage("Not analysing function %s because it does not have an exit point set" % functionName)
    if args.traces:
        Debug.verboseMessage("Generating dummy traces")
        Traces.GenerateTraces(basepath, basename, program, args.traces)
    elif args.tracefile:
        tracefile = os.path.abspath(args.tracefile)
        assert os.path.exists(tracefile), "Trace file '%s' does not exist" % tracefile
        Traces.ParseTraces(basename, tracefile, program)
    if args.gem5traces:
        parseGem5Traces (basename, program)
    if args.calculation:
        Calculations.WCETCalculation(program, basepath, basename)
        
        