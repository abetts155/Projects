#!/usr/bin/env python

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
    import ParseProgramFile, Debug, Trees, Traces, UDrawGraph, Utils, Calculations
    
    args               = commandLine()
    Debug.verbose      = args.verbose
    Debug.debug        = args.debug
    UDrawGraph.enabled = args.udraw
    
    if args.clean:
        Utils.clean()  
    
    assert args.program.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
    # Create the CFGs
    filename = os.path.basename(args.program)
    basepath = os.path.abspath(os.path.dirname(args.program))
    basename = os.path.splitext(filename)[0]
    UDrawGraph.basename = basename
    program  = ParseProgramFile.createProgram(args.program)
    totalSuperBlockProgramPoints = 0
    totalNaiveProgramPoints = 0
    for cfg in program.getCFGs():
        functionName = cfg.getName()
        lnt = Trees.LoopNests(cfg, cfg.getEntryID())
        program.addLNT(lnt, functionName)
        pathg = program.getPathInfoGraph(functionName)
        totalSuperBlockProgramPoints += len(pathg.getMonitoredProgramPoints())
        totalNaiveProgramPoints += min(cfg.numOfVertices(), cfg.numOfEdges())
    print args.program, "[Super block program points = %d" % totalSuperBlockProgramPoints, \
    "CFG program points = %d]" % totalNaiveProgramPoints
    program.generateAllUDrawFiles()
    if args.traces:
        Debug.verboseMessage("Generating dummy traces")
        Traces.GenerateTraces(basepath, basename, program, args.traces)
    elif args.tracefile:
        args.tracefile = os.path.abspath(args.tracefile)
        assert os.path.exists(args.tracefile), "Trace file '%s' does not exist" % args.tracefile
        assert os.path.getmtime(args.program) <= os.path.getmtime(args.tracefile), "Program file modified AFTER trace file generation"
        data = Traces.ParseTraces(basename, args.tracefile, program)
        program.output()
        program.generateAllUDrawFiles()
        Calculations.WCETCalculation(program, data, basepath, basename)
        
        