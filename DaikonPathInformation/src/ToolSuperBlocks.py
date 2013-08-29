#!/usr/bin/python2.7

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
    import ParseProgramFile, Debug, Trees, Traces, UDrawGraph, SuperBlocks, Utils, Calculations
    
    args               = commandLine()
    Debug.verbose      = args.verbose
    Debug.debug        = args.debug
    UDrawGraph.enabled = args.udraw
    
    if args.clean:
        Utils.clean()
    assert args.program.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
    # Create the CFGs
    Debug.verboseMessage("Reading in program file to do super block CFG analysis")
    filename = os.path.basename(args.program)
    basepath = os.path.abspath(os.path.dirname(args.program))
    basename = os.path.splitext(filename)[0]
    UDrawGraph.basename = basename
    program  = ParseProgramFile.createProgram(args.program)
    Debug.verboseMessage("Analysing CFGs")
    for icfg in program.getICFGs():
        functionName = icfg.getName()
        lnt = Trees.LoopNests(icfg, icfg.getEntryID())
        program.addLNT(lnt, functionName)
        superg = SuperBlocks.SuperBlockGraph(icfg, lnt)
        program.addSuperBlockCFG(superg, functionName)
    program.generateAllUDrawFiles()
    if args.traces:
        Debug.verboseMessage("Generating dummy traces")
        Traces.GenerateTraces(basepath, basename, program, args.traces)
    elif args.tracefile:
        args.tracefile = os.path.abspath(args.tracefile)
        assert os.path.exists(args.tracefile), "Trace file '%s' does not exist" % args.tracefile
        assert os.path.getmtime(args.program) <= os.path.getmtime(args.tracefile), "Program file modified AFTER trace file generation"
        data = Traces.ParseTraces(basename, args.tracefile, program)
        program.generateAllUDrawFiles()
        Calculations.WCETCalculation(program, data, basepath, basename)
        
        