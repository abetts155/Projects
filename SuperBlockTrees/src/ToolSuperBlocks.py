#!/usr/bin/python2.7

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Compute WCET using tree-based calculation derived from the super block CFG")
    
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
    
    cmdline.add_argument("-g",
                         "--graphviz",
                         action="store_true",
                         help="generate png files to visualise graphs",
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
    import ParseProgramFile, Debug, Trees, Traces, SuperBlocks, Utils, Calculations, Visualisation
    
    args = commandLine()
    Debug.verbose         = args.verbose
    Debug.debug           = args.debug
    Visualisation.enabled = args.graphviz
    
    if args.clean:
        Utils.clean()
    assert args.program.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
    # Create the CFGs
    Debug.verboseMessage("Reading in program file to do super block CFG analysis")
    filename = os.path.basename(args.program)
    basepath = os.path.abspath(os.path.dirname(args.program))
    basename = os.path.splitext(filename)[0]
    Visualisation.basename = basename
    program  = ParseProgramFile.createProgram(args.program)
    Debug.verboseMessage("Analysing CFGs")
    for cfg in program.getCFGs():
        functionName = cfg.getName()
        lnt = Trees.LoopNests(cfg, cfg.getEntryID())
        program.addLNT(lnt, functionName)
        superg = SuperBlocks.SuperBlockGraph(cfg, lnt)
        program.addSuperBlockCFG(superg, functionName)
    program.generateVisualisationFiles()
    if args.traces:
        Debug.verboseMessage("Generating dummy traces")
        Traces.GenerateTraces(basepath, basename, program, args.traces)
    elif args.tracefile:
        tracefile = os.path.abspath(args.tracefile)
        assert os.path.exists(tracefile), "Trace file '%s' does not exist" % tracefile
        tracefileLastModified = os.stat(tracefile)[8]
        programfileLastModified = os.stat(args.program)[8]
        assert programfileLastModified < tracefileLastModified, "Program file modified AFTER trace file generation"
        data = Traces.ParseTraces(basename, tracefile, program)
        program.generateVisualisationFiles()
        Calculations.WCETCalculation(program, data, basepath, basename)
    
    