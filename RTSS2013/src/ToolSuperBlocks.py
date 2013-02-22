#!/usr/bin/python2.6

def preClean (abspath):
    for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
        files.sort()
        for filename in files:
            if filename.endswith('.udraw'):
                fullPath = os.path.join(paths, filename)
                Debug.verboseMessage("Removing '%s'" % fullPath)
                os.remove(fullPath)
                
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
    
    cmdline.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    return cmdline.parse_args()
        
if __name__ == "__main__":
    import os
    import ParseProgramFile, Debug, Trees, Traces, UDrawGraph, SuperBlocks
    
    args               = commandLine()
    Debug.verbose      = args.verbose
    Debug.debug        = args.debug
    UDrawGraph.enabled = args.udraw
    
    if args.clean:
        preClean(os.path.abspath(os.curdir))
    assert args.program.endswith('.txt'), "Please pass a program file with a '%s' suffix" % ('.txt')
    # Create the CFGs
    filename = os.path.basename(args.program)
    basepath = os.path.abspath(os.path.dirname(args.program))
    basename = os.path.splitext(filename)[0]
    program = ParseProgramFile.createProgram(args.program)
    UDrawGraph.makeUdrawFile(program.getCallGraph(), "%s.%s" % (basename, "callg"))
    for icfg in program.getICFGs():
        functionName = icfg.getName()
        UDrawGraph.makeUdrawFile(icfg, "%s.%s.%s" % (basename, functionName, "icfg"))
        lnt = Trees.LoopNests(icfg, icfg.getEntryID())
        program.addLNT(lnt, functionName)
        UDrawGraph.makeUdrawFile(lnt, "%s.%s.%s" % (basename, functionName, "lnt"))
        superg = SuperBlocks.SuperBlockGraph(icfg, lnt)
        UDrawGraph.makeUdrawFile(superg, "%s.%s.%s" % (basename, functionName, "superg"))
    if args.traces:
        Traces.GenerateTraces(basepath, basename, program, args.traces)
