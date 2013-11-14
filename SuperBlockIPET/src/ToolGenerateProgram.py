#!/usr/bin/python2.7

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Generate random program structure (call graph and CFGs)")
    
    cmdline.add_argument("--clean",
                         action="store_true",
                         help="clean out temporary files",
                         default=False)
    
    cmdline.add_argument("-f",
                         "--filename",
                         help="write to this filename in the current directory")
    
    cmdline.add_argument("-d",
                         "--debug",
                         type=int,
                         help="debug mode",
                         default=0)
    
    cmdline.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    cmdline.add_argument("-g",
                         "--graphviz",
                         action="store_true",
                         help="generate Graphviz files",
                         default=False)
    
    cmdline.add_argument("--subprograms",
                         type=int,
                         help="number of subprograms",
                         default=1,
                         metavar="<INT>")
    
    cmdline.add_argument("--loops",
                         type=int,
                         help="maximum number of loops in a CFG",
                         default=0,
                         metavar="<INT>")
    
    cmdline.add_argument("--self-loops",
                         type=int,
                         help="maximum number of self-loops in a CFG",
                         default=0,
                         metavar="<INT>")
    
    cmdline.add_argument("--nesting-depth",
                         type=int,
                         help="maximum nesting depth of loops",
                         default=1,
                         metavar="<INT>")
    
    cmdline.add_argument("--fan-out",
                         type=int,
                         help="maximum fan out of a CFG vertex",
                         default=2,
                         metavar="<INT>")
    
    cmdline.add_argument("--basic-blocks",
                         type=int,
                         help="maximum number of vertices in a CFG",
                         default=10,
                         metavar="<INT>")
    
    cmdline.add_argument("--breaks",
                         action="store_true",
                         help="allow break-like constructs in the CFG",
                         default=False)
    
    cmdline.add_argument("--continues",
                         action="store_true",
                         help="allow continue-like constructs in the CFG",
                         default=False)
    
    cmdline.add_argument("--unstructured",
                         action="store_true",
                         help="add unstructured edges to the CFG",
                         default=False)
    
    return cmdline.parse_args()
        
if __name__ == "__main__":
    import GenerateProgram, Debug, Visualisation, ParseProgramFile
    import os, re, sys
    
    args                  = commandLine()
    Debug.verbose         = args.verbose
    Debug.debug           = args.debug
    Visualisation.enabled = args.graphviz
    
    assert args.subprograms > 0, "The number of subprograms must be at least one"
    assert args.basic_blocks > 0, "The number of basic blocks per CFG must be a positive integer"
    assert args.fan_out > 1, "The maximum fan out of a basic block must be at least two to allow branches"
    assert args.loops <= (args.basic_blocks - 2)/2, "You need at least %d basic blocks in the CFG to have %d loops" % (2+(args.loops*2), args.loops)
    if args.loops:
        assert args.nesting_depth <= args.loops, "The loop-nesting depth must be lower than the number of loops"
    
    program = GenerateProgram.generate(args.subprograms, 
                                       args.basic_blocks, 
                                       args.fan_out, 
                                       args.loops, 
                                       args.self_loops, 
                                       args.nesting_depth,
                                       args.unstructured,
                                       args.breaks, 
                                       args.continues)
    
    if args.filename:
        args.filesname = os.path.abspath(args.filename)
        if os.path.exists(args.filename):
            Debug.warningMessage("Overwriting file '%s'" % args.filename)
    else:
        files   = [f for f in os.listdir(os.curdir) if os.path.isfile(os.path.join(os.curdir,f))]
        numbers = set([])
        for f in files:
            if re.match(r'program[0-9]+\.txt', f):
                filenumbers = re.findall(r'[0-9]+', f)
                assert len(filenumbers) == 1
                filenumber = filenumbers[0]
                numbers.add(int(filenumber))
        for i in xrange(1,sys.maxint):
            if i not in numbers:
                args.filename = os.path.abspath('program%d.txt' % i)
                break
        assert args.filename, "Unable to create a filename in which to write the program"
    Debug.verboseMessage("Writing program out to file '%s'" % args.filename)
    ParseProgramFile.writeProgram(program, args.filename)
    
    