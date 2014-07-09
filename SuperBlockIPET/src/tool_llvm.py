import ARM, Debug
import os

def commandLine ():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
    
    # The command-line parser and its options
    cmdline = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter,
                             description="Parse LLVM disassembly to internal file format")
    
    cmdline.add_argument("program",
                         help="LLVM disassembly file")
    
    cmdline.add_argument("-d",
                          "--debug",
                          action="store",
                          type=int,
                          help="debug mode",
                          metavar="<INT>",
                          default=0)
    
    cmdline.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    cmdline.add_argument("-r",
                          "--root",
                          action="store",
                          help="the function that is the entry point of the analysis. [This should not be 'main']",
                          metavar="<FUNCTION>")
    
    return cmdline.parse_args()

if __name__ == "__main__":   
    args = commandLine()
    Debug.verboseMessage("Analysing file '%s'" % args.program)
    args.program = os.path.abspath(args.program)
    ext          = os.path.splitext(args.program)[1]
    assert ext == '.dis'
    assert os.path.exists(args.program), "File '%s' does not exist" % args.program
    program = ARM.readARMDisassembly(args.program, args.root)
    
        