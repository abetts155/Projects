#!/usr/bin/python2.7
import os, re, shutil

totalAssertFailures = 0

def countAsserts (filename):
    from subprocess import Popen, PIPE
    cmd  = "ack --count assert %s" % filename
    proc = Popen(cmd, shell=True, stdout=PIPE) 
    proc.wait()
    line = proc.stdout.readlines()[0].strip()
    return int(line)

def uncommentFailingAsserts (filename):
    from subprocess import Popen
    newfilename  = filename + '.temp'
    newfile      = open(newfilename, 'w')
    cmd          = "sed 's/\/\/assert/assert/g' %s" % filename
    proc = Popen(cmd, shell=True, stdout=newfile) 
    proc.wait()
    newfile.close()
    shutil.move(newfilename, filename)

def commentOutFailingAssert (filename, failedAssert):
    from subprocess import Popen
    assertString = "assert(%s);" % failedAssert
    newfilename  = filename + '.temp'
    newfile      = open(newfilename, 'w')
    cmd          = "sed 's/%s/\/\/%s/g' %s" % (assertString, assertString, filename)
    proc = Popen(cmd, shell=True, stdout=newfile) 
    proc.wait()
    newfile.close()
    shutil.move(newfilename, filename)

def getFailingAssert (filename):
    global totalAssertFailures
    totalAssertFailures += 1
    stdin,stdout = os.popen2("tail -n 3 %s" % filename)
    stdin.close()
    lines = stdout.readlines()
    stdout.close()
    line = lines[0].strip()
    assert re.search(r'\s*__count_[0-9]+_[0-9]+', line)
    return line

def runCBMC (cbmc, program):
    from subprocess import Popen
    cmd = '%s -DCBMC %s' % (cbmc, program)
    Debug.verboseMessage("Running '%s'" % cmd)
    with open('stdout.txt','wb') as out, open('stderr.txt','wb') as err:
        proc = Popen(cmd, shell=True, stdout=out, stderr=err)    
        proc.wait()
    success = False
    out = open('stdout.txt','r')
    for line in out:
        if line.startswith("VERIFICATION SUCCESSFUL"):
            success = True
            break
    out.close()
    if not success:
        failedAssert = getFailingAssert('stdout.txt')
        Debug.verboseMessage("Assertion '%s' failed in '%s'" % (failedAssert, program))
        commentOutFailingAssert(program, failedAssert)
        runCBMC(cbmc, program)

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Verify dynamic analysis conjectures using CBMC")
    
    cmdline.add_argument("program",
                         help="a '.c' file with CBMC instrumentation and assertions")
    
    cmdline.add_argument("--CBMC",
                         dest="cbmc",
                         help="path to CBMC")
    
    cmdline.add_argument("--uncomment",
                         action="store_true",
                         help="uncomment failed assertions")
    
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
    
    return cmdline.parse_args()

if __name__ == "__main__":
    import Debug
    args          = commandLine()
    Debug.verbose = args.verbose
    Debug.debug   = args.debug    
    
    assert args.program.endswith('.c'), "Please pass a program file with a '.c' suffix"
    assert os.path.exists(args.program), "The program file '%s' does not exist" % args.program
    assert os.path.isfile(args.program), "The program '%s' is not a file" % args.program
    args.program = os.path.abspath(args.program)
    
    if args.uncomment:
        uncommentFailingAsserts (args.program)
    else:
        assert args.cbmc, "You need to pass the path to CBMC"
        assert os.path.exists(args.cbmc), "The CBMC path '%s' does not exist" % args.cbmc
        assert os.path.isfile(args.cbmc), "'%s' is not a file" % args.cbmc
        args.cbmc = os.path.abspath(args.cbmc)
        runCBMC(args.cbmc, args.program)
        totalAsserts = countAsserts(args.program)
        Debug.verboseMessage("In '%s', %d asserts out of %d failed" % (args.program, totalAssertFailures, totalAsserts))
    
    
    