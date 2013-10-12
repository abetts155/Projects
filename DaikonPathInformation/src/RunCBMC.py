#!/usr/bin/python2.7
import os, re, shutil, signal
from functools import wraps

def commandLine ():
    from argparse import ArgumentParser
    
    # The command-line parser and its options
    cmdline = ArgumentParser(description="Verify dynamic analysis conjectures using CBMC")
    
    cmdline.add_argument("program",
                         help="a '.c' file with CBMC instrumentation and assertions")
    
    cmdline.add_argument("--CBMC",
                         dest="cbmc",
                         help="path to CBMC")
    
    cmdline.add_argument("--timeout",
                         type=int,
                         help="set number of seconds at which to timeout",
                         default=60)
    
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

totalAssertFailures = 0
CBMCproc = None
args = commandLine()

class CBMCAssertFailure:
    UNWIND    = 0
    ASSERTION = 1

class TimeoutException (Exception):
    pass

def timeout(seconds=10):
    def decorator(func):
        def _handle_timeout(signum, frame):
            if not CBMCproc.poll():
                CBMCproc.kill()
                raise TimeoutException()
            
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result

        return wraps(func)(wrapper)
    return decorator

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
    commentedAssert = "\/\/assert(%s);" % failedAssert
    sedAssert       = re.sub(r'\s+', r'.*',failedAssert)
    assertString    = "assert(%s);" % sedAssert
    newfilename     = filename + '.temp'
    newfile         = open(newfilename, 'w')
    cmd             = "sed 's/%s/%s/g' %s" % (assertString, commentedAssert, filename)
    proc = Popen(cmd, shell=True, stdout=newfile) 
    proc.wait()
    newfile.close()
    shutil.move(newfilename, filename)

def getFailingAssert (filename):
    stdin,stdout = os.popen2("tail -n 3 %s" % filename)
    stdin.close()
    lines = stdout.readlines()
    stdout.close()
    line = lines[0].strip()
    if line.startswith("unwinding assertion"):
        return CBMCAssertFailure.UNWIND, None
    else:
        assert re.search(r'\s*__count_[0-9]+_[0-9]+', line)
        return CBMCAssertFailure.ASSERTION, line

def runCBMC (cbmc, unwind, program, rootFunction):
    global CBMCproc
    from subprocess import Popen
    cmd = '%s --unwind %d -DCBMC %s --function %s' % (cbmc, unwind, program, rootFunction)
    Debug.verboseMessage("Running '%s'" % cmd)
    success = False
    totalAssertFailures = 0
    cbmcOutput = os.path.abspath(os.getcwd() + os.sep + '%s.cbmc' % rootFunction)
    while not success:
        with open(cbmcOutput,'wb') as out:
            CBMCproc = Popen(cmd, shell=True, stdout=out)    
            CBMCproc.wait()
        out = open(cbmcOutput,'r')
        for line in out:
            if line.startswith("VERIFICATION SUCCESSFUL"):
                success = True
                break
        out.close()
        if not success:
            failType, failedAssert = getFailingAssert(cbmcOutput)
            if failType == CBMCAssertFailure.ASSERTION:
                totalAssertFailures += 1
                Debug.verboseMessage("Assertion '%s' failed in '%s'" % (failedAssert, program))
                commentOutFailingAssert(program, failedAssert)
            else:
                Debug.verboseMessage("Insufficient unwinding")
                return success, totalAssertFailures
    return success, totalAssertFailures

@timeout(args.timeout)
def run (cbmc, program, rootFunction):
    global totalAssertFailures
    unwind  = 4
    success = False
    while not success:
        success, newAssertFailures = runCBMC(cbmc, unwind, program, rootFunction)
        totalAssertFailures += newAssertFailures
        unwind *= 2

if __name__ == "__main__":
    import Debug
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
        basename     = os.path.basename(args.program)
        rootFunction = os.path.splitext(basename)[0]
        args.cbmc    = os.path.abspath(args.cbmc)
        try:
            run(args.cbmc, args.program, rootFunction)
        except:
            pass
        finally:
            Debug.verboseMessage("In '%s', %d asserts out of %d failed" % (args.program, totalAssertFailures, countAsserts(args.program)))
    
    