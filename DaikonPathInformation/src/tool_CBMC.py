#!/usr/bin/env python

from __future__ import print_function

import os
import re
import shutil
import signal
import functools
import subprocess
import argparse
import config

def the_command_line():
    parser = argparse.ArgumentParser(description="Verify dynamic analysis conjectures using CBMC")
    
    parser.add_argument("program",
                         help="a '.c' file with CBMC instrumentation and assertions")
    
    parser.add_argument("--CBMC",
                         dest="cbmc",
                         help="path to CBMC")
    
    parser.add_argument("--timeout",
                         type=int,
                         help="set number of seconds at which to timeout",
                         default=600)
    
    parser.add_argument("--uncomment",
                         action="store_true",
                         help="uncomment failed assertions")
    
    parser.add_argument("-d",
                         "--debug",
                         type=int,
                         help="debug mode",
                         default=0)
 
    parser.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    setattr(config.Arguments, "basename", os.path.splitext(os.path.basename(config.Arguments.program_file)))

totalAssertFailures = 0
CBMCproc            = None

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
                print("TIMEOUT reached!")
                raise TimeoutException()
            
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result

        return functools.wraps(func)(wrapper)
    return decorator

def countAsserts (filename):
    cmd  = "ack-grep --count assert %s" % filename
    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE) 
    proc.wait()
    line  = proc.stdout.readlines()[0].strip()
    print(line)
    count = re.findall(r':[0-9]+', line)[0]
    return int(count[1:])

def uncommentFailingAsserts (filename):
    newfilename  = filename + '.temp'
    newfile      = open(newfilename, 'w')
    cmd          = "sed 's/\/\/assert/assert/g' %s" % filename
    proc = subprocess.Popen(cmd, shell=True, stdout=newfile) 
    proc.wait()
    newfile.close()
    shutil.move(newfilename, filename)

def commentOutFailingAssert (filename, failedAssert):
    commentedAssert = "\/\/assert(%s);" % failedAssert
    sedAssert       = re.sub(r'\s+', r'.*',failedAssert)
    assertString    = "assert(%s);" % sedAssert
    newfilename     = filename + '.temp'
    newfile         = open(newfilename, 'w')
    cmd             = "sed 's/%s/%s/g' %s" % (assertString, commentedAssert, filename)
    proc = subprocess.Popen(cmd, shell=True, stdout=newfile) 
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
    global CBMCproc, totalAssertFailures
    cmd = '%s --unwind %d -DCBMC %s --function %s' % (cbmc, unwind, program, rootFunction)
    print("Running '%s'" % cmd)
    success = False
    cbmcOutput = os.path.abspath(os.getcwd() + os.sep + '%s.cbmc' % rootFunction)
    while not success:
        with open(cbmcOutput,'wb') as out:
            CBMCproc = subprocess.Popen(cmd, shell=True, stdout=out)    
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
                print("Assertion '%s' failed in '%s'" % (failedAssert, program))
                commentOutFailingAssert(program, failedAssert)
            else:
                print("Insufficient unwinding")
                return success
    return success

@timeout(config.Arguments.timeout)
def run (cbmc, program, rootFunction):
    global totalAssertFailures
    unwind  = 4
    success = False
    while not success:
        success = runCBMC(cbmc, unwind, program, rootFunction)
        unwind *= 2

if __name__ == "__main__":
    the_command_line()
    if config.Arguments.uncomment:
        uncommentFailingAsserts (config.Arguments.program)
    else:
        assert config.Arguments.cbmc, "You need to pass the path to CBMC"
        assert os.path.exists(config.Arguments.cbmc), "The CBMC path '%s' does not exist" % config.Arguments.cbmc
        assert os.path.isfile(config.Arguments.cbmc), "'%s' is not a file" % config.Arguments.cbmc
        rootFunction          = os.path.splitext(config.Arguments.basename)[0]
        config.Arguments.cbmc = os.path.abspath(config.Arguments.cbmc)
        try:
            run(config.Arguments.cbmc, config.Arguments.program, rootFunction)
        except:
            pass
        finally:
            print("In '%s', %d asserts out of %d failed" % \
                  (config.Arguments.program, totalAssertFailures, countAsserts(config.Arguments.program)))
    
    
