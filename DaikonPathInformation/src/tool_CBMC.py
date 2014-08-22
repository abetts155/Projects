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

number_of_assert_failures = 0
the_CBMC_process          = None

class CBMCAssertFailure:
    UNWIND    = 0
    ASSERTION = 1

class TimeoutException (Exception):
    pass

def timeout(seconds=10):
    def decorator(func):
        def handle_timeout(signum, frame):
            if not the_CBMC_process.poll():
                the_CBMC_process.kill()
                print("TIMEOUT reached!")
                raise TimeoutException()
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result
        return functools.wraps(func)(wrapper)
    return decorator

def count_asserts():
    cmd  = "ack-grep --count assert %s" % config.Arguments.program_file
    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE) 
    proc.wait()
    line = proc.stdout.readlines()[0].strip()
    print(line)
    count = re.findall(r':[0-9]+', line)[0]
    return int(count[1:])

def uncomment_failing_asserts():
    new_filename = config.Arguments.program_file + '.temp'
    new_file     = open(new_filename, 'w')
    cmd          = "sed 's/\/\/assert/assert/g' %s" % config.Arguments.program_file
    proc         = subprocess.Popen(cmd, shell=True, stdout=new_file) 
    proc.wait()
    new_file.close()
    shutil.move(new_filename, config.Arguments.program_file)

def comment_failing_assert(failedAssert):
    commented_assert = "\/\/assert(%s);" % failedAssert
    sedAssert        = re.sub(r'\s+', r'.*',failedAssert)
    assert_string    = "assert(%s);" % sedAssert
    new_filename     = config.Arguments.program_file + '.temp'
    new_file         = open(new_filename, 'w')
    cmd              = "sed 's/%s/%s/g' %s" % (assert_string, commented_assert, config.Arguments.program_file)
    proc             = subprocess.Popen(cmd, shell=True, stdout=new_file) 
    proc.wait()
    new_file.close()
    shutil.move(new_filename, config.Arguments.program_file)

def get_failing_assert(filename):
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

def run_CBMC(unwind, root_function):
    global the_CBMC_process, number_of_assert_failures
    cmd = '%s --unwind %d -DCBMC %s --function %s' % (config.Arguments.CBMC, unwind, config.Arguments.program_file, root_function)
    print("Running '%s'" % cmd)
    success = False
    cbmcOutput = os.path.abspath(os.getcwd() + os.sep + '%s.cbmc' % root_function)
    while not success:
        with open(cbmcOutput,'wb') as out:
            the_CBMC_process = subprocess.Popen(cmd, shell=True, stdout=out)    
            the_CBMC_process.wait()
        out = open(cbmcOutput,'r')
        for line in out:
            if line.startswith("VERIFICATION SUCCESSFUL"):
                success = True
                break
        out.close()
        if not success:
            failType, failedAssert = get_failing_assert(cbmcOutput)
            if failType == CBMCAssertFailure.ASSERTION:
                number_of_assert_failures += 1
                print("Assertion '%s' failed in '%s'" % (failedAssert, config.Arguments.program_file))
                comment_failing_assert(failedAssert)
            else:
                print("Insufficient unwinding")
                return success
    return success

@timeout(config.Arguments.timeout)
def run(root_function):
    unwind  = 4
    success = False
    while not success:
        success = run_CBMC(unwind, root_function)
        unwind *= 2

def the_command_line():
    parser = argparse.ArgumentParser(description="Verify dynamic analysis conjectures using CBMC")
    
    parser.add_argument("program_file",
                         help="a '.c' file with CBMC instrumentation and assertions")
    
    parser.add_argument("--CBMC",
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
    
    config.Arguments.basename = os.path.splitext(os.path.basename(config.Arguments.program_file))
    
    if not config.Arguments.uncomment:
        assert config.Arguments.CBMC, "You need to pass the path to CBMC"
        assert os.path.exists(config.Arguments.CBMC), "The CBMC path '%s' does not exist" % config.Arguments.CBMC
        assert os.path.isfile(config.Arguments.CBMC), "'%s' is not a file" % config.Arguments.CBMC
        config.Arguments.CBMC = os.path.abspath(config.Arguments.CBMC)

if __name__ == "__main__":
    the_command_line()
    if config.Arguments.uncomment:
        uncomment_failing_asserts(config.Arguments.program)
    else:
        root_function = os.path.splitext(config.Arguments.basename)[0]
        try:
            run(root_function)
        except:
            pass
        finally:
            print("In %s, %d asserts out of %d failed" % (config.Arguments.program, 
                                                          number_of_assert_failures, 
                                                          count_asserts(config.Arguments.program)))
    
    
