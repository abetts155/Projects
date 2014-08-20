from __future__ import print_function

import atexit
import time

def seconds_to_string(the_time):
    return "%d:%02d:%02d.%03d" % \
        reduce(lambda ll,b : divmod(ll[0],b) + ll[1:], [(the_time*1000,),1000,60,60])

line = "="*60
def log(s, elapsed=None):
    print(line)
    the_time = time.clock()
    print(seconds_to_string(the_time), '-', s)
    if elapsed:
        print("Elapsed time:", elapsed)
    print(line)
    print()
    return the_time

def endlog():
    end = time.clock()
    elapsed = end-start
    log("End Program", seconds_to_string(elapsed))

def now():
    return seconds_to_string(time.clock())

start = time.clock()
atexit.register(endlog)
log("Start Program")
