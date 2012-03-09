#!/usr/bin/python2.6

from optparse import OptionParser
from sys import argv, maxint
from subprocess import Popen, PIPE
from os import environ, sep
from xml.dom import DOMException
from xml.dom.minidom import parse

# The command-line parser and its options
parser = OptionParser(add_help_option=False)

parser.add_option("-d",
                  "--debug",
                  action="store_true",
                  dest="debug",
                  help="Debug mode.",
                  default=False)

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The XML file describing the program",
                  metavar="<FILE>")

parser.add_option("-t",
                  "--trace-file",
                  action="store",
                  type="string",
                  dest="trace",
                  help="The file containing the gem5 trace to be parsed",
                  metavar="<FILE>")

parser.add_option("-o",
                  "--output-file",
                  action="store",
                  type="string",
                  dest="output",
                  help="The output file where the timing information will be written",
                  metavar="<FILE>")

parser.add_option("-a",
                  "--append-output",
                  action="store_true",
                  dest="appendOutput",
                  help="Append the output to the end of the output file rather than overwriting",
                  default=False)

parser.add_option("-H",
                  "--hex-addresses",
                  action="store_true",
                  dest="hexAddrs",
                  help="Output basic block addresses as hex values",
                  default=False)

(opts, args) = parser.parse_args(argv[1:])

# Check that the user has passed the correct options
if opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(1)
if opts.trace is None:
    print("Missing option " + str(parser.get_option("-t")))
    exit(1)


# Extracts first addresses of basic blocks from xml file
def basicBlockInfo(programXML):
    if opts.debug:
        print("Extracting basic block info from xml")

    basicBlockAddrs = []

    for cfg in programXML.getElementsByTagName("cfg"):
        for bb in cfg.getElementsByTagName("bb"):
            insts = bb.getElementsByTagName("inst")
            if len(insts) > 0:
                firstAddr = insts[0].getAttribute("addr")
                sndAddr = insts[len(insts)-1].getAttribute("addr")

                basicBlockAddrs.append(int(firstAddr,16))
                basicBlockAddrs.append(int(sndAddr,16))

    return set(basicBlockAddrs)


# Extracts instruction timing information from the given gem5 trace file
def instructionTimings(trace):
    if opts.debug:
        print("Extracting timings from trace")

    timings = []

    timeIndex = 0
    cpuAddrIndex = 1
    addrIndex = 1
    
    for traceLine in trace:
        splitLine = traceLine.split(':')

        time = splitLine[timeIndex].strip()

        cpuAddr = splitLine[cpuAddrIndex].strip().split(' ')
        address = cpuAddr[addrIndex]

        microInst = address.split('.')
        if len(microInst) == 1:
            timings.append([int(address,16),time])
        else:
            if microInst[1] == "0":
                timings.append([int(microInst[0],16),time])


    return timings


# Create string of basic block times
def getOutputString(instrTimings, basicBlockAddrs):
    if opts.debug:
        print("Creating string of basic block times")

    timingString = ""

    for instrTime in instrTimings:
        if instrTime[0] in basicBlockAddrs:
             if opts.hexAddrs:
                  blockID = str(hex(instrTime[0]))
             else:
                  blockID = str(instrTime[0])
             timingString += (blockID + " " + instrTime[1] + "\n")

    return timingString


# Write string to output file
def writeOutputString(outputString):
    if opts.debug:
        print("Writing string to file")

    if opts.output is None:
        if opts.debug:
            print("No output file provided - writing output to std out")
        print(outputString)

    else:
        if opts.appendOutput:
            outputFile = open(opts.output, 'a')
        else:
            outputFile = open(opts.output, 'w')

        outputFile.write(outputString)
        outputFile.close()


# Open program XML file and extract basic block information
try:
    programFile = open(opts.program, 'r')
except IOError:
    print("Error - cannot open xml file: " + opts.program)
    exit(1)
try:
    programXML = parse(programFile)
    programFile.close()
    basicBlocks = basicBlockInfo(programXML)
except DOMException:
    print("Error parsing xml file: " + opts.program)
    exit(1)

# Open trace file and extract timings for instructions
try:
    traceFile = open(opts.trace, 'r')
    instrTimings = instructionTimings(traceFile)
    traceFile.close()
except IOError:
    print("Error - cannot open gem5 trace file: " + opts.trace)
    exit(1)

# Get output string of basic block timings and output it
outputString = getOutputString(instrTimings, basicBlocks)
writeOutputString(outputString)




