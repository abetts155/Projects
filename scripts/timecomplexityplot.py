#!/usr/bin/python2.6

from optparse import OptionParser
from re import match, split
from sys import argv, maxint
from matplotlib.font_manager import fontManager, FontProperties
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
import numpy as np
from subprocess import Popen, PIPE

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

parser.add_option("-r",
                  "--report",
                  action="store",
                  type="string",
                  dest="report",
                  help="The raw data to be plotted.",
                  metavar="<FILE>")

parser.add_option("-V",
                 "--view",
                 action="store_true",
                 dest="view",
                 help="Display the graph.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

if opts.report is None:
    print("Missing option " + str(parser.get_option("-r")))
    exit(0)

xAxis       = []
yAxis       = []
lowestTime  = maxint
highestTime = 0
xAxisUpperLimit = 1

f = open(opts.report, "r")
for line in f:
    tokens        = split("\s+", line)
    ancestorLevel = int(tokens[0])
    time          = int(tokens[1])
    
    xAxisUpperLimit = ancestorLevel
    xAxis.append(ancestorLevel)
    yAxis.append(time/10**9)

    if time > highestTime:
	highestTime = time
    if lowestTime < time:
	lowestTime = time
f.close()

# Set up the graph
fig = plt.figure()
ax = fig.add_subplot(111)
ax.grid(False)
#ax.set_title("Analysis of " + opts.root, fontsize=16, fontweight='bold')
ax.plot(xAxis, yAxis, markersize=11, marker='^')

ax.set_xlim([0, xAxisUpperLimit])
#ax.set_ylim([lowestTime - 10, highestTime + 100])
ax.set_ylabel("Time (seconds)", fontsize=14, fontweight='bold')
ax.set_xlabel("Ancestor-retrieval level", fontsize=14, fontweight='bold')

# Make the X-axis and Y-axis ticks thicker
for tick in ax.xaxis.get_major_ticks():
    tick.label1.set_fontsize(11)
    tick.label1.set_fontweight('bold')
for tick in ax.yaxis.get_major_ticks():
    tick.label1.set_fontsize(11)
    tick.label1.set_fontweight('bold')

# Set up the legend
font= FontProperties(size='12')

# Save the graph
plt.savefig(opts.report[opts.report.index('.'):] + ".png")

# Show it if requested
if opts.view:
    plt.show()
