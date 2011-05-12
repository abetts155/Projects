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

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The name of the program under analysis.",
                  metavar="<NAME>")

parser.add_option("-r",
                  "--report",
                  action="store",
                  type="string",
                  dest="report",
                  help="The raw data to be plotted.",
                  metavar="<FILE>")

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

parser.add_option("-V",
                 "--view",
                 action="store_true",
                 dest="view",
                 help="Display the graph.",
                 default=False)

parser.add_option("-w",
                  "--wcet-all",
                  action="store_true",
                  dest="wcet_all",
                  help="Plot WCET estimate line derived from all capacity constraints.",
                  default=False)

(opts, args) = parser.parse_args(argv[1:])

if opts.report is None:
    print("Missing option " + str(parser.get_option("-r")))
    exit(0)
elif opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(0)

xAxis           = []
metLine         = []
wcet_All_Line   = []
wcet_DFS_Line   = []
actualWCETLine  = []
lowestMET       = maxint
lowestWCET      = maxint
highestMET      = 0
highestWCET     = 0
xAxisUpperLimit = 1

f = open(opts.report, "r")
for line in f:
    tokens     = split("\s+", line)
    tvNum      = int(tokens[0])
    met        = int(tokens[1])
    wcet_All   = int(tokens[2])
    wcet_DFS   = int(tokens[3])
    actualWCET = int(tokens[4])
    
    xAxisUpperLimit = tvNum
    xAxis.append(tvNum)
    metLine.append(met)
    wcet_All_Line.append(wcet_All)
    wcet_DFS_Line.append(wcet_DFS)
    actualWCETLine.append(actualWCET)

    if wcet_DFS > highestWCET:
        highestWCET = wcet_DFS
    if wcet_All < lowestWCET:
        lowestWCET = wcet_All

    if met > highestMET:
        highestMET = met
    if met < lowestMET:
        lowestMET = met
f.close()

# Set up the graph
fig = plt.figure()
ax = fig.add_subplot(111)
ax.grid(False)
#ax.set_title("Analysis of " + opts.root, fontsize=16, fontweight='bold')
ax.plot(xAxis, metLine, markersize=11, marker='|', linestyle='None')
ax.plot(xAxis, wcet_DFS_Line, markersize=10, marker='^', linestyle='None')
ax.plot(xAxis, actualWCETLine, ls='--', lw='2', color='black')
if opts.wcet_all:
    ax.plot(xAxis, wcet_All_Line, markersize=10, marker='+', linestyle='None')
    legend_str = ("HWMT", "WCET-ALL", "WCET-DFS", "Actual WCET")
else:
    legend_str = ("HWMT", "WCET estimate", "Actual WCET")

ax.set_xlim([0, xAxisUpperLimit])
ax.set_ylim([lowestMET - 10, highestWCET + 100])
ax.set_ylabel("Execution Time (cycles)", fontsize=14, fontweight='bold')
ax.set_xlabel("Test Vector", fontsize=14, fontweight='bold')

# Make the X-axis and Y-axis ticks thicker
for tick in ax.xaxis.get_major_ticks():
    tick.label1.set_fontsize(11)
    tick.label1.set_fontweight('bold')
for tick in ax.yaxis.get_major_ticks():
    tick.label1.set_fontsize(11)
    tick.label1.set_fontweight('bold')

# Set up the legend
font= FontProperties(size='12')
leg = ax.legend(legend_str, "best", prop=font)

# Save the graph
plt.savefig(opts.program + opts.report[opts.report.index('.'):] + ".png")

cmd = "mv %s %s" \
    % (opts.report, opts.program + opts.report[opts.report.index('.'):])
    
proc = Popen(cmd,
             shell=True,
             executable="/bin/bash",
             stderr=PIPE)
proc.wait()

if proc.returncode != 0:
    print("\nProblem running '" + cmd + "'")
    exit(0)

# Show it if requested
if opts.view:
    plt.show()