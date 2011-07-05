#!/usr/bin/python2.6

import os
import shutil
import tarfile
from optparse import OptionParser
from subprocess import Popen, PIPE
from sys import argv

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

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

def verboseMessage (message):
	if opts.verbose:
		print(message)

def runCommand (command):
	verboseMessage("Running: '" + command + "'")

	proc = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
	stdoutdata, stderrdata = proc.communicate()

	if opts.debug:
		print('==================================== STANDARD OUTPUT ===========================================')
		for line in stdoutdata.splitlines():
			print(line)
		print('================================================================================================')


	if proc.returncode != 0:
		print("\nProblem running '" + command + "'")
		print('==================================== STANDARD ERROR ============================================')
		for line in stderrdata.splitlines():
			print(line)
		print('================================================================================================') 
		exit(proc.returncode)
	else:
		verboseMessage("Completed")

def goToParentDirectory ():
	os.chdir(os.path.dirname(os.getcwd()))

mainDirectory         = "simplescalar"           
simpleScalarDirectory = "simplesim-3.0"
simpleUtilsDirectory  = "simpleutils-990811"
gccDirectory          = "gcc-2.7.2.3"

# Expand all the tarballs
verboseMessage("Expanding tarball '" + mainDirectory + ".tar.gz'")

tarball = tarfile.open(mainDirectory + ".tar.gz")
tarball.extractall()
tarball.close()

os.chdir(mainDirectory)
installDirectory = os.getcwd()

verboseMessage("Expanding tarball '" + simpleScalarDirectory + ".tgz'")

tarball = tarfile.open(simpleScalarDirectory + ".tgz")
tarball.extractall()
tarball.close()

verboseMessage("Expanding tarball '" + simpleUtilsDirectory + ".tar.gz'")

tarball = tarfile.open(simpleUtilsDirectory + ".tar.gz")
tarball.extractall()
tarball.close()

# Build SimpleScalar tools
verboseMessage("Building SimpleScalar Tools in '" + simpleScalarDirectory + "'")

os.chdir(simpleScalarDirectory)
runCommand("make")

goToParentDirectory()

# Build SimpleScalar binutils
verboseMessage("Building SimpleScalar Binutils in '" + simpleUtilsDirectory + "'")

os.chdir(simpleUtilsDirectory)
runCommand("./configure --host=i386-*-linux --target=sslittle-na-sstrix --with-gnu-as --with-gnu-ld --prefix=" + installDirectory)
runCommand("make all")
runCommand("make install")

goToParentDirectory()

# Overwrite some files and make sure they are executable
arFile     = "ar"
ranlibFile = "ranlib"

verboseMessage("Changing file permissions")
os.chmod(arFile, 0755)
os.chmod(ranlibFile, 0755)

verboseMessage("Moving files")
shutil.move(arFile, installDirectory + os.pathsep + "sslittle-na-sstrix" + os.pathsep + "bin")
shutil.move(ranlibFile, installDirectory + os.pathsep + "sslittle-na-sstrix" + os.pathsep + "bin")

# Expand tarballs to install GCC cross compiler 
simpleToolsTarball = "simpletools-2v0.tar.gz" 
verboseMessage("Expanding tarball '" +  simpleToolsTarball + "'")

tarball = tarfile.open(simpleToolsTarball)
tarball.extractall()
tarball.close()

verboseMessage("Expanding tarball '" + gccDirectory + ".tar.gz'")

tarball = tarfile.open(gccDirectory + ".tar.gz")
tarball.extractall()
tarball.close()

# Build GCC cross compiler
os.chdir(gccDirectory)

os.environ['PATH'] += installDirectory + os.pathsep + "sslittle-na-sstrix" + os.pathsep + "bin"
runCommand("./configure --host=i386-*-linux --target=sslittle-na-sstrix --with-gnu-as --with-gnu-ld --enable-languages=c --prefix=" + installDirectory)
runCommand("make all")
runCommand("make install")

goToParentDirectory()

# Make symbolic links to SimpleScalar binaries
verboseMessage("Making symbolic links")

bpred    = "sim-bpred"
cache    = "sim-cache"
eio      = "sim-eio"
fast     = "sim-fast"
outorder = "sim-outorder"
profile  = "sim-profile" 
safe     = "sim-safe"

files = [bpred, cache, eio, fast, outorder, profile, safe]
for f in files:
	source = installDirectory + os.pathsep + simpleScalarDirectory + os.pathsep + f
	link   = installDirectory + os.pathsep + "bin" + os.pathsep + f
	os.symlink(source,link)

verboseMessage("DONE!")

