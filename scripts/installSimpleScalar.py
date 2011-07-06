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

def debugMessage (message):
	if opts.debug:
		print(message)

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

def expandTarball (fileName):
	verboseMessage("Expanding tarball '" + fileName + "'")
	tarball = tarfile.open(fileName)
	tarball.extractall()
	tarball.close()

# First move up into the directory where the main tarball is found and untar
goToParentDirectory()
rootDirectory  = "simplescalar"  
expandTarball(rootDirectory + ".tar.gz")

# Go into the expanded directory and set the 'IDIR' environment variable as we need it to compile GCC
os.chdir(rootDirectory)
installDirectory = os.getcwd()
os.environ['IDIR'] = installDirectory
verboseMessage("Installation root directory is '" + installDirectory + "'")

# Expand the SimpleScalar tarball and compile
simpleScalarDirectory = "simplesim-3.0"
expandTarball(simpleScalarDirectory + ".tgz")

os.chdir(simpleScalarDirectory)
verboseMessage("Building SimpleScalar Tools in '" + simpleScalarDirectory + "'")

runCommand("make")

goToParentDirectory()

# Expand the SimpleScalar binutils tarball and compile 
simpleUtilsDirectory = "simpleutils-990811"
expandTarball(simpleUtilsDirectory + ".tar.gz")

os.chdir(simpleUtilsDirectory)
verboseMessage("Building SimpleScalar Binutils in '" + simpleUtilsDirectory + "'")

runCommand("./configure --host=i386-*-linux --target=sslittle-na-sstrix --with-gnu-as --with-gnu-ld --prefix=" + installDirectory)
runCommand("make all")
runCommand("make install")

goToParentDirectory()

# Now overwrite some of the compiled binaries (otherwise we'll get a buffer overflow in the next step) 
# and make sure they are executable
arFileSource          = "ar"
ranlibFileSource      = "ranlib"
arFileDestination     = installDirectory + os.sep + "sslittle-na-sstrix" + os.sep + "bin" + os.sep + "ar"
ranlibFileDestination = installDirectory + os.sep + "sslittle-na-sstrix" + os.sep + "bin" + os.sep + "ranlib"

verboseMessage("Moving '" + arFileSource + "' into '" + arFileDestination + "'")
os.rename(arFileSource,arFileDestination)
verboseMessage("Moving '" + ranlibFileSource + "' into '" + ranlibFileDestination + "'")
os.rename(ranlibFileSource,ranlibFileDestination)

verboseMessage("Changing file permissions")
os.chmod(arFileDestination, 0755)
os.chmod(ranlibFileDestination, 0755)

# First expand tarball which contains GCC cross compiler 
expandTarball("simpletools-2v0.tar.gz")

# Second expand GCC cross compiler tarball and build it 
gccDirectory = "gcc-2.7.2.3"
expandTarball(gccDirectory + ".tar.gz")

os.chdir(gccDirectory)
verboseMessage("Building GCC '" + gccDirectory + "'")

pathExtension = os.pathsep + installDirectory + os.sep + "sslittle-na-sstrix" + os.sep + "bin"
os.environ['PATH'] += pathExtension
verboseMessage("Added '" + pathExtension + "' to environment variable PATH")

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
	source = installDirectory + os.sep + simpleScalarDirectory + os.sep + f
	link   = installDirectory + os.sep + "bin" + os.sep + f
	os.symlink(source,link)

verboseMessage("DONE!")

