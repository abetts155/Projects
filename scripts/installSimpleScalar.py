import os
import sys
import shutil
import tarfile
from optparse import OptionParser

# Add the 'src' directory to the module search and PYTHONPATH
sys.path.append(sys.path[0] + os.sep + "src")

from Debug import Debug

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

(opts, args) = parser.parse_args(sys.argv[1:])

debug = Debug(opts.verbose)

wcetToolsEnvironmentVariable = "WCET_HOME"

try:
	os.environ[wcetToolsEnvironmentVariable]
except KeyError:
	print ("Cannot find environment variable '" + wcetToolsEnvironmentVariable + "' which is needed to run this script.")
	exit(0)

def runCommand (command):
	from subprocess import Popen, PIPE

	debug.verboseMessage("Running: '" + command + "'")

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
		debug.verboseMessage("Completed")

def goToParentDirectory ():
	os.chdir(os.path.dirname(os.getcwd()))

def expandTarball (fileName):
	debug.verboseMessage("Expanding tarball '" + fileName + "'")
	tarball = tarfile.open(fileName)
	tarball.extractall()
	tarball.close()

# First move into the directory where the main tarball is found and untar
rootDirectory  = os.path.abspath(os.environ[wcetToolsEnvironmentVariable])
if not os.path.exists(rootDirectory):
	os.makedirs(rootDirectory)

os.chdir(rootDirectory)
simplescalarDirectory = "simplescalar"

# Assume that, if the 'simplescalar' directory exists, we've already built the tools
if not os.path.exists(simplescalarDirectory):
	# Expand the main tarball
	expandTarball("simplescalar.tar.gz")
	os.chdir("simplescalar")

	# Go into the expanded directory and set the 'IDIR' environment variable as we need it to compile GCC
	installDirectory = os.getcwd()
	os.environ['IDIR'] = installDirectory
	debug.verboseMessage("Installation root directory is '" + installDirectory + "'")

	# Expand the SimpleScalar tarball and compile
	simpleScalarDirectory = "simplesim-3.0"
	expandTarball(simpleScalarDirectory + ".tgz")

	os.chdir(simpleScalarDirectory)
	debug.verboseMessage("Building SimpleScalar Tools in '" + simpleScalarDirectory + "'")

	runCommand("make")

	goToParentDirectory()

	# Expand the SimpleScalar binutils tarball and compile 
	simpleUtilsDirectory = "simpleutils-990811"
	expandTarball(simpleUtilsDirectory + ".tar.gz")

	os.chdir(simpleUtilsDirectory)
	debug.verboseMessage("Building SimpleScalar Binutils in '" + simpleUtilsDirectory + "'")

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

	debug.verboseMessage("Moving '" + arFileSource + "' into '" + arFileDestination + "'")
	os.rename(arFileSource,arFileDestination)
	debug.verboseMessage("Moving '" + ranlibFileSource + "' into '" + ranlibFileDestination + "'")
	os.rename(ranlibFileSource,ranlibFileDestination)

	debug.verboseMessage("Changing file permissions")
	os.chmod(arFileDestination, 0755)
	os.chmod(ranlibFileDestination, 0755)

	# First expand tarball which contains GCC cross compiler 
	expandTarball("simpletools-2v0.tar.gz")

	# Second expand GCC cross compiler tarball and build it 
	gccDirectory = "gcc-2.7.2.3"
	expandTarball(gccDirectory + ".tar.gz")

	os.chdir(gccDirectory)
	debug.verboseMessage("Building GCC '" + gccDirectory + "'")

	pathExtension = os.pathsep + installDirectory + os.sep + "sslittle-na-sstrix" + os.sep + "bin"
	os.environ['PATH'] += pathExtension
	debug.verboseMessage("Added '" + pathExtension + "' to environment variable PATH")

	runCommand("./configure --host=i386-*-linux --target=sslittle-na-sstrix --with-gnu-as --with-gnu-ld --enable-languages=c --prefix=" + installDirectory)
	runCommand("make all")
	runCommand("make install")

	goToParentDirectory()

	# Make symbolic links to SimpleScalar binaries
	debug.verboseMessage("Making symbolic links")

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
else:
	debug.verboseMessage("SimpleScalar is already installed")

debug.verboseMessage("My work is done here")

