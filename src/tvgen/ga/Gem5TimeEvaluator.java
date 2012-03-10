package tvgen.ga;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.zip.GZIPOutputStream;

import tvgen.util.SystemOutput;
import tvgen.util.TestVector;

public class Gem5TimeEvaluator extends TVEvaluator {

	public static final String gem5EnvVar = "GEM5_HOME";
	public static final String wcetEnvVar = "WCET_HOME";
	
	private String gem5Home;
	private String gem5Binary;
	private String gem5TraceFlags;
	private String gem5ConfigScript;
	private String gem5TraceParser;
	
	private String programName;
	private String cpuType;
	
	private String wcetHome;
	
	private static PrintStream traceOutput = null;
	private static String traceFileName = "trace.BASIC_BLOCK.GA.gz";
	
	public Gem5TimeEvaluator(int threadID, String programName, String cpuType) {
		super(threadID);
		gem5Home = System.getenv(gem5EnvVar);
		if(gem5Home == null) {
			SystemOutput.exitWithError("Error: could not find environment variable "
							+ gem5EnvVar + " which is needed to run gem5");
		}
		
		wcetHome = System.getenv(wcetEnvVar);
		if(wcetHome == null) {
			SystemOutput.exitWithError("Error: could not find environment variable "
							+ wcetEnvVar + " which is needed to run program");
		}
		
		gem5Binary = gem5Home + "/build/ARM/gem5.opt";
		gem5TraceFlags = "--debug-flags=ExecEnable,ExecUser,ExecTicks,ExecMicro";
		gem5ConfigScript = wcetHome + "/scripts/gem5Config/se.py";
		gem5TraceParser = wcetHome + "/scripts/gem5TraceParser.py";
		
		this.programName = programName;
		this.cpuType = cpuType;
		
		checkFiles();
	}
	
	public void evaluate(TestVector vector) {
		String cmd = gem5Binary + " " + gem5TraceFlags + " --trace-file=trace.out" +
				" -d m5out/thread" + getThreadID() + " " + gem5ConfigScript + " -c " +
				programName + " --cpu-type=" + cpuType;
		if(cpuType.equals("detailed") || cpuType.equals("inorder")) {
			cmd += " --caches";
		}
		cmd += " " + vector.toString();
		
		SystemOutput.debugMessage("Running :" + cmd);
		
		try {
			Process child = Runtime.getRuntime().exec(cmd);
			
			// Get process output streams
			BufferedReader outputReader = 
					new BufferedReader(new InputStreamReader(child.getInputStream()));

			waitForProcess(child, cmd);
			
			for(String line = outputReader.readLine(); line != null;
					line = outputReader.readLine()) {
				if(line.startsWith("Exiting @ tick")) {
					String scoreString = line.split("\\s+", 5)[3];
					vector.setScore(Double.parseDouble(scoreString));
					sanitiseTrace();
					return;
				}
			}
			
			SystemOutput.exitWithError("Error could not extract score from cmd " + cmd);
			
		} catch(NumberFormatException nfe) {
			SystemOutput.exitWithError("Format error when extracting score from cmd " + cmd);
		} catch(Exception e) {
			SystemOutput.exitWithError("Error running command " + cmd + "\n " + e.getMessage());
		}
	}
	
	private void sanitiseTrace() {
		String cmd = "python " + gem5TraceParser + " -p " + programName +
				".xml -t m5out/thread" + getThreadID() + "/trace.out";
		
		SystemOutput.debugMessage("Running command: " + cmd);
		
		try {
			Process child = Runtime.getRuntime().exec(cmd);
			
			BufferedReader outputReader = 
					new BufferedReader(new InputStreamReader(child.getInputStream()));
			
			waitForProcess(child, cmd);
			
			//Append trace to compressed file
			addToTraceOutput(outputReader);
		} catch(Exception e) {
			SystemOutput.exitWithError("Error running command " + cmd + "\n " + e.getMessage());
		}
	}
	
	private void waitForProcess(Process p, String cmd) {
		try {
			p.waitFor();
			if(p.exitValue() != 0) {
				BufferedReader outputReader = 
						new BufferedReader(new InputStreamReader(p.getInputStream()));
				BufferedReader errorReader = 
						new BufferedReader(new InputStreamReader(p.getErrorStream()));
				
				SystemOutput.debugMessage("\nCmd output stream:");
				for(String line = outputReader.readLine(); line != null;
						line = outputReader.readLine()) {
					SystemOutput.debugMessage(line);
				}
				
				SystemOutput.debugMessage("\nCmd error stream:");
				for(String line = errorReader.readLine(); line != null;
						line = errorReader.readLine()) {
					SystemOutput.debugMessage(line);
				}
				
				SystemOutput.exitWithError("\nError: Command " + cmd +
						"\nTerminated with exit code " + p.exitValue());
			}
		} catch (Exception e) {
			SystemOutput.exitWithError("Error running command " + cmd + "\n " + e.getMessage());
		}
	}
	
	private void checkFiles() {
		File programBin = new File(programName);
		if(!programBin.exists()) {
			SystemOutput.exitWithError("Error: file " + programName + " does not exist");
		}
		
		File g5binary = new File(gem5Binary);
		if(!g5binary.exists()) {
			SystemOutput.exitWithError("Error: file " + gem5Binary + " does not exist");
		}
		
		File config = new File(gem5ConfigScript);
		if(!config.exists()) {
			SystemOutput.exitWithError("Error: file " + gem5ConfigScript + " does not exist");
		}
		
		File parser = new File(gem5TraceParser);
		if(!parser.exists()) {
			SystemOutput.exitWithError("Error: file " + gem5TraceParser + " does not exist");
		}
	}
	
	private synchronized static void addToTraceOutput(BufferedReader content) {
		if(traceOutput == null) {
			try {
				GZIPOutputStream outStream = new GZIPOutputStream(
						new FileOutputStream(traceFileName));
				traceOutput = new PrintStream(outStream);
				
				// Ensure trace output stream is closed at end of execution
				Runtime.getRuntime().addShutdownHook(new Thread() {
					public void run() { closeTraceOutput(); }
				});
			} catch(Exception e) {
				SystemOutput.exitWithError("Error creating trace output file");
			}
		}
		
		try {
			for(String line = content.readLine(); line != null;
					line = content.readLine()) {
				traceOutput.println(line);
			}
		} catch (Exception e) {
			SystemOutput.exitWithError("Error wrting to trace output");
		}
	}
	
	private static void closeTraceOutput() {
		if(traceOutput != null) {
			traceOutput.close();
		}
	}

}
