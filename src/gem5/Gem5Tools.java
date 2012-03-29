package gem5;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.Set;

import tvgen.util.SystemOutput;

public class Gem5Tools {

	public static final String gem5EnvVar = "GEM5_HOME";
	public static final String wcetEnvVar = "WCET_HOME";
	
	private TraceParser traceParser;
	private String programName;
	
	private String gem5Home;
	private String gem5Binary;
	private String gem5TraceFlags;
	private String gem5ConfigScript;
//	private String gem5TraceParser;
	
	private String wcetHome;
	
	public Gem5Tools(String programName) {
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
//		gem5TraceParser = wcetHome + "/scripts/gem5TraceParser.py";
		
		this.programName = programName;
		traceParser = new TraceParser(programName + ".xml");
		
		checkFiles();
	}
	
	public double runGem5(String programArgs, String cpuType,
			String outputFolder, String traceFile) {
		String cmd = gem5Binary + " " + gem5TraceFlags + " --trace-file=" + traceFile +
				" -d " + outputFolder + " " + gem5ConfigScript + " -c " +
				programName + " --cpu-type=" + cpuType;
		if(cpuType.equals("detailed") || cpuType.equals("inorder")) {
			cmd += " --caches";
		}
		cmd += " " + programArgs;
		
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
					return Double.parseDouble(scoreString);
				}
			}
			
			SystemOutput.exitWithError("Error could not extract score from cmd " + cmd);
			
		} catch(NumberFormatException nfe) {
			SystemOutput.exitWithError("Format error when extracting score from cmd " + cmd);
		} catch(Exception e) {
			SystemOutput.exitWithError("Error running command " + cmd + "\n " + e.getMessage());
		}
		
		return 0.0;
	}
	
//	public BufferedReader sanitiseGem5Trace(String traceFile) {
//		String cmd = "python " + gem5TraceParser + " -p " + programName +
//				".xml -t " + traceFile;
//		
//		SystemOutput.debugMessage("Running command: " + cmd);
//		
//		try {
//			Process child = Runtime.getRuntime().exec(cmd);
//			
//			BufferedReader outputReader = 
//					new BufferedReader(new InputStreamReader(child.getInputStream()));
//			
//			waitForProcess(child, cmd);
//			
//			//Append trace to compressed file
//			return outputReader;
//		} catch(Exception e) {
//			SystemOutput.exitWithError("Error running command " + cmd + "\n " + e.getMessage());
//		}
//		return null;
//	}
	
	public String sanitiseGem5Trace(String traceFile, String instrumentation) {
		return traceParser.parseTrace(traceFile, instrumentation, false);
	}
	
	public Set<Integer> getBlockCoverage(String traceFile) {
		return traceParser.basicBlocksInTrace(traceFile);
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
		File g5binary = new File(gem5Binary);
		if(!g5binary.exists()) {
			SystemOutput.exitWithError("Error: file " + gem5Binary + " does not exist");
		}
		
		File config = new File(gem5ConfigScript);
		if(!config.exists()) {
			SystemOutput.exitWithError("Error: file " + gem5ConfigScript + " does not exist");
		}
		
//		File parser = new File(gem5TraceParser);
//		if(!parser.exists()) {
//			SystemOutput.exitWithError("Error: file " + gem5TraceParser + " does not exist");
//		}
	}
	
}
