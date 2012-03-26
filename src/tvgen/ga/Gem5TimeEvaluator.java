package tvgen.ga;

import gem5.Gem5Tools;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.zip.GZIPOutputStream;

import tvgen.util.*;

public class Gem5TimeEvaluator extends TVEvaluator {

	private String programName;
	private String cpuType;
	
	private Gem5Tools g5Tools;
	
	private static PrintStream traceOutput = null;
	private static String traceFileName = "trace.BASIC_BLOCK.GA.gz";
	
	public Gem5TimeEvaluator(int threadID, String programName, String cpuType,
			Gem5Tools g5tools) {
		super(threadID);
		
		this.programName = programName;
		this.cpuType = cpuType;
		
		this.g5Tools = g5tools;
		
		checkFiles();
	}
	
	public void evaluate(TestVector vector) {
		double score = g5Tools.runGem5(vector.toString(), cpuType,
				"m5out/thread" + getThreadID(), "trace.out");
		
		vector.setScore(score);
		String traceOutput = g5Tools.sanitiseGem5Trace(
				"m5out/thread" + getThreadID() + "/trace.out", "BASIC_BLOCK");
		
		//Append trace to compressed file
		addToTraceOutput(traceOutput);
	}
	
	private void checkFiles() {
		File programBin = new File(programName);
		if(!programBin.exists()) {
			SystemOutput.exitWithError("Error: file " + programName + " does not exist");
		}
	}
	
	private synchronized static void addToTraceOutput(String content) {
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
			traceOutput.print(content);
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
