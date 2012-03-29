package gem5;


import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.zip.GZIPOutputStream;

import tvgen.ga.TVEvaluator;
import tvgen.util.*;

public abstract class Gem5Evaluator extends TVEvaluator {

	private String programName;
	protected String cpuType;
	
	protected Gem5Tools g5Tools;
	
	private static PrintStream traceOutput = null;
	private static String traceFileName = "trace.BASIC_BLOCK.GA.gz";
	
	public Gem5Evaluator(int threadID, String programName, String cpuType,
			Gem5Tools g5tools) {
		super(threadID);
		
		this.programName = programName;
		this.cpuType = cpuType;
		
		this.g5Tools = g5tools;
		
		checkFiles();
	}
	
	public abstract void evaluate(TestVector vector);
	
	private void checkFiles() {
		File programBin = new File(programName);
		if(!programBin.exists()) {
			SystemOutput.exitWithError("Error: file " + programName + " does not exist");
		}
	}
	
	protected synchronized static void addToTraceOutput(String content) {
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
