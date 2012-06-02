package gem5;

import tvgen.util.SystemOutput;
import tvgen.util.TestVector;

public class Gem5TimeEvaluator extends Gem5Evaluator {

	String traceFile;
	
	public Gem5TimeEvaluator(int threadID, String programName, Gem5Tools g5tools,
			String entryPoint) {
		super(threadID, programName, g5tools, entryPoint);
		traceFile = "m5out/thread" + threadID + "/trace.out";
	}
	
	public void evaluate(TestVector vector) {
		double time = g5Tools.runGem5(vector.toString(), "m5out/thread" + getThreadID(),
				"trace.out");

		long entryTime = g5Tools.getInstructionTimeDiff(entryBlockInsts, traceFile);
		
		vector.setScore(entryTime);
		vector.setTime(entryTime);
		String traceOutput = g5Tools.sanitiseGem5Trace(traceFile, "BASIC_BLOCK");
		
		//Append trace to compressed file
		addToTraceOutput(vector, traceOutput);
	}
}
