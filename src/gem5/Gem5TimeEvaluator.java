package gem5;

import tvgen.util.TestVector;

public class Gem5TimeEvaluator extends Gem5Evaluator {

	public Gem5TimeEvaluator(int threadID, String programName, String cpuType,
			Gem5Tools g5tools) {
		super(threadID, programName, cpuType, g5tools);
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

}
