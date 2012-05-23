package gem5;

import java.util.Set;

import tvgen.util.TestVector;

public class Gem5CovFractionEval extends Gem5CoverageEvaluator {

	public Gem5CovFractionEval(int threadID, String programName,
			Gem5Tools g5tools, String entryPoint) {
		super(threadID, programName, g5tools, entryPoint);
	}
	
	@Override
	public void evaluate(TestVector vector) {
		double time = g5Tools.runGem5(vector.toString(), "m5out/thread" + getThreadID(),
				"trace.out");
		
		Set<Integer> blocksCovered = g5Tools.getBlockCoverage(traceFile);
		
		blocksCovered.retainAll(basicBlocks);
		int relevantBlocksCovered = blocksCovered.size();
		
		long entryTime = g5Tools.getInstructionTimeDiff(firstInst, lastInst, traceFile);
		
		vector.setScore((double)relevantBlocksCovered / (double)basicBlocks.size());
		vector.setTime(entryTime);
		String traceOutput = g5Tools.sanitiseGem5Trace(traceFile, "BASIC_BLOCK");
		
		//Append trace to compressed file
		addToTraceOutput(traceOutput);
	}

}
