package gem5;

import java.util.Map;
import java.util.Set;

import tvgen.util.TestVector;

public class Gem5CovCountEval extends Gem5CoverageEvaluator {

	public Gem5CovCountEval(int threadID, String programName,
			Gem5Tools g5tools, String entryPoint) {
		super(threadID, programName, g5tools, entryPoint);
	}

	@Override
	public void evaluate(TestVector vector) {
		double time = g5Tools.runGem5(vector.toString(), "m5out/thread" + getThreadID(),
				"trace.out");
		
		Map<Integer,Integer> blockCounts = g5Tools.getBlockCoverageCount(traceFile);
		
		Set<Integer> blocksCovered = blockCounts.keySet();
		blocksCovered.retainAll(basicBlocks);
		
		int score = 0;
		for (Integer bbId : blocksCovered)
		{
			score += blockCounts.get(bbId);
		}
		
		long entryTime = g5Tools.getInstructionTimeDiff(firstInst, lastInst, traceFile);
		
		vector.setScore((double)score);
		vector.setTime(entryTime);
		String traceOutput = g5Tools.sanitiseGem5Trace(traceFile, "BASIC_BLOCK");
		
		//Append trace to compressed file
		addToTraceOutput(traceOutput);
	}

}
