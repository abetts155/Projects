package tvgen.ga;

import java.util.HashSet;
import java.util.Set;

import tvgen.util.SystemOutput;
import tvgen.util.TestVector;

public class GenerationEvaluator {

	private TVEvaluator[] evaluators;
	
	public GenerationEvaluator(TVEvaluator[] evaluators) {
		this.evaluators = evaluators;
	}
	
	public void evaluateGeneration(TestVector[] generation) {
		Set<TestVector> tvs = new HashSet<TestVector>();
		for(int i = 0; i < generation.length; i++) {
			if(!generation[i].hasBeenScored()) {
				tvs.add(generation[i]);
			}
		}
		
		int j = 0;
		for(TestVector vec : tvs) {
			evaluators[j % evaluators.length].addTV(vec);
			j++;
		}
		
		// Start all threads
		Thread[] threads = new Thread[evaluators.length];
		for(int i = 0; i < evaluators.length; i++) {
			threads[i] = new Thread(evaluators[i]);
			threads[i].start();
		}
		
		// Wait for all threads to finish
		for(int i = 0; i < threads.length; i++) {
			try {
				threads[i].join();
			} catch (Exception e) {
				SystemOutput.exitWithError("Error runnning evaluator thread: " +
										e.getMessage());
			}
		}
	}
	
}
