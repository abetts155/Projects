package tvgen.ga;

import tvgen.util.TestVector;

public interface TVEvaluator {
	
	/**
	 * Evaluates the given test vector and returns its score
	 * @param vector
	 * @return
	 */
	public void evaluateTV(TestVector vector);
}
