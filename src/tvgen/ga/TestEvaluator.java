package tvgen.ga;

import tvgen.util.TestVector;

public class TestEvaluator implements TVEvaluator {

	@Override
	public void evaluateTV(TestVector vector) {
		double sum = 0.0;
		int[] vec = vector.getVector();
		for(int i = 0; i < vec.length; i++) {
			sum += vec[i];
		}
		vector.setScore(sum / vec.length);
	}

}
