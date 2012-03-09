package tvgen.ga;

import tvgen.TestVectorGenerator;
import tvgen.util.RandomGenerator;

public class RandomMutator extends Mutator {

	RandomGenerator rand;
	
	public RandomMutator() {
		super();
		rand = TestVectorGenerator.getRandomGenerator();
	}
	
	@Override
	public int mutateValue(int value, int min, int max) {
		return rand.generateRandomInt(min, max);
	}

}
