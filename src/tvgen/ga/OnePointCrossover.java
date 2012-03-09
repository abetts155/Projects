package tvgen.ga;

import tvgen.TestVectorGenerator;
import tvgen.util.RandomGenerator;
import tvgen.util.SystemOutput;

public class OnePointCrossover implements TVCrossover {

	@Override
	public int[] crossVectors(int[] t1, int[] t2) {
		RandomGenerator rand = TestVectorGenerator.getRandomGenerator();
		
		if(t1.length != t2.length) {
			SystemOutput.exitWithError("Error: trying to cross vectors of different lengths");
		}
		
		int splitIndex = rand.generateRandomInt(0, t1.length - 1);
		
		int[] newVector = new int[t1.length];
		System.arraycopy(t1, 0, newVector, 0, splitIndex);
		System.arraycopy(t2, splitIndex, newVector, splitIndex, t2.length - splitIndex);
		
		return newVector;
	}

}
