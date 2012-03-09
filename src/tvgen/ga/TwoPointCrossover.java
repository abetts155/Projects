package tvgen.ga;

import tvgen.TestVectorGenerator;
import tvgen.util.RandomGenerator;
import tvgen.util.SystemOutput;

public class TwoPointCrossover implements TVCrossover {

	@Override
	public int[] crossVectors(int[] t1, int[] t2) {
		RandomGenerator rand = TestVectorGenerator.getRandomGenerator();
		
		if(t1.length != t2.length) {
			SystemOutput.exitWithError("Error: trying to cross vectors of different lengths");
		}
		
		int crossIndex1 = rand.generateRandomInt(0, t1.length - 1);
		int crossIndex2 = rand.generateRandomInt(0, t1.length - 1);
		
		int lowIndex = Math.min(crossIndex1, crossIndex2);
		int highIndex = Math.max(crossIndex1, crossIndex2);
		
		if(lowIndex == highIndex && t1.length > 1) {
			if(highIndex < t1.length - 1) {
				highIndex++;
			} else {
				lowIndex--;
			}
		}
		
		int[] newVector = new int[t1.length];
		System.arraycopy(t1, 0, newVector, 0, lowIndex);
		System.arraycopy(t2, lowIndex, newVector, lowIndex, highIndex - lowIndex);
		System.arraycopy(t1, highIndex, newVector, highIndex, t2.length - highIndex);
		
		return newVector;
	}

}
