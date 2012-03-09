package tvgen.ga;

import tvgen.TestVectorGenerator;
import tvgen.util.*;

public class RandomSelector implements TVSelector {

	@Override
	public TestVector[] selectVectors(TestVector[] vectors, int numVectors) {
		TestVector[] selectedVectors = new TestVector[numVectors];
		RandomGenerator rand = TestVectorGenerator.getRandomGenerator();
		
		for(int i = 0; i < selectedVectors.length; i++) {
			selectedVectors[i] = vectors[rand.generateRandomInt(0, vectors.length-1)];
		}
		
		return selectedVectors;
	}

}
