package tvgen.ga;

import tvgen.TestVectorGenerator;
import tvgen.util.*;

public class ProbabilisticSelector implements TVSelector {

	@Override
	public TestVector[] selectVectors(TestVector[] vectors, int numVectors) {
		SystemOutput.debugMessage("Selecting " + numVectors + " vectors");
		
		// Calculate the cumulative probability of selecting each vector
		double sumScore = 0.0;
		double[] cumulativeProb = new double[vectors.length];
		for(int i =0; i < vectors.length; i++) {
			sumScore += vectors[i].getScore();
			cumulativeProb[i] = vectors[i].getScore();
		}
		
		cumulativeProb[0] /= sumScore;
		for(int i = 1; i < cumulativeProb.length; i++) {
			cumulativeProb[i] /= sumScore;
			cumulativeProb[i] += cumulativeProb[i - 1];
		}
		
		RandomGenerator rand = TestVectorGenerator.getRandomGenerator();
		
		// Generate a random number between 0.0 and 1.0 and pick first vector 
		// who's cumulative probability is greater
		TestVector[] selectedVectors = new TestVector[numVectors];
		for(int i = 0; i < numVectors; i++) {
			double randNum = rand.generateRandomDouble();
			int j = 0;
			while(randNum > cumulativeProb[j] &&
					j < cumulativeProb.length) {
				j++;
			}
			if(j == cumulativeProb.length) {
				j = cumulativeProb.length - 1;
			}
			selectedVectors[i] = vectors[j];
		}
		
		return selectedVectors;
	}

}
