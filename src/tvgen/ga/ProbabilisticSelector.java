package tvgen.ga;

import java.util.Arrays;

import tvgen.TestVectorGenerator;
import tvgen.util.*;

public class ProbabilisticSelector implements TVSelector {

	@Override
	public TestVector[] selectVectors(TestVector[] vectors, int numVectors) {
		SystemOutput.debugMessage("Selecting " + numVectors + " vectors");
		
		TestVector[] sortedVecs = vectors.clone();
		Arrays.sort(sortedVecs);
		
		// Calculate the cumulative probability of selecting each vector
		double sumScore = 0.0;
		double[] cumulativeProb = new double[sortedVecs.length];
		for(int i =0; i < sortedVecs.length; i++) {
			sumScore += sortedVecs[i].getScore();
			cumulativeProb[i] = sortedVecs[i].getScore();
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
		selectedVectors[0] = sortedVecs[sortedVecs.length - 1];
		for(int i = 1; i < numVectors; i++) {
			double randNum = rand.generateRandomDouble();
			int j = 0;
			while(randNum > cumulativeProb[j] &&
					j < cumulativeProb.length) {
				j++;
			}
			if(j == cumulativeProb.length) {
				j = cumulativeProb.length - 1;
			}
			selectedVectors[i] = sortedVecs[j];
		}
		
		return selectedVectors;
	}

}
