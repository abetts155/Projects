package tvgen.util;

import java.util.Random;

/**
 * Utility for generating random numbers and test vectors
 * @author chris
 *
 */
public class RandomGenerator {
	
	Random randomGen;
	
	public RandomGenerator() {
		randomGen = new Random();
	}
	
	public TestVector[] generateRandomVectors(int numVectors, int length, int min, int max) {
		TestVector[] vectors = new TestVector[numVectors];
		for(int i = 0; i < vectors.length; i++) {
			vectors[i] = generateRandomVector(length, min, max);
		}
		return vectors;
	}
	
	public TestVector generateRandomVector(int length, int min, int max) {
		int[] vector = new int[length];
		
		for (int i = 0; i < vector.length; i++) {
			vector[i] = generateRandomInt(min, max);
		}
		
		return new TestVector(vector);
	}
	
	public int generateRandomInt(int min, int max) {
		int range = max - min;
		if (range < 0) {
			SystemOutput.exitWithError("Error generating random number: range < 0");
		}
		int random = randomGen.nextInt(range + 1);
		return random + min;
	}
	
	/**
	 * Generate random double between 0.0 and 1.0 inclusive
	 * @return
	 */
	public double generateRandomDouble() {
		return randomGen.nextDouble();
	}
}
