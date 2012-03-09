package tvgen.ga;

import tvgen.util.TestVector;

/**
 * Used to select which test vectors in a generation will be used for
 * crossover and mutation
 * @author chris
 *
 */
public interface TVSelector {

	/**
	 * Selects vectors to be used for crossover and mutation
	 * @param vectors - The vectors selected from
	 * @param numVectors - The number of vectors to select
	 * @return - An array of length numVectors containing the selected TestVectors
	 */
	public TestVector[] selectVectors(TestVector[] vectors, int numVectors);
	
}
