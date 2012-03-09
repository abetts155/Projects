package tvgen.ga;

public interface TVCrossover {
	/**
	 * Crosses the two supplied test vectors and returns the result
	 * @param t1
	 * @param t2
	 */
	public int[] crossVectors(int[] t1, int[] t2);
}
