package tvgen.ga;

import java.util.Arrays;

import tvgen.util.SystemOutput;
import tvgen.util.TestVector;

public class ElitistSelector implements TVSelector {

	@Override
	public TestVector[] selectVectors(TestVector[] vectors, int numVectors) {
		SystemOutput.debugMessage("Selecting " + numVectors + " vectors");
		
		TestVector[] selectedVectors = new TestVector[numVectors];
		
		TestVector[] sorted = vectors.clone();
		Arrays.sort(sorted);
		
		for(int i = 0; i < selectedVectors.length; i++) {
			selectedVectors[i] = sorted[sorted.length - 1 - i];
		}
		
		return selectedVectors;
	}

}
