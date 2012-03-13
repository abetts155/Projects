package tvgen.random;

import tvgen.WCETFinder;
import tvgen.util.RandomGenerator;

public class RandomWCETFinder extends WCETFinder {

	private int numVecsToGen;
	
	public RandomWCETFinder(int threadID, String programName, String cpuType,
			int numVecsToGen) {
		super(threadID, programName, cpuType);
		this.numVecsToGen = numVecsToGen;
	}
	
	@Override
	public void run() {
		RandomGenerator rand = new RandomGenerator();
		for(int i = 0; i < numVecsToGen; i++) {
			evaluateVector(rand.generateRandomVector(getVectorLength(),
					getLowerBound(), getUpperBound()));
		}
	}

}
