package tvgen.random;

import gem5.Gem5Tools;
import tvgen.WCETFinder;
import tvgen.util.RandomGenerator;

public class RandomWCETFinder extends WCETFinder {

	private int numVecsToGen;
	
	public RandomWCETFinder(int threadID, String programName, Gem5Tools g5Tools,
			int numVecsToGen) {
		super(threadID, programName, g5Tools);
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
