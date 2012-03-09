package tvgen.random;

import java.io.File;

import tvgen.TestVectorGenerator;
import tvgen.util.*;

public class RandomTVGenerator extends TestVectorGenerator {
	
	private int numVectors;
	
	public RandomTVGenerator(File outFile) {
		super(outFile);
		numVectors = 100;
	}
	
	@Override
	public void generate() {
		SystemOutput.debugMessage("Generating test vectors");
		
		RandomGenerator rand = TestVectorGenerator.getRandomGenerator();
		
		for(int i = 0; i < numVectors; i++) {
			TVs.add(rand.generateRandomVector(getVectorLength(), getLowerBound(), getUpperBound()));
		}
	}
	
	public void setNumberOfVectors(int num) {
		numVectors = num;
	}

}
