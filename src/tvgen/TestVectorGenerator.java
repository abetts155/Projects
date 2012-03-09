package tvgen;

import java.io.File;
import java.io.PrintStream;
import java.util.LinkedList;
import java.util.List;

import tvgen.util.*;

/**
 * Class which generates and prints a set of test vectors for a given program
 * 
 * @author chris
 *
 */
public abstract class TestVectorGenerator {
	
	protected List<TestVector> TVs;
	private int vectorLength;
	private int upperBound, lowerBound;
	
	private final File outputFile;
	
	private static RandomGenerator randomGenerator = new RandomGenerator();
	
	public TestVectorGenerator(File outFile) {
		this.setVectorLength(20);
		this.upperBound = Integer.MAX_VALUE;
		this.lowerBound = Integer.MIN_VALUE;
		TVs = new LinkedList<TestVector>();
		
		this.outputFile = outFile;
		
		// Set hook to print test vectors to file on exit
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() { printTVs(outputFile); }
		});
	}
	
	public static RandomGenerator getRandomGenerator() {
		return randomGenerator;
	}
	
	/**
	 * Generate a set of test vectors
	 */
	public abstract void generate();
	
	/**
	 * Prints the generated test vectors to the supplied PrintStream
	 * @param p
	 */
	public void printTVs(File f) {
		TestVector highestScoring = null;
		try {
			SystemOutput.debugMessage("Writing TVs to file");
			PrintStream p = new PrintStream(f);
			for(TestVector vec : TVs) {
				p.println(vec.toString());
				highestScoring = updateHighestScoring(highestScoring, vec);
			}
			p.close();
			SystemOutput.printMessage("Highest scoring vector with score = " +
								highestScoring.getScore() + " is " + highestScoring.toString());
			SystemOutput.printMessage("Completed writing TVs to file");
		} catch (Exception e) {
			SystemOutput.errorMessage("Error writing test vectors to file");
		}
	}
	
	private TestVector updateHighestScoring(TestVector current, TestVector tested) {
		if(current == null || tested.getScore() > current.getScore()) {
			return tested;
		}
		return current;
	}

	public int getVectorLength() {
		return vectorLength;
	}

	public void setVectorLength(int vectorLength) {
		this.vectorLength = vectorLength;
	}
	
	public int getUpperBound() {
		return upperBound;
	}

	public void setUpperBound(int upperBound) {
		this.upperBound = upperBound;
	}

	public int getLowerBound() {
		return lowerBound;
	}

	public void setLowerBound(int lowerBound) {
		this.lowerBound = lowerBound;
	}
}
