package tvgen;

import gem5.Gem5Tools;
import tvgen.util.*;

public abstract class WCETFinder implements Runnable {

	private int threadID;
	
	private int vectorLength;
	private int upperBound;
	private int lowerBound;
	
	private Gem5Tools g5Tools;
	
	public WCETFinder(int threadID, String programName, Gem5Tools g5Tools) {
		this.threadID = threadID;
		
		upperBound = Integer.MAX_VALUE;
		lowerBound = Integer.MIN_VALUE;
		
		vectorLength = 10;
		
		this.g5Tools = g5Tools;
	}
	
	@Override
	abstract public void run();
	
	public void evaluateVector(TestVector vector) {
		double score = g5Tools.runGem5(vector.toString(), "m5out/thread" + threadID,
				"trace.out");
		
		vector.setScore(score);
		
		FindWCETVector.logVector(vector);
	}

	public void setVectorLength(int vectorLength) {
		this.vectorLength = vectorLength;
	}

	public void setUpperBound(int upperBound) {
		this.upperBound = upperBound;
	}

	public void setLowerBound(int lowerBound) {
		this.lowerBound = lowerBound;
	}

	public int getVectorLength() {
		return vectorLength;
	}

	public int getUpperBound() {
		return upperBound;
	}

	public int getLowerBound() {
		return lowerBound;
	}

}
