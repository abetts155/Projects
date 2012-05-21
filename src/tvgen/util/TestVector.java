package tvgen.util;

import java.util.Arrays;

public class TestVector implements Comparable<TestVector> {

	private int[] vector;
	private double score;
	private boolean scored;
	private double time;
	
	public TestVector(int[] vector) {
		this.vector = vector;
		score = 0;
		scored = false;
		time = 0;
	}
	
	public double getScore() {
		return score;
	}
	
	public synchronized void setScore(double score) {
		if(!scored) {
			this.score = score;
			scored = true;
		} else {
			SystemOutput.errorMessage("Trying to set score for test vector twice");
		}
	}
	
	public int[] getVector() {
		return vector;
	}
	
	public boolean equalsVector(int[] v) {
		return Arrays.equals(vector, v);
	}
	
	public boolean hasBeenScored() {
		return scored;
	}
	
	@Override
	public String toString() {
		String vec = "";
		for (int num : vector) {
			vec += num + " ";
		}
		return vec.trim();
	}

	@Override
	public int compareTo(TestVector o) {
		return Double.compare(score, o.getScore());
	}

	public double getTime() {
		return time;
	}

	public void setTime(double time) {
		this.time = time;
	}

}
