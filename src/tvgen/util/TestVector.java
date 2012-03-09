package tvgen.util;

import java.util.Arrays;

public class TestVector implements Comparable<TestVector> {

	private int[] vector;
	private double score;
	private boolean scored;
	
	public TestVector(int[] vector) {
		this.vector = vector;
		score = 0;
		scored = false;
	}
	
	public double getScore() {
		return score;
	}
	
	public void setScore(double score) {
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

}
