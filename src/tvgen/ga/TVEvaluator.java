package tvgen.ga;

import java.util.LinkedList;
import java.util.List;

import tvgen.util.TestVector;

public abstract class TVEvaluator implements Runnable {
	
	private int threadID;
	private List<TestVector> vectors;
	
	public TVEvaluator(int threadID) {
		this.threadID = threadID;
		vectors = new LinkedList<TestVector>();
	}
	
	public void addTV(TestVector vector) {
		vectors.add(vector);
	}
	
	public int getThreadID() {
		return threadID;
	}
	
	@Override
	public void run() {
		for(TestVector vec : vectors) {
			evaluate(vec);
		}
		vectors.clear();
	}
	
	abstract protected void evaluate(TestVector vector);
}
