package tvgen;

import tvgen.util.TestVector;

public class SystematicWCETFinder extends WCETFinder {

	private int upperRange;
	private int lowerRange;
	
	public SystematicWCETFinder(int threadID, String programName, String cpuType,
			int upperRange, int lowerRange) {
		super(threadID, programName, cpuType);
		this.upperRange = upperRange;
		this.lowerRange = lowerRange;
	}
	
	@Override
	public void run() {
		search(0, new int[getVectorLength()]);
	}
	
	private void search(int depth, int[] vector) {
		if(depth == getVectorLength() - 1) {
			for(int i = getLowerBound(); i <= getUpperBound(); i++) {
				vector[depth] = i;
				evaluateVector(new TestVector(vector));
			}
		} else if(depth != 0) {
			for(int i = getLowerBound(); i <= getUpperBound(); i++) {
				vector[depth] = i;
				search(depth + 1, vector);
			}
		} else {
			for(int i = lowerRange; i < upperRange; i++) {
				vector[depth] = i;
				search(depth + 1, vector);
			}
		}
	}

}
