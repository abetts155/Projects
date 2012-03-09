package tvgen.ga;

import tvgen.util.SystemOutput;

public abstract class Mutator {

	public abstract int mutateValue(int value, int min, int max);
	
	protected void checkBounds(int min, int max) {
		if(max < min) {
			SystemOutput.exitWithError("Error with bounds in mutator");
		}
	}
	
}
