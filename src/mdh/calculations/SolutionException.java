package se.mdh.calculations;

public class SolutionException extends Exception
{
	protected final int solution;

	public SolutionException (int solution)
	{
		super ();
		this.solution = solution;
	}
}
