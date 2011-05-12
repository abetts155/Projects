package se.mdh.calculations;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import se.mdh.programs.Program;

public class Database
{
	/*
	 * The program used during trace parsing to populate the database
	 */
	protected Program program;

	/*
	 * The calculation engine used after parsing
	 */
	protected CalculationEngine engine;

	/*
	 * The WCETs of the units of computation: either edges or vertices in the
	 * graph
	 */
	protected HashMap<Integer, HashMap<Integer, Long>> unitWCETs = new HashMap<Integer, HashMap<Integer, Long>> ();

	/*
	 * Loop bounds for each subprogram
	 */
	protected HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> loopBounds = new HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> ();

	/*
	 * Infeasible path data for each subprogram
	 */
	protected HashMap<Integer, HashMap<Integer, HashSet<Integer>>> observedPaths = new HashMap<Integer, HashMap<Integer, HashSet<Integer>>> ();

	/*
	 * To record number of tests for each subprogram
	 */
	protected HashMap<Integer, Long> tests = new HashMap<Integer, Long> ();

	/*
	 * METs of each subprogram
	 */
	protected HashMap<Integer, Long> mets = new HashMap<Integer, Long> ();

	public Database (final Program program)
	{
		this.program = program;
	}

	public final long getUnitWCET (int subprogramID, int unitID)
	{
		return unitWCETs.get (subprogramID).get (unitID);
	}

	public final int getLoopBound (int subprogramID,
			int headerID,
			int ancestorID)
	{
		return loopBounds.get (subprogramID).get (headerID).get (ancestorID);
	}

	public Set<Integer> getInfeasibleUnits (int subprogramID, int unitID)
	{
		Set<Integer> infeasible = new HashSet<Integer> ();
		for (int ID: unitWCETs.get (subprogramID).keySet ())
		{
			/*
			 * Add the vertex or edge ID to the set of infeasible units only if
			 * it was not seen during its execution
			 */
			if (!observedPaths.get (subprogramID).get (unitID).contains (ID))
			{
				infeasible.add (ID);
			}
		}
		return infeasible;
	}

	public final int unitsCovered (int subprogramID)
	{
		int count = 0;
		for (int edgeID: unitWCETs.get (subprogramID).keySet ())
		{
			if (unitWCETs.get (subprogramID).get (edgeID) != 0)
			{
				count++;
			}
		}
		return count;
	}

	public final boolean isCovered (int subprogramID, int unitID)
	{
		return unitWCETs.get (subprogramID).get (unitID) > 0;
	}

	public final long getTests (int subprogramID)
	{
		return tests.get (subprogramID);
	}

	public final long getMET (int subprogramID)
	{
		return mets.get (subprogramID);
	}
}
