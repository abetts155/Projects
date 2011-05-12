package se.mdh.calculations;

import lpsolve.LpSolve;
import lpsolve.LpSolveException;
import se.mdh.graphs.CFGStar;

public class IPETModelCFGStar extends IPETModel
{
	public IPETModelCFGStar (CFGStar cfgStar)
	{
		try
		{
			/*
			 * The Linear Program Will Always Have at Least |V| Structural
			 * Constraints (Equivalent To Rows) and exactly |V| Variables
			 * (equivalent to Columns).
			 */
			numOfColumns = cfgStar.numOfVertices ();
			lp = LpSolve.makeLp (cfgStar.numOfVertices (), numOfColumns);
		}
		catch (LpSolveException e)
		{
			e.printStackTrace ();
			System.exit (1);
		}
	}
}
