package se.mdh.calculations;

import java.io.IOException;
import java.util.HashMap;

import se.mdh.graphs.IpointGraph;
import se.mdh.graphs.trees.DepthFirstTree;
import se.mdh.outputs.WCETOutput;
import se.mdh.programs.Program;
import se.mdh.programs.Subprogram;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Globals;

public class CalculationEngine
{
	protected Database database;
	protected Program program;
	protected DepthFirstTree dfs;
	protected HashMap<String, IPETModel> ipetModels = new HashMap<String, IPETModel> ();

	public CalculationEngine (IPGDatabase database, Program program)
	{
		this.program = program;
		this.database = database;
		dfs = new DepthFirstTree (program.getCallGraph (), program.getRootID ());
	}

	public void calculateWCETWithIPGs ()
	{
		try
		{
			for (int i = 1; i <= dfs.numOfVertices (); ++i)
			{
				int subprogramID = dfs.getPostVertexID (i);
				Subprogram subprogram = program.getSubprogram (subprogramID);
				String subprogramName = subprogram.getSubprogramName ();

				Debug.debugMessage (getClass (), "Building IPET of "
						+ subprogramName, 3);

				IpointGraph ipg = subprogram.getIPG (Globals
						.getInstrumentationProfile ());
				IPETModelIPG ipet = new IPETModelIPG (
						this,
						database,
						ipg,
						subprogram.getCFGStar (
								Globals.getInstrumentationProfile ()).getLNT (),
						subprogramID, subprogramName);
				ipetModels.put (subprogramName, ipet);

				Debug.debugMessage (getClass (), "WCET(" + subprogramName
						+ ") = " + ipet.wcet, 3);

				float edgesCovered = database.unitsCovered (subprogramID);
				float traceEdges = ipg.numberOfTraceEdges ();

				/*
				 * Some IPGs do not have any trace edges. For example, a CFG*
				 * with a single basic block. In this case, we have to ensure
				 * that the divisor is not 0
				 */
				if (traceEdges == 0)
				{
					traceEdges = 1;
				}

				long tests = database.getTests (subprogramID);
				long MET = ((IPGDatabase) database).getMET (subprogramID);

				WCETOutput.writeTimingData (subprogramName, tests, MET,
						ipet.wcet, edgesCovered / traceEdges * 100.0);

				if (subprogramID == program.getRootID ())
				{
					WCETOutput.writeToGNUPlotFile (tests, MET, ipet.wcet);
				}
			}

			WCETOutput.writeRunDelimiter ();
		}
		catch (IOException e)
		{
			e.printStackTrace ();
			System.exit (1);
		}
	}



	public final long getWCET (String subprogramName)
	{
		return ipetModels.get (subprogramName).wcet;
	}

	public final long getWCET (int subprogramID)
	{
		for (String subprogramName: ipetModels.keySet ())
		{
			if (program.getSubprogram (subprogramName).getSubprogramID () == subprogramID)
			{
			
				return ipetModels.get (subprogramName).wcet;
			}
		}
		return 0;
	}
}
