package se.mdh.outputs;

import se.mdh.calculations.CFGDatabase;
import se.mdh.calculations.CalculationEngineAST;
import se.mdh.calculations.CalculationEngineCFG;
import se.mdh.graphs.trees.DepthFirstTree;
import se.mdh.programs.Program;
import se.mdh.programs.Subprogram;

public class AnalysisOutput
{

	public AnalysisOutput (Program program,
			CFGDatabase database,
			CalculationEngineCFG cfgCalculations,
			CalculationEngineAST astCalculations)
	{
		DepthFirstTree dfs = new DepthFirstTree (program.getCallGraph (),
				program.getRootID ());
		for (int i = 1; i <= dfs.numOfVertices (); ++i)
		{
			int subprogramID = dfs.getPostVertexID (i);
			Subprogram subprogram = program.getSubprogram (subprogramID);
			String subprogramName = subprogram.getSubprogramName ();

			System.out.println ("MET(" + subprogramName + ") = "
					+ database.getMET (subprogramID));
			System.out.println ("CFG-ILP: WCET(" + subprogramName + ") = "
					+ cfgCalculations.getWCET (subprogramID));
			System.out.println ("AST-ILP: WCET(" + subprogramName + ") = "
					+ astCalculations.getIPETWCET (subprogramID));
			System.out.println ("AST-TS: WCET(" + subprogramName + ") = "
					+ astCalculations.getTimingSchemaWCET (subprogramID));
		}
	}
}
