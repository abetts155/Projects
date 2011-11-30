package adam.betts.outputs;

import java.util.Iterator;

import adam.betts.calculations.CFGDatabase;
import adam.betts.calculations.CalculationEngineAST;
import adam.betts.calculations.CalculationEngineCFG;
import adam.betts.calculations.LoopBoundDatabase;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainLoopAnalyser;
import adam.betts.utilities.Globals;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class AnalysisOutput
{
	public AnalysisOutput (Program program, CFGDatabase database,
			CalculationEngineCFG cfgCalculations, CalculationEngineAST astCalculations)
	{
		DepthFirstTree dfs = new DepthFirstTree (program.getCallGraph (), program.getRootID ());
		for (int i = 1; i <= dfs.numOfVertices (); ++i)
		{
			int subprogramID = dfs.getPostVertexID (i);
			Subprogram subprogram = program.getSubprogram (subprogramID);
			String subprogramName = subprogram.getSubprogramName ();

			System.out.println ("MET(" + subprogramName + ") = " + database.getMET (subprogramID));
			System.out.println ("CFG-ILP: WCET(" + subprogramName + ") = "
					+ cfgCalculations.getWCET (subprogramID));
			System.out.println ("AST-ILP: WCET(" + subprogramName + ") = "
					+ astCalculations.getIPETWCET (subprogramID));
			System.out.println ("AST-TS: WCET(" + subprogramName + ") = "
					+ astCalculations.getTimingSchemaWCET (subprogramID));
		}
	}

	public AnalysisOutput (Program program, LoopBoundDatabase database)
	{
		DepthFirstTree dfs = new DepthFirstTree (program.getCallGraph (), program.getRootID ());

		System.out.println ("**************************");
		System.out.println ("***** BOUND LEVEL: " + MainLoopAnalyser.getBoundLevel () + " *****");
		System.out.println ("**************************\n");

		System.out.println ("************************************");
		System.out.println ("***** TIME TO PARSE: " + database.getParseTime () + " *****");
		System.out.println ("************************************\n");

		final String headerColumn = "Header";
		final String ancestorColumn = "Ancestor";
		final String boundColumn = "Bound";

		System.out.println ("********** BOUNDS ******************");
		for (int i = 1; i <= dfs.numOfVertices (); i++)
		{
			int subprogramID = dfs.getPreVertexID (i);
			Subprogram subprogram = program.getSubprogram (subprogramID);

			System.out.println ("Subprogram: " + subprogram.getSubprogramName ().toUpperCase ());

			LoopNests lnt = subprogram.getCFGStar (Globals.getInstrumentationProfile ()).getLNT ();

			if (lnt.numOfLoops () == 1)
			{
				System.out.println ("<No loops>");
			} else
			{
				final String tableHeader = "|" + headerColumn + "|" + ancestorColumn + "|"
						+ boundColumn + "|";

				printRowDivider (tableHeader.length ());
				System.out.println (tableHeader);
				printRowDivider (tableHeader.length ());

				for (int level = lnt.getHeight () - 1; level > 0; --level)
				{
					Iterator <TreeVertex> levelIt = lnt.levelIterator (level);
					while (levelIt.hasNext ())
					{
						TreeVertex v = levelIt.next ();

						if (v instanceof HeaderVertex)
						{
							HeaderVertex headerv = (HeaderVertex) v;
							int headerID = headerv.getHeaderID ();
							int vertexID = headerv.getVertexID ();

							for (int ancestorID : lnt.getProperAncestors (vertexID))
							{
								HeaderVertex ancestorv = (HeaderVertex) lnt.getVertex (ancestorID);

								if (headerv.getLevel () - ancestorv.getLevel () <= MainLoopAnalyser
										.getBoundLevel ())
								{
									System.out.print ("|");
									System.out.print (headerv.getHeaderID ());
									printGap (headerColumn.length ()
											- Integer.toString (headerv.getHeaderID ()).length ());
									System.out.print ("|");

									System.out.print (ancestorv.getHeaderID ());
									printGap (ancestorColumn.length ()
											- Integer.toString (ancestorv.getHeaderID ()).length ());
									System.out.print ("|");

									int bound = database.getBound (subprogramID, vertexID,
											ancestorID);
									System.out.print (bound);
									printGap (boundColumn.length ()
											- Integer.toString (bound).length ());
									System.out.println ("|");
								}
							}
						}
					}
				}

				printRowDivider (tableHeader.length ());
			}

			if (i < dfs.numOfVertices ())
			{
				System.out.println ();
			}
		}
		System.out.println ("********************************");
	}

	private void printRowDivider (int length)
	{
		for (int i = 0; i < length; ++i)
		{
			System.out.print ('-');
		}
		System.out.println ();
	}

	private void printGap (int distance)
	{
		for (int i = 0; i < distance; ++i)
		{
			System.out.print (" ");
		}
	}
}
