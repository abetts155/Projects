package adam.betts.calculations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import lpsolve.LpSolve;
import lpsolve.LpSolveException;
import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainTraceParser;
import adam.betts.utilities.Debug;
import adam.betts.vertices.BasicBlock;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;

public class CalculationEngineCFG
{
	protected final Program program;
	protected final CallGraph callg;
	protected final Database database;
	protected final HashMap<String, IPETModel> ILPs = new HashMap<String, IPETModel> ();

	public CalculationEngineCFG (Program program, Database database)
	{
		this.program = program;
		this.callg = program.getCallGraph ();
		this.database = database;
		
		DepthFirstTree dfs = new DepthFirstTree (callg, program.getRootID ());
		for (int i = 1; i <= dfs.numOfVertices (); ++i)
		{
			int subprogramID = dfs.getPostVertexID (i);
			Subprogram subprogram = program.getSubprogram (subprogramID);
			String subprogramName = subprogram.getSubprogramName ();

			Debug.debugMessage (getClass (), "Building IPET of "
					+ subprogramName, 3);

			ControlFlowGraph cfg = subprogram.getCFG ();
			IPETModelCFG ilp = new IPETModelCFG (cfg, cfg.getLNT (),
					subprogramID, subprogramName);
			ILPs.put (subprogramName, ilp);

			Debug.debugMessage (getClass (), "CFG-ILP: WCET(" + subprogramName
					+ ") = " + ilp.wcet, 3);
		}
	}

	public final long getWCET (int subprogramID)
	{
		for (String subprogramName: ILPs.keySet ())
		{
			if (program.getSubprogram (subprogramName).getSubprogramID () == subprogramID)
			{
				return ILPs.get (subprogramName).wcet;
			}
		}
		return 0;
	}

	private class IPETModelCFG extends IPETModel
	{
		protected final ControlFlowGraph cfg;

		public IPETModelCFG (ControlFlowGraph cfg,
				LoopNests lnt,
				int subprogramID,
				String subprogramName)
		{
			this.cfg = cfg;

			partitionCFGEdges (lnt);

			try
			{
				/*
				 * The Linear Program Will Always Have at Least |V| Structural
				 * Constraints (Equivalent To Rows) and exactly |V| Variables
				 * (equivalent to Columns).
				 */
				numOfColumns = cfg.numOfVertices ();
				lp = LpSolve.makeLp (numOfColumns, numOfColumns);

				final String fileName = subprogramName + ".cfg.lp";
				final File file = new File (ILPdirectory, fileName);

				try
				{
					BufferedWriter out = new BufferedWriter (new FileWriter (
							file.getAbsolutePath ()));

					Debug.debugMessage (getClass (), "Writing ILP model to "
							+ file.getCanonicalPath (), 3);

					Debug.debugMessage (getClass (),
							"Writing objective function", 3);
					writeObjectiveFunction (subprogramID, out);

					Debug.debugMessage (getClass (),
							"Writing flow constraints", 3);
					writeFlowConstraints (out);

					Debug.debugMessage (getClass (),
							"Writing loop constraints", 3);
					writeLoopConstraints (subprogramID, lnt, out);

					Debug.debugMessage (getClass (),
							"Writing integer constraints", 3);
					writeIntegerConstraints (out);

					out.close ();
				}
				catch (IOException e)
				{
					System.err.println ("Problem with file " + fileName);
					System.exit (1);
				}

				lp = LpSolve.readLp (file.getAbsolutePath (), IPETModel
						.getLpSolveVerbosity (), null);
				try
				{
					lp = LpSolve.readLp (file.getAbsolutePath (),
							MainTraceParser.Globals.getLpSolveVerbosity (),
							null);
					solve ();
				}
				catch (SolutionException e)
				{
					System.exit (1);
				}
			}
			catch (LpSolveException e)
			{
				e.printStackTrace ();
				System.exit (1);
			}
		}

		private void partitionCFGEdges (LoopNests lnt)
		{
			for (int level = 0; level < lnt.getHeight (); ++level)
			{
				Iterator<TreeVertex> levelIt = lnt.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();
					int vertexID = v.getVertexID ();

					if (lnt.isLoopHeader (vertexID))
					{
						entryEdges.put (vertexID, new ArrayList<Integer> ());
						backEdges.put (vertexID, new ArrayList<Integer> ());
						ancestors.put (vertexID, new ArrayList<Integer> ());
						ancestors.get (vertexID).add (vertexID);

						if (vertexID != lnt.getRootID ())
						{
							ancestors.get (vertexID).addAll (
									ancestors.get (v.getParentID ()));

							BasicBlock cfgv = cfg.getBasicBlock (vertexID);
							Iterator<Edge> predIt = cfgv.predecessorIterator ();
							while (predIt.hasNext ())
							{
								FlowEdge e = (FlowEdge) predIt.next ();
								int edgeID = e.getEdgeID ();
								int predID = e.getVertexID ();

								if (!lnt.isLoopTail (vertexID, predID))
								{
									entryEdges.get (vertexID).add (edgeID);
								}
							}
						}
					}
				}
			}
		}

		private void writeObjectiveFunction (int subprogramID,
				BufferedWriter out) throws IOException
		{
			out.write ("// Objective function\n");
			out.write ("max: ");

			int num = 1;
			int numOfVertices = cfg.numOfVertices ();
			for (Vertex v: cfg)
			{
				int vertexID = v.getVertexID ();
				long wcet = 0;

				/*
				 * Check whether this basic block is a call site
				 */
				int calleeID = callg.isCallSite (subprogramID, vertexID);

				if (calleeID == Vertex.DUMMY_VERTEX_ID)
				{
					wcet = database.getUnitWCET (subprogramID, vertexID);
				}
				else
				{
					wcet = getWCET (calleeID);
				}

				Debug.debugMessage (getClass (), "WCET(v_" + vertexID + ") = "
						+ wcet, 4);
				out.write (Long.toString (wcet) + " " + vertexPrefix
						+ Integer.toString (vertexID));

				if (num < numOfVertices)
				{
					out.write (" + ");
				}
				if (num % 10 == 0)
				{
					out.newLine ();
				}
				num++;
			}

			out.write (";\n\n");
		}

		private void writeFlowConstraints (BufferedWriter out) throws IOException
		{
			for (Vertex v: cfg)
			{
				/*
				 * Only add the flow constraint for a vertex if it has both
				 * successors and predecessors
				 */
				if (v.hasPredecessors () && v.hasSuccessors ())
				{
					out.write ("// Vertex "
							+ Integer.toString (v.getVertexID ()) + "\n");

					/*
					 * First write out the vertex constraint
					 */
					out.write (vertexPrefix + Long.toString (v.getVertexID ())
							+ " = ");

					int num = 1;
					Iterator<Edge> predIt = v.predecessorIterator ();
					while (predIt.hasNext ())
					{
						FlowEdge e = (FlowEdge) predIt.next ();
						int edgeID = e.getEdgeID ();
						out.write (edgePrefix + Integer.toString (edgeID));

						if (num++ < v.numOfPredecessors ())
						{
							out.write (" + ");
						}
					}
					out.write (";\n");

					num = 1;
					Iterator<Edge> succIt = v.successorIterator ();
					while (succIt.hasNext ())
					{
						FlowEdge e = (FlowEdge) succIt.next ();
						int edgeID = e.getEdgeID ();
						out.write (edgePrefix + Integer.toString (edgeID));

						if (num++ < v.numOfSuccessors ())
						{
							out.write (" + ");
						}
					}

					out.write (" = ");

					num = 1;
					predIt = v.predecessorIterator ();
					while (predIt.hasNext ())
					{
						FlowEdge e = (FlowEdge) predIt.next ();
						int edgeID = e.getEdgeID ();
						out.write (edgePrefix + Integer.toString (edgeID));

						if (num++ < v.numOfPredecessors ())
						{
							out.write (" + ");
						}
					}

					out.write (";\n\n");
				}
			}
		}

		private void writeLoopConstraints (int subprogramID,
				LoopNests lnt,
				BufferedWriter out) throws IOException
		{
			for (int level = lnt.getHeight () - 1; level >= 0; --level)
			{
				Iterator<TreeVertex> levelIt = lnt.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();
					int vertexID = v.getVertexID ();

					if (lnt.isLoopHeader (vertexID))
					{
						out.write ("// Header " + Integer.toString (vertexID)
								+ "\n");
						if (vertexID == cfg.getEntryID ())
						{
							out.write (vertexPrefix + Long.toString (vertexID)
									+ " = 1;\n");
						}
						else
						{
							writeInnerLoopConstraints (subprogramID, lnt,
									vertexID, out);
						}
						out.newLine ();
					}
				}
			}
		}

		private void writeInnerLoopConstraints (int subprogramID,
				LoopNests lnt,
				int headerID,
				BufferedWriter out) throws IOException
		{
			for (int ancestorID: ancestors.get (headerID))
			{
				if (ancestorID != cfg.getEntryID ())
				{
					int parentID = lnt.getVertex (ancestorID).getParentID ();

					TreeVertex h = lnt.getVertex (headerID);
					TreeVertex p = lnt.getVertex (parentID);

					if (h.getLevel () - p.getLevel () <= loopConstraintLevel)
					{
						out.write ("//...with respect to "
								+ Integer.toString (parentID) + "\n");

						int bound = database.getLoopBound (subprogramID,
								headerID, parentID);
						Debug.debugMessage (getClass (),
								"Adding constraint on loop " + headerID
										+ " relative to loop " + parentID
										+ ". Bound = " + bound, 4);

						writeSuccessorEdges (out, headerID);
						out.write (" <= ");
						writeImplicatingEdges (out, ancestorID, bound);
						out.write (";\n");
					}
				}
			}
		}

		private void writeSuccessorEdges (BufferedWriter out, int headerID) throws IOException
		{
			int num = 1;

			Vertex v = cfg.getVertex (headerID);
			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				FlowEdge e = (FlowEdge) succIt.next ();
				int edgeID = e.getEdgeID ();
				out.write (edgePrefix + Integer.toString (edgeID));

				if (num++ < v.numOfSuccessors ())
				{
					out.write (" + ");
				}
			}
		}

		private void writeImplicatingEdges (BufferedWriter out,
				int headerID,
				int bound) throws IOException
		{
			int num = 1;
			for (int edgeID: entryEdges.get (headerID))
			{
				out.write (Integer.toString (bound) + " " + edgePrefix
						+ Integer.toString (edgeID));

				if (num++ < entryEdges.get (headerID).size ())
				{
					out.write (" + ");
				}
			}
		}

		private void writeInfeasiblePathConstraints (int subprogramID,
				BufferedWriter out) throws IOException
		{
			out.write ("// Infeasible path constraints\n\n");
			for (Vertex v: cfg)
			{
				int vertexID = v.getVertexID ();
				Set<Integer> infeasibleVertices = database.getInfeasibleUnits (
						subprogramID, vertexID);

				if (!infeasibleVertices.isEmpty ())
				{
					out.write ("// Vertex " + Integer.toString (vertexID)
							+ "\n");

					for (int infeasibleVertexID: infeasibleVertices)
					{
						if (infeasibleVertexID != vertexID)
						{
							out.write (vertexPrefix
									+ Integer.toString (vertexID) + " + "
									+ vertexPrefix
									+ Integer.toString (infeasibleVertexID)
									+ " <= 1;\n");
						}
					}
					out.write ("\n");
				}
			}
		}

		private void writeIntegerConstraints (BufferedWriter out) throws IOException
		{
			int num = 1;
			int numOfVertices = cfg.numOfVertices ();

			out.write ("// Integer constraints\n");
			out.write ("int ");
			for (Vertex v: cfg)
			{
				out.write (vertexPrefix + Integer.toString (v.getVertexID ()));

				if (num++ < numOfVertices)
				{
					out.write (", ");
				}
			}

			out.write (";\n");
		}

		private void solve () throws LpSolveException, SolutionException
		{
			int solution = lp.solve ();
			switch (solution)
			{
				case LpSolve.OPTIMAL:
					Debug.debugMessage (getClass (), "Optimal solution found "
							+ lp.getObjective (), 3);
					wcet = Math.round (lp.getObjective ());
					break;
				default:
					Debug.debugMessage (getClass (),
							"Problem with the LP model: " + solution, 2);
					throw new SolutionException (solution);
			}
		}
	}
}
