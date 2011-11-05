package adam.betts.calculations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import lpsolve.LpSolve;
import lpsolve.LpSolveException;
import adam.betts.edges.Edge;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.graphs.trees.SyntaxTree;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainTraceParser;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.AlternativeVertex;
import adam.betts.vertices.trees.LeafVertex;
import adam.betts.vertices.trees.LoopVertex;
import adam.betts.vertices.trees.SequenceVertex;
import adam.betts.vertices.trees.SyntaxVertex;
import adam.betts.vertices.trees.TreeVertex;

public class CalculationEngineAST
{
	protected final Program program;
	protected final CallGraph callg;
	protected final Database database;
	protected final HashMap <String, TimingSchema> timingSchemas = new HashMap <String, TimingSchema> ();
	protected final HashMap <String, IPETModelAST> ILPs = new HashMap <String, IPETModelAST> ();

	public CalculationEngineAST (Program program, Database database)
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

			/*
			 * Timing schema calculation
			 */
			Debug.debugMessage (getClass (), "Timing schema on " + subprogramName, 3);

			SyntaxTree stree = subprogram.getSyntaxTree ();
			TimingSchema ts = new TimingSchema (stree, subprogram.getCFG ().getLNT (), subprogramID);
			timingSchemas.put (subprogramName, ts);

			Debug
					.debugMessage (getClass (),
							"AST-TS: WCET(" + subprogramName + ") = " + ts.wcet, 3);

			/*
			 * ILP calculation
			 */

			Debug.debugMessage (getClass (), "Building IPET of " + subprogramName, 3);

			IPETModelAST ilp = new IPETModelAST (stree, subprogram.getCFG ().getLNT (),
					subprogramID, subprogramName);
			ILPs.put (subprogramName, ilp);

			Debug.debugMessage (getClass (), "AST-ILP: WCET(" + subprogramName + ") = " + ilp.wcet,
					3);

		}
	}

	public final long getTimingSchemaWCET (int subprogramID)
	{
		for (String subprogramName : timingSchemas.keySet ())
		{
			if (program.getSubprogram (subprogramName).getSubprogramID () == subprogramID)
			{
				return timingSchemas.get (subprogramName).wcet;
			}
		}
		return 0;
	}

	public final long getIPETWCET (int subprogramID)
	{
		for (String subprogramName : ILPs.keySet ())
		{
			if (program.getSubprogram (subprogramName).getSubprogramID () == subprogramID)
			{
				return ILPs.get (subprogramName).wcet;
			}
		}
		return 0;
	}

	private class TimingSchema
	{
		protected final SyntaxTree stree;
		protected final LoopNests lnt;
		protected final int subprogramID;
		protected long wcet = 0;

		public TimingSchema (SyntaxTree stree, LoopNests lnt, int subprogramID)
		{
			this.stree = stree;
			this.lnt = lnt;
			this.subprogramID = subprogramID;

			markLeavesWithWCETS ();
			calculate ();
		}

		private void markLeavesWithWCETS ()
		{
			for (Vertex v : stree)
			{
				if (v instanceof LeafVertex)
				{
					LeafVertex leafv = (LeafVertex) v;
					if (!leafv.isLambdaVertex ())
					{
						int bbID = leafv.getCFGVertexID ();
						int calleeID = callg.isCallSite (subprogramID, bbID);

						if (calleeID == Vertex.DUMMY_VERTEX_ID)
						{
							leafv.setWCET (database.getUnitWCET (subprogramID, bbID));
						} else
						{
							leafv.setWCET (getTimingSchemaWCET (calleeID));
						}
					}
				}
			}
		}

		private void calculate ()
		{
			for (int level = stree.getHeight () - 1; level >= 0; --level)
			{
				Iterator <TreeVertex> levelIt = stree.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();

					if (v instanceof SequenceVertex)
					{
						calculateSEQVertex ((SequenceVertex) v);
					} else if (v instanceof AlternativeVertex)
					{
						calculateALTVertex ((AlternativeVertex) v);
					} else if (v instanceof LoopVertex)
					{
						calculateLOOPVertex ((LoopVertex) v);
					}
				}
			}

			wcet = stree.getVertex (stree.getRootID ()).getWCET ();
		}

		private void calculateSEQVertex (SequenceVertex seq)
		{
			long wcet = 0;
			Iterator <Edge> succIt = seq.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				SyntaxVertex s = stree.getVertex (e.getVertexID ());
				wcet += s.getWCET ();
			}
			seq.setWCET (wcet);

			Debug.debugMessage (getClass (), "WCET(SEQ_" + seq.getVertexID () + ") = "
					+ +seq.getWCET (), 4);
		}

		private void calculateALTVertex (AlternativeVertex alt)
		{
			Iterator <Edge> succIt = alt.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				SyntaxVertex s = stree.getVertex (e.getVertexID ());
				if (s.getWCET () > alt.getWCET ())
				{
					alt.setWCET (s.getWCET ());
				}
			}

			Debug.debugMessage (getClass (), "WCET(ALT_" + alt.getVertexID () + ") = "
					+ +alt.getWCET (), 4);
		}

		private void calculateLOOPVertex (LoopVertex loop)
		{
			int headerID = loop.getHeaderID ();
			int parentID = lnt.getVertex (headerID).getParentID ();
			int bound = database.getLoopBound (subprogramID, headerID, parentID);
			long wcet = 0;

			Iterator <Edge> succIt = loop.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				SyntaxVertex s = stree.getVertex (e.getVertexID ());
				wcet += s.getWCET ();
			}

			loop.setWCET (wcet * bound);

			Debug.debugMessage (getClass (), "WCET(LOOP_" + loop.getVertexID () + ") = "
					+ +loop.getWCET (), 4);
		}
	}

	private class IPETModelAST extends IPETModel
	{
		protected final static String loopPrefix = "LOOP_";
		protected final static String altPrefix = "ALT_";
		protected final static String seqPrefix = "SEQ_";

		protected final SyntaxTree stree;
		protected final LoopNests lnt;
		protected final int subprogramID;
		protected HashMap <Integer, HashSet <Integer>> bbToLeafIDs = new HashMap <Integer, HashSet <Integer>> ();

		public IPETModelAST (SyntaxTree stree, LoopNests lnt, int subprogramID,
				String subprogramName)
		{
			this.stree = stree;
			this.lnt = lnt;
			this.subprogramID = subprogramID;

			try
			{
				/*
				 * The Linear Program Will Always Have at Least |V| Structural
				 * Constraints (Equivalent To Rows) and exactly |V| Variables
				 * (equivalent to Columns).
				 */
				numOfColumns = stree.numOfVertices ();
				lp = LpSolve.makeLp (numOfColumns, numOfColumns);

				final String fileName = subprogramName + ".stree.lp";
				final File file = new File (ILPdirectory, fileName);

				try
				{
					BufferedWriter out = new BufferedWriter (new FileWriter (file
							.getAbsolutePath ()));

					Debug.debugMessage (getClass (), "Writing ILP model to "
							+ file.getCanonicalPath (), 3);

					Debug.debugMessage (getClass (), "Writing objective function", 3);
					writeObjectiveFunction (subprogramID, out);

					Debug.debugMessage (getClass (), "Writing parent-ancestor constraints", 3);
					writeParentAncestorConstraints (out);

					Debug.debugMessage (getClass (), "Writing integer constraints", 3);
					writeIntegerConstraints (out);

					out.close ();
				} catch (IOException e)
				{
					System.err.println ("Problem with file " + fileName);
					System.exit (1);
				}

				lp = LpSolve.readLp (file.getAbsolutePath (), IPETModel.getLpSolveVerbosity (),
						null);
				try
				{
					lp = LpSolve.readLp (file.getAbsolutePath (), MainTraceParser
							.getLpSolveVerbosity (), null);
					solve ();
				} catch (SolutionException e)
				{
					System.exit (1);
				}
			} catch (LpSolveException e)
			{
				e.printStackTrace ();
				System.exit (1);
			}
		}

		private void writeObjectiveFunction (int subprogramID, BufferedWriter out)
				throws IOException
		{
			out.write ("// Objective function\n");
			out.write ("max: ");

			int num = 1;
			int numOfLeaves = stree.numOfLeaves ();
			for (Vertex v : stree)
			{
				if (v instanceof LeafVertex)
				{
					int vertexID = v.getVertexID ();
					long wcet = 0;

					LeafVertex leafv = (LeafVertex) v;
					if (!leafv.isLambdaVertex ())
					{
						int bbID = leafv.getCFGVertexID ();
						int calleeID = callg.isCallSite (subprogramID, bbID);

						/*
						 * Check whether this basic block is a call site
						 */
						if (calleeID == Vertex.DUMMY_VERTEX_ID)
						{
							wcet = database.getUnitWCET (subprogramID, bbID);
						} else
						{
							wcet = getIPETWCET (calleeID);
						}

						if (!bbToLeafIDs.containsKey (bbID))
						{
							bbToLeafIDs.put (bbID, new HashSet <Integer> ());
						}
						bbToLeafIDs.get (bbID).add (vertexID);
					}

					Debug.debugMessage (getClass (), "WCET(v_" + vertexID + ") = " + wcet, 4);
					out.write (Long.toString (wcet) + " " + vertexPrefix
							+ Integer.toString (vertexID));

					if (num < numOfLeaves)
					{
						out.write (" + ");
					}
					if (num % 10 == 0)
					{
						out.newLine ();
					}
					num++;
				}
			}

			out.write (";\n\n");
		}

		private void writeParentAncestorConstraints (BufferedWriter out) throws IOException
		{
			HashSet <SequenceVertex> SEQs = new HashSet <SequenceVertex> ();
			HashSet <LoopVertex> LOOPs = new HashSet <LoopVertex> ();
			HashSet <AlternativeVertex> ALTs = new HashSet <AlternativeVertex> ();
			HashSet <LeafVertex> leaves = new HashSet <LeafVertex> ();

			for (Vertex v : stree)
			{
				TreeVertex treev = (TreeVertex) v;

				if (v instanceof SequenceVertex)
				{
					SEQs.add ((SequenceVertex) treev);
				} else if (v instanceof AlternativeVertex)
				{
					ALTs.add ((AlternativeVertex) treev);
				} else if (v instanceof LoopVertex)
				{
					LOOPs.add ((LoopVertex) treev);
				} else
				{
					leaves.add ((LeafVertex) treev);
				}
			}

			out.write ("// Leaf constraints\n");
			for (LeafVertex v : leaves)
			{
				out.write (writeLeafConstraint (v.getVertexID (), v.getParentID ()));
			}
			out.write ("\n");

			out.write ("// SEQ constraints\n");
			for (SequenceVertex v : SEQs)
			{
				int vertexID = v.getVertexID ();

				if (vertexID == stree.getRootID ())
				{
					out.write (seqPrefix + Integer.toString (vertexID) + " = "
							+ Integer.toString (1) + ";\n");
				} else
				{
					SyntaxVertex parent = stree.getVertex (v.getParentID ());
					if (parent instanceof LoopVertex)
					{
						/*
						 * The other case is handled by the alternative vertex
						 */
						out.write (seqPrefix + Integer.toString (vertexID) + " = " + loopPrefix
								+ Integer.toString (v.getParentID ()) + ";\n");
					}
				}
			}
			out.write ("\n");

			out.write ("// ALT constraints\n");
			for (AlternativeVertex v : ALTs)
			{
				out.write (writeALTConstraint (v.getVertexID (), v.getParentID ()));
			}

			for (LoopVertex v : LOOPs)
			{
				out.write (writeLOOPConstraint (v.getVertexID ()));
			}
		}

		private String writeALTConstraint (int vertexID, int parentID)
		{
			StringBuffer buffer = new StringBuffer ();

			/*
			 * The parent of an ALT is always a SEQ
			 */
			buffer.append (altPrefix + Integer.toString (vertexID) + " = " + seqPrefix
					+ Integer.toString (parentID) + ";\n");

			int num = 1;
			Vertex altv = stree.getVertex (vertexID);
			Iterator <Edge> succIt = altv.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				int succID = e.getVertexID ();
				buffer.append (seqPrefix + Integer.toString (succID));

				if (num++ < altv.numOfSuccessors ())
				{
					buffer.append (" + ");
				}
			}

			buffer.append (" = " + altPrefix + Integer.toString (vertexID) + ";\n\n");

			return buffer.toString ();
		}

		private String writeLOOPConstraint (int vertexID)
		{
			StringBuffer buffer = new StringBuffer ();

			LoopVertex loop = (LoopVertex) stree.getVertex (vertexID);
			int headerID = loop.getHeaderID ();
			int ancestorID = headerID;

			buffer.append ("// Header " + Integer.toString (headerID) + "\n");

			while (ancestorID != lnt.getRootID ())
			{
				int parentID = lnt.getVertex (ancestorID).getParentID ();

				TreeVertex h = lnt.getVertex (headerID);
				TreeVertex p = lnt.getVertex (parentID);

				if (h.getLevel () - p.getLevel () <= loopConstraintLevel)
				{
					buffer.append ("//...with respect to " + Integer.toString (parentID) + "\n");

					int bound = database.getLoopBound (subprogramID, headerID, parentID);

					Debug.debugMessage (getClass (), "Adding constraint on loop " + headerID
							+ " relative to loop " + parentID + ". Bound = " + bound, 4);

					Iterator <Integer> bodyIt = lnt.bodyIterator (headerID);
					while (bodyIt.hasNext ())
					{
						Integer bbID = bodyIt.next ();
						boolean write = true;

						if (lnt.isLoopHeader (bbID) && bbID != headerID)
						{
							write = false;
						}

						if (write)
						{
							int num = 1;
							HashSet <Integer> leafIDs = bbToLeafIDs.get (bbID);

							for (int leafID : leafIDs)
							{
								buffer.append (vertexPrefix + Integer.toString (leafID));
								if (num++ < leafIDs.size ())
								{
									buffer.append (" + ");
								}
							}

							buffer.append (" <= "
									+ Integer.toString (bound)
									+ " "
									+ seqPrefix
									+ Integer.toString (stree.getLoopVertex (ancestorID)
											.getParentID ()) + ";\n");

						}

					}

					buffer.append ("\n");
				}

				ancestorID = parentID;
			}

			return buffer.toString ();
		}

		private String writeLeafConstraint (int vertexID, int parentID)
		{
			StringBuffer buffer = new StringBuffer ();
			buffer.append (vertexPrefix + Integer.toString (vertexID) + " = ");

			SyntaxVertex parent = stree.getVertex (parentID);
			if (parent instanceof AlternativeVertex)
			{
				buffer.append (altPrefix);
			} else if (parent instanceof LoopVertex)
			{
				buffer.append (loopPrefix);
			} else
			{
				buffer.append (seqPrefix);
			}

			buffer.append (Integer.toString (parentID) + ";\n");
			return buffer.toString ();
		}

		private void writeInfeasiblePathConstraints (int subprogramID, BufferedWriter out)
				throws IOException
		{
			out.write ("// Infeasible path constraints\n\n");
			for (int bbID : bbToLeafIDs.keySet ())
			{
				Set <Integer> infeasibleVertices = database.getInfeasibleUnits (subprogramID, bbID);

				if (!infeasibleVertices.isEmpty ())
				{
					out.write ("// Vertex " + Integer.toString (bbID) + "\n");

					for (int infeasibleBBID : infeasibleVertices)
					{
						if (infeasibleBBID != bbID)
						{
							for (int leaf1ID : bbToLeafIDs.get (bbID))
							{
								for (int leaf2ID : bbToLeafIDs.get (infeasibleBBID))
								{
									out.write (vertexPrefix + Integer.toString (leaf1ID) + " + "
											+ vertexPrefix + Integer.toString (leaf2ID)
											+ " <= 1;\n");
								}
							}

						}
					}
					out.write ("\n");
				}
			}
		}

		private void writeIntegerConstraints (BufferedWriter out) throws IOException
		{
			int num = 1;
			int numOfLeaves = stree.numOfLeaves ();

			out.write ("// Integer constraints\n");
			out.write ("int ");
			for (Vertex v : stree)
			{
				if (v instanceof LeafVertex)
				{
					out.write (vertexPrefix + Integer.toString (v.getVertexID ()));

					if (num++ < numOfLeaves)
					{
						out.write (", ");
					}
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
					Debug.debugMessage (getClass (),
							"Optimal solution found " + lp.getObjective (), 3);
					wcet = Math.round (lp.getObjective ());
					break;
				default:
					Debug.debugMessage (getClass (), "Problem with the LP model: " + solution, 2);
					throw new SolutionException (solution);
			}
		}
	}
}
