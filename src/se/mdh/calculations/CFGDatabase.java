package se.mdh.calculations;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import se.mdh.graphs.CallGraph;
import se.mdh.graphs.ControlFlowGraph;
import se.mdh.graphs.trees.LoopNests;
import se.mdh.programs.Program;
import se.mdh.programs.Subprogram;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Globals;
import se.mdh.vertices.BasicBlock;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.call.CallVertex;
import se.mdh.vertices.trees.TreeVertex;

public class CFGDatabase extends Database
{
	public CFGDatabase (final Program program)
	{
		super (program);

		TraceParser parser = new TraceParser (this);
		initialise (parser);

		Debug.verboseMessage ("Parsing trace file "
				+ Globals.getTraceFileName ());
		parser.doParsing ();

		for (int subprogramID: unitWCETs.keySet ())
		{
			Subprogram subprogram = program.getSubprogram (subprogramID);

			System.out.println ("=== " + subprogram.getSubprogramName ()
					+ " ===");
			ControlFlowGraph cfg = subprogram.getCFG ();
			for (Vertex v: cfg)
			{
				System.out.println ("WCET(" + v.getVertexID () + ") = "
						+ unitWCETs.get (subprogramID).get (v.getVertexID ()));
			}

			for (int headerID: loopBounds.get (subprogramID).keySet ())
			{
				for (int ancestorID: loopBounds.get (subprogramID).get (
						headerID).keySet ())
				{
					System.out.println ("bound("
							+ headerID
							+ ", "
							+ ancestorID
							+ ") = "
							+ loopBounds.get (subprogramID).get (headerID).get (
									ancestorID));
				}
			}

			for (int bbID: observedPaths.get (subprogramID).keySet ())
			{
				System.out.println ("For " + bbID + " saw: "
						+ observedPaths.get (subprogramID).get (bbID));
			}
		}
	}

	private void initialise (TraceParser parser)
	{
		HashSet<Integer> reachableSubprograms = program.getCallGraph ()
				.getReachableVertices (program.getRootID ());

		for (int subprogramID: reachableSubprograms)
		{
			Subprogram subprogram = program.getSubprogram (subprogramID);
			unitWCETs.put (subprogramID, new HashMap<Integer, Long> ());
			loopBounds.put (subprogramID,
					new HashMap<Integer, HashMap<Integer, Integer>> ());
			parser.tempLoopBounds.put (subprogramID,
					new HashMap<Integer, HashMap<Integer, Integer>> ());
			parser.properDescendants.put (subprogramID,
					new HashMap<Integer, HashSet<Integer>> ());
			observedPaths.put (subprogramID,
					new HashMap<Integer, HashSet<Integer>> ());
			parser.paths.put (subprogramID, new HashSet<Integer> ());
			tests.put (subprogramID, (long) 0);
			mets.put (subprogramID, (long) 0);

			ControlFlowGraph cfg = subprogram.getCFG ();
			for (Vertex v: cfg)
			{
				unitWCETs.get (subprogramID).put (v.getVertexID (), (long) 0);
				observedPaths.get (subprogramID).put (v.getVertexID (),
						new HashSet<Integer> ());
			}

			LoopNests lnt = cfg.getLNT ();
			HashMap<Integer, HashSet<Integer>> ancestors = new HashMap<Integer, HashSet<Integer>> ();
			for (int level = 0; level < lnt.getHeight (); ++level)
			{
				Iterator<TreeVertex> levelIt = lnt.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();
					int vertexID = v.getVertexID ();

					if (lnt.isLoopHeader (vertexID))
					{
						parser.properDescendants.get (subprogramID).put (
								vertexID, new HashSet<Integer> ());

						if (vertexID == lnt.getRootID ())
						{
							ancestors.put (vertexID, new HashSet<Integer> ());
							ancestors.get (vertexID).add (vertexID);
						}
						else
						{
							int parentID = v.getParentID ();
							ancestors.put (vertexID, new HashSet<Integer> ());
							ancestors.get (vertexID).addAll (
									ancestors.get (parentID));

							HashMap<Integer, Integer> ancestorToBound = new HashMap<Integer, Integer> ();
							HashMap<Integer, Integer> ancestorToBound2 = new HashMap<Integer, Integer> ();

							for (int ancestorID: ancestors.get (vertexID))
							{
								ancestorToBound.put (ancestorID, 0);
								ancestorToBound2.put (ancestorID, 0);
								parser.properDescendants.get (subprogramID)
										.get (ancestorID).add (vertexID);
							}
							loopBounds.get (subprogramID).put (vertexID,
									ancestorToBound);
							parser.tempLoopBounds.get (subprogramID).put (
									vertexID, ancestorToBound2);

							ancestors.get (vertexID).add (vertexID);
						}
					}
				}
			}
		}
	}

	private class TraceParser
	{
		/*
		 * Stacks needed during trace parsing
		 */
		private Stack<Integer> callStack = new Stack<Integer> ();
		private Stack<BasicBlock> bbStack = new Stack<BasicBlock> ();
		private Stack<Long> timestampStack = new Stack<Long> ();

		/*
		 * To keep track of the subprogram and where we are in the trace
		 */
		private int subprogramID;
		private ControlFlowGraph cfg;
		private LoopNests lnt;
		private BasicBlock bb;
		private boolean newTrace = true;

		/*
		 * To keep track of times
		 */
		private long oldTS = 0;
		private long newTS = 0;
		private long runStart = 0;

		/*
		 * Temporary loop bounds
		 */
		protected HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> tempLoopBounds = new HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> ();
		protected HashMap<Integer, HashMap<Integer, HashSet<Integer>>> properDescendants = new HashMap<Integer, HashMap<Integer, HashSet<Integer>>> ();

		/*
		 * The paths executed in each subprogram
		 */
		protected HashMap<Integer, HashSet<Integer>> paths = new HashMap<Integer, HashSet<Integer>> ();

		private TraceParser (CFGDatabase database)
		{
		}

		private void doParsing ()
		{
			try
			{
				BufferedReader in = new BufferedReader (new FileReader (Globals
						.getTraceFileName ()));
				String str;
				while ( (str = in.readLine ()) != null)
				{
					if (!str.startsWith ("//") && !str.startsWith ("/*"))
					{
						final String[] lexemes = str.split ("\\s+");
						long address;

						if (lexemes[0].startsWith ("0x"))
						{
							address = Long.parseLong (lexemes[0].substring (2));
						}
						else
						{
							address = Long.parseLong (lexemes[0]);
						}

						newTS = Long.parseLong (lexemes[1]);

						if (newTrace)
						{
							resetToRoot ();
							newTrace = false;
						}
						else
						{
							Subprogram subprogram = program
									.getSubprogram (address);

							if (subprogram.getSubprogramID () != subprogramID)
							{
								CallGraph callg = program.getCallGraph ();
								CallVertex callv = callg
										.getVertex (subprogramID);

								if (callv.hasSuccessor (subprogram
										.getSubprogramID ()))
								{
									/*
									 * Function call
									 */
									handleCall (subprogram, address);
								}
								else
								{
									/*
									 * End of function call
									 */
									handleReturn (subprogram, address);
								}
							}
							else
							{
								/*
								 * In the same function
								 */
								handleTransition (subprogram, address);
							}
						}

						oldTS = newTS;
					}
					else
					{
						Debug.debugMessage (getClass (), str, 4);
					}
				}
			}
			catch (Exception e)
			{
				System.err.println ("Error: " + e.getMessage ());
				e.printStackTrace ();
				System.exit (1);
			}
		}

		private void handleCall (Subprogram subprogram, long address)
		{
			paths.get (subprogramID).add (bb.getVertexID ());

			/*
			 * Remember where we were
			 */
			callStack.push (subprogramID);
			bbStack.push (bb);
			timestampStack.push (newTS);

			subprogramID = subprogram.getSubprogramID ();
			cfg = subprogram.getCFG ();
			lnt = cfg.getLNT ();
			bb = cfg.getBasicBlock (address);

			/*
			 * Record the fact this basic block was seen in this subprogram
			 * execution
			 */
			paths.get (subprogramID).add (bb.getVertexID ());

			Debug.debugMessage (getClass (), "CALL: In "
					+ subprogram.getSubprogramName () + " @ basic block "
					+ bb.getVertexID (), 4);
		}

		private void handleReturn (Subprogram subprogram, long address)
		{
			/*
			 * The timestamp we saw for the call site
			 */
			long callTS = timestampStack.pop ();

			if (newTS - callTS > mets.get (subprogramID))
			{
				mets.put (subprogramID, newTS - callTS);
			}

			/*
			 * Record which basic blocks were on the observed path
			 */
			HashSet<Integer> path = paths.get (subprogramID);
			for (int bbID: path)
			{
				observedPaths.get (subprogramID).get (bbID).addAll (path);
			}
			/*
			 * Get ready for the next invocation of the function
			 */
			paths.get (subprogramID).clear ();

			subprogramID = callStack.pop ();
			subprogram = program.getSubprogram (subprogramID);

			cfg = subprogram.getCFG ();
			lnt = cfg.getLNT ();

			/*
			 * This is the basic block where the call occurred
			 */
			bb = bbStack.pop ();
			int predID = bb.getVertexID ();

			if (newTS - callTS > unitWCETs.get (subprogramID).get (predID))
			{
				unitWCETs.get (subprogramID).put (predID, newTS - callTS);
			}

			if (lnt.isLoopHeader (predID) && predID != cfg.getEntryID ())
			{
				handleLoopIteration (predID);
			}

			/*
			 * Since we have now seen a basic block in the caller, advance the
			 * parser beyond the call site
			 */
			bb = cfg.getSuccessor (bb, address);
			determineLoopExit (predID, bb.getVertexID ());

			paths.get (subprogramID).add (bb.getVertexID ());

			if (newTS - oldTS > unitWCETs.get (subprogramID).get (
					bb.getVertexID ()))
			{
				unitWCETs.get (subprogramID).put (bb.getVertexID (),
						newTS - oldTS);
			}

			Debug.debugMessage (getClass (), "RETURN: In "
					+ subprogram.getSubprogramName () + " @ basic block "
					+ bb.getVertexID (), 4);
		}

		private void handleTransition (Subprogram subprogram, long address)
		{
			if (newTS - oldTS > unitWCETs.get (subprogramID).get (
					bb.getVertexID ()))
			{
				unitWCETs.get (subprogramID).put (bb.getVertexID (),
						newTS - oldTS);
			}

			if (subprogramID == program.getRootID ()
					&& address == cfg.getLastAddress ())
			{
				newTrace = true;

				/*
				 * See if this is the HWMT
				 */
				if (newTS - runStart > mets.get (subprogramID))
				{
					mets.put (subprogramID, newTS - runStart);
				}
			}
			else
			{
				int predID = bb.getVertexID ();
				if (lnt.isLoopHeader (predID) && predID != cfg.getEntryID ())
				{
					handleLoopIteration (predID);
				}

				bb = cfg.getSuccessor (bb, address);
				determineLoopExit (predID, bb.getVertexID ());

				/*
				 * Record the fact this basic block was seen in this subprogram
				 * execution
				 */
				paths.get (subprogramID).add (bb.getVertexID ());

				Debug.debugMessage (getClass (), "In "
						+ subprogram.getSubprogramName () + " @ basic block "
						+ bb.getVertexID (), 4);
			}
		}

		private void resetToRoot ()
		{
			subprogramID = program.getRootID ();
			cfg = program.getSubprogram (subprogramID).getCFG ();
			lnt = cfg.getLNT ();
			bb = cfg.getBasicBlock (cfg.getFirstAddress ());
			runStart = newTS;

			HashSet<Integer> path = paths.get (subprogramID);
			for (int bbID: path)
			{
				observedPaths.get (subprogramID).get (bbID).addAll (path);
			}
		}

		private void handleLoopIteration (int headerID)
		{
			for (int ancestorID: tempLoopBounds.get (subprogramID).get (
					headerID).keySet ())
			{
				int bound = tempLoopBounds.get (subprogramID).get (headerID)
						.get (ancestorID);
				tempLoopBounds.get (subprogramID).get (headerID).put (
						ancestorID, bound + 1);
			}
		}

		private void determineLoopExit (int predID, int vertexID)
		{
			int pHeaderID = lnt.getLoopHeader (predID);
			int vHeaderID = lnt.getLoopHeader (vertexID);

			if (pHeaderID != vHeaderID)
			{
				if (lnt.isNested (pHeaderID, vHeaderID))
				{
					/*
					 * Exiting into vHeaderID
					 */

					Debug.debugMessage (getClass (), "EXIT from loop "
							+ pHeaderID + " on edge " + predID + " => "
							+ vertexID, 4);

					for (int descendantID: properDescendants.get (subprogramID)
							.get (vHeaderID))
					{
						int bound = tempLoopBounds.get (subprogramID).get (
								descendantID).get (vHeaderID);
						tempLoopBounds.get (subprogramID).get (descendantID)
								.put (vHeaderID, 0);

						int currentBound = loopBounds.get (subprogramID).get (
								descendantID).get (vHeaderID);

						if (bound > currentBound)
						{
							Debug.debugMessage (getClass (), "bound("
									+ descendantID
									+ ", "
									+ vHeaderID
									+ ") = "
									+ tempLoopBounds.get (subprogramID).get (
											descendantID).get (vHeaderID), 4);

							loopBounds.get (subprogramID).get (descendantID)
									.put (vHeaderID, bound);
						}
						else
						{
							Debug.debugMessage (getClass (), "Bound("
									+ descendantID + ", " + vHeaderID + ") = "
									+ currentBound + " (UNCHANGED)", 4);
						}
					}
				}
			}
		}
	}
}