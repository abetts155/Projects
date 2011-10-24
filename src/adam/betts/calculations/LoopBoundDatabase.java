package adam.betts.calculations;

import java.io.BufferedReader;
import java.io.FileReader;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import adam.betts.edges.IPGEdge;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.graphs.utils.LeastCommonAncestor;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.trees.TreeVertex;

public class LoopBoundDatabase extends Database
{
	public LoopBoundDatabase (final Program program)
	{
		super (program);

		TraceParser parser = new TraceParser (this);
		initialise (parser);

		Debug.verboseMessage ("Parsing trace file "
				+ Globals.getTraceFileName ());

		long time1 = System.nanoTime ();
		parser.doParsing ();
		long time2 = System.nanoTime ();

		for (int subprogramID: loopBounds.keySet ())
		{
			Subprogram subprogram = program.getSubprogram (subprogramID);

			Debug.debugMessage (getClass (), "=== "
					+ subprogram.getSubprogramName () + " ===", 4);

			LoopNests lnt = subprogram.getCFGStar (
					Globals.getInstrumentationProfile ()).getLNT ();

			for (int level = lnt.getHeight () - 1; level > 0; --level)
			{
				Iterator<TreeVertex> levelIt = lnt.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();
					int vertexID = v.getVertexID ();

					if (v.numOfSuccessors () > 0)
					{
						HashMap<Integer, Integer> boundsForHeader = loopBounds
								.get (subprogramID).get (vertexID);

						int ancestorID = vertexID;
						while (ancestorID != lnt.getRootID ())
						{
							ancestorID = lnt.getVertex (ancestorID)
									.getParentID ();

							Debug.debugMessage (getClass (), "bound("
									+ vertexID + ", " + ancestorID + ") = "
									+ boundsForHeader.get (ancestorID), 4);
						}
					}
				}
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
			loopBounds.put (subprogramID,
					new HashMap<Integer, HashMap<Integer, Integer>> ());
			parser.tempLoopBounds.put (subprogramID,
					new HashMap<Integer, HashMap<Integer, Integer>> ());
			parser.properDescendants.put (subprogramID,
					new HashMap<Integer, HashSet<Integer>> ());

			LoopNests lnt = subprogram.getCFGStar (
					Globals.getInstrumentationProfile ()).getLNT ();
			LeastCommonAncestor lca = new LeastCommonAncestor (lnt);
			parser.lcas.put (subprogramID, lca);

			HashMap<Integer, HashSet<Integer>> ancestors = new HashMap<Integer, HashSet<Integer>> ();
			for (int level = 0; level < lnt.getHeight (); ++level)
			{
				Iterator<TreeVertex> levelIt = lnt.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();
					int vertexID = v.getVertexID ();

					if (v.numOfSuccessors () > 0)
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
		 * Cached LCA data structures
		 */
		private HashMap<Integer, LeastCommonAncestor> lcas = new HashMap<Integer, LeastCommonAncestor> ();

		/*
		 * Stacks needed during trace parsing
		 */
		private Stack<Integer> callStack = new Stack<Integer> ();
		private Stack<Ipoint> ipointStack = new Stack<Ipoint> ();

		/*
		 * To keep track of the subprogram and where we are in the trace
		 */
		private int subprogramID;
		private Subprogram subprogram;
		private IpointGraph ipg;
		private LoopNests lnt;
		private Ipoint source;
		private Ipoint destination;
		private IPGEdge transition;
		private boolean newRun = true;

		/*
		 * Temporary loop bounds
		 */
		protected HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> tempLoopBounds = new HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> ();
		protected HashMap<Integer, HashMap<Integer, HashSet<Integer>>> properDescendants = new HashMap<Integer, HashMap<Integer, HashSet<Integer>>> ();

		private TraceParser (LoopBoundDatabase database)
		{
		}

		private void doParsing ()
		{
			try
			{
				/*
				 * The automata walking begins from the IPG of the root
				 * subprogram
				 */
				resetToRoot ();

				BufferedReader in = new BufferedReader (new FileReader (Globals
						.getTraceFileName ()));
				String str;
				while ( (str = in.readLine ()) != null)
				{
					if (!str.startsWith ("//") && !str.startsWith ("/*"))
					{
						final String[] lexemes = str.split ("\\s+");
						long ipointID;

						if (lexemes[0].startsWith ("0x"))
						{
							ipointID = Long
									.parseLong (lexemes[0].substring (2));
						}
						else
						{
							ipointID = Long.parseLong (lexemes[0]);
						}

						long timeStamp = Long.parseLong (lexemes[1]);

						Debug.debugMessage (getClass (), "Ipoint = " + ipointID
								+ ", t = " + timeStamp, 3);

						try
						{
							if (source.hasTraceSuccessor (ipointID))
							{
								if (newRun)
								{
									startNewRun (ipointID);
								}
								else
								{
									continueRun (ipointID);
								}
							}
							else
							{
								throw new NoTransitionException (subprogram
										.getSubprogramName (), source
										.getVertexID (), ipointID);
							}
						}
						catch (NoTransitionException e)
						{
							/*
							 * Only exit the program
							 */
							if (source.getVertexID () != ipg.getEntryID ())
							{
								if (ipg.isMasterExit (source.getVertexID ()))
								{
									resetToRoot ();
								}
								else
								{
									System.err.println (e.getMessage ());
									System.exit (1);
								}
							}
						}
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

		private void resetToRoot ()
		{
			subprogramID = program.getRootID ();
			subprogram = program.getSubprogram (subprogramID);
			ipg = subprogram.getIPG (Globals.getInstrumentationProfile ());
			lnt = subprogram.getCFGStar (Globals.getInstrumentationProfile ())
					.getLNT ();
			source = ipg.getVertex (ipg.getEntryID ());
		}

		private void startNewRun (long ipointID)
		{
			Debug.debugMessage (getClass (), "NEW RUN", 4);
			newRun = false;
			transition = source.getTraceSuccessor (ipointID);
			source = ipg.getVertex (transition.getVertexID ());
		}

		private void continueRun (long ipointID)
		{
			Debug.debugMessage (getClass (), "CONTINUE RUN", 4);
			transition = source.getTraceSuccessor (ipointID);
			destination = ipg.getVertex (transition.getVertexID ());

			Debug.debugMessage (getClass (), "Analysing "
					+ source.getVertexID () + " => "
					+ destination.getVertexID () + " in "
					+ subprogram.getSubprogramName (), 3);

			updateLoopBounds ();
			advanceTransition ();
		}

		private void updateLoopBounds ()
		{
			for (int vertexID: transition.getEdgeLabel ())
			{
				boolean analyse = false;
				if (lnt.isLoopHeader (vertexID))
				{
					if (vertexID == source.getVertexID ())
					{
						if (source.getVertexID () == destination.getVertexID ())
						{
							analyse = true;
						}
					}
					else
					{
						analyse = true;
					}
				}

				if (analyse)
				{
					int headerVertexID = lnt.getVertex (vertexID)
							.getParentID ();
					TreeVertex headerv = lnt.getVertex (headerVertexID);

					int ancestorID = headerVertexID;
					while (ancestorID != lnt.getRootID ())
					{
						ancestorID = lnt.getVertex (ancestorID).getParentID ();
						TreeVertex ancestorv = lnt.getVertex (ancestorID);

						if (headerv.getLevel () - ancestorv.getLevel () <= adam.betts.tools.MainLoopAnalyser.Globals
								.getBoundLevel ())
						{
							Debug.debugMessage (getClass (),
									"Incrementing bound(" + headerVertexID
											+ ", " + ancestorID + ") in "
											+ subprogram.getSubprogramName (),
									4);

							int bound = tempLoopBounds.get (subprogramID).get (
									headerVertexID).get (ancestorID);
							tempLoopBounds.get (subprogramID).get (
									headerVertexID).put (ancestorID, bound + 1);
						}
					}
				}
			}

			int uParentID = lnt.getVertex (source.getVertexID ())
					.getParentID ();
			int vParentID = lnt.getVertex (destination.getVertexID ())
					.getParentID ();

			if (uParentID != vParentID)
			{
				if (properDescendants.get (subprogramID).get (vParentID)
						.contains (uParentID))
				{
					int lcaID = lcas.get (subprogramID).getLCA (
							source.getVertexID (), destination.getVertexID ());
					TreeVertex lca = lnt.getVertex (lcaID);

					for (int descendantID: properDescendants.get (subprogramID)
							.get (lcaID))
					{
						TreeVertex descendantv = lnt.getVertex (descendantID);

						Debug.debugMessage (getClass (), descendantID
								+ " is proper descendant of " + lcaID + " in "
								+ subprogram.getSubprogramName (), 4);

						int ancestorID = descendantID;
						while (ancestorID != lnt.getRootID ())
						{
							ancestorID = lnt.getVertex (ancestorID)
									.getParentID ();
							TreeVertex ancestorv = lnt.getVertex (ancestorID);

							if (descendantv.getLevel () - ancestorv.getLevel () <= adam.betts.tools.MainLoopAnalyser.Globals
									.getBoundLevel ())
							{
								if (ancestorv.getLevel () >= lca.getLevel ())
								{
									int bound = tempLoopBounds.get (
											subprogramID).get (descendantID)
											.get (ancestorID);
									tempLoopBounds.get (subprogramID).get (
											descendantID).put (ancestorID, 0);

									int currentBound = loopBounds.get (
											subprogramID).get (descendantID)
											.get (ancestorID);

									if (bound > currentBound)
									{
										Debug.debugMessage (getClass (),
												"Bound(" + descendantID + ", "
														+ ancestorID + ") = "
														+ bound, 4);

										loopBounds.get (subprogramID).get (
												descendantID).put (ancestorID,
												bound);
									}
									else
									{
										Debug.debugMessage (getClass (),
												"Bound(" + descendantID + ", "
														+ ancestorID + ") = "
														+ currentBound
														+ " (UNCHANGED)", 4);
									}
								}
							}
						}

					}
				}
			}
		}

		private void advanceTransition ()
		{
			if (destination.isInlinedEntry ())
			{
				final String destinationSubprogramName = destination
						.getSubprogramName ();
				final int destinationSubprogramID = program
						.getSubprogramID (destinationSubprogramName);

				Debug.debugMessage (getClass (), "Calling "
						+ destinationSubprogramName, 3);

				callStack.push (subprogramID);
				ipointStack.push (destination);
				subprogramID = destinationSubprogramID;
				subprogram = program.getSubprogram (subprogramID);
				ipg = subprogram.getIPG (Globals.getInstrumentationProfile ());
				lnt = subprogram.getCFGStar (
						Globals.getInstrumentationProfile ()).getLNT ();
				source = ipg.getVertex (ipg.getEntryID ());
				transition = source.getTraceSuccessor (destination
						.getIpointID ());
				source = ipg.getVertex (transition.getVertexID ());
			}
			else if (ipg.isMasterExit (destination.getVertexID ()))
			{
				if (subprogramID == program.getRootID ())
				{
					Debug.debugMessage (getClass (), "New run detected", 4);
					newRun = true;
					source = ipg.getVertex (ipg.getEntryID ());
				}
				else
				{
					Debug.debugMessage (getClass (), "Returning from "
							+ program.getSubprogram (subprogramID)
									.getSubprogramName (), 3);

					subprogramID = callStack.pop ();
					subprogram = program.getSubprogram (subprogramID);
					ipg = subprogram.getIPG (Globals
							.getInstrumentationProfile ());
					lnt = subprogram.getCFGStar (
							Globals.getInstrumentationProfile ()).getLNT ();
					source = ipointStack.pop ();
					transition = source.getTraceSuccessor (destination
							.getIpointID ());
					source = ipg.getVertex (transition.getVertexID ());
				}
			}
			else
			{
				source = destination;
			}
		}
	}

	@SuppressWarnings("serial")
	private class NoTransitionException extends Exception
	{
		public NoTransitionException (String subprogramName,
				int sourceID,
				long ipointID)
		{
			super ("Unable to find successor of ipoint " + sourceID
					+ " (in IPG of '" + subprogramName + "') with ipoint ID 0x"
					+ Long.toHexString (ipointID));
		}
	}
}
