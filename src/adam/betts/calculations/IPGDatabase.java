package adam.betts.calculations;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import adam.betts.edges.Edge;
import adam.betts.edges.IPGEdge;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.graphs.utils.LeastCommonAncestor;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainWCETAnalyser;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.utilities.Enums.IPGEdgeType;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class IPGDatabase extends Database
{
	protected HashMap <Integer, Long> observedWCETs = new HashMap <Integer, Long> ();

	public IPGDatabase (final Program program)
	{
		super (program);

		TraceParser parser = new TraceParser (this);
		initialise (parser);

		Debug.verboseMessage ("Parsing trace file " + Globals.getTraceFileName ());
		parser.doParsing (false);

		Debug.verboseMessage ("Doing final WCET calculation");
		engine.calculateWCETWithIPGs ();

		if (MainWCETAnalyser.Globals.doObservedWCET ())
		{
			Debug.verboseMessage ("Reparsing for Observed WCETs");
			parser.doParsing (true);
		}
	}

	public final long getObservedWCET (int subprogramID)
	{
		return observedWCETs.get (subprogramID);
	}

	private void initialise (TraceParser parser)
	{
		HashSet <Integer> reachableSubprograms = program.getCallGraph ().getReachableVertices (
				program.getRootID ());

		for (int subprogramID : reachableSubprograms)
		{
			Subprogram subprogram = program.getSubprogram (subprogramID);
			unitWCETs.put (subprogramID, new HashMap <Integer, Long> ());
			loopBounds.put (subprogramID, new HashMap <Integer, HashMap <Integer, Integer>> ());
			parser.tempLoopBounds.put (subprogramID,
					new HashMap <Integer, HashMap <Integer, Integer>> ());
			parser.properDescendants
					.put (subprogramID, new HashMap <Integer, HashSet <Integer>> ());
			mets.put (subprogramID, (long) 0);
			observedWCETs.put (subprogramID, (long) 0);
			tests.put (subprogramID, (long) 0);

			IpointGraph ipg = subprogram.getIPG (Globals.getInstrumentationProfile ());
			for (Vertex v : ipg)
			{
				Iterator <Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					IPGEdge e = (IPGEdge) succIt.next ();
					int edgeID = e.getEdgeID ();

					if (e.getEdgeType () == IPGEdgeType.TRACE_EDGE)
					{
						unitWCETs.get (subprogramID).put (edgeID, (long) 0);
					}
				}
			}

			Debug.debugMessage (getClass (), "Subprogram " + subprogram.getSubprogramName (), 3);
			LoopNests lnt = subprogram.getCFGStar (Globals.getInstrumentationProfile ()).getLNT ();
			LeastCommonAncestor lca = new LeastCommonAncestor (lnt);
			parser.lcas.put (subprogramID, lca);

			HashMap <Integer, HashSet <Integer>> ancestors = new HashMap <Integer, HashSet <Integer>> ();
			for (int level = 0; level < lnt.getHeight (); ++level)
			{
				Iterator <TreeVertex> levelIt = lnt.levelIterator (level);
				while (levelIt.hasNext ())
				{
					TreeVertex v = levelIt.next ();
					int vertexID = v.getVertexID ();

					HeaderVertex headerv = (HeaderVertex) v;

					if (lnt.isLoopHeader (vertexID) && !lnt.isSelfLoop (vertexID))
					{
						parser.properDescendants.get (subprogramID).put (vertexID,
								new HashSet <Integer> ());

						if (vertexID == lnt.getRootID ())
						{
							ancestors.put (vertexID, new HashSet <Integer> ());
							ancestors.get (vertexID).add (vertexID);
						} else
						{
							int parentID = v.getParentID ();
							ancestors.put (vertexID, new HashSet <Integer> ());
							ancestors.get (vertexID).addAll (ancestors.get (parentID));

							HashMap <Integer, Integer> ancestorToBound = new HashMap <Integer, Integer> ();
							HashMap <Integer, Integer> ancestorToBound2 = new HashMap <Integer, Integer> ();

							for (int ancestorID : ancestors.get (vertexID))
							{
								ancestorToBound.put (ancestorID, 0);
								ancestorToBound2.put (ancestorID, 0);
								parser.properDescendants.get (subprogramID).get (ancestorID).add (
										vertexID);
							}
							loopBounds.get (subprogramID).put (vertexID, ancestorToBound);
							parser.tempLoopBounds.get (subprogramID).put (vertexID,
									ancestorToBound2);

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
		private HashMap <Integer, LeastCommonAncestor> lcas = new HashMap <Integer, LeastCommonAncestor> ();

		/*
		 * Stacks needed during trace parsing
		 */
		private Stack <Integer> callStack = new Stack <Integer> ();
		private Stack <Ipoint> ipointStack = new Stack <Ipoint> ();
		private Stack <Long> metStack = new Stack <Long> ();
		private Stack <Long> observedWCETStack = new Stack <Long> ();

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
		private int edgeID;
		boolean newRun = true;

		private int testCounter;

		/*
		 * To keep track of times
		 */
		private long observedWCET = 0;
		private long T1 = 0;
		private long T2 = 0;

		/*
		 * Temporary loop bounds
		 */
		protected HashMap <Integer, HashMap <Integer, HashMap <Integer, Integer>>> tempLoopBounds = new HashMap <Integer, HashMap <Integer, HashMap <Integer, Integer>>> ();
		protected HashMap <Integer, HashMap <Integer, HashSet <Integer>>> properDescendants = new HashMap <Integer, HashMap <Integer, HashSet <Integer>>> ();

		private TraceParser (IPGDatabase database)
		{
		}

		private void doParsing (boolean doObservedWCET)
		{
			testCounter = 0;

			try
			{
				/*
				 * The automata walking begins from the IPG of the root
				 * subprogram
				 */
				resetToRoot ();

				BufferedReader in = new BufferedReader (
						new FileReader (Globals.getTraceFileName ()));
				String str;
				while ((str = in.readLine ()) != null)
				{
					if (!str.startsWith ("//") && !str.startsWith ("/*"))
					{
						final String[] lexemes = str.split ("\\s+");
						long ipointID;

						if (lexemes[0].startsWith ("0x"))
						{
							ipointID = Long.parseLong (lexemes[0].substring (2));
						} else
						{
							ipointID = Long.parseLong (lexemes[0]);
						}

						long timeStamp = Long.parseLong (lexemes[1]);

						Debug.debugMessage (getClass (), "Ipoint = " + ipointID + ", t = "
								+ timeStamp, 3);

						try
						{
							if (source.hasTraceSuccessor (ipointID))
							{
								if (newRun)
								{
									startNewRun (doObservedWCET, ipointID, timeStamp);
								} else
								{
									continueRun (doObservedWCET, ipointID, timeStamp);
								}
							} else
							{
								throw new NoTransitionException (subprogram.getSubprogramName (),
										source.getVertexID (), ipointID);
							}
						} catch (NoTransitionException e)
						{
							/*
							 * Only exit the program
							 */
							if (source.getVertexID () != ipg.getEntryID ())
							{
								if (ipg.isMasterExit (source.getVertexID ()))
								{
									resetToRoot ();
								} else
								{
									System.err.println (e.getMessage ());
									System.exit (1);
								}
							}
						}
					} else
					{
						Debug.debugMessage (getClass (), str, 4);
					}
				}
			} catch (Exception e)
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
			lnt = subprogram.getCFGStar (Globals.getInstrumentationProfile ()).getLNT ();
			source = ipg.getVertex (ipg.getEntryID ());
		}

		private void startNewRun (boolean doObservedWCET, long ipointID, long timeStamp)
		{
			newRun = false;
			transition = source.getTraceSuccessor (ipointID);
			source = ipg.getVertex (transition.getVertexID ());
			testCounter++;

			if (!doObservedWCET)
			{
				if (MainWCETAnalyser.Globals.doIncrementalWCET ())
				{
					/*
					 * Only do an incremental WCET computation if we have
					 * processed at least one run
					 */
					if (tests.get (program.getRootID ()) > 0)
					{
						engine.calculateWCETWithIPGs ();
					}
				}

				T1 = timeStamp;
				metStack.push (T1);
				tests.put (subprogramID, tests.get (subprogramID) + 1);
			} else
			{
				observedWCET = 0;
			}
		}

		private void continueRun (boolean doObservedWCET, long ipointID, long timeStamp)
		{
			transition = source.getTraceSuccessor (ipointID);
			destination = ipg.getVertex (transition.getVertexID ());
			edgeID = transition.getEdgeID ();

			Debug.debugMessage (getClass (), "Analysing " + source.getVertexID () + " => "
					+ destination.getVertexID () + " in " + subprogram.getSubprogramName (), 3);

			if (!doObservedWCET)
			{
				T2 = timeStamp;
				long transitionWCET = T2 - T1;

				if (transitionWCET > unitWCETs.get (subprogramID).get (edgeID))
				{
					unitWCETs.get (subprogramID).put (edgeID, transitionWCET);
				}

				updateLoopBounds ();

				T1 = T2;
			} else
			{
				observedWCET += unitWCETs.get (subprogramID).get (edgeID);
			}

			advanceTransition (doObservedWCET);
		}

		private void updateLoopBounds ()
		{
			int sourceHeaderID = lnt.getLoopHeader (source.getVertexID ());
			int destinationHeaderID = lnt.getLoopHeader (destination.getVertexID ());

			if (transition.isIterationEdge ())
			{
				Debug.debugMessage (getClass (), source.getVertexID () + " => "
						+ destination.getVertexID () + " is an iteration edge for "
						+ transition.iterationHeaderIDs (), 4);

				for (int headerID : transition.iterationHeaderIDs ())
				{
					for (int ancestorID : tempLoopBounds.get (subprogramID).get (headerID)
							.keySet ())
					{
						int bound = tempLoopBounds.get (subprogramID).get (headerID).get (
								ancestorID);
						tempLoopBounds.get (subprogramID).get (headerID)
								.put (ancestorID, bound + 1);
					}
				}

				if (sourceHeaderID != destinationHeaderID)
				{
					if (lnt.isNested (sourceHeaderID, destinationHeaderID))
					{
						handleLoopExit (sourceHeaderID, destinationHeaderID);
					}
				}
			} else if (transition.isExitEdge ())
			{
				handleLoopExit (sourceHeaderID, destinationHeaderID);
			}

		}

		private void handleLoopExit (int sourceHeaderID, int destinationHeaderID)
		{
			Debug.debugMessage (getClass (), "Loop exit detected", 4);

			int vertexID = sourceHeaderID;
			int lcaID = lcas.get (subprogramID).getLCA (source.getVertexID (),
					destination.getVertexID ());

			while (vertexID != lcaID)
			{
				TreeVertex v = lnt.getVertex (vertexID);
				vertexID = v.getParentID ();

				for (int descendantID : properDescendants.get (subprogramID).get (vertexID))
				{
					int bound = tempLoopBounds.get (subprogramID).get (descendantID).get (vertexID);
					tempLoopBounds.get (subprogramID).get (descendantID).put (vertexID, 0);

					int currentBound = loopBounds.get (subprogramID).get (descendantID).get (
							vertexID);

					if (bound > currentBound)
					{
						Debug.debugMessage (getClass (), "Bound(" + descendantID + ", " + vertexID
								+ ") = " + bound, 4);

						loopBounds.get (subprogramID).get (descendantID).put (vertexID, bound);
					} else
					{
						Debug.debugMessage (getClass (), "Bound(" + descendantID + ", " + vertexID
								+ ") = " + currentBound + " (UNCHANGED)", 4);
					}
				}
			}
		}

		private void advanceTransition (boolean doObservedWCET)
		{
			if (destination.isInlinedEntry ())
			{
				final String destinationSubprogramName = destination.getSubprogramName ();
				final int destinationSubprogramID = program
						.getSubprogramID (destinationSubprogramName);

				Debug.debugMessage (getClass (), "Calling " + destinationSubprogramName, 3);

				if (!doObservedWCET)
				{
					metStack.push (T2);
					tests.put (destinationSubprogramID, tests.get (destinationSubprogramID) + 1);
				} else
				{
					observedWCETStack.push (observedWCET);
					observedWCET = 0;
				}

				callStack.push (subprogramID);
				ipointStack.push (destination);
				subprogramID = destinationSubprogramID;
				subprogram = program.getSubprogram (subprogramID);
				ipg = subprogram.getIPG (Globals.getInstrumentationProfile ());
				lnt = subprogram.getCFGStar (Globals.getInstrumentationProfile ()).getLNT ();
				source = ipg.getVertex (ipg.getEntryID ());
				transition = source.getTraceSuccessor (destination.getIpointID ());
				source = ipg.getVertex (transition.getVertexID ());
			} else if (ipg.isMasterExit (destination.getVertexID ()))
			{
				if (!doObservedWCET)
				{
					long startTime = metStack.pop ();
					long met = T2 - startTime;

					if (met > mets.get (subprogramID))
					{
						mets.put (subprogramID, met);
					}
				} else
				{
					if (observedWCET > observedWCETs.get (subprogramID))
					{
						observedWCETs.put (subprogramID, observedWCET);
					}
				}

				if (subprogramID == program.getRootID ())
				{
					Debug.debugMessage (getClass (), "New run detected", 4);
					newRun = true;
					source = ipg.getVertex (ipg.getEntryID ());
				} else
				{
					Debug.debugMessage (getClass (), "Returning from "
							+ program.getSubprogram (subprogramID).getSubprogramName (), 3);

					if (doObservedWCET)
					{
						long startTime = observedWCETStack.pop ();
						observedWCET += startTime;
					}

					subprogramID = callStack.pop ();
					subprogram = program.getSubprogram (subprogramID);
					ipg = subprogram.getIPG (Globals.getInstrumentationProfile ());
					lnt = subprogram.getCFGStar (Globals.getInstrumentationProfile ()).getLNT ();
					source = ipointStack.pop ();
					transition = source.getTraceSuccessor (destination.getIpointID ());
					source = ipg.getVertex (transition.getVertexID ());
				}
			} else
			{
				source = destination;
			}
		}
	}

	@SuppressWarnings("serial")
	private static class NoTransitionException extends Exception
	{
		public NoTransitionException (String subprogramName, int sourceID, long ipointID)
		{
			super ("Unable to find successor of ipoint " + sourceID + " (in IPG of '"
					+ subprogramName + "') with ipoint ID 0x" + Long.toHexString (ipointID));
		}
	}
}
