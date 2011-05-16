package adam.betts.graphs;

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Stack;

import adam.betts.edges.CallEdge;
import adam.betts.edges.Edge;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.programs.Program;
import adam.betts.tools.MainWCETAnalyser;
import adam.betts.utilities.Debug;
import adam.betts.vertices.call.CallVertex;
import adam.betts.vertices.call.ContextVertex;

public class ContextGraph extends DirectedGraph
{
	protected CallGraph callg;
	protected int rootID;
	protected LinkedHashMap<Integer, HashSet<Integer>> subprogramToContextIDs = new LinkedHashMap<Integer, HashSet<Integer>> ();
	protected LinkedHashMap<Integer, Stack<Integer>> subprogramToUnusedIDs = new LinkedHashMap<Integer, Stack<Integer>> ();

	public ContextGraph (final Program program)
	{
		this.callg = program.getCallGraph ();
		this.rootID = program.getRootID ();

		final DepthFirstTree dfs = new DepthFirstTree (callg, rootID);
		addVertices (program, dfs);
		addEdges (program, dfs);
	}

	public ContextVertex getVertex (int vertexID)
	{
		return (ContextVertex) idToVertex.get (vertexID);
	}

	private void addVertices (Program program, DepthFirstTree dfs)
	{
		Debug.debugMessage (getClass (), "Adding vertices", 3);

		for (int i = dfs.numOfVertices (); i >= 1; --i)
		{
			int subprogramID = dfs.getPostVertexID (i);

			Debug.debugMessage (getClass (),
					"Analysing "
							+ program.getSubprogram (subprogramID)
									.getSubprogramName (), 4);

			subprogramToContextIDs.put (subprogramID, new HashSet<Integer> ());
			subprogramToUnusedIDs.put (subprogramID, new Stack<Integer> ());
			CallVertex v = callg.getVertex (subprogramID);

			if (subprogramID == rootID)
			{
				addVertex (v.getSubprogramName (), subprogramID);
			}
			else
			{
				if (MainWCETAnalyser.Globals.expandContexts ())
				{
					Iterator<Edge> predIt = v.predecessorIterator ();
					while (predIt.hasNext ())
					{
						CallEdge e = (CallEdge) predIt.next ();
						int predID = e.getVertexID ();

						for (int j = 0; j < e.numOfCalls (); ++j)
						{
							for (int k = 0; k < subprogramToContextIDs.get (
									predID).size (); ++k)
							{
								addVertex (v.getSubprogramName (), subprogramID);
							}
						}
					}
				}
				else
				{
					addVertex (v.getSubprogramName (), subprogramID);
				}
			}
		}
	}

	private void addVertex (String subprogramName, int subprogramID)
	{
		int contextID = getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding context vertex for "
				+ subprogramName + " with context id = " + contextID, 4);

		idToVertex.put (contextID, new ContextVertex (contextID,
				subprogramName, subprogramID));
		subprogramToContextIDs.get (subprogramID).add (contextID);
		subprogramToUnusedIDs.get (subprogramID).add (contextID);
	}

	private void addEdges (Program program, DepthFirstTree dfs)
	{
		Debug.debugMessage (getClass (), "Adding edges", 3);

		for (int i = dfs.numOfVertices (); i >= 1; --i)
		{
			int callerID = dfs.getPostVertexID (i);

			Debug.debugMessage (getClass (), "Analysing "
					+ program.getSubprogram (callerID).getSubprogramName (), 4);

			CallVertex v = callg.getVertex (callerID);

			for (int callerContextID: subprogramToContextIDs.get (callerID))
			{
				ContextVertex callerv = (ContextVertex) idToVertex
						.get (callerContextID);

				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					CallEdge e = (CallEdge) succIt.next ();
					int calleeID = e.getVertexID ();

					if (MainWCETAnalyser.Globals.expandContexts ())
					{
						for (int siteID: e.callSites ())
						{
							int calleeContextID = subprogramToUnusedIDs.get (
									calleeID).pop ();

							Debug.debugMessage (getClass (), "Adding edge "
									+ callerContextID + " => "
									+ calleeContextID + " @ call site "
									+ siteID, 4);

							ContextVertex calleev = (ContextVertex) idToVertex
									.get (calleeContextID);
							calleev.addPredecessor (callerContextID, siteID);
							callerv.addSuccessor (calleeContextID, siteID);
						}
					}
					else
					{
						int calleeContextID = subprogramToUnusedIDs.get (
								calleeID).pop ();

						Debug.debugMessage (getClass (), "Adding edge "
								+ callerContextID + " => " + calleeContextID
								+ " @ call sites " + e.callSites (), 4);

						ContextVertex calleev = (ContextVertex) idToVertex
								.get (calleeContextID);
						calleev
								.addPredecessor (callerContextID, e
										.callSites ());
						callerv.addSuccessor (calleeContextID, e.callSites ());
					}
				}
			}
		}
	}
}
