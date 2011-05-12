package se.mdh.graphs;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;

import se.mdh.edges.CallEdge;
import se.mdh.edges.Edge;
import se.mdh.graphs.trees.DepthFirstTree;
import se.mdh.graphs.trees.LoopNests;
import se.mdh.outputs.UDrawGraph;
import se.mdh.programs.Program;
import se.mdh.programs.Subprogram;
import se.mdh.utilities.CallBack;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Globals;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.call.CallLoopVertex;
import se.mdh.vertices.call.CallVertex;
import se.mdh.vertices.call.LoopVertex;
import se.mdh.vertices.call.SubprogramVertex;
import se.mdh.vertices.trees.TreeVertex;

public class CallLoopGraph extends DirectedGraph
{
	protected final Program program;
	protected final int rootID;

	public CallLoopGraph (final Program program)
	{
		this.program = program;
		this.rootID = program.getRootID ();

		DepthFirstTree dfs = new DepthFirstTree (program.getCallGraph (),
				program.getRootID ());
		addSubprogramVertices (dfs);
		addEdges (dfs);

		Debug.debugMessage (new CallBack ()
		{
			public void doJob ()
			{
				new CheckGraphProperties ();
			}
		}, 3);
	}

	public CallLoopVertex getVertex (int vertexID)
	{
		return (CallLoopVertex) idToVertex.get (vertexID);
	}

	public final int getRootID ()
	{
		return rootID;
	}

	private void addSubprogramVertices (DepthFirstTree dfs)
	{
		Debug.debugMessage (getClass (), "Adding subprogram vertices", 3);

		for (Vertex v: dfs)
		{
			int subprogramID = v.getVertexID ();
			Subprogram subprogram = program.getSubprogram (subprogramID);
			SubprogramVertex subprogramv = new SubprogramVertex (subprogramID,
					subprogramID, subprogram.getSubprogramName ());
			idToVertex.put (subprogramID, subprogramv);

			Debug.debugMessage (getClass (), "Adding subprogram vertex for "
					+ subprogram.getSubprogramName () + " (with vertex ID "
					+ subprogramID + ")", 4);
		}
	}

	private void addEdges (DepthFirstTree dfs)
	{
		Debug.debugMessage (getClass (), "Adding edges", 3);

		CallGraph callg = program.getCallGraph ();
		for (int i = dfs.numOfVertices (); i >= 1; --i)
		{
			final int subprogramID = dfs.getPostVertexID (i);
			final Subprogram subprogram = program.getSubprogram (subprogramID);
			final ControlFlowGraph cfg = subprogram.getCFG ();
			cfg.addEntryAndExitEdges ();
			final int entryID = cfg.getEntryID ();
			final LoopNests lnt = new LoopNests (cfg, entryID);
			final CallVertex callv = callg.getVertex (subprogramID);

			if (Globals.uDrawDirectorySet ())
			{
				UDrawGraph.makeUDrawFile (cfg, subprogram.getSubprogramName ());
				UDrawGraph.makeUDrawFile (lnt, subprogram.getSubprogramName ());
			}

			Set<Integer> loopsAdded = new HashSet<Integer> ();
			Set<LoopVertex> verticesAdded = new HashSet<LoopVertex> ();

			Debug.debugMessage (getClass (), "Analysing subprogram "
					+ subprogram.getSubprogramName (), 4);

			for (int siteID: callv.getCallSites ())
			{
				addLoopVertex (lnt, loopsAdded, verticesAdded, siteID, entryID,
						subprogram);
			}

			linkLoopVertices (lnt, loopsAdded, verticesAdded, entryID,
					subprogram);

			Iterator<Edge> succIt = callv.successorIterator ();
			while (succIt.hasNext ())
			{
				CallEdge e = (CallEdge) succIt.next ();
				int calleeID = e.getVertexID ();
				linkCallVertex (lnt, verticesAdded, calleeID, e.callSites (),
						entryID, subprogram);
			}
		}
	}

	private void addLoopVertex (LoopNests lnt, Set<Integer> loopsAdded,
			Set<LoopVertex> verticesAdded, int siteID, int entryID,
			Subprogram subprogram)
	{
		Debug.debugMessage (getClass (), "Adding loop vertex for call site "
				+ siteID, 4);

		TreeVertex treev = lnt.getVertex (siteID);
		int headerID;
		if (treev.isInternalVertex ())
		{
			headerID = siteID;
		}
		else
		{
			headerID = treev.getParentID ();
		}

		while (!loopsAdded.contains (headerID) && headerID != entryID)
		{
			int vertexID = getNextVertexID ();
			LoopVertex loopv = new LoopVertex (vertexID, subprogram
					.getSubprogramID (), subprogram.getSubprogramName (),
					headerID);
			idToVertex.put (vertexID, loopv);
			verticesAdded.add (loopv);
			loopsAdded.add (headerID);

			Debug.debugMessage (getClass (),
					"Adding loop vertex for loop header " + headerID
							+ " in subprogram "
							+ subprogram.getSubprogramName ()
							+ " (with vertex ID " + vertexID + ")", 4);

			treev = lnt.getVertex (headerID);
			headerID = treev.getParentID ();
		}
	}

	private void linkLoopVertices (LoopNests lnt, Set<Integer> loopsAdded,
			Set<LoopVertex> verticesAdded, int entryID, Subprogram subprogram)
	{
		for (int headerID: loopsAdded)
		{
			LoopVertex v = getLoopVertex (verticesAdded, headerID);
			TreeVertex treev = lnt.getVertex (headerID);
			int parentID = treev.getParentID ();

			int predID;
			if (parentID == entryID)
			{
				predID = subprogram.getSubprogramID ();
			}
			else
			{
				LoopVertex p = getLoopVertex (verticesAdded, parentID);
				predID = p.getVertexID ();
			}

			addEdge (predID, v.getVertexID ());
			Debug.debugMessage (getClass (), "Adding edge " + predID + " => "
					+ v.getVertexID (), 4);
		}
	}

	private LoopVertex getLoopVertex (Set<LoopVertex> verticesAdded,
			int headerID)
	{
		for (LoopVertex v: verticesAdded)
		{
			if (v.getHeaderID () == headerID)
			{
				return v;
			}
		}
		return null;
	}

	private void linkCallVertex (LoopNests lnt, Set<LoopVertex> verticesAdded,
			int calleeID, Set<Integer> callSites, int entryID,
			Subprogram subprogram)
	{
		final int subprogramID = subprogram.getSubprogramID ();

		for (int siteID: callSites)
		{
			TreeVertex treev = lnt.getVertex (siteID);
			int parentID;
			if (treev.isInternalVertex ())
			{
				parentID = siteID;
			}
			else
			{
				parentID = treev.getParentID ();
			}

			if (parentID == entryID)
			{
				Vertex v = idToVertex.get (subprogramID);
				if (!v.hasSuccessor (calleeID))
				{
					addEdge (subprogramID, calleeID);

					Debug.debugMessage (getClass (), "Adding edge "
							+ subprogramID + " => " + calleeID, 4);
				}
			}
			else
			{
				LoopVertex loopv = getLoopVertex (verticesAdded, parentID);
				if (!loopv.hasSuccessor (calleeID))
				{
					addEdge (loopv.getVertexID (), calleeID);

					Debug.debugMessage (getClass (), "Adding edge "
							+ loopv.getVertexID () + " => " + calleeID, 4);
				}
			}
		}
	}

	private class CheckGraphProperties
	{
		private CheckGraphProperties ()
		{
			Set<Integer> noPreds = new HashSet<Integer> ();
			Set<LoopVertex> noSuccs = new HashSet<LoopVertex> ();

			for (Vertex v: idToVertex.values ())
			{
				if (v.numOfPredecessors () == 0)
				{
					noPreds.add (v.getVertexID ());
				}
				if (v instanceof LoopVertex)
				{
					if (v.numOfSuccessors () == 0)
					{
						noSuccs.add ((LoopVertex) v);
					}
				}
			}

			if (noPreds.size () == 0)
			{
				Debug.debugMessage (getClass (), "No root found in CLDS", 1);
				System.exit (1);
			}
			else if (noPreds.size () > 1)
			{
				Debug.debugMessage (getClass (),
						"Too many roots found in CLDS: " + noPreds, 1);
				System.exit (1);
			}

			if (noSuccs.size () != 0)
			{
				Debug.debugMessage (getClass (),
						"The following loop vertices have no successors", 1);
				for (LoopVertex v: noSuccs)
				{
					Debug.debugMessage (getClass (), "Vertex "
							+ v.getVertexID () + " modelling the loop "
							+ v.getHeaderID () + " in "
							+ v.getSubprogramName (), 1);
				}
				System.exit (1);
			}

			doDFS ();
		}

		private void doDFS ()
		{
			HashSet<Integer> visited = new HashSet<Integer> ();
			Stack<Integer> stack = new Stack<Integer> ();
			stack.push (rootID);

			while (!stack.isEmpty ())
			{
				int vertexID = stack.pop ();
				visited.add (vertexID);

				Vertex v = idToVertex.get (vertexID);
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge succEdge = succIt.next ();
					int succID = succEdge.getVertexID ();
					if (!visited.contains (succID))
					{
						stack.push (succID);
					}
				}
			}

			if (visited.size () != numOfVertices ())
			{
				Debug.debugMessage (getClass (),
						"The graph is not connected. Only visited " + visited,
						1);
				System.exit (1);
			}
		}
	}
}
