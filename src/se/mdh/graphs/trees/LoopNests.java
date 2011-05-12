package se.mdh.graphs.trees;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import se.mdh.edges.Edge;
import se.mdh.graphs.DirectedGraph;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Enums.DFSEdgeType;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.trees.HeaderVertex;
import se.mdh.vertices.trees.TreeVertex;

public class LoopNests extends Tree
{
	protected final DirectedGraph directedg;
	protected HashMap<Integer, Set<Integer>> headerToLoop = new LinkedHashMap<Integer, Set<Integer>> ();
	protected HashMap<Integer, Set<Integer>> headerToTails = new LinkedHashMap<Integer, Set<Integer>> ();
	protected HashMap<Integer, Set<Integer>> headerToExits = new LinkedHashMap<Integer, Set<Integer>> ();
	private HashMap<Integer, Integer> parent = new LinkedHashMap<Integer, Integer> ();

	public LoopNests (DirectedGraph directedg, int rootID)
	{
		this.directedg = directedg;

		initialise ();
		findLoops (rootID);
		setRoot ();
		identifyLoopExits ();
		setHeight ();
	}

	public final boolean isLoopHeader (int vertexID)
	{
		return headerToLoop.containsKey (vertexID);
	}

	public final int getLoopHeader (int vertexID)
	{
		if (headerToLoop.containsKey (vertexID))
		{
			return vertexID;
		}
		else
		{
			TreeVertex v = (TreeVertex) idToVertex.get (vertexID);
			return v.getParentID ();
		}
	}

	public final boolean isSelfLoop (int vertexID)
	{
		if (!headerToLoop.containsKey (vertexID))
		{
			return false;
		}
		else
		{
			return headerToLoop.get (vertexID).size () == 1;
		}
	}

	public final int numOfSelfLoops ()
	{
		int total = 0;
		for (int headerID: headerToLoop.keySet ())
		{
			if (isSelfLoop (headerID))
			{
				total++;
			}
		}
		return total;
	}

	public final boolean hasSelfLoops ()
	{
		return numOfSelfLoops () > 0;
	}

	public final int numOfLoops ()
	{
		return headerToLoop.size ();
	}

	public final Iterator<Integer> bodyIterator (int headerID)
	{
		return headerToLoop.get (headerID).iterator ();
	}

	public boolean inLoopBody (int headerID, int vertexID)
	{
		if (headerID == vertexID)
		{
			return true;
		}
		else
		{
			TreeVertex v = (TreeVertex) idToVertex.get (vertexID);
			return v.getParentID () == headerID;
		}
	}

	public final Iterator<Integer> tailIterator (int headerID)
	{
		return headerToTails.get (headerID).iterator ();
	}

	public final Set<Integer> getTails (int headerID)
	{
		return headerToTails.get (headerID);
	}

	public final int numOfTails (int headerID)
	{
		return headerToTails.get (headerID).size ();
	}

	public final boolean isLoopTail (int headerID, int vertexID)
	{
		return headerToTails.get (headerID).contains (vertexID);
	}

	public final Iterator<Integer> exitIterator (int headerID)
	{
		return headerToExits.get (headerID).iterator ();
	}

	public final int numOfExits (int headerID)
	{
		return headerToExits.get (headerID).size ();
	}

	public final boolean isLoopExit (int headerID, int vertexID)
	{
		return headerToExits.get (headerID).contains (vertexID);
	}

	public final Iterator<Integer> headerIterator ()
	{
		return headerToLoop.keySet ().iterator ();
	}

	public final boolean isNested (int left, int right)
	{
		return isProperAncestor (right, left);
	}

	private void initialise ()
	{
		Debug.debugMessage (getClass (), "Initialising", 3);

		for (Vertex v: directedg)
		{
			int vertexID = v.getVertexID ();
			parent.put (vertexID, vertexID);
			addVertex (vertexID);
		}
	}

	private void findLoops (int rootID)
	{
		DepthFirstTree dfsTree = new DepthFirstTree (directedg, rootID);
		for (int i = dfsTree.numOfVertices (); i >= 1; --i)
		{
			Integer vertexID = dfsTree.getPreVertexID (i);
			Vertex v = directedg.getVertex (vertexID);

			ArrayList<Integer> workList = new ArrayList<Integer> ();

			Iterator<Edge> predIt = v.predecessorIterator ();
			while (predIt.hasNext ())
			{
				Edge e = predIt.next ();
				int predID = e.getVertexID ();

				Debug.debugMessage (getClass (), "Analysing " + predID + " => "
						+ vertexID, 4);

				if (dfsTree.getEdgeType (predID, vertexID) == DFSEdgeType.BACK_EDGE)
				{
					if (predID == vertexID)
					{
						Debug.debugMessage (getClass (), vertexID
								+ " is self-loop", 3);
						addSelfLoop (vertexID);
					}
					else
					{
						workList.add (parent.get (predID));
					}
				}
			}
			workList.remove (vertexID);

			if (!workList.isEmpty ())
			{
				findLoop (dfsTree, workList, vertexID);
			}
		}
	}

	private void addSelfLoop (int headerID)
	{
		Debug.debugMessage (getClass (),
				"Header " + headerID + " is self-loop", 3);

		int vertexID = getNextVertexID ();
		HeaderVertex header = new HeaderVertex (vertexID, headerID);
		idToVertex.put (vertexID, header);
		addEdge (header.getVertexID (), headerID);

		HashSet<Integer> loopBody = new HashSet<Integer> ();
		loopBody.add (headerID);
		headerToLoop.put (headerID, loopBody);

		HashSet<Integer> tails = new HashSet<Integer> ();
		tails.add (headerID);
		headerToTails.put (headerID, tails);
	}

	private HeaderVertex findHeader (int headerID)
	{
		for (Vertex v: this)
		{
			if (v instanceof HeaderVertex)
			{
				HeaderVertex headerv = (HeaderVertex) v;
				if (headerv.getHeaderID () == headerID)
				{
					return headerv;
				}
			}
		}
		return null;
	}

	private void findLoop (DepthFirstTree dfsTree,
			ArrayList<Integer> workList,
			int headerID)
	{
		Debug.debugMessage (getClass (), "Header " + headerID + " tails = "
				+ workList, 1);

		if (headerToTails.containsKey (headerID))
		{
			headerToTails.get (headerID).addAll (workList);
		}
		else
		{
			HashSet<Integer> tails = new HashSet<Integer> ();
			tails.addAll (workList);
			headerToTails.put (headerID, tails);

			/*
			 * Add the internal vertex to the tree representing this header
			 */
			int vertexID = getNextVertexID ();
			HeaderVertex header = new HeaderVertex (vertexID, headerID);
			idToVertex.put (vertexID, header);
		}

		HashSet<Integer> loopBody = new HashSet<Integer> ();
		while (!workList.isEmpty ())
		{
			int listID = workList.remove (workList.size () - 1);
			loopBody.add (listID);

			Vertex v = directedg.getVertex (listID);
			Iterator<Edge> predIt = v.predecessorIterator ();
			while (predIt.hasNext ())
			{
				Edge e = predIt.next ();
				int predID = e.getVertexID ();

				if (dfsTree.getEdgeType (predID, listID) != DFSEdgeType.BACK_EDGE)
				{
					int repID = parent.get (predID);

					if (!workList.contains (repID)
							&& !loopBody.contains (repID) && repID != headerID)
					{
						workList.add (repID);
					}
				}
			}
		}

		if (!loopBody.isEmpty ())
		{
			HeaderVertex headerv = findHeader (headerID);

			for (int vertexID: loopBody)
			{
				Debug.debugMessage (getClass (), vertexID + " is in loop body",
						4);

				parent.put (vertexID, headerID);
				if (isLoopHeader (vertexID))
				{
					HeaderVertex innerHeaderv = findHeader (vertexID);
					addEdge (headerv.getVertexID (), innerHeaderv
							.getVertexID ());
				}
				else
				{
					addEdge (headerv.getVertexID (), vertexID);
				}
			}

			loopBody.add (headerID);
			addEdge (headerv.getVertexID (), headerID);

			if (!headerToLoop.containsKey (headerID))
			{
				headerToLoop.put (headerID, loopBody);
			}
			else
			{
				headerToLoop.get (headerID).addAll (loopBody);
			}
		}
	}

	private void setRoot ()
	{
		for (Vertex v: this)
		{
			if (v.numOfPredecessors () == 0)
			{
				this.rootID = v.getVertexID ();
			}
		}
	}

	private void identifyLoopExits ()
	{
		Debug.debugMessage (getClass (), "Identifying loop exits", 3);

		for (int headerID: headerToLoop.keySet ())
		{
			headerToExits.put (headerID, new HashSet<Integer> ());

			for (int vertexID: headerToLoop.get (headerID))
			{
				Vertex v = directedg.getVertex (vertexID);
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					int succID = e.getVertexID ();

					if (!headerToLoop.get (headerID).contains (succID))
					{
						if (headerID != vertexID && isLoopHeader (vertexID))
						{
							if (!headerToLoop.get (vertexID).contains (succID))
							{
								headerToExits.get (headerID).add (vertexID);
							}
						}
						else
						{
							headerToExits.get (headerID).add (vertexID);
						}
					}
				}
			}
		}
	}
}
