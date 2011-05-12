package se.mdh.graphs.trees;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;

import se.mdh.edges.Edge;
import se.mdh.graphs.FlowGraph;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Enums.DominatorTreeType;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.trees.TreeVertex;

public class DominatorTree extends Tree
{
	protected FlowGraph flowg;
	protected HashMap<Integer, Integer> vToIdom = new LinkedHashMap<Integer, Integer> ();
	protected DominatorTreeType type;

	public DominatorTree (FlowGraph flowg, int rootID, DominatorTreeType type)
	{
		this.flowg = flowg;
		this.rootID = rootID;
		this.type = type;

		initialise ();
		solveDFF ();
		addEdges ();
		setHeight ();
	}

	public final DominatorTreeType getType ()
	{
		return type;
	}

	public final int getImmediateDominator (int vertexID)
	{
		TreeVertex v = (TreeVertex) idToVertex.get (vertexID);
		return v.getParentID ();
	}

	public final boolean dominates (int left, int right)
	{
		return isAncestor (left, right);
	}

	public final boolean strictlyDominates (int left, int right)
	{
		return isProperAncestor (left, right);
	}

	private void initialise ()
	{
		for (Vertex v: flowg)
		{
			int vertexID = v.getVertexID ();
			addVertex (vertexID);
			if (vertexID == rootID)
			{
				vToIdom.put (vertexID, vertexID);
			}
			else
			{
				vToIdom.put (vertexID, Vertex.DUMMY_VERTEX_ID);
			}
		}
	}

	private void solveDFF ()
	{
		DepthFirstTree dfs = new DepthFirstTree (flowg, rootID);

		boolean changed = true;
		while (changed)
		{
			changed = false;

			for (int i = dfs.numOfVertices (); i >= 1; --i)
			{
				int vertexID = dfs.getPostVertexID (i);
				Vertex v = flowg.getVertex (vertexID);

				/*
				 * Ignore any edges incident to the root
				 */
				if (vertexID != rootID)
				{
					int processedPredID = Vertex.DUMMY_VERTEX_ID;
					int newIdomID = Vertex.DUMMY_VERTEX_ID;

					Iterator<Edge> predIt = v.predecessorIterator ();
					while (predIt.hasNext ())
					{
						Edge e = predIt.next ();
						int predID = e.getVertexID ();

						if (vToIdom.get (predID) != Vertex.DUMMY_VERTEX_ID)
						{
							processedPredID = predID;
							newIdomID = processedPredID;
						}
					}

					predIt = v.predecessorIterator ();
					while (predIt.hasNext ())
					{
						Edge e = predIt.next ();
						int predID = e.getVertexID ();

						if (predID != processedPredID)
						{
							if (vToIdom.get (predID) != Vertex.DUMMY_VERTEX_ID)
							{
								newIdomID = intersect (dfs, predID, newIdomID);
							}
						}
					}

					if (newIdomID != Vertex.DUMMY_VERTEX_ID
							&& vToIdom.get (vertexID) != newIdomID)
					{
						changed = true;
						vToIdom.put (vertexID, newIdomID);
					}
				}
			}
		}
	}

	private int intersect (DepthFirstTree dfs, int left, int right)
	{
		int uID = left;
		int vID = right;

		while (dfs.getPostID (uID) != dfs.getPostID (vID))
		{
			while (dfs.getPostID (uID) < dfs.getPostID (vID))
			{
				uID = vToIdom.get (uID);
			}

			while (dfs.getPostID (vID) < dfs.getPostID (uID))
			{
				vID = vToIdom.get (vID);
			}
		}
		return uID;
	}

	private void addEdges ()
	{
		for (Vertex v: flowg)
		{
			int vertexID = v.getVertexID ();
			if (vertexID != rootID)
			{
				Debug.debugMessage (getClass (), "idom(" + vertexID + ") = "
						+ vToIdom.get (vertexID), 3);
				addEdge (vToIdom.get (vertexID), vertexID);
			}
		}
	}
}
