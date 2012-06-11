package adam.betts.graphs.trees;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.DirectedGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DFSEdgeType;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;

public class DepthFirstTree extends Tree
{

	protected DirectedGraph directedg;
	protected HashMap <Integer, Integer> vToPre = new LinkedHashMap <Integer, Integer> ();
	protected HashMap <Integer, Integer> vToPost = new LinkedHashMap <Integer, Integer> ();
	protected HashMap <Integer, Integer> preToV = new LinkedHashMap <Integer, Integer> ();
	protected HashMap <Integer, Integer> postToV = new LinkedHashMap <Integer, Integer> ();
	protected HashMap <Integer, Set <Integer>> vToBackEdge = new LinkedHashMap <Integer, Set <Integer>> ();
	protected Set <Integer> visited = new HashSet <Integer> ();
	protected int preID = 1;
	protected int postID = 1;

	public DepthFirstTree (DirectedGraph directedg, int rootID)
	{
		this.directedg = directedg;
		this.rootID = rootID;

		Debug.debugMessage (getClass (), "Root = " + rootID, 2);

		initialise ();
		doDFS (rootID);
		setHeight ();
	}

	private void initialise ()
	{
		for (Vertex v : directedg)
		{
			addVertex (v.getVertexID ());
			vToPre.put (v.getVertexID (), 0);
			vToPost.put (v.getVertexID (), 0);
			vToBackEdge.put (v.getVertexID (), new HashSet <Integer> ());
		}
	}

	private void doDFS (int vertexID)
	{
		Debug.debugMessage (getClass (), "Visiting " + vertexID + " (pre-order = " + preID + ")", 4);

		visited.add (vertexID);
		vToPre.put (vertexID, preID);
		preToV.put (preID, vertexID);
		preID++;

		Vertex v = directedg.getVertex (vertexID);
		Iterator <Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			int succID = e.getVertexID ();

			if (!visited.contains (succID))
			{
				addEdge (vertexID, succID);
				doDFS (succID);
			} else if (vToPre.get (vertexID) < vToPre.get (succID))
			{
				;
			} else if (vToPost.get (succID) == 0)
			{
				vToBackEdge.get (vertexID).add (succID);
			}
		}

		vToPost.put (vertexID, postID);
		postToV.put (postID, vertexID);
		postID++;
	}

	public final int getPreID (int vertexID)
	{
		assert vToPre.containsKey (vertexID);
		return vToPre.get (vertexID);
	}

	public final int getPreVertexID (int preID)
	{
		assert preToV.containsKey (preID) : "Cannot find vertex ID corresponding to pre-order ID "
				+ preID;
		return preToV.get (preID);
	}

	public final int getPostID (int vertexID)
	{
		assert vToPost.containsKey (vertexID);
		return vToPost.get (vertexID);
	}

	public final int getPostVertexID (int postID)
	{
		assert postToV.containsKey (postID) : "Cannot find vertex ID corresponding to post-order ID "
				+ postID;
		return postToV.get (postID);
	}

	public final DFSEdgeType getEdgeType (int sourceID, int destinationID)
	{
		TreeVertex destination = (TreeVertex) idToVertex.get (destinationID);
		if (destination.getParentID () == sourceID && sourceID != rootID)
		{
			return DFSEdgeType.TREE_EDGE;
		} else if (vToBackEdge.get (sourceID).contains (destinationID))
		{
			return DFSEdgeType.BACK_EDGE;
		} else
		{
			return DFSEdgeType.CROSS_FORWARD_EDGE;
		}
	}
}
