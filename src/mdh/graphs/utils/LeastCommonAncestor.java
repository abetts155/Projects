package se.mdh.graphs.utils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;

import se.mdh.edges.Edge;
import se.mdh.graphs.trees.Tree;
import se.mdh.utilities.Debug;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.trees.TreeVertex;

public class LeastCommonAncestor
{
	private Tree tree;
	private HashMap<Integer, Integer> representative = new LinkedHashMap<Integer, Integer> ();
	private int[] euler;
	private int eulerIndex = 0;
	private int[] level;
	private int dummyLevel;

	public LeastCommonAncestor (Tree tree)
	{
		this.tree = tree;
		dummyLevel = tree.getHeight () + 1;
		euler = new int[2 * tree.numOfVertices () - 1];
		level = new int[2 * tree.numOfVertices () - 1];

		doDFS (tree.getRootID ());
		computeLevelsAndRepresentatives ();
	}

	private void doDFS (int vertexID)
	{
		euler[eulerIndex] = vertexID;
		eulerIndex++;

		Vertex v = tree.getVertex (vertexID);
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			doDFS (e.getVertexID ());

			euler[eulerIndex] = vertexID;
			eulerIndex++;
		}
	}

	private void computeLevelsAndRepresentatives ()
	{
		for (int i = 0; i < 2 * tree.numOfVertices () - 1; ++i)
		{
			int vertexID = euler[i];
			TreeVertex v = tree.getVertex (vertexID);
			level[i] = v.getLevel ();
			representative.put (vertexID, i);
		}
	}

	public final int getLCA (int left, int right)
	{
		int repID1 = representative.get (left);
		int repID2 = representative.get (right);
		int lowestLevel = dummyLevel;
		int startIndex;
		int endIndex;
		int levelIndex = 2 * tree.numOfVertices ();

		if (repID1 < repID2)
		{
			startIndex = repID1;
			endIndex = repID2;
		}
		else
		{
			startIndex = repID2;
			endIndex = repID1;
		}

		for (int i = startIndex; i <= endIndex; ++i)
		{
			if (level[i] < lowestLevel)
			{
				lowestLevel = level[i];
				levelIndex = i;
			}
		}

		Debug.debugMessage (getClass (), "lca(" + left + "," + right + ") = "
				+ euler[levelIndex], 4);
		return euler[levelIndex];
	}
}
