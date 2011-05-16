package adam.betts.graphs.utils;

import java.util.HashMap;
import java.util.LinkedHashMap;

public class SetUnion
{
	private HashMap<Integer, Integer> rank = new LinkedHashMap<Integer, Integer> ();
	private HashMap<Integer, Integer> parent = new LinkedHashMap<Integer, Integer> ();

	public SetUnion ()
	{
	}

	public final void makeSet (int vertexID)
	{
		rank.put (vertexID, 0);
		parent.put (vertexID, vertexID);
	}

	public final int findSet (int vertexID)
	{
		int parentID = parent.get (vertexID);
		if (vertexID != parentID)
		{
			parentID = findSet (parentID);
		}
		return parentID;
	}

	private void link (int left, int right)
	{
		if (rank.get (left) > rank.get (right))
		{
			parent.put (right, left);
		}
		else
		{
			parent.put (left, right);
			if (rank.get (left) == rank.get (right))
			{
				rank.put (right, rank.get (left) + 1);
			}
		}
	}

	public final void union (int left, int right)
	{
		link (findSet (left), findSet (right));
	}
}
