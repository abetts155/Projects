package se.mdh.graphs.trees;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Stack;

import se.mdh.edges.Edge;
import se.mdh.graphs.Graph;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.trees.TreeVertex;

public class Tree extends Graph
{
	protected int rootID;
	protected HashMap<Integer, HashSet<TreeVertex>> levelMap = new LinkedHashMap<Integer, HashSet<TreeVertex>> ();

	public Tree ()
	{
	}

	public void addVertex (int vertexID)
	{
		TreeVertex v = new TreeVertex (vertexID);
		idToVertex.put (vertexID, v);
	}

	public TreeVertex getVertex (int vertexID)
	{
		return (TreeVertex) idToVertex.get (vertexID);
	}

	public final void setRootID (int rootID)
	{
		this.rootID = rootID;
	}

	public final int getRootID ()
	{
		return rootID;
	}

	public final void addEdge (int parentID, int childID)
	{
		if (!idToVertex.containsKey (parentID))
		{
			addVertex (parentID);
		}
		if (!idToVertex.containsKey (childID))
		{
			addVertex (childID);
		}

		TreeVertex parent = (TreeVertex) idToVertex.get (parentID);
		parent.addSuccessor (childID);
		TreeVertex child = (TreeVertex) idToVertex.get (childID);
		child.setParentID (parentID);
	}

	public final void removeEdge (int parentID, int childID)
	{
		TreeVertex parent = (TreeVertex) idToVertex.get (parentID);
		parent.removeSuccessor (childID);
		TreeVertex child = (TreeVertex) idToVertex.get (childID);
		child.setParentID (childID);
	}

	public final void setHeight ()
	{
		HashSet<Integer> visited = new HashSet<Integer> ();
		Stack<Integer> stack = new Stack<Integer> ();
		stack.push (rootID);
		while (!stack.isEmpty ())
		{
			int vertexID = stack.pop ();
			TreeVertex v = (TreeVertex) idToVertex.get (vertexID);
			visited.add (vertexID);
			int level;

			if (vertexID == rootID)
			{
				level = 0;
			}
			else
			{
				TreeVertex p = (TreeVertex) idToVertex.get (v.getParentID ());
				level = p.getLevel () + 1;
			}

			if (!levelMap.containsKey (level))
			{
				levelMap.put (level, new HashSet<TreeVertex> ());
			}
			v.setLevel (level);
			levelMap.get (level).add (v);

			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				int succID = e.getVertexID ();

				if (!visited.contains (succID))
				{
					stack.push (succID);
				}
			}
		}
	}

	public final int getHeight ()
	{
		return levelMap.size ();
	}

	public Iterator<TreeVertex> levelIterator (int level)
	{
		return levelMap.get (level).iterator ();
	}

	public final boolean isAncestor (int left, int right)
	{
		boolean answer;
		if (left == right)
		{
			answer = true;
		}
		else if (right == rootID)
		{
			answer = false;
		}
		else
		{
			int vertexID = right;
			TreeVertex v = getVertex (vertexID);
			int parentID = v.getParentID ();

			while (parentID != rootID && parentID != left)
			{
				vertexID = parentID;
				v = getVertex (vertexID);
				parentID = v.getParentID ();
			}

			if (parentID == left)
			{
				answer = true;
			}
			else
			{
				answer = false;
			}
		}
		return answer;
	}

	public final boolean isProperAncestor (int left, int right)
	{
		if (left == right)
		{
			return false;
		}
		else
		{
			return isAncestor (left, right);
		}
	}

	public final int numOfLeaves ()
	{
		int count = 0;
		for (Vertex v: this)
		{
			if (v.numOfSuccessors () == 0)
			{
				count++;
			}
		}
		return count;
	}

	public final int numOfInternalVertices ()
	{
		return numOfVertices () - numOfLeaves ();
	}
}
