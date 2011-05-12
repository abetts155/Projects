package se.mdh.graphs;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;

import se.mdh.edges.Edge;
import se.mdh.vertices.Vertex;

public abstract class Graph implements Iterable<Vertex>
{
	protected HashMap<Integer, Vertex> idToVertex = new LinkedHashMap<Integer, Vertex> ();

	public final boolean hasVertex (int vertexID)
	{
		return idToVertex.containsKey (vertexID);
	}

	public final void removeVertex (int vertexID)
	{
		Vertex v = idToVertex.get (vertexID);

		Iterator<Edge> predIt = v.predecessorIterator ();
		while (predIt.hasNext ())
		{
			Edge e = predIt.next ();
			int predID = e.getVertexID ();
			if (idToVertex.containsKey (predID))
			{
				Vertex p = idToVertex.get (e.getVertexID ());
				if (p.hasSuccessor (vertexID))
				{
					p.removeSuccessor (vertexID);
				}
			}
		}

		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			int succID = e.getVertexID ();
			if (idToVertex.containsKey (succID))
			{
				Vertex s = idToVertex.get (e.getVertexID ());
				if (s.hasPredecessor (vertexID))
				{
					s.removePredecessor (vertexID);
				}
			}
		}

		idToVertex.remove (vertexID);
	}

	public Vertex getVertex (int vertexID)
	{
		return idToVertex.get (vertexID);
	}

	public final int numOfVertices ()
	{
		return idToVertex.size ();
	}

	public final int numOfEdges ()
	{
		int total = 0;
		for (Vertex v: this)
		{
			total += v.numOfSuccessors ();
		}
		return total;
	}

	public Iterator<Vertex> iterator ()
	{
		return idToVertex.values ().iterator ();
	}

	public final int getNextVertexID ()
	{
		int nextID = 1;
		boolean stop = false;

		while (nextID < Integer.MAX_VALUE && !stop)
		{
			if (!idToVertex.containsKey (nextID))
			{
				stop = true;
			}
			else
			{
				nextID++;
			}
		}
		return nextID;
	}
}
