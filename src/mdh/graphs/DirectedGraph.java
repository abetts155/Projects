package se.mdh.graphs;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import se.mdh.edges.Edge;
import se.mdh.vertices.Vertex;

public class DirectedGraph extends Graph
{
	public DirectedGraph ()
	{
	}

	public void addVertex (int vertexID)
	{
		idToVertex.put (vertexID, new Vertex (vertexID));
	}

	public Vertex getVertex (int vertexID)
	{
		return idToVertex.get (vertexID);
	}

	public void addEdge (int sourceID, int destinationID)
	{
		if (!idToVertex.containsKey (sourceID))
		{
			addVertex (sourceID);
		}
		if (!idToVertex.containsKey (destinationID))
		{
			addVertex (destinationID);
		}

		idToVertex.get (sourceID).addSuccessor (destinationID);
		idToVertex.get (destinationID).addPredecessor (sourceID);
	}

	public final void removeEdge (int sourceID, int destinationID)
	{
		idToVertex.get (sourceID).removeSuccessor (destinationID);
		idToVertex.get (destinationID).removePredecessor (sourceID);
	}

	public final void removeAllPredecessorEdges ()
	{
		for (Vertex v: idToVertex.values ())
		{
			int sourceID = v.getVertexID ();
			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				int destinationID = e.getVertexID ();
				Vertex destination = idToVertex.get (destinationID);
				if (destination.hasPredecessor (sourceID))
				{
					destination.removePredecessor (sourceID);
				}
			}
		}
	}

	public void reverseGraph (DirectedGraph reverseGraph)
	{
		for (Vertex v: idToVertex.values ())
		{
			int vertexID = v.getVertexID ();
			reverseGraph.addVertex (vertexID);
		}

		for (Vertex v: idToVertex.values ())
		{
			int sourceID = v.getVertexID ();
			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				int destinationID = e.getVertexID ();
				reverseGraph.addEdge (destinationID, sourceID);
			}
		}
	}

	public final HashSet<Integer> getReachableVertices (int vertexID)
	{
		HashSet<Integer> reachable = new HashSet<Integer> ();
		HashSet<Integer> visited = new HashSet<Integer> ();
		Stack<Integer> stack = new Stack<Integer> ();
		stack.push (vertexID);
		while (!stack.isEmpty ())
		{
			int sourceID = stack.pop ();
			Vertex v = idToVertex.get (sourceID);
			visited.add (sourceID);

			reachable.add (sourceID);

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
		return reachable;
	}
}
