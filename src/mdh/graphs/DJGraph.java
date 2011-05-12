package se.mdh.graphs;

import java.util.Iterator;

import se.mdh.edges.Edge;
import se.mdh.graphs.trees.DominatorTree;
import se.mdh.utilities.Enums.DJEdgeType;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.trees.TreeVertex;

public class DJGraph extends FlowGraph
{
	protected FlowGraph flowg;
	protected DominatorTree dominatort;

	public DJGraph (FlowGraph flowg, DominatorTree dominatort)
	{
		this.flowg = flowg;
		this.dominatort = dominatort;

		addVertices ();
		addDominatorTreeEdges ();
		addMissingEdges ();
	}

	public final DJEdgeType getEdgeType (int sourceID, int destinationID)
	{
		if (dominatort.getVertex (sourceID).hasSuccessor (destinationID))
		{
			return DJEdgeType.DOMINATOR_EDGE;
		}
		else if (dominatort.isAncestor (destinationID, sourceID))
		{
			return DJEdgeType.BACK_JOIN_EDGE;
		}
		else
		{
			return DJEdgeType.CROSS_JOIN_EDGE;
		}
	}

	private void addVertices ()
	{
		for (Vertex v: flowg)
		{
			int vertexID = v.getVertexID ();
			idToVertex.put (vertexID, new Vertex (vertexID));
		}
	}

	private void addDominatorTreeEdges ()
	{
		for (Vertex v: dominatort)
		{
			int vertexID = v.getVertexID ();
			TreeVertex treev = dominatort.getVertex (vertexID);
			int parentID = treev.getParentID ();
			addEdge (parentID, vertexID);
		}
	}

	private void addMissingEdges ()
	{
		for (Vertex v: flowg)
		{
			int vertexID = v.getVertexID ();
			Vertex djv = getVertex (vertexID);

			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				int succID = e.getVertexID ();

				if (!djv.hasSuccessor (succID))
				{
					addEdge (vertexID, succID);
				}
			}
		}
	}
}
