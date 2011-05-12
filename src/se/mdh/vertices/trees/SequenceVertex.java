package se.mdh.vertices.trees;

import se.mdh.edges.Edge;

public class SequenceVertex extends SyntaxVertex
{
	public SequenceVertex (int vertexID)
	{
		super (vertexID);
	}

	public final void addAsFirstSuccessor (int successorID)
	{
		successors.add (0, new Edge (successorID));
	}
}
