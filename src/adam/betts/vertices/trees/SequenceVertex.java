package adam.betts.vertices.trees;

import adam.betts.edges.Edge;

public class SequenceVertex extends ProgramSyntaxVertex
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
