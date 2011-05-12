package se.mdh.vertices.trees;

public class LeafVertex extends SyntaxVertex
{
	protected int CFGVertexID;
	protected boolean lamdba = false;

	public LeafVertex (int vertexID)
	{
		super (vertexID);
	}

	public final void setCFGVertexID (int CFGVertexID)
	{
		this.CFGVertexID = CFGVertexID;
	}

	public final int getCFGVertexID ()
	{
		return CFGVertexID;
	}

	public final void setLambdaVertex ()
	{
		this.lamdba = true;
	}

	public final boolean isLambdaVertex ()
	{
		return lamdba;
	}
}
