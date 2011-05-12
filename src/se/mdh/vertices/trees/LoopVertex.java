package se.mdh.vertices.trees;

public class LoopVertex extends SyntaxVertex
{
	protected int headerID;
	protected int bound;

	public LoopVertex (int vertexID, int headerID)
	{
		super (vertexID);
		this.headerID = headerID;
	}

	public final int getHeaderID ()
	{
		return headerID;
	}

	public final void setBound (int bound)
	{
		this.bound = bound;
	}

	public final int getBound ()
	{
		return bound;
	}
}
