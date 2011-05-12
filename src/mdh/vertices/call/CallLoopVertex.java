package se.mdh.vertices.call;

import se.mdh.vertices.Vertex;

public abstract class CallLoopVertex extends Vertex
{
	protected final int subprogramID;
	protected final String subprogramName;

	public CallLoopVertex (int vertexID, int subprogramID, String subprogramName)
	{
		super (vertexID);
		this.subprogramID = subprogramID;
		this.subprogramName = subprogramName;
	}

	public final int getSubprogramID ()
	{
		return subprogramID;
	}

	public final String getSubprogramName ()
	{
		return subprogramName;
	}
}
