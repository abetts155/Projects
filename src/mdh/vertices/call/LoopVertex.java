package se.mdh.vertices.call;

public class LoopVertex extends CallLoopVertex
{
	protected final int headerID;
	protected int bound = 0;

	public LoopVertex (int vertexID, int subprogramID, String subprogramName,
			int headerID)
	{
		super (vertexID, subprogramID, subprogramName);
		this.headerID = headerID;
	}

	public final int getHeaderID ()
	{
		return headerID;
	}

	public final void setLoopBound (int bound)
	{
		this.bound = bound;
	}

	public final int getLoopBound ()
	{
		return bound;
	}
}
