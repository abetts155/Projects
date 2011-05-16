package adam.betts.vertices.call;

public class ContextVertex extends CallVertex
{
	protected int subprogramID;

	public ContextVertex (int contextID, String subprogramName, int subprogramID)
	{
		super (contextID, subprogramName);
		this.subprogramID = subprogramID;
	}

	public final int getContextID ()
	{
		return getVertexID ();
	}

	public final int getSubprogramID ()
	{
		return subprogramID;
	}
}
