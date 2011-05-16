package adam.betts.vertices;

public class FlowVertex extends Vertex
{
	protected boolean dummy = false;

	public FlowVertex (int vertexID)
	{
		super (vertexID);
	}

	public final boolean isDummy ()
	{
		return dummy;
	}

	public final void setDummy ()
	{
		dummy = true;
	}
}
