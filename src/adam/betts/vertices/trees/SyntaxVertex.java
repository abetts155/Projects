package adam.betts.vertices.trees;

public class SyntaxVertex extends TreeVertex
{
	protected long WCET = 0;

	public SyntaxVertex (int vertexID)
	{
		super (vertexID);
	}

	public final void setWCET (long WCET)
	{
		this.WCET = WCET;
	}

	public final long getWCET ()
	{
		return WCET;
	}
}
