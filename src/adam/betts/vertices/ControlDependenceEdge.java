package adam.betts.vertices;

public class ControlDependenceEdge extends ControlDependenceVertex
{
	protected final int predID;
	protected final int succID;

	public ControlDependenceEdge (int vertexID, int predID, int succID)
	{
		super (vertexID);
		this.predID = predID;
		this.succID = succID;
	}

	public final int getPredecessorID ()
	{
		return predID;
	}

	public final int getSuccessorID ()
	{
		return succID;
	}
}
