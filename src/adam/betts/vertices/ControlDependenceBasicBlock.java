package adam.betts.vertices;

public class ControlDependenceBasicBlock extends ControlDependenceVertex
{
	protected final int basicBlockID;

	public ControlDependenceBasicBlock (int vertexID, int basicBlockID)
	{
		super (vertexID);
		this.basicBlockID = basicBlockID;
	}

	public final int getBasicBlockID ()
	{
		return basicBlockID;
	}
}
