package adam.betts.edges;

import adam.betts.utilities.Enums.BranchType;

public class FlowEdge extends Edge
{
	public final static int FIRST_EDGE_ID = 1;

	protected int edgeID;
	protected BranchType type;

	public FlowEdge (int vertexID)
	{
		super (vertexID);
	}

	public final void setEdgeID (int edgeID)
	{
		this.edgeID = edgeID;
	}

	public final int getEdgeID ()
	{
		return edgeID;
	}

	public final void setBranchType (BranchType type)
	{
		this.type = type;
	}

	public final BranchType getBranchType ()
	{
		return type;
	}
}
