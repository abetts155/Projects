package adam.betts.edges;

import adam.betts.utilities.Enums.SuperBlockCFGStructureEdgeType;

public class SuperBlockCFGStructureEdge extends FlowEdge
{

    private SuperBlockCFGStructureEdgeType type;

    public SuperBlockCFGStructureEdge (int vertexID, int edgeID)
    {
        super(vertexID);
        this.edgeID = edgeID;
        type = SuperBlockCFGStructureEdgeType.NORMAL;
    }

    public final void setEdgeType (SuperBlockCFGStructureEdgeType type)
    {
        this.type = type;
    }

    public final SuperBlockCFGStructureEdgeType getEdgeType ()
    {
        return type;
    }
}
