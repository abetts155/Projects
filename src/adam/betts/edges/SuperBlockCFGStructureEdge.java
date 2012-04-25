package adam.betts.edges;

import adam.betts.utilities.Enums.SuperBlockCFGStructureEdgeType;

public class SuperBlockCFGStructureEdge extends Edge
{

    private final SuperBlockCFGStructureEdgeType type;

    public SuperBlockCFGStructureEdge (int vertexID,
            SuperBlockCFGStructureEdgeType type)
    {
        super(vertexID);
        this.type = type;
    }

    public final SuperBlockCFGStructureEdgeType getEdgeType ()
    {
        return type;
    }
}
