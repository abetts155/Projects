package adam.betts.edges;

import adam.betts.utilities.Enums.SuperBlockCFGStructureEdgeType;
import adam.betts.vertices.Vertex;

public class SuperBlockCFGStructureEdge extends FlowEdge
{

    private SuperBlockCFGStructureEdgeType type;
    private Integer basicBlockID;

    public SuperBlockCFGStructureEdge (int vertexID, int edgeID)
    {
        super(vertexID);
        this.edgeID = edgeID;
        type = SuperBlockCFGStructureEdgeType.NORMAL;
        basicBlockID = Vertex.DUMMY_VERTEX_ID;
    }

    public final void setEdgeType (SuperBlockCFGStructureEdgeType type)
    {
        this.type = type;
    }

    public final SuperBlockCFGStructureEdgeType getEdgeType ()
    {
        return type;
    }

    public final void setBasicBlockID (int bbID)
    {
        this.basicBlockID = bbID;
    }

    public final Integer getBasicBlockID ()
    {
        return basicBlockID;
    }
}
