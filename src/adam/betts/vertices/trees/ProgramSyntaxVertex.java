package adam.betts.vertices.trees;

import adam.betts.vertices.Vertex;

public class ProgramSyntaxVertex extends Vertex
{

    protected long WCET = 0;

    public ProgramSyntaxVertex (int vertexID)
    {
        super(vertexID);
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
