package adam.betts.edges;

public class Edge
{

    protected int vertexID;

    public Edge (int vertexID)
    {
        this.vertexID = vertexID;
    }

    public final void setVertexID (int vertexID)
    {
        this.vertexID = vertexID;
    }

    public final int getVertexID ()
    {
        return vertexID;
    }

    public boolean equals (Edge e)
    {
        return e.vertexID == vertexID;
    }
}
