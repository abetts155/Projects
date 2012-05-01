package adam.betts.vertices.trees;

public class HeaderVertex extends TreeVertex
{

    protected final int headerID;
    protected boolean isDoWhile;

    public HeaderVertex (int vertexID, int headerID)
    {
        super(vertexID);
        this.headerID = headerID;
        isDoWhile = false;
    }

    public final int getHeaderID ()
    {
        return headerID;
    }

    public final void setDoWhile ()
    {
        isDoWhile = true;
    }

    public final boolean isDoWhile ()
    {
        return isDoWhile;
    }
}
