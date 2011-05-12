package se.mdh.vertices.trees;

public class HeaderVertex extends TreeVertex
{
	protected final int headerID;

	public HeaderVertex (int vertexID, int headerID)
	{
		super (vertexID);
		this.headerID = headerID;
	}

	public final int getHeaderID ()
	{
		return headerID;
	}
}
