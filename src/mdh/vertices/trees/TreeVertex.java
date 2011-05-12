package se.mdh.vertices.trees;

import se.mdh.vertices.Vertex;

public class TreeVertex extends Vertex
{
	protected int parentID;
	protected int level;

	public TreeVertex (int vertexID)
	{
		super (vertexID);
		parentID = vertexID;
	}

	public final void setParentID (int parentID)
	{
		this.parentID = parentID;
	}

	public final int getParentID ()
	{
		return parentID;
	}

	public final void setLevel (int level)
	{
		this.level = level;
	}

	public final int getLevel ()
	{
		return level;
	}

	public final boolean isInternalVertex ()
	{
		return successors.size () > 0;
	}

	public final boolean isLeaf ()
	{
		return successors.size () == 0;
	}
}
