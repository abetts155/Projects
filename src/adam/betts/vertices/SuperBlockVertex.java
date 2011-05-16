package adam.betts.vertices;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SuperBlockVertex extends Vertex
{
	protected ArrayList<Integer> basicBlocks = new ArrayList<Integer> ();

	public SuperBlockVertex (int vertexID)
	{
		super (vertexID);
	}

	public final void addBasicBlock (int vertexID)
	{
		basicBlocks.add (vertexID);
	}

	public final boolean containsBasicBlock (int vertexID)
	{
		return basicBlocks.contains (vertexID);
	}

	public final int numberOfBasicBlocks ()
	{
		return basicBlocks.size ();
	}

	public final List<Integer> basicBlockIDs ()
	{
		return Collections.unmodifiableList (basicBlocks);
	}
}
