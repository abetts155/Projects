package adam.betts.edges;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import adam.betts.utilities.Enums.IPGEdgeType;
import adam.betts.vertices.Vertex;

public class IPGEdge extends FlowEdge
{
	protected int entryID = Vertex.DUMMY_VERTEX_ID;
	protected int exitID = Vertex.DUMMY_VERTEX_ID;
	protected Set<Integer> headerIDs = new HashSet<Integer> ();
	protected Set<Integer> label = new HashSet<Integer> ();
	protected IPGEdgeType ipgEdgetype;

	public IPGEdge (int vertexID, int edgeID, IPGEdgeType ipgEdgetype)
	{
		super (vertexID);
		this.edgeID = edgeID;
		this.ipgEdgetype = ipgEdgetype;
	}

	public final IPGEdgeType getEdgeType ()
	{
		return ipgEdgetype;
	}

	public final void setEntryEdge (int headerID)
	{
		this.entryID = headerID;
	}

	public final boolean isEntryEdge ()
	{
		return entryID != Vertex.DUMMY_VERTEX_ID;
	}

	public final int getEntryHeaderID ()
	{
		return entryID;
	}

	public final void setExitEdge (int headerID)
	{
		this.exitID = headerID;
	}

	public final boolean isExitEdge ()
	{
		return exitID != Vertex.DUMMY_VERTEX_ID;
	}

	public final int getExitHeaderID ()
	{
		return exitID;
	}

	public final void addToEdgeLabel (HashSet<Integer> label)
	{
		this.label.addAll (label);
	}

	public final void setIterationHeaderID (int headerID)
	{
		headerIDs.add (headerID);
	}

	public final boolean isIterationEdge ()
	{
		return headerIDs.size () > 0;
	}

	public final Set<Integer> iterationHeaderIDs ()
	{
		return Collections.unmodifiableSet (headerIDs);
	}

	public final Set<Integer> getEdgeLabel ()
	{
		return Collections.unmodifiableSet (label);
	}

	public final boolean labelContains (int vertexID)
	{
		return label.contains (vertexID);
	}

	public final int labelSize ()
	{
		return label.size ();
	}
}
