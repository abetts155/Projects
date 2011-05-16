package adam.betts.vertices;

import java.util.HashMap;

import adam.betts.edges.Edge;
import adam.betts.edges.IPGEdge;
import adam.betts.outputs.Output;
import adam.betts.utilities.Enums.IPGEdgeType;

public class Ipoint extends FlowVertex implements Cloneable
{
	public final static long GHOST_IPOINT_ID = 0;

	protected long ipointID;
	protected String subprogramName = null;
	protected boolean inlinedEntry = false;
	protected boolean inlinedExit = false;
	protected HashMap<Long, IPGEdge> traceSuccessors = new HashMap<Long, IPGEdge> ();

	public Ipoint (int vertexID, long ipointID)
	{
		super (vertexID);
		this.ipointID = ipointID;
	}

	public Ipoint (int vertexID, long ipointID, String subprogramName)
	{
		super (vertexID);
		this.ipointID = ipointID;
		this.subprogramName = subprogramName;
	}

	public final boolean isGhostIpoint ()
	{
		return ipointID == GHOST_IPOINT_ID;
	}

	public final long getIpointID ()
	{
		return ipointID;
	}

	public final void setSubprogramName (String subprogramName)
	{
		this.subprogramName = subprogramName;
	}

	public final String getSubprogramName ()
	{
		return subprogramName;
	}

	public final boolean isInlined ()
	{
		return inlinedEntry || inlinedExit;
	}

	public final void setInlinedEntry ()
	{
		inlinedEntry = true;
	}

	public final boolean isInlinedEntry ()
	{
		return inlinedEntry;
	}

	public final void setInlinedExit ()
	{
		inlinedExit = true;
	}

	public final boolean isInlinedExit ()
	{
		return inlinedExit;
	}

	public final void addPredecessor (int predecessorID,
			int edgeID,
			IPGEdgeType type)
	{
		IPGEdge e = new IPGEdge (predecessorID, edgeID, type);
		predecessors.add (e);
	}

	public final void addSuccessor (int successorID,
			long ipointID,
			int edgeID,
			IPGEdgeType type)
	{
		IPGEdge e = new IPGEdge (successorID, edgeID, type);
		successors.add (e);
		traceSuccessors.put (ipointID, e);
	}

	public final boolean hasTraceSuccessor (long ipointID)
	{
		return traceSuccessors.containsKey (ipointID);
	}

	public final IPGEdge getTraceSuccessor (long ipointID)
	{
		return traceSuccessors.get (ipointID);
	}

	public String toString ()
	{
		StringBuffer buffer = new StringBuffer ();
		String out = "Ipoint " + vertexID + " with id "
				+ Long.toHexString (ipointID) + "\n";
		buffer.append (out + Output.getPadderString (out.length (), '-'));

		buffer.append ("pred(" + vertexID + ") = {");
		int i = 1;
		for (Edge e: predecessors)
		{
			buffer.append (e.getVertexID ());
			if (i++ < predecessors.size ())
			{
				buffer.append (", ");
			}
		}
		buffer.append ("}\n");

		buffer.append ("succ(" + vertexID + ") = {");
		i = 1;
		for (Edge e: successors)
		{
			buffer.append (e.getVertexID ());
			if (i++ < successors.size ())
			{
				buffer.append (", ");
			}
		}
		buffer.append ("}\n");

		return buffer.toString ();
	}

	public Ipoint clone ()
	{
		Ipoint v = new Ipoint (vertexID, ipointID);
		v.subprogramName = subprogramName;
		v.inlinedEntry = inlinedEntry;
		v.inlinedExit = inlinedExit;
		return v;
	}
}
