package se.mdh.vertices.call;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import se.mdh.edges.CallEdge;
import se.mdh.edges.Edge;
import se.mdh.vertices.Vertex;

public class CallVertex extends Vertex
{
	protected String subprogramName;

	public CallVertex (int vertexID, String subprogramName)
	{
		super (vertexID);
		this.subprogramName = subprogramName;
	}

	public final String getSubprogramName ()
	{
		return subprogramName;
	}

	public final void addPredecessor (int callerID, int callSiteID)
	{
		if (hasPredecessor (callerID))
		{
			CallEdge e = (CallEdge) getPredecessor (callerID);
			e.addCallSite (callSiteID);
		}
		else
		{
			CallEdge e = new CallEdge (callerID);
			e.addCallSite (callSiteID);
			predecessors.add (e);
		}
	}

	public final void addPredecessor (int callerID, Set<Integer> callSites)
	{
		if (hasPredecessor (callerID))
		{
			CallEdge e = (CallEdge) getPredecessor (callerID);
			e.addCallSites (callSites);
		}
		else
		{
			CallEdge e = new CallEdge (callerID);
			e.addCallSites (callSites);
			predecessors.add (e);
		}
	}

	public final void addSuccessor (int calleeID, int callSiteID)
	{
		if (hasSuccessor (calleeID))
		{
			CallEdge e = (CallEdge) getSuccessor (calleeID);
			e.addCallSite (callSiteID);
		}
		else
		{
			CallEdge e = new CallEdge (calleeID);
			e.addCallSite (callSiteID);
			successors.add (e);
		}
	}

	public final void addSuccessor (int calleeID, Set<Integer> callSites)
	{
		if (hasSuccessor (calleeID))
		{
			CallEdge e = (CallEdge) getSuccessor (calleeID);
			e.addCallSites (callSites);
		}
		else
		{
			CallEdge e = new CallEdge (calleeID);
			e.addCallSites (callSites);
			successors.add (e);
		}
	}

	public final int numberOfCalls (int calleeID)
	{
		for (Edge e: successors)
		{
			if (e.getVertexID () == calleeID)
			{
				return ((CallEdge) e).numOfCalls ();
			}
		}
		return 0;
	}

	public final Set<Integer> getCallSites ()
	{
		Set<Integer> callSites = new HashSet<Integer> ();
		for (Edge e: successors)
		{
			callSites.addAll ( ((CallEdge) e).callSites ());
		}
		return Collections.unmodifiableSet (callSites);
	}
}
