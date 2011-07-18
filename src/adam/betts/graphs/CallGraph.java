package adam.betts.graphs;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;

import adam.betts.edges.CallEdge;
import adam.betts.edges.Edge;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.call.CallVertex;

public class CallGraph extends DirectedGraph
{
	protected HashMap<String, CallVertex> nameToVertex = new LinkedHashMap<String, CallVertex> ();
	
	public CallGraph ()
	{
	}

	public void addVertex (int subprogramID, String subprogramName)
	{
		CallVertex v = new CallVertex (subprogramID, subprogramName);
		idToVertex.put (subprogramID, v);
		nameToVertex.put (subprogramName, v);
	}

	public CallVertex getVertex (String subprogramName)
	{
		return nameToVertex.get (subprogramName);
	}

	public CallVertex getVertex (int vertexID)
	{
		return (CallVertex) idToVertex.get (vertexID);
	}

	public final boolean hasSubprogram (String subprogramName)
	{
		return nameToVertex.containsKey (subprogramName);
	}

	public final boolean hasSubprogram (int subprogramID)
	{
		return hasVertex (subprogramID);
	}

	public final void addCall (String caller, String callee, int callSiteID)
	{
		CallVertex source = nameToVertex.get (caller);
		CallVertex destination = nameToVertex.get (callee);
		source.addSuccessor (destination.getVertexID (), callSiteID);
		destination.addPredecessor (source.getVertexID (), callSiteID);
	}

	public final void addCall (int callerID, int calleeID, int callSiteID)
	{
		
		CallVertex source = (CallVertex) idToVertex.get (callerID);
		CallVertex destination = (CallVertex) idToVertex.get (calleeID);
		source.addSuccessor (destination.getVertexID (), callSiteID);
		destination.addPredecessor (source.getVertexID (), callSiteID);
	}

	public final boolean calls (String caller, String callee)
	{
		CallVertex source = nameToVertex.get (caller);
		CallVertex destination = nameToVertex.get (callee);
		return source.hasSuccessor (destination.getVertexID ());
	}

	public final boolean calls (int callerID, int calleeID)
	{
		CallVertex source = (CallVertex) idToVertex.get (callerID);
		return source.hasSuccessor (calleeID);
	}

	public final int numOfCalls (String caller, String callee)
	{
		CallVertex source = nameToVertex.get (caller);
		CallVertex destination = nameToVertex.get (callee);
		return source.numberOfCalls (destination.getVertexID ());
	}

	public final int numOfCalls (int callerID, int calleeID)
	{
		CallVertex source = (CallVertex) idToVertex.get (callerID);
		return source.numberOfCalls (calleeID);
	}

	public final boolean isLeaf (String subprogramName)
	{
		return nameToVertex.get (subprogramName).numOfSuccessors () == 0;
	}

	public final boolean isLeaf (int subprogramID)
	{
		return idToVertex.get (subprogramID).numOfSuccessors () == 0;
	}

	public final int numOfSubprograms ()
	{
		return numOfVertices ();
	}

	public final int isCallSite (String subprogramName, int vertexID)
	{
		CallVertex v = nameToVertex.get (subprogramName);
		return isCallSite (v.getVertexID (), vertexID);
	}

	public final int isCallSite (int subprogramID, int vertexID)
	{
		CallVertex v = (CallVertex) idToVertex.get (subprogramID);
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			CallEdge e = (CallEdge) succIt.next ();
			if (e.hasCallSite (vertexID))
			{
				return e.getVertexID ();
			}
		}
		return Vertex.DUMMY_VERTEX_ID;
	}
}
