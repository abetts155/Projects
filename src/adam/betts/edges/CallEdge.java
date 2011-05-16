package adam.betts.edges;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class CallEdge extends Edge implements Iterable<Integer>
{
	protected Set<Integer> callSites = new HashSet<Integer> ();

	public CallEdge (int vertexID)
	{
		super (vertexID);
	}

	public final void addCallSite (int vertexID)
	{
		callSites.add (vertexID);
	}

	public final void addCallSites (Set<Integer> vertexIDs)
	{
		callSites.addAll (vertexIDs);
	}

	public final boolean hasCallSite (int vertexID)
	{
		return callSites.contains (vertexID);
	}

	public final int numOfCalls ()
	{
		return callSites.size ();
	}

	public Set<Integer> callSites ()
	{
		return Collections.unmodifiableSet (callSites);
	}

	public Iterator<Integer> iterator ()
	{
		return callSites.iterator ();
	}
}
