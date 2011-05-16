package adam.betts.graphs.utils;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import adam.betts.edges.Edge;
import adam.betts.graphs.DirectedGraph;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;

public class StronglyConnectedComponents
{
	protected DirectedGraph directedg;
	protected int sccCounter = 1;
	protected int preCounter = 1;
	protected Stack<Integer> stack = new Stack<Integer> ();
	protected HashSet<Integer> trivialSCCs = new HashSet<Integer> ();
	protected HashMap<Integer, Integer> vertexToScc = new HashMap<Integer, Integer> ();
	protected HashMap<Integer, Integer> index = new HashMap<Integer, Integer> ();
	protected HashMap<Integer, Integer> lowLinks = new HashMap<Integer, Integer> ();

	public StronglyConnectedComponents (DirectedGraph directedg)
	{
		this.directedg = directedg;

		initialise ();
		computeSCCs ();
	}

	public final int numberOfSccs ()
	{
		return sccCounter - 1;
	}

	public final int numberOfTrivialSccs ()
	{
		return trivialSCCs.size ();
	}

	public final boolean isTrivialScc (int sccID)
	{
		return trivialSCCs.contains (sccID);
	}

	public final int getSCCID (int vertexID)
	{
		return vertexToScc.get (vertexID);
	}

	private void initialise ()
	{
		for (Vertex v: directedg)
		{
			int vertexID = v.getVertexID ();
			index.put (vertexID, 0);
			lowLinks.put (vertexID, 0);
			vertexToScc.put (vertexID, 0);
		}
	}

	private void computeSCCs ()
	{
		for (Vertex v: directedg)
		{
			int vertexID = v.getVertexID ();
			if (index.get (vertexID) == 0)
			{
				doSearch (vertexID);
			}
		}
	}

	private void doSearch (int vertexID)
	{
		index.put (vertexID, preCounter);
		lowLinks.put (vertexID, preCounter);
		preCounter++;
		stack.push (vertexID);

		Vertex v = directedg.getVertex (vertexID);
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			int succID = e.getVertexID ();

			if (index.get (succID) == 0)
			{
				doSearch (succID);

				int lowLink = Math.min (lowLinks.get (vertexID), lowLinks.get (succID));
				lowLinks.put (vertexID, lowLink);
			}
			else if (stack.contains (succID))
			{
				int lowLink = Math.min (lowLinks.get (vertexID), index.get (succID));
				lowLinks.put (vertexID, lowLink);
			}
		}

		if (lowLinks.get (vertexID) == index.get (vertexID))
		{
			int counter = 0;
			int poppedID;
			do
			{
				poppedID = stack.pop ();
				Debug.debugMessage (getClass (), "Vertex " + poppedID
						+ " is in SCC " + sccCounter, 4);
				vertexToScc.put (poppedID, sccCounter);
				counter++;
			}
			while (poppedID != vertexID);

			if (counter == 1)
			{
				Debug.debugMessage (getClass (), "SCC " + sccCounter
						+ " is a trivial SCC", 4);
				trivialSCCs.add (sccCounter);
			}

			sccCounter++;
		}
	}
}
