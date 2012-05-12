package adam.betts.graphs.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import adam.betts.edges.Edge;
import adam.betts.graphs.DirectedGraph;
import adam.betts.outputs.OutputGraph;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;

public class StronglyConnectedComponents
{
	protected DirectedGraph directedg;
	protected int sccCounter = 1;
	protected int preCounter = 1;
	protected Stack <Integer> stack = new Stack <Integer> ();
	protected HashSet <Integer> trivialSCCs = new HashSet <Integer> ();
	protected HashMap <Integer, Integer> vertexToScc = new HashMap <Integer, Integer> ();
	protected HashMap <Integer, Integer> index = new HashMap <Integer, Integer> ();
	protected HashMap <Integer, Integer> lowLinks = new HashMap <Integer, Integer> ();

	enum COLOR
	{
		WHITE, BLACK, GRAY, BLUE, RED
	};

	protected HashMap <Integer, COLOR> vertexColor = new HashMap <Integer, COLOR> ();
	protected HashMap <Integer, Integer> vertexStartTime = new HashMap <Integer, Integer> ();
	protected HashMap <Integer, Integer> vertexFinishTime = new HashMap <Integer, Integer> ();
	protected HashMap <Integer, Vertex> vertexComponent = new HashMap <Integer, Vertex> ();
	protected HashMap <Integer, Integer> vertexToScc2 = new HashMap <Integer, Integer> ();
	protected int preCounter2;
	protected int sccCounter2;

	public StronglyConnectedComponents (DirectedGraph directedg)
	{
		this.directedg = directedg;

		initialise ();
		computeSCCs ();
		algorithm2 ();
	}

	private void algorithm2 ()
	{
		for (Vertex v : directedg)
		{
			vertexColor.put (v.getVertexID (), COLOR.WHITE);
		}

		ArrayList <Integer> vertexList = new ArrayList <Integer> ();
		preCounter2 = 0;
		for (Vertex v : directedg)
		{
			if (vertexColor.get (v.getVertexID ()) == COLOR.WHITE)
			{
				visit1 (directedg, v, vertexList);
			}
		}

		for (Vertex v : directedg)
		{
			vertexToScc2.put (v.getVertexID (), 0);
		}

		DirectedGraph reverseg = new DirectedGraph ();
		directedg.reverseGraph (reverseg);

		sccCounter2 = 0;
		for (int i = vertexList.size () - 1; i >= 0; --i)
		{
			int vertexID = vertexList.get (i);
			if (vertexColor.get (vertexID) == COLOR.BLACK)
			{
				sccCounter2++;
				visit2 (reverseg, reverseg.getVertex (vertexID));
			}
		}
	}

	private void visit1 (DirectedGraph directedg, Vertex v, ArrayList <Integer> vertexList)
	{
		Stack <Vertex> theStack = new Stack <Vertex> ();
		theStack.push (v);

		while (theStack.isEmpty () == false)
		{
			Vertex popv = theStack.pop ();

			if (vertexColor.get (popv.getVertexID ()) == COLOR.WHITE)
			{
				preCounter2++;
				vertexStartTime.put (popv.getVertexID (), preCounter2);
				vertexColor.put (popv.getVertexID (), COLOR.GRAY);
				theStack.push (popv);

				Iterator <Edge> succIt = popv.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					int succID = e.getVertexID ();

					if (vertexColor.get (succID) == COLOR.WHITE)
					{
						vertexComponent.put (succID, popv);
						theStack.push (directedg.getVertex (succID));
					}
				}
			} else if (vertexColor.get (popv.getVertexID ()) == COLOR.GRAY)
			{
				preCounter2++;
				vertexFinishTime.put (popv.getVertexID (), preCounter2);
				vertexColor.put (popv.getVertexID (), COLOR.BLACK);
				vertexList.add (popv.getVertexID ());
			}
		}
	}

	private void visit2 (DirectedGraph reverseg, Vertex v)
	{
		Stack <Vertex> theStack = new Stack <Vertex> ();
		theStack.push (v);

		while (theStack.isEmpty () == false)
		{
			Vertex popv = theStack.pop ();
			vertexToScc2.put (popv.getVertexID (), sccCounter2);

			Debug.debugMessage (getClass (), "Vertex " + popv.getVertexID () + " is in SCC "
					+ sccCounter2, 4);

			if (vertexColor.get (popv.getVertexID ()) == COLOR.BLACK)
			{
				vertexColor.put (popv.getVertexID (), COLOR.BLUE);
				theStack.push (popv);

				Iterator <Edge> succIt = popv.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					int succID = e.getVertexID ();

					if (vertexColor.get (succID) == COLOR.BLACK)
					{
						vertexComponent.put (succID, popv);
						theStack.push (reverseg.getVertex (succID));
					}
				}

			} else if (vertexColor.get (popv.getVertexID ()) == COLOR.BLUE)
			{
				vertexColor.put (popv.getVertexID (), COLOR.RED);
			}
		}
	}

	public final int numberOfSccs ()
	{
		return sccCounter2;
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
		return vertexToScc2.get (vertexID);
	}

	private void initialise ()
	{
		for (Vertex v : directedg)
		{
			int vertexID = v.getVertexID ();
			index.put (vertexID, 0);
			lowLinks.put (vertexID, 0);
			vertexToScc.put (vertexID, 0);
		}
	}

	private void computeSCCs ()
	{
		for (Vertex v : directedg)
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
		Iterator <Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			int succID = e.getVertexID ();

			if (index.get (succID) == 0)
			{
				doSearch (succID);

				int lowLink = Math.min (lowLinks.get (vertexID), lowLinks.get (succID));
				lowLinks.put (vertexID, lowLink);
			} else if (stack.contains (succID))
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
				Debug.debugMessage (getClass (), "Vertex " + poppedID + " is in SCC " + sccCounter,
						4);
				vertexToScc.put (poppedID, sccCounter);
				counter++;
			} while (poppedID != vertexID);

			if (counter == 1)
			{
				Debug.debugMessage (getClass (), "SCC " + sccCounter + " is a trivial SCC", 4);
				trivialSCCs.add (sccCounter);
			}

			sccCounter++;
		}
	}
}
