package adam.betts.vertices;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

import adam.betts.edges.Edge;

public class Vertex
{
	public final static int DUMMY_VERTEX_ID = -1;
	public final static int FIRST_VERTEX_ID = 1;

	protected int vertexID;
	protected ArrayList <Edge> predecessors = new ArrayList <Edge> ();
	protected ArrayList <Edge> successors = new ArrayList <Edge> ();

	public Vertex (int vertexID)
	{
		this.vertexID = vertexID;
	}

	public boolean equals (Vertex v)
	{
		return v.vertexID == vertexID;
	}

	public final int getVertexID ()
	{
		return vertexID;
	}

	public void addPredecessor (Integer predecessorID)
	{
		predecessors.add (new Edge (predecessorID));
	}

	public final void removePredecessor (Integer predecessorID)
	{
		int i = 0;
		for (Edge e : predecessors)
		{
			if (e.getVertexID () == predecessorID)
			{
				break;
			}
			i++;
		}
		if (i < predecessors.size ())
		{
			predecessors.remove (i);
		}
	}

	public final boolean hasPredecessor (Integer predecessorID)
	{
		for (Edge e : predecessors)
		{
			if (e.getVertexID () == predecessorID)
			{
				return true;
			}
		}
		return false;
	}

	public final Edge getPredecessor (Integer predecessorID)
	{
		for (Edge e : predecessors)
		{
			if (e.getVertexID () == predecessorID)
			{
				return e;
			}
		}
		return null;
	}

	public final Edge getNthPredecessor (int i)
	{
		return predecessors.get (i);
	}

	public final boolean hasPredecessors ()
	{
		return predecessors.size () > 0;
	}

	public final int numOfPredecessors ()
	{
		return predecessors.size ();
	}

	public Iterator <Edge> predecessorIterator ()
	{
		return predecessors.iterator ();
	}

	public final String predecessorString ()
	{
		StringBuffer buffer = new StringBuffer ();
		int i = 0;
		for (Edge e : predecessors)
		{
			buffer.append (e.toString ());
			if (i++ < predecessors.size ())
			{
				buffer.append ("\n");
			}
		}
		return buffer.toString ();
	}

	public final HashSet <Integer> getPredecessorIDs ()
	{
		HashSet <Integer> predIDs = new HashSet <Integer> ();
		for (Edge e : predecessors)
		{
			predIDs.add (e.getVertexID ());
		}
		return predIDs;
	}

	public void addSuccessor (Integer successorID)
	{
		successors.add (new Edge (successorID));
	}

	public final void removeSuccessor (Integer successorID)
	{
		int i = 0;
		for (Edge e : successors)
		{
			if (e.getVertexID () == successorID)
			{
				break;
			}
			i++;
		}
		if (i < successors.size ())
		{
			successors.remove (i);
		}
	}

	public final boolean hasSuccessor (Integer successorID)
	{
		for (Edge e : successors)
		{
			if (e.getVertexID () == successorID)
			{
				return true;
			}
		}
		return false;
	}

	public final Edge getSuccessor (Integer successorID)
	{
		for (Edge e : successors)
		{
			if (e.getVertexID () == successorID)
			{
				return e;
			}
		}
		return null;
	}

	public final Edge getNthSuccessor (int i)
	{
		return successors.get (i);
	}

	public final boolean hasSuccessors ()
	{
		return successors.size () > 0;
	}

	public final int numOfSuccessors ()
	{
		return successors.size ();
	}

	public Iterator <Edge> successorIterator ()
	{
		return successors.iterator ();
	}

	public final String successorString ()
	{
		StringBuffer buffer = new StringBuffer ();
		int i = 0;
		for (Edge e : successors)
		{
			buffer.append (e.toString ());
			if (i++ < successors.size ())
			{
				buffer.append ("\n");
			}
		}
		return buffer.toString ();
	}

	public final HashSet <Integer> getSuccessorIDs ()
	{
		HashSet <Integer> succIDs = new HashSet <Integer> ();
		for (Edge e : successors)
		{
			succIDs.add (e.getVertexID ());
		}
		return succIDs;
	}
}
