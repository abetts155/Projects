package se.mdh.graphs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Stack;

import se.mdh.edges.Edge;
import se.mdh.edges.FlowEdge;
import se.mdh.graphs.trees.LoopNests;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Enums.BranchType;
import se.mdh.vertices.BasicBlock;
import se.mdh.vertices.Vertex;

public class ControlFlowGraph extends FlowGraph implements Cloneable
{
	/*
	 * To ensure edge IDs are unique across all CFGs
	 */
	private static int edgeID = 1;

	protected String subprogramName;
	protected Long firstAddress;
	protected Long lastAddress;
	protected LoopNests lnt = null;

	public ControlFlowGraph ()
	{
	}

	public ControlFlowGraph clone ()
	{
		ControlFlowGraph cfg = new ControlFlowGraph ();
		for (Vertex v: this)
		{
			cfg.addBasicBlock (v.getVertexID ());
		}

		for (Vertex v: this)
		{
			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				FlowEdge e = (FlowEdge) succIt.next ();
				int succID = e.getVertexID ();
				BranchType type = e.getBranchType ();
				cfg.addEdge (v.getVertexID (), succID, type);
			}
		}

		return cfg;
	}

	public void inline (ControlFlowGraph cfg, String subprogramName)
	{
		for (Vertex v: cfg)
		{
			BasicBlock bb = (BasicBlock) v;
			BasicBlock clone = bb.clone ();
			clone.setSubprogramName (subprogramName);
			idToVertex.put (v.getVertexID (), clone);
		}

		for (Vertex v: cfg)
		{
			int vertexID = v.getVertexID ();
			if (vertexID != cfg.getExitID ())
			{
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					FlowEdge e = (FlowEdge) succIt.next ();
					int succID = e.getVertexID ();
					BranchType type = e.getBranchType ();

					this.addEdge (v.getVertexID (), succID, type);
				}
			}
		}
	}

	public void addEdge (int sourceID, int destinationID, BranchType type)
	{
		if (!idToVertex.containsKey (sourceID))
		{
			addBasicBlock (sourceID);
		}
		if (!idToVertex.containsKey (destinationID))
		{
			addBasicBlock (destinationID);
		}

		BasicBlock u = (BasicBlock) idToVertex.get (sourceID);
		BasicBlock v = (BasicBlock) idToVertex.get (destinationID);
		u.addSuccessor (destinationID, type, edgeID);
		v.addPredecessor (sourceID, type, edgeID);
		edgeID++;
	}

	public final void addAllPredecessorEdges ()
	{
		for (Vertex v: this)
		{
			int sourceID = v.getVertexID ();
			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				FlowEdge e = (FlowEdge) succIt.next ();
				int destinationID = e.getVertexID ();
				BasicBlock destination = getBasicBlock (destinationID);
				if (!destination.hasPredecessor (sourceID))
				{
					destination.addPredecessor (sourceID, e.getBranchType (), e
							.getEdgeID ());
				}
			}
		}
	}

	public final void addBasicBlock (int vertexID)
	{
		idToVertex.put (vertexID, new BasicBlock (vertexID));
	}

	public final BasicBlock getBasicBlock (int vertexID)
	{
		return (BasicBlock) idToVertex.get (vertexID);
	}

	public final BasicBlock getBasicBlock (long address)
	{
		for (Vertex v: this)
		{
			if ( ((BasicBlock) v).hasAddress (address))
			{
				return (BasicBlock) v;
			}
		}
		return null;
	}

	public final BasicBlock getSuccessor (BasicBlock u, Long address)
	{
		Iterator<Edge> succIt = u.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			BasicBlock v = getBasicBlock (e.getVertexID ());
			if (v.getFirstAddress () == address)
			{
				return v;
			}
		}
		return null;
	}

	public final void setEntry ()
	{
		long firstAddress = Long.MAX_VALUE;
		for (Vertex v: this)
		{
			long address = ((BasicBlock) v).getFirstAddress ();
			if (address < firstAddress)
			{
				firstAddress = address;
				entryID = v.getVertexID ();
			}
		}

		if (entryID == Vertex.DUMMY_VERTEX_ID)
		{
			Debug
					.debugMessage (
							getClass (),
							"Could not find a unique entry point using basic block addresses",
							1);

			ArrayList<Vertex> noPreds = new ArrayList<Vertex> ();
			for (Vertex v: this)
			{
				if (v.numOfPredecessors () == 0)
				{
					noPreds.add (v);
				}
			}

			if (noPreds.size () == 1)
			{
				entryID = noPreds.get (noPreds.size () - 1).getVertexID ();
			}
			else
			{
				Debug
						.debugMessage (
								getClass (),
								"Could not find a unique entry point through predecessor inspection either. Giving up. Potential entry points found: "
										+ noPreds, 1);
				System.exit (1);
			}
		}
	}

	public final void setExit ()
	{
		ArrayList<Vertex> noSuccs = new ArrayList<Vertex> ();
		for (Vertex v: this)
		{
			if (v.numOfSuccessors () == 0)
			{
				noSuccs.add (v);
			}
		}

		if (noSuccs.size () == 1)
		{
			exitID = noSuccs.get (noSuccs.size () - 1).getVertexID ();
		}
		else
		{
			Debug.debugMessage (getClass (),
					"Could not find a unique exit point. Giving up. Potential exit points found: "
							+ noSuccs, 1);
			System.exit (1);
		}
	}

	public final boolean isExit (int vertexID)
	{
		return idToVertex.get (vertexID).numOfSuccessors () == 0;
	}

	public final void setSubprogramName (String subprogramName)
	{
		this.subprogramName = subprogramName;
	}

	public final String getSubprogramName ()
	{
		return subprogramName;
	}

	public final void setFirstAndLastAddress ()
	{
		firstAddress = Long.MAX_VALUE;
		lastAddress = Long.MIN_VALUE;

		for (Vertex v: this)
		{
			if (v instanceof BasicBlock)
			{
				long bbFirstAddress = ((BasicBlock) v).getFirstAddress ();
				long bbLastAddress = ((BasicBlock) v).getLastAddress ();
				if (bbFirstAddress < firstAddress)
				{
					firstAddress = bbFirstAddress;
				}
				if (bbLastAddress > lastAddress)
				{
					lastAddress = bbLastAddress;
				}
			}
		}
	}

	public final void setFirstAddress (long firstAddress)
	{
		this.firstAddress = firstAddress;
	}

	public final long getFirstAddress ()
	{
		return firstAddress;
	}

	public final void setLastAddress (long lastAddress)
	{
		this.lastAddress = lastAddress;
	}

	public final long getLastAddress ()
	{
		return lastAddress;
	}

	public final boolean hasAddress (long address)
	{
		return address >= firstAddress && address <= lastAddress;
	}

	public final void removeDeadCode ()
	{
		HashSet<Integer> toRemove = new HashSet<Integer> (idToVertex.keySet ());
		HashSet<Integer> visited = new HashSet<Integer> ();

		Stack<Integer> stack = new Stack<Integer> ();
		stack.push (entryID);
		while (!stack.isEmpty ())
		{
			int vertexID = stack.pop ();
			toRemove.remove (vertexID);
			visited.add (vertexID);

			Vertex v = idToVertex.get (vertexID);
			Iterator<Edge> succIt = v.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge succEdge = succIt.next ();
				int succID = succEdge.getVertexID ();
				if (!visited.contains (succID))
				{
					stack.push (succID);
				}
			}
		}

		for (int vertexID: toRemove)
		{
			Debug.debugMessage (getClass (),
					"Removing basic block " + vertexID, 3);
			Debug.debugMessage (getClass (), idToVertex.get (vertexID)
					.toString (), 4);
			removeVertex (vertexID);
		}
	}

	public void addEntryAndExitEdges ()
	{
		ArrayList<Integer> noPreds = new ArrayList<Integer> ();
		ArrayList<Integer> noSuccs = new ArrayList<Integer> ();
		for (Vertex v: this)
		{
			int vertexID = v.getVertexID ();
			if (v.numOfPredecessors () == 0)
			{
				noPreds.add (vertexID);
			}
			if (v.numOfSuccessors () == 0)
			{
				noSuccs.add (vertexID);
			}
		}

		if (noPreds.size () == 1)
		{
			this.entryID = noPreds.get (noPreds.size () - 1);
		}
		else
		{
			this.entryID = getNextVertexID ();
			Debug.debugMessage (getClass (), "Adding entry " + entryID, 3);
			addBasicBlock (entryID);

			for (int vertexID: noPreds)
			{
				addEdge (entryID, vertexID, BranchType.UNKNOWN);
			}
		}

		if (noSuccs.size () == 1)
		{
			this.exitID = noSuccs.get (noSuccs.size () - 1);
		}
		else
		{
			this.exitID = getNextVertexID ();
			Debug.debugMessage (getClass (), "Adding exit " + exitID, 3);
			addBasicBlock (exitID);

			for (int vertexID: noSuccs)
			{
				addEdge (vertexID, exitID, BranchType.UNKNOWN);
			}
		}

		addEdge (exitID, entryID, BranchType.UNKNOWN);
	}

	public final LoopNests getLNT ()
	{
		if (lnt == null)
		{
			lnt = new LoopNests (this, this.entryID);
		}
		return lnt;
	}
}
