package adam.betts.graphs.trees;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.utils.LeastCommonAncestor;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DFSEdgeType;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.utilities.Enums.LoopType;
import adam.betts.vertices.Vertex;

public class GeneralLoopNests
{
	protected final FlowGraph flowg;
	protected DepthFirstTree dfs;
	protected DominatorTree predomTree;
	protected LeastCommonAncestor lca;

	protected int nextLoopVertexID;

	protected int zID;
	protected HashMap <Integer, LoopVertex> loopParent = new HashMap <Integer, LoopVertex> ();
	protected HashMap <Integer, Integer> loopEntry = new HashMap <Integer, Integer> ();
	protected HashMap <Integer, HashSet <Integer>> loopContains = new HashMap <Integer, HashSet <Integer>> ();
	protected HashMap <Integer, HashSet <Integer>> vertexToGenerators = new HashMap <Integer, HashSet <Integer>> ();

	public GeneralLoopNests (FlowGraph flowg)
	{
		this.flowg = flowg;

		dfs = new DepthFirstTree (flowg, flowg.getEntryID ());
		predomTree = new DominatorTree (flowg, flowg.getEntryID (), DominatorTreeType.PRE_DOMINATOR);
		lca = new LeastCommonAncestor (predomTree);

		initialise ();
		//findLoops ();
	}

	public void output ()
	{

		for (Vertex v : flowg)
		{
			final int vertexID = v.getVertexID ();

			System.out.println ("parent(" + vertexID + ") = "
					+ loopParent.get (vertexID).getVertexID ());
		}
	}

	private void initialise ()
	{
		for (Vertex v : flowg)
		{
			final int vertexID = v.getVertexID ();

			loopParent.put (vertexID, null);
			loopEntry.put (vertexID, vertexID);
			loopContains.put (vertexID, new HashSet <Integer> ());
			loopContains.get (vertexID).add (vertexID);
			vertexToGenerators.put (vertexID, new HashSet <Integer> ());
		}
	}

	private void findLoops ()
	{
		for (int postID = 1; postID <= dfs.numOfVertices (); ++postID)
		{
			final int vertexID = dfs.getPostVertexID (postID);

			final Set <Integer> generators = vertexToGenerators.get (vertexID);

			if (generators.size () > 0)
			{
				findBody (generators, vertexID, LoopType.MULTIPLE_ENTRY);
			}

			findLoop (vertexID);
		}
	}

	private void findBody (Set <Integer> generators, final int vertexID, LoopType type)
	{
		Debug.debugMessage (getClass (), "Finding body of " + vertexID, 4);

		Set <Integer> loop = new HashSet <Integer> ();
		List <Integer> queue = new ArrayList <Integer> ();

		for (int bID : generators)
		{
			int ancestorID = loopAncestor (bID).getVertexID ();

			if (loop.contains (ancestorID) == false)
			{
				loop.add (ancestorID);
				queue.add (ancestorID);
			}
		}

		while (queue.isEmpty () == false)
		{
			int bID = queue.remove (queue.size () - 1);

			Debug.debugMessage (getClass (), "Analysing " + bID, 4);

			int loopEntryID = loopEntry.get (bID);

			Iterator <Edge> predIt = flowg.getVertex (loopEntryID).predecessorIterator ();
			while (predIt.hasNext ())
			{
				Edge prede = predIt.next ();
				final int predID = prede.getVertexID ();

				if (predID != vertexID)
				{
					int ancestorID = loopAncestor (predID).getVertexID ();

					if (loop.contains (ancestorID) == false)
					{
						loop.add (ancestorID);
						queue.add (ancestorID);
					}
				}
			}
		}

		loop.add (vertexID);

		LoopVertex loopv = new LoopVertex (flowg.getNextVertexID () + nextLoopVertexID);

		loopContains.put (loopv.getVertexID (), new HashSet <Integer> ());
		loopContains.get (loopv.getVertexID ()).addAll (loop);

		loopEntry.put (loopv.getVertexID (), zID);
		loopParent.put (loopv.getVertexID (), null);

		for (int bID : loop)
		{
			loopParent.put (bID, loopv);
		}
	}

	private LoopVertex loopAncestor (int vertexID)
	{
		LoopVertex loopv = loopParent.get (vertexID);

		LoopVertex lastv = loopv;
		
		while (loopv != null)
		{
			lastv = loopv;
			loopv = loopParent.get (loopv.getVertexID ());
		}

		return lastv;
	}

	private void findLoop (final int vertexID)
	{
		Debug.debugMessage (getClass (), "Finding loop of " + vertexID, 4);

		zID = vertexID;

		Set <Integer> loop = new HashSet <Integer> ();

		Iterator <Edge> predIt = flowg.getVertex (vertexID).predecessorIterator ();
		while (predIt.hasNext ())
		{
			Edge prede = predIt.next ();
			final int predID = prede.getVertexID ();

			if (dfs.getEdgeType (predID, vertexID) == DFSEdgeType.BACK_EDGE)
			{
				zID = lca.getLCA (zID, predID);

				if (loop.contains (predID) == false && predID != vertexID)
				{
					loop.add (predID);
				}
			}
		}

		if (zID != vertexID)
		{
			vertexToGenerators.get (zID).addAll (loop);
		} else
		{
			findBody (loop, zID, LoopType.SINGLE_ENTRY);
		}
	}

	private class LoopVertex extends Vertex
	{
		public LoopVertex (int vertexID)
		{
			super (vertexID);
		}
	}
}
