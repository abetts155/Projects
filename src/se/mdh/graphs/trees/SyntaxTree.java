package se.mdh.graphs.trees;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import se.mdh.edges.Edge;
import se.mdh.graphs.ControlFlowGraph;
import se.mdh.graphs.FlowGraph;
import se.mdh.graphs.utils.LeastCommonAncestor;
import se.mdh.outputs.UDrawGraph;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Globals;
import se.mdh.utilities.Enums.DFSEdgeType;
import se.mdh.utilities.Enums.DominatorTreeType;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.trees.AlternativeVertex;
import se.mdh.vertices.trees.LeafVertex;
import se.mdh.vertices.trees.LoopVertex;
import se.mdh.vertices.trees.SequenceVertex;
import se.mdh.vertices.trees.SyntaxVertex;
import se.mdh.vertices.trees.TreeVertex;

public class SyntaxTree extends Tree
{
	protected final ControlFlowGraph cfg;
	protected final String subprogramName;
	protected final DepthFirstTree dfs;
	protected final LoopNests lnt;
	protected DominatorTree pret;
	protected DominatorTree postt;
	protected LeastCommonAncestor lca;
	protected HashMap<Integer, LoopVertex> headerToSyntaxTree = new HashMap<Integer, LoopVertex> ();
	protected int headerID;

	public SyntaxTree (ControlFlowGraph cfg, String subprogramName)
	{
		this.cfg = cfg;
		this.subprogramName = subprogramName;
		dfs = new DepthFirstTree (cfg, cfg.getEntryID ());
		lnt = new LoopNests (cfg, cfg.getEntryID ());

		build ();

		if (Globals.uDrawDirectorySet ())
		{
			UDrawGraph.makeUDrawFile (cfg, subprogramName);
			UDrawGraph.makeUDrawFile (lnt, subprogramName);
			UDrawGraph.makeUDrawFile (this, subprogramName);
		}
	}

	public final SyntaxVertex getVertex (int vertexID)
	{
		return (SyntaxVertex) idToVertex.get (vertexID);
	}

	public final LoopVertex getLoopVertex (int headerID)
	{
		for (Vertex v: this)
		{
			if (v instanceof LoopVertex)
			{
				LoopVertex loop = (LoopVertex) v;
				if (loop.getHeaderID () == headerID)
				{
					return loop;
				}
			}
		}
		return null;
	}

	private void build ()
	{
		for (int level = lnt.getHeight () - 1; level >= 0; --level)
		{
			Iterator<TreeVertex> levelIt = lnt.levelIterator (level);
			while (levelIt.hasNext ())
			{
				TreeVertex v = levelIt.next ();
				int cfgVertexID = v.getVertexID ();

				if (lnt.isLoopHeader (cfgVertexID))
				{
					headerID = cfgVertexID;

					Debug.debugMessage (getClass (),
							"Building portion of tree inside CFG header "
									+ headerID, 3);

					FlowGraph flowg = new FlowGraph ();
					induceSubraph (flowg);
					FlowGraph reverseg = new FlowGraph ();
					flowg.reverseGraph (reverseg);

					Debug.debugMessage (getClass (),
							"Building pre-dominator tree", 4);
					pret = new DominatorTree (flowg, flowg.getEntryID (),
							DominatorTreeType.PRE_DOMINATOR);

					Debug.debugMessage (getClass (),
							"Building post-dominator tree", 4);
					postt = new DominatorTree (reverseg, flowg.getExitID (),
							DominatorTreeType.POST_DOMINATOR);
					lca = new LeastCommonAncestor (postt);

					SyntaxVertex rootVertex = whichSubTree (flowg, flowg
							.getEntryID (), flowg.getExitID ());

					if (cfgVertexID == cfg.getEntryID ())
					{
						this.rootID = rootVertex.getVertexID ();
						setHeight ();
					}
					else
					{
						LoopVertex loop = addLoopVertex (cfgVertexID);
						addEdge (loop.getVertexID (), rootVertex.getVertexID ());
						headerToSyntaxTree.put (cfgVertexID, loop);
					}
				}
			}
		}
	}

	private void induceSubraph (FlowGraph flowg)
	{
		ArrayList<Integer> workList = new ArrayList<Integer> ();
		workList.addAll (lnt.getTails (headerID));

		// To track whether the induced subgraph has a unique exit point or not
		ArrayList<Integer> succs = new ArrayList<Integer> ();

		while (!workList.isEmpty ())
		{
			int vertexID = workList.remove (workList.size () - 1);

			if (!flowg.hasVertex (vertexID))
			{
				flowg.addVertex (vertexID);
				succs.add (vertexID);

				Vertex cfgv = cfg.getVertex (vertexID);
				Iterator<Edge> predIt = cfgv.predecessorIterator ();
				while (predIt.hasNext ())
				{
					Edge e = predIt.next ();
					int predID = e.getVertexID ();
					int headerPredID = lnt.getLoopHeader (predID);

					if (headerPredID == headerID
							|| lnt.isNested (headerPredID, headerID))
					{
						if (dfs.getEdgeType (predID, vertexID) != DFSEdgeType.BACK_EDGE)
						{
							workList.add (predID);
						}
					}
				}
			}
		}

		for (Vertex v: flowg)
		{
			Integer vertexID = v.getVertexID ();
			Vertex cfgv = cfg.getVertex (vertexID);

			Iterator<Edge> succIt = cfgv.successorIterator ();
			while (succIt.hasNext ())
			{
				Edge e = succIt.next ();
				int succID = e.getVertexID ();

				if (flowg.hasVertex (succID)
						&& dfs.getEdgeType (vertexID, succID) != DFSEdgeType.BACK_EDGE)
				{
					succs.remove (vertexID);
					flowg.addEdge (vertexID, succID);
				}
			}
		}

		// Either add another vertex to ensure there is a unique exit (when
		// there are multiple tails) or set it to the unique tail
		if (succs.size () != 1)
		{
			int exitID = flowg.getNextVertexID ();
			flowg.addExit (exitID);
			flowg.getVertex (exitID).setDummy ();
			Debug.debugMessage (getClass (), "Adding exit vertex " + exitID, 3);

			for (int vertexID: succs)
			{
				flowg.addEdge (vertexID, exitID);
			}
		}
		else
		{
			int exitID = succs.get (succs.size () - 1);
			flowg.setExitID (exitID);
		}

		// The entry vertex of the induced subgraph is the loop header
		flowg.setEntryID (headerID);
	}

	private LoopVertex addLoopVertex (int headerID)
	{
		int vertexID = getNextVertexID ();
		LoopVertex loop = new LoopVertex (vertexID, headerID);
		idToVertex.put (vertexID, loop);
		return loop;
	}

	private LeafVertex addLeafVertex (int cfgVertexID)
	{
		int vertexID = getNextVertexID ();
		LeafVertex leaf = new LeafVertex (vertexID);
		leaf.setCFGVertexID (cfgVertexID);
		idToVertex.put (vertexID, leaf);
		return leaf;
	}

	private LeafVertex addLambdaLeafVertex ()
	{
		int vertexID = getNextVertexID ();
		LeafVertex leaf = new LeafVertex (vertexID);
		leaf.setLambdaVertex ();
		idToVertex.put (vertexID, leaf);
		return leaf;
	}

	private AlternativeVertex addALTVertex ()
	{
		int vertexID = getNextVertexID ();
		AlternativeVertex alt = new AlternativeVertex (vertexID);
		idToVertex.put (vertexID, alt);
		return alt;
	}

	private SequenceVertex addSEQVertex ()
	{
		int vertexID = getNextVertexID ();
		SequenceVertex seq = new SequenceVertex (vertexID);
		idToVertex.put (vertexID, seq);
		return seq;
	}

	private SequenceVertex buildSEQLambda ()
	{
		SequenceVertex seq = addSEQVertex ();
		LeafVertex leaf = addLambdaLeafVertex ();
		addEdge (seq.getVertexID (), leaf.getVertexID ());
		return seq;
	}

	private SyntaxVertex whichSubTree (FlowGraph flowg,
			int sourceID,
			int destinationID)
	{
		if (flowg.getVertex (sourceID).numOfSuccessors () == 0)
		{
			return buildSEQ (flowg, sourceID, sourceID);
		}
		else
		{
			SequenceVertex seq = buildSEQ (flowg, sourceID, destinationID);
			if (!flowg.getVertex (destinationID).isDummy ())
			{
				LeafVertex leaf = addLeafVertex (destinationID);
				addEdge (seq.getVertexID (), leaf.getVertexID ());
			}
			return seq;
		}
	}

	private AlternativeVertex buildALT (FlowGraph flowg, int branchID)
	{
		CompressedDominatorTree comt = new CompressedDominatorTree (flowg,
				postt, lca, branchID);

		HashMap<Integer, AlternativeVertex> mergeRoots = new HashMap<Integer, AlternativeVertex> ();
		int ipostID = postt.getImmediateDominator (branchID);

		for (int level = comt.getHeight () - 1; level >= 1; --level)
		{
			Iterator<TreeVertex> vertexIt = comt.levelIterator (level);
			while (vertexIt.hasNext ())
			{
				TreeVertex comtreev = vertexIt.next ();
				int cfgvertexID = comtreev.getVertexID ();
				int parentID = comtreev.getParentID ();
				SequenceVertex seq = buildSEQ (flowg, cfgvertexID, parentID);

				if (!comtreev.isLeaf ())
				{
					AlternativeVertex alt = mergeRoots.get (cfgvertexID);
					addEdge (seq.getVertexID (), alt.getVertexID ());
				}

				if (!mergeRoots.containsKey (parentID))
				{
					mergeRoots.put (parentID, addALTVertex ());
				}
				AlternativeVertex alt = mergeRoots.get (parentID);
				addEdge (alt.getVertexID (), seq.getVertexID ());
			}
		}

		/*
		 * If there is an edge from the branch to its ipost then add a lambda
		 * sequence
		 */
		Vertex branch = flowg.getVertex (branchID);
		if (branch.hasSuccessor (ipostID))
		{
			SequenceVertex seq = buildSEQLambda ();
			AlternativeVertex alt = mergeRoots.get (ipostID);
			addEdge (alt.getVertexID (), seq.getVertexID ());
		}

		return mergeRoots.get (ipostID);
	}

	private SequenceVertex buildSEQ (FlowGraph flowg,
			int sourceID,
			int destinationID)
	{
		SequenceVertex seq = addSEQVertex ();

		int cfgVertexID = sourceID;
		do
		{
			if (lnt.isLoopHeader (cfgVertexID) && headerID != cfgVertexID)
			{
				LoopVertex loop = headerToSyntaxTree.get (cfgVertexID);

				if (loop.getParentID () == loop.getVertexID ())
				{
					addEdge (seq.getVertexID (), loop.getVertexID ());
				}
			}

			if (!flowg.getVertex (cfgVertexID).isDummy ())
			{
				LeafVertex leaf = addLeafVertex (cfgVertexID);
				addEdge (seq.getVertexID (), leaf.getVertexID ());
			}

			Vertex cfgv = flowg.getVertex (cfgVertexID);
			if (cfgv.numOfSuccessors () > 1)
			{
				AlternativeVertex alt = buildALT (flowg, cfgVertexID);
				addEdge (seq.getVertexID (), alt.getVertexID ());
			}

			cfgVertexID = postt.getImmediateDominator (cfgVertexID);
		}
		while (cfgVertexID != destinationID);

		return seq;
	}

	public final int countLeaves ()
	{
		int count = 0;
		for (Vertex v: this)
		{
			if (v instanceof LeafVertex)
			{
				count++;
			}
		}
		return count;
	}

	public final int countLambdaLeaves ()
	{
		int count = 0;
		for (Vertex v: this)
		{
			if (v instanceof LeafVertex)
			{
				LeafVertex leafv = (LeafVertex) v;
				if (leafv.isLambdaVertex ())
				{
					count++;
				}
			}
		}
		return count;
	}

	public final int countALTVertices ()
	{
		int count = 0;
		for (Vertex v: this)
		{
			if (v instanceof AlternativeVertex)
			{
				count++;
			}
		}
		return count;
	}

	public final int countSEQVertices ()
	{
		int count = 0;
		for (Vertex v: this)
		{
			if (v instanceof SequenceVertex)
			{
				count++;
			}
		}
		return count;
	}
}
