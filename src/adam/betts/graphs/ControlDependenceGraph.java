package adam.betts.graphs;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.outputs.OutputGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.vertices.ControlDependenceBasicBlock;
import adam.betts.vertices.ControlDependenceEdge;
import adam.betts.vertices.ControlDependenceVertex;
import adam.betts.vertices.Vertex;

public class ControlDependenceGraph extends DirectedGraph
{
	private final FlowGraph flowg;
	private final DominatorTree postTree;

	public ControlDependenceGraph (final FlowGraph flowg)
	{
		this.flowg = flowg;

		// Reverse the flow graph
		FlowGraph reverseg = new FlowGraph ();
		flowg.reverseGraph (reverseg);

		// Compute the post-dominator tree
		postTree = new DominatorTree (reverseg, flowg.getExitID (),
				DominatorTreeType.POST_DOMINATOR);

		// Add vertices representing basic blocks and control-dependent edges
		initialise ();

		// Compute control dependence
		compute ();

		// Remove vertices not part of the control dependence relation
		trim ();
	}

	private void initialise ()
	{
		int nextVertexID = 1;

		for (Vertex v : flowg)
		{
			ControlDependenceBasicBlock bbvertex = new ControlDependenceBasicBlock (nextVertexID,
					v.getVertexID ());

			idToVertex.put (nextVertexID, bbvertex);

			nextVertexID++;

			if (v.numOfSuccessors () > 1)
			{
				Iterator <Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					final int succID = e.getVertexID ();

					ControlDependenceEdge edgeVertex = new ControlDependenceEdge (nextVertexID,
							v.getVertexID (), succID);

					idToVertex.put (nextVertexID, edgeVertex);

					nextVertexID++;
				}
			}
		}
	}

	private void trim ()
	{
		Set <Integer> disconnected = new HashSet <Integer> ();

		for (Vertex v : this)
		{
			if (v.numOfPredecessors () == 0 && v.numOfSuccessors () == 0)
			{
				disconnected.add (v.getVertexID ());
			}
		}

		for (int vertexID : disconnected)
		{
			removeVertex (vertexID);
		}
	}

	private ControlDependenceBasicBlock getBasicBlockVertex (int basicBlockID)
	{
		for (Vertex v : this)
		{
			if (v instanceof ControlDependenceBasicBlock)
			{
				ControlDependenceBasicBlock controlv = (ControlDependenceBasicBlock) v;

				if (controlv.getBasicBlockID () == basicBlockID)
				{
					return controlv;
				}
			}
		}

		assert false;

		return null;
	}

	private ControlDependenceEdge getEdgeVertex (int predID, int succID)
	{
		for (Vertex v : this)
		{
			if (v instanceof ControlDependenceEdge)
			{
				ControlDependenceEdge controlv = (ControlDependenceEdge) v;

				if (controlv.getPredecessorID () == predID && controlv.getSuccessorID () == succID)
				{
					return controlv;
				}
			}
		}

		assert false;

		return null;
	}

	private void compute ()
	{
		for (Vertex v : flowg)
		{
			if (v.numOfSuccessors () > 1)
			{
				final int predID = v.getVertexID ();

				Iterator <Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					final int succID = e.getVertexID ();

					Set <Integer> depends = computeEdgeControlDepedence (predID, succID);

					for (int basicBlockID : depends)
					{
						Debug.debugMessage (getClass (), basicBlockID
								+ " is control dependent on (" + v.getVertexID () + "," + succID
								+ ")", 4);

						ControlDependenceVertex predv = getBasicBlockVertex (basicBlockID);
						ControlDependenceVertex succv = getEdgeVertex (predID, succID);

						addEdge (predv.getVertexID (), succv.getVertexID ());
					}
				}
			}
		}
	}

	private Set <Integer> computeEdgeControlDepedence (int vertexID, int succID)
	{
		HashSet <Integer> depends = new HashSet <Integer> ();

		final int postID = postTree.getImmediateDominator (vertexID);
		int vID = succID;

		while (vID != postID)
		{
			depends.add (vID);
			vID = postTree.getImmediateDominator (vID);
		}

		return depends;
	}
}
