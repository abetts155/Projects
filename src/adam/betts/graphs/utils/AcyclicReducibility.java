package adam.betts.graphs.utils;

import java.util.HashSet;
import java.util.Set;

import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.vertices.Vertex;

public class AcyclicReducibility
{
	protected final FlowGraph flowg;
	protected final FlowGraph reverseg;
	protected final Set <Integer> irreducibleBranches = new HashSet <Integer> ();
	protected final Set <Integer> irreducibleMerges = new HashSet <Integer> ();

	protected final DominatorTree predomt;
	protected final DominatorTree postdomt;

	public AcyclicReducibility (FlowGraph flowg)
	{
		this.flowg = flowg;

		predomt = new DominatorTree (flowg, flowg.getEntryID (), DominatorTreeType.PRE_DOMINATOR);

		reverseg = new FlowGraph ();
		flowg.reverseGraph (reverseg);

		postdomt = new DominatorTree (reverseg, reverseg.getEntryID (),
				DominatorTreeType.POST_DOMINATOR);

		compute ();
	}

	public AcyclicReducibility (FlowGraph flowg, FlowGraph reverseg, DominatorTree predomt,
			DominatorTree postdomt)
	{
		this.flowg = flowg;
		this.reverseg = reverseg;
		this.predomt = predomt;
		this.postdomt = postdomt;

		compute ();
	}

	public final boolean isReducible ()
	{
		return irreducibleBranches.size () == 0 && irreducibleMerges.size () == 0;
	}

	public final boolean isReducibleBranch (int vertexID)
	{
		return !irreducibleBranches.contains (vertexID);
	}

	public final boolean isReducibleMerge (int vertexID)
	{
		return !irreducibleMerges.contains (vertexID);
	}

	private void compute ()
	{
		DominanceFrontiers preDF = new DominanceFrontiers (flowg, predomt);
		DominanceFrontiers postDF = new DominanceFrontiers (reverseg, postdomt);

		for (Vertex v : flowg)
		{
			int vertexID = v.getVertexID ();

			if (v.numOfSuccessors () > 1)
			{
				int ipostID = postdomt.getImmediateDominator (vertexID);
				int ipreID = predomt.getImmediateDominator (ipostID);

				if (ipreID != vertexID)
				{
					if (preDF.size (vertexID) > 1)
					{
						irreducibleBranches.add (vertexID);
					}
				}
			}

			if (v.numOfPredecessors () > 1)
			{
				int ipreID = predomt.getImmediateDominator (vertexID);
				int ipostID = postdomt.getImmediateDominator (ipreID);

				if (ipostID != vertexID)
				{
					if (postDF.size (vertexID) > 1)
					{
						irreducibleMerges.add (vertexID);
					}
				}
			}
		}
	}
}
