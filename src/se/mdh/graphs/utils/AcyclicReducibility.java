package se.mdh.graphs.utils;

import java.util.HashSet;
import java.util.Set;

import se.mdh.graphs.FlowGraph;
import se.mdh.graphs.trees.DominatorTree;
import se.mdh.utilities.Enums.DominatorTreeType;
import se.mdh.vertices.Vertex;

public class AcyclicReducibility
{
	protected FlowGraph flowg;
	protected Set<Integer> irreducibleBranches = new HashSet<Integer> ();
	protected Set<Integer> irreducibleMerges = new HashSet<Integer> ();

	public AcyclicReducibility (FlowGraph flowg)
	{
		this.flowg = flowg;

		compute ();
	}

	public final boolean isReducible ()
	{
		return irreducibleBranches.size () == 0
				&& irreducibleMerges.size () == 0;
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
		DominatorTree predomt = new DominatorTree (flowg, flowg.getEntryID (),
				DominatorTreeType.PRE_DOMINATOR);
		DominanceFrontiers preDF = new DominanceFrontiers (flowg, predomt);
		FlowGraph reverseg = new FlowGraph ();
		flowg.reverseGraph (reverseg);
		DominatorTree postdomt = new DominatorTree (reverseg, reverseg
				.getEntryID (), DominatorTreeType.POST_DOMINATOR);
		DominanceFrontiers postDF = new DominanceFrontiers (reverseg, postdomt);

		for (Vertex v: flowg)
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
