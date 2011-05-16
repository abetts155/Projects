package adam.betts.graphs;

import java.util.HashMap;

import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.utils.BracketList;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;

public class CycleEquivalence
{
	protected final FlowGraph flowg;
	private HashMap<Integer, BracketList> bracketLists = new HashMap<Integer, BracketList> ();

	public CycleEquivalence (FlowGraph flowg)
	{
		this.flowg = flowg;
		compute ();
	}

	private void compute ()
	{
		DepthFirstTree dfsTree = new DepthFirstTree (flowg, flowg.getEntryID ());
		for (int i = dfsTree.numOfVertices (); i >= 1; --i)
		{
			Integer vertexID = dfsTree.getPreVertexID (i);
			Vertex v = flowg.getVertex (vertexID);

			Debug.debugMessage (getClass (), "Analysing " + vertexID
					+ " (pre-order = " + i + ")", 4);

			int i0;
			int i1;
			int i2;

		}
	}

}
