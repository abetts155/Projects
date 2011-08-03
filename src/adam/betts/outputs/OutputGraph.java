package adam.betts.outputs;

import java.util.Iterator;

import adam.betts.edges.CallEdge;
import adam.betts.edges.Edge;
import adam.betts.graphs.CFGStar;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.Graph;
import adam.betts.graphs.trees.Tree;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.call.CallVertex;
import adam.betts.vertices.trees.TreeVertex;

public class OutputGraph
{
	public static void output (Graph g)
	{
		if (g instanceof Tree)
		{
			outputTree ((Tree) g);
		} else if (g instanceof CFGStar)
		{
			outputCFGStar ((CFGStar) g);
		} else if (g instanceof ControlFlowGraph)
		{
			outputCFG ((ControlFlowGraph) g);
		} else if (g instanceof CallGraph)
		{
			outputCallGraph ((CallGraph) g);
		} else
		{
			for (Vertex v : g)
			{
				System.out.print ("pred(" + v.getVertexID () + ") = {");
				int i = 1;
				Iterator <Edge> predIt = v.predecessorIterator ();
				while (predIt.hasNext ())
				{
					Edge e = predIt.next ();
					System.out.print (e.getVertexID ());
					if (i++ < v.numOfPredecessors ())
					{
						System.out.print (", ");
					}
				}
				System.out.println ("}");

				System.out.print ("succ(" + v.getVertexID () + ") = {");
				i = 1;
				Iterator <Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					System.out.print (e.getVertexID ());
					if (i++ < v.numOfSuccessors ())
					{
						System.out.print (", ");
					}
				}
				System.out.println ("}\n");
			}
		}
	}

	private static void outputTree (Tree tree)
	{
		for (Vertex v : tree)
		{
			int vertexID = v.getVertexID ();

			if (tree.getRootID () == vertexID)
			{
				System.out.println (vertexID + " is root");
			} else
			{
				TreeVertex treev = (TreeVertex) v;
				System.out.println ("parent(" + vertexID + ") = " + treev.getParentID ());
			}
		}
	}

	private static void outputCFGStar (CFGStar cfgStar)
	{
		for (Vertex v : cfgStar)
		{
			int vertexID = v.getVertexID ();
			if (cfgStar.isIpoint (vertexID))
			{
				System.out.println (((Ipoint) v).toString ());
			} else
			{
				System.out.println (cfgStar.getCFG ().getBasicBlock (vertexID).toString ());
			}
		}
	}

	private static void outputCFG (ControlFlowGraph cfg)
	{
		for (Vertex v : cfg)
		{
			int vertexID = v.getVertexID ();
			System.out.println (cfg.getBasicBlock (vertexID).toString ());
		}
	}

	private static void outputCallGraph (CallGraph callg)
	{
		for (Vertex v : callg)
		{
			int vertexID = v.getVertexID ();
			CallVertex callv = (CallVertex) v;

			String out = callv.getSubprogramName () + " (id = " + vertexID + ")";
			System.out.println (out);
			Output.outputPadderString (out.length (), '-');

			if (v.numOfSuccessors () == 0)
			{
				System.out.println ("<LEAF>");
			} else
			{
				Iterator <Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					CallEdge e = (CallEdge) succIt.next ();
					CallVertex w = callg.getVertex (e.getVertexID ());
					System.out.println (w.getSubprogramName () + " @ " + e.callSites ());
				}
			}
		}
	}
}
