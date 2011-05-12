package se.mdh.outputs;

import java.util.Iterator;

import se.mdh.edges.CallEdge;
import se.mdh.edges.Edge;
import se.mdh.graphs.CFGStar;
import se.mdh.graphs.CallGraph;
import se.mdh.graphs.ControlFlowGraph;
import se.mdh.graphs.Graph;
import se.mdh.vertices.Ipoint;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.call.CallVertex;

public class OutputGraph
{
	public static void output (Graph g)
	{
		System.out.println ("|V| = " + g.numOfVertices () + ", |E| = "
				+ g.numOfEdges () + "\n");

		if (g instanceof CFGStar)
		{
			outputCFGStar ((CFGStar) g);
		}
		else if (g instanceof ControlFlowGraph)
		{
			outputCFG ((ControlFlowGraph) g);
		}
		else if (g instanceof CallGraph)
		{
			outputCallGraph ((CallGraph) g);
		}
		else
		{
			for (Vertex v: g)
			{
				System.out.print ("pred(" + v.getVertexID () + ") = {");
				int i = 1;
				Iterator<Edge> predIt = v.predecessorIterator ();
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
				Iterator<Edge> succIt = v.successorIterator ();
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

	private static void outputCFGStar (CFGStar cfgStar)
	{
		for (Vertex v: cfgStar)
		{
			int vertexID = v.getVertexID ();
			if (cfgStar.isIpoint (vertexID))
			{
				System.out.println ( ((Ipoint) v).toString ());
			}
			else
			{
				System.out.println (cfgStar.getCFG ().getBasicBlock (vertexID)
						.toString ());
			}
		}
	}

	private static void outputCFG (ControlFlowGraph cfg)
	{
		for (Vertex v: cfg)
		{
			int vertexID = v.getVertexID ();
			System.out.println (cfg.getBasicBlock (vertexID).toString ());
		}
	}

	private static void outputCallGraph (CallGraph callg)
	{
		for (Vertex v: callg)
		{
			int vertexID = v.getVertexID ();
			CallVertex callv = (CallVertex) v;

			String out = callv.getSubprogramName () + " (id = " + vertexID
					+ ")";
			System.out.println (out);
			Output.outputPadderString (out.length (), '-');

			if (v.numOfSuccessors () == 0)
			{
				System.out.println ("<LEAF>");
			}
			else
			{
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					CallEdge e = (CallEdge) succIt.next ();
					CallVertex w = callg.getVertex (e.getVertexID ());
					System.out.println (w.getSubprogramName () + " @ "
							+ e.callSites ());
				}
			}
		}
	}
}
