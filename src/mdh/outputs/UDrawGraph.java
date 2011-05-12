package se.mdh.outputs;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;

import se.mdh.edges.CallEdge;
import se.mdh.edges.Edge;
import se.mdh.edges.FlowEdge;
import se.mdh.edges.IPGEdge;
import se.mdh.graphs.CFGStar;
import se.mdh.graphs.CallGraph;
import se.mdh.graphs.CallLoopGraph;
import se.mdh.graphs.ContextGraph;
import se.mdh.graphs.ControlFlowGraph;
import se.mdh.graphs.IpointGraph;
import se.mdh.graphs.SuperBlockGraph;
import se.mdh.graphs.trees.DepthFirstTree;
import se.mdh.graphs.trees.DominatorTree;
import se.mdh.graphs.trees.LoopNests;
import se.mdh.graphs.trees.SyntaxTree;
import se.mdh.graphs.trees.Tree;
import se.mdh.instructions.Instruction;
import se.mdh.utilities.Globals;
import se.mdh.utilities.Enums.DominatorTreeType;
import se.mdh.utilities.Enums.IProfile;
import se.mdh.vertices.BasicBlock;
import se.mdh.vertices.Ipoint;
import se.mdh.vertices.SuperBlockVertex;
import se.mdh.vertices.Vertex;
import se.mdh.vertices.call.CallLoopVertex;
import se.mdh.vertices.call.CallVertex;
import se.mdh.vertices.call.ContextVertex;
import se.mdh.vertices.call.LoopVertex;
import se.mdh.vertices.trees.AlternativeVertex;
import se.mdh.vertices.trees.HeaderVertex;
import se.mdh.vertices.trees.LeafVertex;
import se.mdh.vertices.trees.SequenceVertex;
import se.mdh.vertices.trees.SyntaxVertex;
import se.mdh.vertices.trees.TreeVertex;

public class UDrawGraph
{
	private enum COLOR
	{
		RED, YELLOW, BLUE, GREEN, BLACK
	};

	private enum SHAPE
	{
		BOX, CIRCLE, ELLIPSE, RHOMBUS, TRIANGLE
	};

	private enum EDGESHAPE
	{
		SOLID, DOTTED, DASHED
	};

	private final static String fileNameSuffix = ".udraw";
	private final static String beginGraph = "[\n";
	private final static String endGraph = "]\n";
	private final static String endVertex = "])),";
	private final static String newEdge = "\ne(\"tEdge\",";
	private final static String endEdge = ")";
	private final static String beginAttributes = "[";
	private final static String endAttibutes = "],";
	private final static String setDirectionalLessEdge = "a(\"_DIR\", \"none\"),";

	private final static String newVertex (int vertexID)
	{
		return "l(\"v" + vertexID + "\",n(\"tVertex\",";
	}

	private final static String edgeLink (int vertexID)
	{
		return "r(\"v" + vertexID + "\")";
	}

	private final static String setName (String name)
	{
		return "a(\"OBJECT\", \"" + name + "\"),";
	}

	private final static String setColor (COLOR color)
	{
		return "a(\"COLOR\", \"" + color.toString ().toLowerCase () + "\"),";
	}

	private final static String setShape (SHAPE shape)
	{
		return "a(\"_GO\", \"" + shape.toString ().toLowerCase () + "\"),";
	}

	private final static String setToolTip (String tooltip)
	{
		return "a(\"INFO\", \"" + tooltip + "\"),";
	}

	private final static String setEdgePattern (EDGESHAPE shape, int width)
	{
		return "a(\"EDGEPATTERN\", \"single;"
				+ shape.toString ().toLowerCase () + ";" + width + ";1\"),";
	}

	private final static String setEdgeColor (COLOR color)
	{
		return "a(\"EDGECOLOR\", \"" + color.toString ().toLowerCase ()
				+ "\"),";
	}

	public final static void makeUDrawFile (ControlFlowGraph cfg,
			String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + ".cfg" + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);

			/*
			 * Write the entry vertex so it appears at the top of the graph
			 */
			writeBasicBlock (out, cfg.getBasicBlock (cfg.getEntryID ()));

			for (Vertex v: cfg)
			{
				BasicBlock bb = (BasicBlock) v;
				if (bb.getVertexID () != cfg.getEntryID ())
				{
					writeBasicBlock (out, bb);
				}
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	private static void writeBasicBlock (BufferedWriter out, BasicBlock bb) throws IOException
	{
		int vertexID = bb.getVertexID ();
		out.write (newVertex (vertexID));
		out.write (beginAttributes);

		String subprogramName = bb.getSubprogramName ();
		if (subprogramName == null)
		{
			out.write (setName (Integer.toString (vertexID)));
		}
		else
		{
			out.write (setName (Integer.toString (vertexID) + "\\n"
					+ subprogramName));
		}

		out.write (setToolTip (basicBlockString (bb)));
		out.write (endAttibutes);

		out.write (beginAttributes);
		writeCFGEdges (out, bb);
		out.write (endVertex + "\n");
	}

	private static void writeCFGEdges (BufferedWriter out, BasicBlock bb) throws IOException
	{
		Iterator<Edge> succIt = bb.successorIterator ();
		while (succIt.hasNext ())
		{
			FlowEdge e = (FlowEdge) succIt.next ();
			out.write (newEdge);
			out.write (beginAttributes);
			out.write (setName (Integer.toString (e.getEdgeID ())));
			out.write (setToolTip (e.getBranchType ().toString ()));
			out.write (endAttibutes);
			out.write (edgeLink (e.getVertexID ()));
			out.write (endEdge + ",\n");
		}
	}

	public final static void makeUDrawFile (IProfile iprofile,
			CFGStar cfgStar,
			String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + ".cfg." + iprofile + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);

			/*
			 * Write the entry vertex so it appears at the top of the graph
			 */
			writeCFGStarVertex (out, cfgStar, cfgStar.getVertex (cfgStar
					.getEntryID ()));

			for (Vertex v: cfgStar)
			{
				if (v.getVertexID () != cfgStar.getEntryID ())
				{
					writeCFGStarVertex (out, cfgStar, v);
				}
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	private static void writeCFGStarVertex (BufferedWriter out,
			CFGStar cfgStar,
			Vertex v) throws IOException
	{
		int vertexID = v.getVertexID ();
		out.write (newVertex (vertexID));
		out.write (beginAttributes);

		if (cfgStar.isIpoint (vertexID))
		{
			out.write (setShape (SHAPE.CIRCLE));

			if (vertexID == cfgStar.getEntryID ())
			{
				out.write (setColor (COLOR.BLACK));
				out.write (setToolTip ("Entry (id =" + vertexID + ")"));
			}
			else if (vertexID == cfgStar.getExitID ())
			{
				out.write (setColor (COLOR.BLACK));
				out.write (setToolTip ("Exit (id =" + vertexID + ")"));
			}
			else
			{
				Ipoint i = (Ipoint) v;

				out.write (setName (Integer.toString (vertexID)));
				out.write (setToolTip (ipointString (i)));

				if (i.isInlinedEntry () || i.isInlinedExit ())
				{
					out.write (setColor (COLOR.YELLOW));
				}
				else
				{
					out.write (setColor (COLOR.BLUE));
				}
			}
		}
		else
		{
			out.write (setName (Integer.toString (vertexID)));
			ControlFlowGraph cfg = cfgStar.getCFG ();
			BasicBlock bb = cfg.getBasicBlock (vertexID);
			out.write (setToolTip (basicBlockString (bb)));
		}
		out.write (endAttibutes);

		out.write (beginAttributes);
		writeEdges (out, v);
		out.write (endVertex + "\n");
	}

	private static String basicBlockString (BasicBlock bb)
	{
		StringBuffer buffer = new StringBuffer ();
		int i = 1;
		Iterator<Instruction> instrIt = bb.instructionIterator ();
		while (instrIt.hasNext ())
		{
			Instruction instr = instrIt.next ();
			buffer.append (Long.toHexString (instr.getAddress ()) + ": "
					+ instr.getInstruction ());
			if (i++ < bb.numberofInstructions ())
			{
				buffer.append ("\\n");
			}
		}
		if (bb.numberofInstructions () == 0)
		{
			buffer.append ("<no instructions>");
		}
		return buffer.toString ();
	}

	private static String ipointString (Ipoint v)
	{
		StringBuffer buffer = new StringBuffer ();
		buffer.append ("Ipoint ID = 0x" + Long.toHexString (v.getIpointID ())
				+ " (" + Long.toString (v.getIpointID ()) + ")");
		if (v.isInlinedEntry ())
		{
			buffer.append ("\\nMaster entry for " + v.getSubprogramName ());
		}
		else if (v.isInlinedExit ())
		{
			buffer.append ("\\nMaster exit for " + v.getSubprogramName ());
		}
		return buffer.toString ();
	}

	private static void writeEdges (BufferedWriter out, Vertex v) throws IOException
	{
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			out.write (newEdge);
			out.write (beginAttributes);
			out.write (endAttibutes);
			out.write (edgeLink (e.getVertexID ()));
			out.write (endEdge + ",\n");
		}
	}

	public final static void makeUDrawFile (IProfile iprofile,
			IpointGraph ipg,
			String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + ".ipg." + iprofile + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);

			/*
			 * Write the entry vertex so it appears at the top of the graph
			 */
			writeIPGVertex (out, ipg, ipg.getVertex (ipg.getEntryID ()));

			for (Vertex v: ipg)
			{
				if (v.getVertexID () != ipg.getEntryID ())
				{
					writeIPGVertex (out, ipg, v);
				}
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	private static void writeIPGVertex (BufferedWriter out,
			IpointGraph ipg,
			Vertex v) throws IOException
	{
		Ipoint i = (Ipoint) v;
		int vertexID = v.getVertexID ();
		out.write (newVertex (v.getVertexID ()));
		out.write (beginAttributes);
		out.write (setShape (SHAPE.CIRCLE));

		if (vertexID == ipg.getEntryID () && i.isGhostIpoint ())
		{
			out.write (setColor (COLOR.BLACK));
			out.write (setToolTip ("Entry (id =" + vertexID + ")"));
		}
		else if (vertexID == ipg.getExitID () && i.isGhostIpoint ())
		{
			out.write (setColor (COLOR.BLACK));
			out.write (setToolTip ("Exit (id =" + vertexID + ")"));
		}
		else
		{
			out.write (setName (Integer.toString (vertexID)));
			out.write (setToolTip (ipointString (i)));

			if (i.isInlinedEntry () || i.isInlinedExit ())
			{
				out.write (setColor (COLOR.YELLOW));
			}
		}
		out.write (endAttibutes);

		out.write (beginAttributes);
		writeIPGEdges (out, v);
		out.write (endVertex + "\n");
	}

	private static void writeIPGEdges (BufferedWriter out, Vertex v) throws IOException
	{
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			IPGEdge e = (IPGEdge) succIt.next ();
			out.write (newEdge);
			out.write (beginAttributes);
			StringBuffer buffer = new StringBuffer ();
			buffer.append (e.getEdgeLabel ());
			if (e.isEntryEdge ())
			{
				out.write (setEdgePattern (EDGESHAPE.DASHED, 2));
				buffer.append ("\\nEntry for " + e.getEntryHeaderID ());
			}
			else if (e.isExitEdge ())
			{
				out.write (setEdgePattern (EDGESHAPE.DASHED, 2));
				buffer.append ("\\nExit for " + e.getExitHeaderID ());
			}
			else if (e.isIterationEdge ())
			{
				out.write (setEdgePattern (EDGESHAPE.SOLID, 3));
				buffer.append ("\\nIteration edge for "
						+ e.iterationHeaderIDs ());
			}
			out.write (setToolTip (buffer.toString ()));
			out.write (setName ("e" + Integer.toString (e.getEdgeID ())));
			out.write (endAttibutes);
			out.write (edgeLink (e.getVertexID ()));
			out.write (endEdge + ",\n");
		}
	}

	public final static void makeUDrawFile (SuperBlockGraph superg,
			String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + ".super" + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);
			for (Vertex v: superg)
			{
				writeSuperBlockVertex (out, (SuperBlockVertex) v);
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	private static void writeSuperBlockVertex (BufferedWriter out,
			SuperBlockVertex v) throws IOException
	{
		out.write (newVertex (v.getVertexID ()));
		out.write (beginAttributes);
		out.write (setShape (SHAPE.ELLIPSE));
		out.write (setName (v.basicBlockIDs ().toString ()));
		out.write (endAttibutes);

		out.write (beginAttributes);
		writeEdges (out, v);
		out.write (endVertex + "\n");
	}

	private final static void makeUDrawFile (Tree tree, String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);

			/*
			 * Write the root vertex so it appears at the top of the graph
			 */
			writeTreeVertex (out, tree.getVertex (tree.getRootID ()));

			for (Vertex v: tree)
			{
				if (v.getVertexID () != tree.getRootID ())
				{
					writeTreeVertex (out, (TreeVertex) v);
				}
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	public final static void makeUDrawFile (LoopNests lnt, String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);

			/*
			 * Write the root vertex so it appears at the top of the graph
			 */
			writeHeaderVertex (out, (HeaderVertex) lnt.getVertex (lnt
					.getRootID ()));

			for (Vertex v: lnt)
			{
				if (v.getVertexID () != lnt.getRootID ())
				{
					if (v instanceof HeaderVertex)
					{
						writeHeaderVertex (out, (HeaderVertex) v);
					}
					else
					{
						writeTreeVertex (out, (TreeVertex) v);
					}
				}
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	public final static void makeUDrawFile (SyntaxTree syntaxTree,
			String fileNamePrefix)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					fileNamePrefix + ".stree" + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);

			/*
			 * Write the root vertex so it appears at the top of the graph
			 */
			writeSyntaxTreeVertex (out, syntaxTree.getVertex (syntaxTree
					.getRootID ()));

			for (Vertex v: syntaxTree)
			{
				if (v.getVertexID () != syntaxTree.getRootID ())
				{
					writeSyntaxTreeVertex (out, (SyntaxVertex) v);
				}
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	public final static void makeUDrawFile (IProfile iprofile,
			LoopNests lnt,
			String fileNamePrefix)
	{
		makeUDrawFile (lnt, fileNamePrefix + ".lnt." + iprofile);
	}

	public final static void makeUDrawFile (DepthFirstTree dfs,
			String fileNamePrefix)
	{
		makeUDrawFile ((Tree) dfs, fileNamePrefix + ".dfs");
	}

	public final static void makeUDrawFile (DominatorTree domt,
			DominatorTreeType type,
			String fileNamePrefix)
	{
		makeUDrawFile ((Tree) domt, fileNamePrefix + ".domt." + type);
	}

	private static void writeTreeVertex (BufferedWriter out, TreeVertex v) throws IOException
	{
		out.write (newVertex (v.getVertexID ()));
		out.write (beginAttributes);
		out.write (setName (Integer.toString (v.getVertexID ())));
		out.write (setToolTip ("Level = " + v.getLevel ()));
		out.write (endAttibutes);

		out.write (beginAttributes);
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			out.write (newEdge);
			out.write (beginAttributes);
			out.write (setDirectionalLessEdge);
			out.write (endAttibutes);
			out.write (edgeLink (e.getVertexID ()));
			out.write (endEdge + ",\n");
		}
		out.write (endVertex + "\n");
	}

	private static void writeHeaderVertex (BufferedWriter out, HeaderVertex v) throws IOException
	{
		out.write (newVertex (v.getVertexID ()));
		out.write (beginAttributes);
		out.write (setShape (SHAPE.CIRCLE));
		out.write (setName (Integer.toString (v.getVertexID ())));
		out.write (setToolTip ("Header = " + v.getHeaderID () + "\\nLevel = "
				+ v.getLevel ()));
		out.write (endAttibutes);

		out.write (beginAttributes);
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			out.write (newEdge);
			out.write (beginAttributes);
			out.write (setDirectionalLessEdge);
			out.write (endAttibutes);
			out.write (edgeLink (e.getVertexID ()));
			out.write (endEdge + ",\n");
		}
		out.write (endVertex + "\n");
	}

	private static void writeSyntaxTreeVertex (BufferedWriter out,
			SyntaxVertex v) throws IOException
	{
		out.write (newVertex (v.getVertexID ()));
		out.write (beginAttributes);
		if (v instanceof SequenceVertex)
		{
			out.write (setName ("SEQ_" + Integer.toString (v.getVertexID ())));
			out.write (setColor (COLOR.YELLOW));
			out.write (setShape (SHAPE.RHOMBUS));
		}
		else if (v instanceof AlternativeVertex)
		{
			out.write (setName ("ALT_" + Integer.toString (v.getVertexID ())));
			out.write (setColor (COLOR.BLUE));
			out.write (setShape (SHAPE.RHOMBUS));
		}
		else if (v instanceof se.mdh.vertices.trees.LoopVertex)
		{
			out.write (setName ("LOOP_" + Integer.toString (v.getVertexID ())));
			out.write (setColor (COLOR.RED));
			out.write (setShape (SHAPE.RHOMBUS));
		}
		else
		{
			LeafVertex leafv = (LeafVertex) v;
			if (leafv.isLambdaVertex ())
			{
				out.write (setName ("<>"));
				out.write (setShape (SHAPE.BOX));
			}
			else
			{
				out.write (setToolTip ("Vertex id = " + v.getVertexID ()));
				out
						.write (setName (Integer.toString (leafv
								.getCFGVertexID ())));
			}
		}

		out.write (endAttibutes);

		out.write (beginAttributes);
		Iterator<Edge> succIt = v.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			out.write (newEdge);
			out.write (beginAttributes);
			out.write (setDirectionalLessEdge);
			out.write (endAttibutes);
			out.write (edgeLink (e.getVertexID ()));
			out.write (endEdge + ",\n");
		}
		out.write (endVertex + "\n");
	}

	public final static void makeUDrawFile (CallGraph callg)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (), "callg"
					+ fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);
			for (Vertex v: callg)
			{
				CallVertex callv = (CallVertex) v;

				out.write (newVertex (v.getVertexID ()));

				out.write (beginAttributes);
				out.write (setName (callv.getSubprogramName ()));
				if (callv.numOfPredecessors () == 0)
				{
					out.write (setColor (COLOR.YELLOW));
				}
				else if (callv.numOfSuccessors () == 0)
				{
					out.write (setColor (COLOR.RED));
				}
				out.write (setToolTip ("Subprogram ID = "
						+ Integer.toString (callv.getVertexID ())));
				out.write (endAttibutes);

				out.write (beginAttributes);
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					CallEdge e = (CallEdge) succIt.next ();
					out.write (newEdge);
					out.write (beginAttributes);
					out.write (setToolTip (e.callSites ().toString ()));
					out.write (endAttibutes);
					out.write (edgeLink (e.getVertexID ()));
					out.write (endEdge + ",\n");
				}

				out.write (endVertex + "\n");
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	public final static void makeUDrawFile (CallLoopGraph clg)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (), "clg"
					+ fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);
			for (Vertex v: clg)
			{
				CallLoopVertex clv = (CallLoopVertex) v;

				out.write (newVertex (v.getVertexID ()));

				out.write (beginAttributes);
				if (clv instanceof LoopVertex)
				{
					out.write (setName (Integer.toString (clv.getVertexID ())));
					out.write (setColor (COLOR.BLUE));
					out.write (setShape (SHAPE.RHOMBUS));
					out.write (setToolTip (clv.getSubprogramName ()
							+ "\\nHeader = "
							+ ((LoopVertex) clv).getHeaderID ()));
				}
				else
				{
					out.write (setName (clv.getSubprogramName ()));
					if (clv.numOfPredecessors () == 0)
					{
						out.write (setColor (COLOR.YELLOW));
					}
					else if (clv.numOfSuccessors () == 0)
					{
						out.write (setColor (COLOR.RED));
					}
				}
				out.write (endAttibutes);

				out.write (beginAttributes);
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					Edge e = succIt.next ();
					out.write (newEdge);
					out.write (beginAttributes);
					out.write (endAttibutes);
					out.write (edgeLink (e.getVertexID ()));
					out.write (endEdge + ",\n");
				}

				out.write (endVertex + "\n");
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	public final static void makeUDrawFile (ContextGraph contextg)
	{
		try
		{
			final File file = new File (Globals.getUDrawDirectory (),
					"contextg" + fileNameSuffix);
			BufferedWriter out = new BufferedWriter (new FileWriter (file
					.getAbsolutePath ()));

			out.write (beginGraph);
			for (Vertex v: contextg)
			{
				ContextVertex contextv = (ContextVertex) v;

				out.write (newVertex (v.getVertexID ()));

				out.write (beginAttributes);
				out.write (setName (contextv.getSubprogramName ()
						+ " (context id = "
						+ Integer.toString (v.getVertexID ()) + ")"));
				out.write (endAttibutes);

				out.write (beginAttributes);
				Iterator<Edge> succIt = v.successorIterator ();
				while (succIt.hasNext ())
				{
					CallEdge e = (CallEdge) succIt.next ();
					out.write (newEdge);
					out.write (beginAttributes);
					out.write (setToolTip (e.callSites ().toString ()));
					out.write (endAttibutes);
					out.write (edgeLink (e.getVertexID ()));
					out.write (endEdge + ",\n");
				}

				out.write (endVertex + "\n");
			}
			out.write (endGraph);

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}
}
