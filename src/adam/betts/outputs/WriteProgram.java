package adam.betts.outputs;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;

import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.instructions.Instruction;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.BasicBlock;
import adam.betts.vertices.Vertex;

public class WriteProgram
{
	private Program program;

	public WriteProgram (Program program)
	{
		this.program = program;
		new XMLOutput ();
	}

	private class XMLOutput
	{
		public XMLOutput ()
		{
			try
			{
				String fileName = program.getName () + ".xml";
				BufferedWriter out = new BufferedWriter (new FileWriter (fileName));
				writeDTD (out);
				out.write ("<program name=\"" + program.getName () + "\">\n");
				for (Subprogram subprg : program)
				{
					writeCFGStar (out, subprg, program.getCallGraph ());
				}
				out.write ("</program>\n");

				out.close ();
			} catch (Exception e)
			{
				System.err.println ("Error: " + e.getMessage ());
				System.exit (1);
			}
		}

		private void writeDTD (BufferedWriter out) throws IOException
		{
			out.write ("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>\n");
			out.write ("<!DOCTYPE program [\n");
			out.write ("<!ATTLIST program\n");
			out.write ("   name CDATA #IMPLIED>\n");
			out.write ("<!ELEMENT program (cfg*)>\n");
			out.write ("<!ATTLIST cfg\n");
			out.write ("  id ID #REQUIRED\n");
			out.write ("  main CDATA #IMPLIED\n");
			out.write ("  name CDATA #IMPLIED>\n");
			out.write ("<!ELEMENT cfg (bb+)>\n");
			out.write ("<!ATTLIST bb\n");
			out.write ("   id ID #REQUIRED>\n");
			out.write ("<!ELEMENT bb (prec?,inst*,succ?)>\n");
			out.write ("<!ELEMENT prec (link+)>\n");
			out.write ("<!ELEMENT succ (link+)>\n");
			out.write ("<!ATTLIST link\n");
			out.write ("  type (taken|nottaken|call) #REQUIRED\n");
			out.write ("  cfg CDATA #REQUIRED\n");
			out.write ("  bb IDREF #REQUIRED>\n");
			out.write ("<!ELEMENT link EMPTY>\n");
			out.write ("<!ATTLIST inst\n");
			out.write ("   addr CDATA #REQUIRED\n");
			out.write ("   instr CDATA #IMPLIED>\n");
			out.write ("<!ELEMENT inst EMPTY>\n");
			out.write ("]>\n");
		}

		private void writeCFGStar (BufferedWriter out, Subprogram subprogram, CallGraph callg)
				throws IOException
		{
			Debug.debugMessage (getClass (), "In " + subprogram.getSubprogramName (), 3);
			ControlFlowGraph cfg = subprogram.getCFG ();
			out.write ("  <cfg id=\"" + subprogram.getSubprogramID () + "\" name=\""
					+ subprogram.getSubprogramName () + "\">\n");

			Debug.debugMessage (getClass (), "Writing basic blocks and edges", 3);
			for (Vertex v : cfg)
			{
				BasicBlock bb = (BasicBlock) v;
				out.write ("    <bb id=\"" + bb.getVertexID () + "\">\n");

				Iterator <Instruction> instrIt = bb.instructionIterator ();
				while (instrIt.hasNext ())
				{
					Instruction instr = instrIt.next ();
					out.write ("      <inst addr=\"0x" + Long.toHexString (instr.getAddress ())
							+ "\" instr=\"" + instr.getInstruction () + "\"/>\n");
				}

				out.write ("      <succ>\n");
				Iterator <Edge> succIt = bb.successorIterator ();
				while (succIt.hasNext ())
				{
					FlowEdge succEdge = (FlowEdge) succIt.next ();
					if (succEdge.getVertexID () != cfg.getEntryID ())
					{
						out.write ("        <link type=\""
								+ succEdge.getBranchType ().toString ().toLowerCase ()
								+ "\" cfg=\"" + Integer.toString (subprogram.getSubprogramID ())
								+ "\" bb=\"" + Integer.toString (succEdge.getVertexID ())
								+ "\"/>\n");
					}
				}

				int calleeID = callg.isCallSite (subprogram.getSubprogramID (), bb.getVertexID ());
				if (calleeID != Vertex.DUMMY_VERTEX_ID)
				{
					ControlFlowGraph calleeCFG = program.getSubprogram (calleeID).getCFG ();
					out.write ("        <link type=\"" + BranchType.CALL.toString ().toLowerCase ()
							+ "\" cfg=\"" + Integer.toString (calleeID) + "\" bb=\""
							+ Integer.toString (calleeCFG.getEntryID ()) + "\"/>\n");
				}

				out.write ("      </succ>\n");
				out.write ("    </bb>\n");
			}
			out.write ("  </cfg>\n");
		}
	}
}
