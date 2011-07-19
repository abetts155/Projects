package adam.betts.vertices;

import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;

import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.instructions.Instruction;
import adam.betts.outputs.Output;
import adam.betts.utilities.Enums.BranchType;

public class BasicBlock extends FlowVertex implements Cloneable
{
	/*
	 * The subprogram name for inlined basic blocks
	 */
	String subprogramName = null;
	
	/*
	 * The instructions inside a basic block
	 */
	private TreeSet<Instruction> instructions = new TreeSet<Instruction> (
			new Comparator<Instruction> ()
			{
				public int compare (Instruction instr1, Instruction instr2)
				{
					if (instr1.getAddress () < instr2.getAddress ())
					{
						return -1;
					}
					else if (instr1.getAddress () > instr2.getAddress ())
					{
						return 1;
					}
					else
					{
						return 0;
					}
				}
			});

	public BasicBlock (int vertexID)
	{
		super (vertexID);
	}

	public BasicBlock clone ()
	{
		BasicBlock theClone = new BasicBlock (this.vertexID);
		for (Instruction instr: instructions)
		{
			theClone.instructions.add (instr);
		}
		return theClone;
	}

	public final void setSubprogramName (String subprogramName)
	{
		this.subprogramName = subprogramName;
	}

	public final String getSubprogramName ()
	{
		return subprogramName;
	}

	public void addPredecessor (int predecessorID, BranchType type, int edgeID)
	{
		FlowEdge e = new FlowEdge (predecessorID);
		e.setBranchType (type);
		e.setEdgeID (edgeID);
		predecessors.add (e);
	}

	public void addSuccessor (int successorID, BranchType type, int edgeID)
	{
		FlowEdge e = new FlowEdge (successorID);
		e.setBranchType (type);
		e.setEdgeID (edgeID);
		successors.add (e);
	}

	public final void addInstruction (Instruction instruction)
	{
		instructions.add (instruction);
	}

	public final Iterator<Instruction> instructionIterator ()
	{
		return instructions.iterator ();
	}

	public final int numberofInstructions ()
	{
		return instructions.size ();
	}

	public final boolean hasAddress (long address)
	{
		for (Instruction instr: instructions)
		{
			if (instr.getAddress () == address)
			{
				return true;
			}
		}
		return false;
	}

	public final long getFirstAddress ()
	{
		if (instructions.size () == 0)
		{
			return Long.MAX_VALUE;
		}
		else
		{
			return instructions.first ().getAddress ();
		}
	}

	public final long getLastAddress ()
	{
		if (instructions.size () == 0)
		{
			return Long.MIN_VALUE;
		}
		else
		{
			return instructions.last ().getAddress ();
		}
	}

	public final Instruction getFirstInstruction ()
	{
		return instructions.first ();
	}

	public final Instruction getLastInstruction ()
	{
		return instructions.last ();
	}

	public String toString ()
	{
		StringBuffer buffer = new StringBuffer ();
		String out = "Basic block " + vertexID + "\n";
		buffer.append (out + Output.getPadderString (out.length (), '-'));

		buffer.append ("pred(" + vertexID + ") = {");
		int i = 1;
		for (Edge e: predecessors)
		{
			buffer.append (e.getVertexID ());
			if (i++ < predecessors.size ())
			{
				buffer.append (", ");
			}
		}
		buffer.append ("}\n");

		buffer.append ("succ(" + vertexID + ") = {");
		i = 1;
		for (Edge e: successors)
		{
			buffer.append (e.getVertexID ());
			if (i++ < successors.size ())
			{
				buffer.append (", ");
			}
		}
		buffer.append ("}\n\n");

		i = 1;
		for (Instruction inst: instructions)
		{
			buffer.append (Long.toHexString (inst.getAddress ()) + ": "
					+ inst.getInstruction ());
			if (i++ < instructions.size ())
			{
				buffer.append ("\n");
			}
		}
		if (instructions.size () == 0)
		{
			buffer.append ("<no instructions>\n");
		}

		buffer.append ("\n");
		return buffer.toString ();
	}
}
