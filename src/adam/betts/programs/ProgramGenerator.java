package adam.betts.programs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;
import java.util.Stack;

import adam.betts.edges.Edge;
import adam.betts.graphs.CFGGenerator;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.outputs.UDrawGraph;
import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.vertices.Vertex;

public class ProgramGenerator
{
	protected Random random = new Random ();
	protected Program program = new Program ();
	protected CallGraph callgraph = new CallGraph ();
	protected int rootID = Vertex.DUMMY_VERTEX_ID;
	protected HashMap <Integer, ArrayList <Integer>> candidateCallSites = new HashMap <Integer, ArrayList <Integer>> ();
	protected ArrayList <Integer> disconnectedNodes = new ArrayList <Integer> ();
	protected HashMap <Integer, Integer> levelMap = new HashMap <Integer, Integer> ();

	public ProgramGenerator ()
	{
		addSubprograms ();
		setCallGraphRoot ();
		addTreeEdgesToCallGraph ();
		addOtherAcyclicEdgesToCallGraph ();

		program.callg = callgraph;

		if (Globals.uDrawDirectorySet ())
		{
			UDrawGraph.makeUDrawFile (program.getCallGraph ());
		}
	}

	public final Program getProgram ()
	{
		return program;
	}

	private void addSubprograms ()
	{
		Debug.debugMessage (getClass (), "Adding subprograms", 1);

		for (int subprogramID = 1; subprogramID <= MainProgramGenerator.Globals
				.getNumberOfSubprograms (); ++subprogramID)
		{
			final String subprogramName = "F" + Integer.toString (subprogramID);
			program.addSubprogram (subprogramID, subprogramName);
			Subprogram s = program.getSubprogram (subprogramID);

			Debug.debugMessage (getClass (), "Adding subprogram " + subprogramName + " with id = "
					+ subprogramID, 1);

			CFGGenerator generator = new CFGGenerator ();
			final ControlFlowGraph cfg = generator.getCFG ();
			s.setCFG (cfg);

			callgraph.addVertex (subprogramID, s.getSubprogramName ());
			disconnectedNodes.add (subprogramID);
			candidateCallSites.put (subprogramID, new ArrayList <Integer> ());
			levelMap.put (subprogramID, 0);

			for (Vertex v : cfg)
			{
				if (v.numOfSuccessors () == 1 && v.getVertexID () != cfg.getExitID ())
				{
					candidateCallSites.get (subprogramID).add (v.getVertexID ());
				}
			}

			if (Globals.uDrawDirectorySet ())
			{
				UDrawGraph.makeUDrawFile (cfg, subprogramName);
			}

			LoopNests loop = new LoopNests (cfg, cfg.getEntryID ());

			if (Globals.uDrawDirectorySet ())
			{
				UDrawGraph.makeUDrawFile (loop, subprogramName);
			}
		}
	}

	private void setCallGraphRoot ()
	{
		Debug.debugMessage (getClass (), "Setting call graph root", 2);

		int totalCallSites = 0;
		int maxCallSites = 0;

		for (Subprogram s : program)
		{
			int callSites = candidateCallSites.get (s.getSubprogramID ()).size ();

			totalCallSites += callSites;

			if (candidateCallSites.get (s.getSubprogramID ()).size () > maxCallSites)
			{
				maxCallSites = callSites;
				rootID = s.getSubprogramID ();
			}
		}

		Debug.debugMessage (getClass (), "Root set to " + rootID, 2);
		disconnectedNodes.remove (new Integer (rootID));

		if (rootID == Vertex.DUMMY_VERTEX_ID)
		{
			Debug.errorMessage (getClass (), "Unable to set root");
		} else if (totalCallSites < disconnectedNodes.size ())
		{
			Debug.errorMessage (getClass (), "Unable to link the call graph together as there are "
					+ disconnectedNodes.size () + " subprograms but only " + totalCallSites
					+ " call sites are available");
		}
	}

	private int getPredecessorWithMaximumCallSites (int subprogramID)
	{
		if (subprogramID == rootID)
		{
			return rootID;
		} else
		{
			ArrayList <Vertex> candidatePredecessors = new ArrayList <Vertex> ();

			Stack <Vertex> stack = new Stack <Vertex> ();
			stack.push (callgraph.getVertex (subprogramID));
			while (!stack.isEmpty ())
			{
				Vertex v = stack.pop ();
				Iterator <Edge> predIt = v.predecessorIterator ();
				while (predIt.hasNext ())
				{
					Edge e = predIt.next ();
					Vertex p = callgraph.getVertex (e.getVertexID ());
					candidatePredecessors.add (p);
					stack.push (p);
				}
			}

			int maxSize = 0;
			int bestPredecessorID = Vertex.DUMMY_VERTEX_ID;
			for (Vertex v : candidatePredecessors)
			{
				if (candidateCallSites.get (v.getVertexID ()).size () > maxSize)
				{
					maxSize = candidateCallSites.get (v.getVertexID ()).size ();
					bestPredecessorID = v.getVertexID ();
				}
			}

			if (bestPredecessorID == Vertex.DUMMY_VERTEX_ID)
			{
				Debug.errorMessage (getClass (),
						"Unable to find a suitable vertex to which to backtrack");
			}

			return bestPredecessorID;
		}
	}

	private void addTreeEdgesToCallGraph ()
	{
		Debug.debugMessage (getClass (), "Adding tree edges to call graph", 1);

		int callerID = rootID;

		while (!disconnectedNodes.isEmpty ())
		{
			int subprogramID = disconnectedNodes.remove (disconnectedNodes.size () - 1);

			int callSiteIDIndex = random.nextInt (candidateCallSites.get (callerID).size ());
			int callSiteID = candidateCallSites.get (callerID).remove (callSiteIDIndex);

			callgraph.addCall (callerID, subprogramID, callSiteID);

			int newLevel = levelMap.get (callerID) + 1;
			levelMap.put (subprogramID, newLevel);

			if (newLevel == MainProgramGenerator.Globals.getDepthOfCallGraph ()
					|| candidateCallSites.get (subprogramID).size () == 0 || random.nextBoolean ())
			{
				callerID = getPredecessorWithMaximumCallSites (subprogramID);

				Debug.debugMessage (getClass (), "Backtracking up call graph to " + callerID, 4);
			} else
			{
				callerID = subprogramID;

				Debug.debugMessage (getClass (), "Moving the predecessor down to " + callerID, 4);
			}
		}
	}

	private void getCandidatePredecessors (Vertex subprogram, ArrayList <Integer> candidateCallers)
	{
		Debug.debugMessage (getClass (), "Finding candidate predecessors for subprogram "
				+ subprogram.getVertexID (), 1);

		for (Vertex v : callgraph)
		{
			int subprogramID = v.getVertexID ();

			if (subprogramID != subprogram.getVertexID ())
			{
				if (levelMap.get (subprogramID) < levelMap.get (subprogram.getVertexID ())
						&& subprogram.hasPredecessor (subprogramID) == false
						&& candidateCallSites.get (subprogramID).size () > 0)
				{
					candidateCallers.add (subprogramID);
				}
			}
		}
	}

	private void addOtherAcyclicEdgesToCallGraph ()
	{
		Debug.debugMessage (getClass (), "Adding other acyclic edges to call graph", 1);

		ArrayList <Integer> callerCandidates = new ArrayList <Integer> ();

		for (Vertex v : callgraph)
		{
			callerCandidates.clear ();
			getCandidatePredecessors (v, callerCandidates);

			if (callerCandidates.size () > 0)
			{
				int numberOfCallers = random.nextInt (callerCandidates.size ()) + 1;

				for (int i = 0; i < numberOfCallers; ++i)
				{
					int callerIDIndex = random.nextInt (callerCandidates.size ());
					int callerID = callerCandidates.remove (callerIDIndex);

					int numberOfCallSites = random.nextInt (candidateCallSites.get (callerID)
							.size ()) + 1;
					for (int j = 0; j < numberOfCallSites; ++j)
					{
						int callSiteIDIndex = random.nextInt (candidateCallSites.get (callerID)
								.size ());
						int callSiteID = candidateCallSites.get (callerID).remove (callSiteIDIndex);

						callgraph.addCall (callerID, v.getVertexID (), callSiteID);
					}
				}
			}
		}
	}
}
