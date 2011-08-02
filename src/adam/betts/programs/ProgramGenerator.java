package adam.betts.programs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

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
	protected ArrayList <Subprogram> subprograms = new ArrayList <Subprogram> ();
	protected Program program = new Program ();
	protected CallGraph callgraph = new CallGraph ();
	protected int rootID;
	protected Random gen = new Random ();
	protected final int numOfSubprograms = MainProgramGenerator.Globals.getNumberOfSubprograms ();
	protected final int depth = MainProgramGenerator.Globals.getDepthOfCallGraph ();
	protected HashMap <Integer, ArrayList <Integer>> callSites = new HashMap <Integer, ArrayList <Integer>> ();
	protected ArrayList <Integer> disconnectedNodes = new ArrayList <Integer> ();
	protected ArrayList <Subprogram> possibleRoots = new ArrayList <Subprogram> ();
	protected int level = 0;
	protected HashMap <Integer, Integer> levelMap = new HashMap <Integer, Integer> ();

	public ProgramGenerator ()
	{
		addSubprograms ();
		addCalls ();
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
		Debug.debugMessage (getClass (), "Adding subprograms", 3);

		for (int i = 1; i <= numOfSubprograms; ++i)
		{
			final String subprogramName = "F" + Integer.toString (i);
			program.addSubprogram (i, subprogramName);
			Subprogram subprogram = program.getSubprogram (i);

			Debug.debugMessage (getClass (), "Adding subprogram " + subprogramName, 4);

			CFGGenerator generator = new CFGGenerator ();
			final ControlFlowGraph cfg = generator.getCFG ();
			subprogram.setCFG (cfg);

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

	private void addCalls ()
	{
		initiateCallGraph ();
		initLevelMap ();
		setRoot ();
		addEdges ();
	}

	private void initiateCallGraph ()
	{
		for (Subprogram s : program)
		{
			int subprogramID = s.getSubprogramID ();
			disconnectedNodes.add (subprogramID);
			callgraph.addVertex (subprogramID, s.getSubprogramName ());
			callSites.put (subprogramID, new ArrayList <Integer> ());

			for (Vertex v : s.getCFG ())
			{
				if (v.numOfSuccessors () == 1 && v.getVertexID () != s.getCFG ().getExitID ())
				{
					callSites.get (subprogramID).add (v.getVertexID ());
				}
			}

			if (s.getCFG ().numOfVertices () > 0)
			{
				possibleRoots.add (s);
			}
		}
	}

	private void setRoot ()
	{
		int maxCallSites = 0;
		for (Subprogram s : possibleRoots)
		{
			if (callSites.get (s.getSubprogramID ()).size () > maxCallSites)
			{
				maxCallSites = callSites.get (s.getSubprogramID ()).size ();
				rootID = s.getSubprogramID ();
			}
		}

		disconnectedNodes.remove (new Integer (rootID));
		level++;
		levelMap.put (rootID, level);
	}

	private void initLevelMap ()
	{
		for (Subprogram s : program)
		{
			levelMap.put (s.getSubprogramID (), 0);
		}
	}

	private void addEdges ()
	{
		while (!disconnectedNodes.isEmpty ())
		{
			// pick a random node
			int node = disconnectedNodes.remove (disconnectedNodes.size () - 1);

			System.out.println ("Analyzing node " + node);

			ArrayList <Integer> candidateNodes = new ArrayList <Integer> ();

			for (Integer key : levelMap.keySet ())
			{
				int level = levelMap.get (key);
				if (level != 0 && level < depth)
				{
					if (callSites.get (key).size () > 0)
					{
						candidateNodes.add (key);
					}
				}
			}

			System.out.println ("candidateNodes = " + candidateNodes);

			int predecessors = gen.nextInt (candidateNodes.size ()) + 1;
			for (int i = 1; i <= predecessors; ++i)
			{
				int predecessorIndex = gen.nextInt (candidateNodes.size ());
				int predecessorID = candidateNodes.remove (predecessorIndex);

				int noOfCallSites = gen.nextInt (callSites.get (predecessorID).size ()) + 1;
				System.out.println ("callSites chosen: " + noOfCallSites);
				System.out.println ("actual callSites: " + callSites.get (predecessorID).size ());

				for (int j = 0; j < noOfCallSites; ++j)
				{
					int callSiteID = callSites.get (predecessorID).get (j);
					Debug.debugMessage (getClass (), "Adding call from " + predecessorID + " to "
							+ node + " at " + callSiteID, 4);
					callgraph.addCall (predecessorID, node, callSiteID);
				}

				ArrayList <Integer> remainingCallSites = new ArrayList <Integer> ();

				for (int j = noOfCallSites; j < callSites.get (predecessorID).size (); ++j)
				{
					remainingCallSites.add (callSites.get (predecessorID).get (j));
				}
				callSites.put (predecessorID, remainingCallSites);
				System.out.println ("remainingCallsites: " + callSites.get (predecessorID));

				if (levelMap.get (predecessorID) > levelMap.get (node))
				{
					levelMap.put (node, levelMap.get (predecessorID) + 1);
				}
			}
		}
	}
}
