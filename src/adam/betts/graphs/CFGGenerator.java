package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.Vertex;

public class CFGGenerator
{
	protected final static float branchToMergeBias = 0.2f;
	protected final static int maxPathLength = 5;
	private static final int remainingComponents = 0;

	protected Random random = new Random ();
	protected ControlFlowGraph cfg = new ControlFlowGraph ();
	protected HashMap<Integer, Integer> branchToMerge = new HashMap<Integer, Integer> ();
	protected ArrayList<Integer> disconnectedBranches = new ArrayList<Integer> ();
	protected ArrayList<Integer> disconnectedVertices = new ArrayList<Integer> ();
	protected ArrayList<Integer> loopBody = new ArrayList<Integer> ();
	protected boolean takenBranchAdded;

	public CFGGenerator ()
	{
		int noOfVertices = MainProgramGenerator.Globals.getNumberOfVerticesInCFG ();
		//addNonBranchComponents(noOfVertices);
		//addIfThenComponents (noOfVertices);
		//addIfElseComponents (noOfVertices);
		addForLoopBody ();
		//if (disconnectedBranches.size () > 1)
		//{
		//	enforceSingleEntry ();
		//}
		checkExits ();
		//addSelfLoops ();
		//addLoops ();
		setEntry ();
		setExit ();
	}

	public final ControlFlowGraph getCFG ()
	{
		return cfg;
	}

	private void addNonBranchComponents (int n) 
	{
		boolean odd = false;
		if (n % 2 != 0)
		{	
			n -= 1;
			odd = true;
		}
			
		for (int i = 1; i <= n; i = i+2)
		{
			int branchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(branchID);
			int mergeID = cfg.getNextVertexID ();
			cfg.addBasicBlock(mergeID);
			branchToMerge.put (branchID, mergeID);
			takenBranchAdded = false;
							
			Debug.debugMessage (getClass (), "Adding edge " + branchID
					+ "=>" + mergeID, 4);
			cfg.addEdge (branchID, mergeID, BranchType.TAKEN);
			disconnectedVertices.add (branchID);
			disconnectedVertices.add (mergeID);
		}
		
		if (odd)
			disconnectedVertices.add (cfg.getNextVertexID());
		for (int i = 1; i < disconnectedVertices.size () - 1; ++i)
		{	
			cfg.addEdge(disconnectedVertices.get (i), disconnectedVertices.get (i+1), BranchType.TAKEN);
		
		}
	}
	
	private void addIfThenComponents (int vertices) 
	{
		int noOfComponents = vertices / 3;
		int remainingComponents = vertices % 3;
		for (int i = 1; i <= noOfComponents; ++i) 
		{
			int branchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(branchID);
			int ifBranchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(ifBranchID);
			int mergeID = cfg.getNextVertexID ();
			cfg.addBasicBlock(mergeID);
				
			Debug.debugMessage (getClass (), "Adding edge " + branchID
					+ "=>" + ifBranchID, 4);
			cfg.addEdge (branchID, ifBranchID, BranchType.TAKEN);
			Debug.debugMessage (getClass (), "Adding edge " + branchID
					+ "=>" + mergeID, 4);
			cfg.addEdge (branchID, mergeID, BranchType.TAKEN);
			Debug.debugMessage (getClass (), "Adding edge " + ifBranchID
					+ "=>" + mergeID, 4);
			cfg.addEdge (ifBranchID, mergeID, BranchType.TAKEN);
			
			disconnectedBranches.add (branchID);
			disconnectedBranches.add (mergeID);
		}
		addNonBranchComponents(remainingComponents);
		connectBranches();
	}
	
	private void addIfElseComponents (int vertices) 
	{
		int noOfComponents = vertices / 4;
		int remainingComponents = vertices % 4;
		for (int i = 1; i <= noOfComponents; ++i) 
		{
			int branchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(branchID);
			int ifBranchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(ifBranchID);
			int elseBranchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(elseBranchID);
			int mergeID = cfg.getNextVertexID ();
			cfg.addBasicBlock(mergeID);
							
			Debug.debugMessage (getClass (), "Adding edge " + branchID
					+ "=>" + ifBranchID, 4);
			cfg.addEdge (branchID, ifBranchID, BranchType.TAKEN);
			Debug.debugMessage (getClass (), "Adding edge " + branchID
					+ "=>" + elseBranchID, 4);
			cfg.addEdge (branchID, elseBranchID, BranchType.TAKEN);
			Debug.debugMessage (getClass (), "Adding edge " + ifBranchID
					+ "=>" + mergeID, 4);
			cfg.addEdge (ifBranchID, mergeID, BranchType.TAKEN);
			Debug.debugMessage (getClass (), "Adding edge " + elseBranchID
					+ "=>" + mergeID, 4);
			cfg.addEdge (elseBranchID, mergeID, BranchType.TAKEN);
			
			disconnectedBranches.add (branchID);
			disconnectedBranches.add (mergeID);
		}
		addNonBranchComponents(remainingComponents);
		connectBranches();
	}	
		
	private void connectBranches () 
	{
		for (int i = 0; i < disconnectedBranches.size () - 3; i = i+2)
		{
			if (random.nextBoolean()) 
			{
				int destinationToRemove = random.nextInt(3);
				if (destinationToRemove > 0)
				{
					int sourceID = disconnectedBranches.get (i);
					int destinationRemovedID = cfg.getVertex(sourceID).getNthSuccessor(destinationToRemove-1).getVertexID();
					
					Debug.debugMessage (getClass (), "Removing edge " + sourceID
							+ "=>" + cfg.getVertex(sourceID).getNthSuccessor(0).getVertexID(), 4);
					cfg.removeEdge(sourceID, destinationRemovedID); 
					
					int newDestinationID = disconnectedBranches.get (i+2);
					
					Debug.debugMessage (getClass (), "Adding edge " + sourceID
							+ "=>" + newDestinationID, 4);
					cfg.addEdge(sourceID, newDestinationID, BranchType.TAKEN);
					
					sourceID = disconnectedBranches.get (i+3);
					
					Debug.debugMessage (getClass (), "Adding edge " + sourceID
							+ "=>" + destinationRemovedID, 4);
					cfg.addEdge(sourceID, destinationRemovedID, BranchType.TAKEN);
				}	
			}
			else	
				cfg.addEdge (disconnectedBranches.get (i+1), disconnectedBranches.get (i+2), BranchType.TAKEN);
		}
		
		if (disconnectedVertices.size () != 0)
		{
			ArrayList<Vertex> noSucc = new ArrayList<Vertex> ();
			for (Vertex v: cfg)
			{
				if (v.numOfSuccessors () == 0)
				{
					noSucc.add (v);
				}
			}
			cfg.addEdge(noSucc.get (0).getVertexID (), disconnectedVertices.get (0), BranchType.TAKEN);
		}
	}
	
	private void addForLoopBody ()
	{
		int vertices = MainProgramGenerator.Globals.getNumberOfVerticesInCFG();
		
		
		int headerID = cfg.getNextVertexID();
		cfg.addBasicBlock(headerID);	
		int failCaseID = cfg.getNextVertexID();
		cfg.addBasicBlock(failCaseID);
		int loopStartID = cfg.getNextVertexID();
		cfg.addBasicBlock(loopStartID);

		cfg.addEdge (headerID, loopStartID, BranchType.TAKEN);
		cfg.addEdge (headerID, failCaseID, BranchType.TAKEN);
		
		addNonBranchComponents(5);
		cfg.addEdge(loopStartID, disconnectedVertices.get (0), BranchType.TAKEN);
		
		int tailID = cfg.getNextVertexID();
		cfg.addBasicBlock(tailID);
		cfg.addEdge(disconnectedVertices.get (disconnectedVertices.size () - 1), tailID, BranchType.TAKEN);
		cfg.addEdge(tailID, headerID, BranchType.TAKEN);
		
		
	}
	
	
	private void setEntry ()
	{
		ArrayList<Vertex> noPreds = new ArrayList<Vertex> ();
		for (Vertex v: cfg)
		{
			if (v.numOfPredecessors () == 0)
			{
				Debug.debugMessage (getClass (), v.getVertexID ()
						+ " is currently an entry vertex", 4);
				noPreds.add (v);
			}
		}

		if (noPreds.size () > 1)
		{
			Debug
					.debugMessage (
							getClass (),
							"Unable to find entry vertex : too many vertices with no predecessors.",
							1);
			System.exit (1);
		}
		else if (noPreds.size () == 0)
		{
			Debug
					.debugMessage (
							getClass (),
							"Unable to find entry vertex : no vertex without predecessors found. ",
							1);
			System.exit (1);
		}
		else
		{
			int entryID = noPreds.get (noPreds.size () - 1).getVertexID ();
			cfg.setEntryID (entryID);
			Debug.debugMessage (getClass (), "Setting entry id to " + entryID,
					4);
		}
	}

	private void setExit ()
	{
		ArrayList<Vertex> noSuccs = new ArrayList<Vertex> ();
		for (Vertex v: cfg)
		{
			if (v.numOfSuccessors () == 0)
			{
				Debug.debugMessage (getClass (), v.getVertexID ()
						+ " is currently an exit vertex", 4);
				noSuccs.add (v);
			}
		}

		if (noSuccs.size () > 1)
		{
			Debug
					.debugMessage (
							getClass (),
							"Unable to find exit vertex : too many vertices with no predecessors.",
							1);
			System.exit (1);
		}
		else if (noSuccs.size () == 0)
		{
			Debug
					.debugMessage (
							getClass (),
							"Unable to find exit vertex : no vertex without successors found. ",
							1);
			System.exit (1);
		}
		else
		{
			int exitID = noSuccs.get (noSuccs.size () - 1).getVertexID ();
			cfg.setExitID (exitID);
			Debug.debugMessage (getClass (), "Setting exit id to " + exitID, 4);
		}
	}

	private void enforceSingleEntry ()
	{
		Debug.debugMessage (getClass (), "Enforcing single entry in CFG", 3);

		ArrayList<Vertex> noPreds = new ArrayList<Vertex> ();
		for (Vertex v: cfg)
		{
			if (v.numOfPredecessors () == 0)
			{
				Debug.debugMessage (getClass (), v.getVertexID ()
						+ " is currently an entry vertex", 4);
				noPreds.add (v);
			}
		}

		while (noPreds.size () > 1)
		{
			int newVertexID = cfg.getNextVertexID ();
			cfg.addBasicBlock (newVertexID);
			Debug.debugMessage (getClass (), "Adding basic block "
					+ newVertexID, 4);

			for (int i = 1; i <= MainProgramGenerator.Globals.getFanOut (); ++i)
			{
				if (!noPreds.isEmpty ())
				{
					Vertex v = noPreds.remove (noPreds.size () - 1);
					Debug.debugMessage (getClass (), "...and connecting it to "
							+ v.getVertexID (), 4);

					if (i == 1)
					{
						cfg.addEdge (newVertexID, v.getVertexID (),
								BranchType.TAKEN);
					}
					else
					{
						cfg.addEdge (newVertexID, v.getVertexID (),
								BranchType.NOTTAKEN);
					}
				}
			}

			noPreds.add (cfg.getVertex (newVertexID));
		}
	}

	private void checkExits ()
	{
		Debug.debugMessage (getClass (), "Enforcing single exit out of CFG", 3);

		ArrayList<Vertex> noSuccs = new ArrayList<Vertex> ();
		for (Vertex v: cfg)
		{
			if (v.numOfSuccessors () == 0)
			{
				Debug.debugMessage (getClass (), v.getVertexID ()
						+ " is currently an exit vertex", 4);
				noSuccs.add (v);
			}
		}

		if (noSuccs.size () > MainProgramGenerator.Globals
				.getNumberOfReturns ())
		{
			int newVertexID = cfg.getNextVertexID ();
			cfg.addBasicBlock (newVertexID);
			Debug.debugMessage (getClass (), "Adding basic block "
					+ newVertexID, 4);

			while (!noSuccs.isEmpty ())
			{
				Vertex v = noSuccs.remove (noSuccs.size () - 1);
				Debug.debugMessage (getClass (), "...and connecting it from "
						+ v.getVertexID (), 4);
				cfg.addEdge (v.getVertexID (), newVertexID, BranchType.TAKEN);
			}
		}
	}

	private void addSelfLoops ()
	{
		int counter = MainProgramGenerator.Globals.getNumberOfSelfLoops ();
		while (counter > 0)
		{
			int vertexID;
			do
			{
				vertexID = random.nextInt (cfg.numOfVertices ()) + 1;
			}
			while (vertexID == cfg.getEntryID ());

			Debug.debugMessage (getClass (), "Adding self-loop " + vertexID
					+ " => " + vertexID, 4);
			cfg.addEdge (vertexID, vertexID, BranchType.TAKEN);
			counter--;
		}
	}

	private void addLoops ()
	{
		
	}
}
