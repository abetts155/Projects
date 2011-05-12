package se.mdh.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

import se.mdh.tools.MainProgramGenerator;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Enums.BranchType;
import se.mdh.vertices.Vertex;

public class CFGGenerator
{
	protected final static float branchToMergeBias = 0.2f;
	protected final static int maxPathLength = 5;

	protected Random random = new Random ();
	protected ControlFlowGraph cfg = new ControlFlowGraph ();
	protected HashMap<Integer, Integer> branchToMerge = new HashMap<Integer, Integer> ();
	protected ArrayList<Integer> disconnectedBranches = new ArrayList<Integer> ();
	protected boolean takenBranchAdded;

	public CFGGenerator ()
	{
		addAcyclicComponents ();

		if (disconnectedBranches.size () > 1)
		{
			enforceSingleEntry ();
		}

		checkExits ();
		addSelfLoops ();
		addLoops ();
		setEntry ();
		setExit ();
	}

	public final ControlFlowGraph getCFG ()
	{
		return cfg;
	}

	private void addAcyclicComponents ()
	{
		int numberOfComponents = random.nextInt (10) + 1;
		Debug.debugMessage (getClass (), "Adding " + numberOfComponents
				+ " acyclic components", 3);

		for (int i = 1; i <= numberOfComponents; ++i)
		{
			int branchID = cfg.getNextVertexID ();
			cfg.addBasicBlock (branchID);

			int mergeID = cfg.getNextVertexID ();
			cfg.addBasicBlock (mergeID);

			branchToMerge.put (branchID, mergeID);
			takenBranchAdded = false;

			Debug.debugMessage (getClass (), "Adding branch vertex " + branchID
					+ " with corresponding merge vertex " + mergeID, 4);

			int numOfSucc = random.nextInt (MainProgramGenerator.Globals
					.getFanOut () - 2 + 1) + 2;

			Debug.debugMessage (getClass (), "Fan out for branch " + branchID
					+ " = " + numOfSucc, 4);

			for (int j = 1; j <= numOfSucc; ++j)
			{
				if (random.nextFloat () < branchToMergeBias
						&& !cfg.getBasicBlock (branchID).hasSuccessor (mergeID))
				{
					Debug.debugMessage (getClass (), "Adding edge " + branchID
							+ "=>" + mergeID, 4);

					if (j == 1)
					{
						cfg.addEdge (branchID, mergeID, BranchType.TAKEN);
						takenBranchAdded = true;
					}
					else
					{
						cfg.addEdge (branchID, mergeID, BranchType.NOTTAKEN);
					}
				}
				else
				{
					addAcyclicPath (branchID, mergeID);
				}
			}

			disconnectedBranches.add (branchID);
		}
	}

	private void addAcyclicPath (int branchID, int mergeID)
	{
		boolean linkToOtherComponent = disconnectedBranches.size () > 0
				&& random.nextBoolean ();
		int pathLength = random.nextInt (maxPathLength) + 1;
		int sourceID = branchID;
		int destinationID;

		for (int i = 1; i <= pathLength; ++i)
		{
			if (linkToOtherComponent && random.nextBoolean ())
			{
				linkToOtherComponent = false;
				destinationID = disconnectedBranches
						.remove (disconnectedBranches.size () - 1);

				Debug.debugMessage (getClass (), "Adding edge " + sourceID
						+ "=>" + destinationID, 4);

				if (i == 1 && !takenBranchAdded)
				{
					cfg.addEdge (sourceID, destinationID, BranchType.TAKEN);
					takenBranchAdded = true;
				}
				else
				{
					cfg.addEdge (sourceID, destinationID, BranchType.NOTTAKEN);
				}

				sourceID = branchToMerge.get (destinationID);
			}
			else
			{
				destinationID = cfg.getNextVertexID ();
				cfg.addBasicBlock (destinationID);

				Debug.debugMessage (getClass (), "Adding basic block "
						+ destinationID, 4);
				Debug.debugMessage (getClass (), "Adding edge " + sourceID
						+ "=>" + destinationID, 4);

				if (i == 1 && !takenBranchAdded)
				{
					cfg.addEdge (sourceID, destinationID, BranchType.TAKEN);
					takenBranchAdded = true;
				}
				else
				{
					cfg.addEdge (sourceID, destinationID, BranchType.NOTTAKEN);
				}

				sourceID = destinationID;
			}
		}

		Debug.debugMessage (getClass (), "Adding edge " + sourceID + "=>"
				+ mergeID, 4);
		cfg.addEdge (sourceID, mergeID, BranchType.TAKEN);
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
