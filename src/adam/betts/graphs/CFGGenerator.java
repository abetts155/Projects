package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import adam.betts.outputs.OutputGraph;
import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;
import adam.betts.graphs.trees.*;

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
	protected Tree disconnectedLoops = new Tree ();
	protected ArrayList<Integer> disconnectedLoopsArray = new ArrayList<Integer> ();
	protected ArrayList<Integer> loopBody = new ArrayList<Integer> ();
	protected boolean takenBranchAdded;

	public CFGGenerator ()
	{
		//addNonBranchComponents(11);
		//addIfThenComponents (noOfVertices);
		//addIfElseComponents (noOfVertices);
		addLoopBody ();
		
		if (disconnectedBranches.size () > 1)
		{
			enforceSingleEntry ();
		}
		
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
		disconnectedVertices = new ArrayList<Integer> ();	
		for (int i = 0; i < n; i = i+2)
		{
			int branchID = cfg.getNextVertexID ();
			cfg.addBasicBlock(branchID);
			int mergeID = cfg.getNextVertexID ();
			cfg.addBasicBlock(mergeID);
										
			Debug.debugMessage (getClass (), "Adding edge " + branchID
					+ "=>" , 4);
			cfg.addEdge (branchID, mergeID, BranchType.TAKEN);
			disconnectedVertices.add (branchID);
			disconnectedVertices.add (mergeID);
		}
		
		for (int i = 1; i < disconnectedVertices.size () - 1; i = i + 2)
		{	
			cfg.addEdge(disconnectedVertices.get (i), disconnectedVertices.get (i+1), BranchType.TAKEN);
		}
		
		if (n % 2 != 0) 
		{
			int oddLastID = cfg.getNextVertexID();
			cfg.addBasicBlock(oddLastID);
			cfg.addEdge (disconnectedVertices.get(disconnectedVertices.size () - 1), oddLastID, BranchType.TAKEN);
			disconnectedVertices.add (oddLastID);
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
	
	private void addLoopBody ()
	{
		int loops = MainProgramGenerator.Globals.getNumberOfLoops ();	
		
		for (int i = 0; i < loops; ++i)	
		{
			int headerID = cfg.getNextVertexID();
			cfg.addBasicBlock(headerID);
			int tailID = cfg.getNextVertexID();
			cfg.addBasicBlock (tailID);
			Debug.debugMessage (getClass (), "Removing edge " + headerID
					+ "=>" + tailID, 4);
			cfg.addEdge (headerID, tailID, BranchType.TAKEN);
			cfg.addEdge (tailID, headerID, BranchType.TAKEN);
			
			disconnectedLoops.addVertex(headerID);
			disconnectedLoopsArray.add (headerID);
		}		
		
		int entryID = cfg.getNextVertexID();
		cfg.addBasicBlock (entryID);
		cfg.addEdge (entryID, disconnectedLoopsArray.get (0), BranchType.TAKEN);
		
		int vertices = disconnectedLoopsArray.size () - 1;
		
		for (int i = 1; i <= vertices; ++i)	
		{
			int source = disconnectedLoopsArray.get (random.nextInt (i));
			int destination = disconnectedLoopsArray.get (i);
			disconnectedLoops.addEdge (source, destination);
		}
		
		disconnectedLoops.setRootID(disconnectedLoopsArray.get (0));
		disconnectedLoops.setHeight();
		
		
		for (int i = disconnectedLoops.getHeight() - 1; i > 0; --i)
		{
			Iterator<TreeVertex> it = disconnectedLoops.levelIterator(i);
			
			int childID = it.next().getVertexID(); 
			int parentID = disconnectedLoops.getVertex(childID).getParentID();		
			
			Debug.debugMessage (getClass (), "Adding edge " + parentID
					+ "=>" + childID, 4);
			cfg.addEdge (parentID, childID, BranchType.TAKEN); 
			
			while (it.hasNext())
			{
				int nextChildID = it.next().getVertexID();
				int nextParentID = disconnectedLoops.getVertex(nextChildID).getParentID();
				if (parentID == nextParentID)
				{	
					Debug.debugMessage (getClass (), "Adding edge " + childID
							+ "=>" + nextChildID, 4);
					cfg.addEdge (childID, nextChildID, BranchType.TAKEN);
				}	
				else
				{
					Debug.debugMessage (getClass (), "Adding edge " + cfg.getVertex(nextParentID).getNthSuccessor(0).getVertexID()
							+ "=>" + nextChildID, 4);
					cfg.addEdge (cfg.getVertex(nextParentID).getNthSuccessor(0).getVertexID(), nextChildID, BranchType.TAKEN);
					parentID = nextParentID;
				}	
				childID = nextChildID;
			}
			
			Debug.debugMessage (getClass (), "Adding edge " + childID
					+ "=>" + cfg.getVertex(parentID).getNthSuccessor(0).getVertexID(), 4);
			cfg.addEdge(childID, cfg.getVertex(parentID).getNthSuccessor(0).getVertexID(), BranchType.TAKEN);
			
			System.out.println (cfg.getVertex(parentID).numOfSuccessors());
			if (cfg.getVertex(parentID).numOfSuccessors() > 1)
			{
				Debug.debugMessage (getClass (), "Removing edge " + parentID
						+ "=>" + cfg.getVertex(parentID).getNthSuccessor(0).getVertexID(), 4);
				cfg.removeEdge (parentID, cfg.getVertex(parentID).getNthSuccessor(0).getVertexID());
			}
		}
		
		int exitID = cfg.getNextVertexID();
		cfg.addBasicBlock(exitID);
		cfg.addEdge (entryID, exitID, BranchType.TAKEN);
		
		//cfg.addEdge(disconnectedLoopsArray.get(vertices), exitID, BranchType.TAKEN);
		
		/*if (cfg.getVertex(disconnectedLoopsArray.get(vertices)).numOfSuccessors() > 2)
		{
			Debug.debugMessage (getClass (), "Removing edge " + disconnectedLoopsArray.get(vertices)
					+ "=>" + cfg.getVertex(disconnectedLoopsArray.get(vertices)).getNthSuccessor(0).getVertexID(), 4);
			cfg.removeEdge (disconnectedLoopsArray.get(vertices), 
					cfg.getVertex(disconnectedLoopsArray.get(vertices)).getNthSuccessor(0).getVertexID());
		}*/
		
		OutputGraph.output(disconnectedLoops);
		OutputGraph.output(cfg);
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
