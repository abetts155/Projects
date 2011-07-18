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
		//addNonBranchComponents(MainProgramGenerator.Globals.getNumberOfVerticesInCFG());
		//addIfThenComponents (MainProgramGenerator.Globals.getNumberOfVerticesInCFG());
		//addIfElseComponents (MainProgramGenerator.Globals.getNumberOfVerticesInCFG());
		addLoops ();
		//addSelfLoops ();
		
		if (disconnectedBranches.size () > 1)
		{
			enforceSingleEntry ();
		}
		
		checkExits ();
		setEntry ();
		setExit ();
		//LoopNests loop = new LoopNests (cfg, cfg.getEntryID());	
	}

	public final ControlFlowGraph getCFG ()
	{
		return cfg;
	}

	private void addNonBranchComponents (int n) 
	{
		disconnectedVertices = new ArrayList<Integer> ();	
		for (int i = 0; i < n; ++i)						
		{
			int vertexID = cfg.getNextVertexID ();
			cfg.addBasicBlock (vertexID);
			disconnectedVertices.add (vertexID);
		}
		
		for (int i = 0; i < disconnectedVertices.size () - 1; ++i)
		{	
			cfg.addEdge (disconnectedVertices.get (i), disconnectedVertices.get (i+1), BranchType.TAKEN);
		}
	}
	
	
	private void addIfThenComponents (int vertices) 
	{
		disconnectedBranches = new ArrayList <Integer> ();
		int noOfComponents = vertices / 3;
		int remainingComponents = vertices % 3;
		if (noOfComponents > 0)
		{	
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
			if (noOfComponents != 1)
			{	
				connectBranches(3);
			}
		}
		else
		{
			addNonBranchComponents(remainingComponents);
		}
	}
	
	private void addIfElseComponents (int vertices) 
	{
		disconnectedBranches = new ArrayList<Integer> ();
		int noOfComponents = vertices / 4;
		int remainingComponents = vertices % 4;
		
		if (noOfComponents > 0)
		{
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
			
			addNonBranchComponents (remainingComponents);
			if (noOfComponents == 1)
			{	
				connectBranches (4);
			}	
		}	
		else
		{
			addNonBranchComponents(remainingComponents);
			disconnectedBranches.add (disconnectedVertices.get (0));
			disconnectedBranches.add (disconnectedVertices.get (disconnectedVertices.size () - 1));
		}
				
	}	
	
	private void connectBranches (int structure) 
	{
		int destinationToRemove = 1;
		if (structure == 4)
		{
			destinationToRemove = random.nextInt (2) + 1;
		}
		for (int i = 0; i < disconnectedBranches.size () - 3; i = i+2)
		{
			if (random.nextBoolean()) 
			{
				if (destinationToRemove > 0)
				{
					int sourceID = disconnectedBranches.get (i);
					int destinationRemovedID = cfg.getVertex (sourceID).getNthSuccessor (destinationToRemove - 1).getVertexID ();
					System.out.println (i + " destRemoved: " + destinationRemovedID);					
					
					Debug.debugMessage (getClass (), "Removing edge " + sourceID 
							+ "=>" + destinationRemovedID, 4);
					cfg.removeEdge (sourceID, destinationRemovedID); 
					
					int newDestinationID = disconnectedBranches.get (i+2);

					Debug.debugMessage (getClass (), "Adding edge " + sourceID
							+ "=>" + destinationRemovedID, 4);
					cfg.addEdge (sourceID, newDestinationID, BranchType.TAKEN);
					
					sourceID = disconnectedBranches.get (i+3);
					
					Debug.debugMessage (getClass (), "Adding edge " + sourceID
							+ "=>" + destinationRemovedID, 4);
					cfg.addEdge (sourceID, destinationRemovedID, BranchType.TAKEN);
				}	
			}
			else	
				cfg.addEdge (disconnectedBranches.get (i+1), disconnectedBranches.get (i+2), BranchType.TAKEN);
		}
	}
	
	private int setLoopEntry (int n)
	{
		int exitID;
		int entryID = cfg.getNextVertexID();
		float decide = random.nextFloat ();
		
		if (decide < 0.33)
		{	
			addNonBranchComponents (n);
			cfg.addEdge(entryID, disconnectedVertices.get (0), BranchType.TAKEN);
			exitID = disconnectedVertices.get (disconnectedVertices.size () - 1);
		}	
		else 
		{	
			if (decide < 0.66)
				addIfElseComponents(n);
			else
				addIfThenComponents(n);
			cfg.addEdge(entryID, disconnectedBranches.get(0), BranchType.TAKEN);
			exitID = disconnectedBranches.get(disconnectedBranches.size() - 1);
		}	
		return exitID;
	}
	
	private void addLoops ()
	{	
		int loops = MainProgramGenerator.Globals.getNumberOfLoops ();	
		int vertix = MainProgramGenerator.Globals.getNumberOfVerticesInCFG();
		int remainingVertices = vertix - 2 * loops - 2;		
		int loopEntry = random.nextInt (remainingVertices / 2 + 1) + 1;
		
		int entryID = setLoopEntry (loopEntry);
		cfg.addBasicBlock (entryID);
		
		
		for (int i = 0; i < loops; ++i)	
		{
			int headerID = cfg.getNextVertexID();	
			cfg.addBasicBlock(headerID);
			int tailID = cfg.getNextVertexID();
			cfg.addBasicBlock (tailID);
			
			int loopBody = random.nextInt (remainingVertices/2 + 1) + 1; 
			remainingVertices -= loopBody;
			
			if (random.nextBoolean ())	
			{
				float decideLoopBody = random.nextFloat ();
				boolean componentType;
				
				if (decideLoopBody < 0.5)
				{
					addIfElseComponents (loopBody);
					componentType = true;
				}
				else
				{
					addIfThenComponents (loopBody);
					componentType = false;
				}
				
				if ((!componentType && loopBody < 3) || componentType && loopBody < 4)
				{	
					Debug.debugMessage (getClass (), "Adding edge " + headerID
							+ "=>" + disconnectedVertices.get(0), 4);
					cfg.addEdge (headerID, disconnectedVertices.get(0), BranchType.TAKEN); 
					Debug.debugMessage (getClass (), "Adding edge " + disconnectedVertices.get (disconnectedVertices.size () - 1)
							+ "=>" + tailID, 4);
					cfg.addEdge (disconnectedVertices.get (disconnectedVertices.size () - 1), tailID, BranchType.TAKEN);	
				}
				else
				{
					Debug.debugMessage (getClass (), "Adding edge " + headerID
							+ "=>" + disconnectedBranches.get(0), 4);
					cfg.addEdge (headerID, disconnectedBranches.get(0), BranchType.TAKEN); 
					Debug.debugMessage (getClass (), "Adding edge " + disconnectedBranches.get (disconnectedBranches.size () - 1)
							+ "=>" + tailID, 4);
					cfg.addEdge (disconnectedBranches.get (disconnectedBranches.size () - 1), tailID, BranchType.TAKEN);
				}
			}	
			else 
			{	
				addNonBranchComponents (loopBody);
				Debug.debugMessage (getClass (), "Adding edge " + headerID
						+ "=>" + disconnectedVertices.get(0), 4);
				cfg.addEdge (headerID, disconnectedVertices.get(0), BranchType.TAKEN); 
				Debug.debugMessage (getClass (), "Adding edge " + disconnectedVertices.get (disconnectedVertices.size () - 1)
						+ "=>" + tailID, 4);
				cfg.addEdge (disconnectedVertices.get (disconnectedVertices.size () - 1), tailID, BranchType.TAKEN);		
			
			}	
				
			Debug.debugMessage (getClass (), "Adding edge " + tailID + "=>" + headerID, 4);
			cfg.addEdge(tailID, headerID, BranchType.TAKEN);
			
			disconnectedLoops.addVertex(headerID);
			disconnectedLoopsArray.add (headerID);
		}		
		
		//cfg.addEdge (entryID, disconnectedLoopsArray.get (0), BranchType.TAKEN);
		
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
	}
	
	private void addSelfLoops ()
	{
		int counter = MainProgramGenerator.Globals.getNumberOfSelfLoops ();
		while (counter > 0)
		{
			int vertexID;
			do
			{
				vertexID = random.nextInt (cfg.numOfVertices () + 1);
			}
			while (vertexID == cfg.getEntryID ());

			Debug.debugMessage (getClass (), "Adding self-loop " + vertexID
					+ " => " + vertexID, 4);
			cfg.addEdge (vertexID, vertexID, BranchType.TAKEN);
			counter--;
		}
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
}
