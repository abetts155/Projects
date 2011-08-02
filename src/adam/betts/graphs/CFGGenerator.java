package adam.betts.graphs;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;

import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;
import adam.betts.edges.Edge;
import adam.betts.graphs.trees.*;

public class CFGGenerator
{
	protected Random random = new Random ();
	protected ControlFlowGraph cfg = new ControlFlowGraph ();
	protected ArrayList<Integer> disconnectedBranches = new ArrayList<Integer> ();
	protected ArrayList<Integer> disconnectedVertices = new ArrayList<Integer> ();
	protected Tree disconnectedLoops = new Tree ();
	protected ArrayList<Integer> disconnectedLoopsArray = new ArrayList<Integer> ();
	protected ArrayList<Integer> loopBody = new ArrayList<Integer> ();
	protected int remainingVertices;
	
	public CFGGenerator ()
	{
		//addNonBranchComponents(MainProgramGenerator.Globals.getNumberOfVerticesInCFG());
		//addBranchComponents (MainProgramGenerator.Globals.getNumberOfVerticesInCFG(), 4);
		addLoops ();
		//addSelfLoops ();
		
		if (disconnectedBranches.size () >= 1)
		{
			enforceSingleEntry ();
		}
		
		checkExits ();
		setEntry ();
		setExit ();
	
		Debug.debugMessage(getClass(), "Adding edge " + cfg.getExitID () + "=>" + cfg.getExitID (), 4);
		cfg.addEdge (cfg.getExitID(), cfg.getEntryID(), BranchType.TAKEN);
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
			Debug.debugMessage(getClass(), "Adding basic block " + vertexID , 4);
			cfg.addBasicBlock (vertexID);
			disconnectedVertices.add (vertexID);
		}
		
		for (int i = 0; i < disconnectedVertices.size () - 1; ++i)
		{	
			Debug.debugMessage (getClass (), "Adding edge " + disconnectedVertices.get (i)
					+ "=>" + disconnectedVertices.get (i+1), 4);
			cfg.addEdge (disconnectedVertices.get (i), disconnectedVertices.get (i+1), BranchType.TAKEN);
		}
	}
	
	private void addBranchComponents (int vertices, int type)
	{
		disconnectedBranches = new ArrayList<Integer> ();
		int noOfComponents = vertices / type;
		int remainingComponents = vertices % type;
		
		if (noOfComponents > 0)
		{
			if (type == 3)
				buildIndividualIfThenBlocks (noOfComponents);
			else if (type == 4)
				buildIndividualIfElseBlocks (noOfComponents);
			
			addNonBranchComponents (remainingComponents);
			
			if (noOfComponents > 1)
			{	
				connectBranches (type);
			}	
		}	
		else
		{
			addNonBranchComponents (remainingComponents);
			disconnectedBranches.add (disconnectedVertices.get (0));
			disconnectedBranches.add (disconnectedVertices.get (disconnectedVertices.size () - 1));	
		}		
	}
	
	private void buildIndividualIfThenBlocks (int noOfComponents)
	{
	
		for (int i = 0; i < noOfComponents; ++i) 
		{
			int branchID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + branchID, 4);
			cfg.addBasicBlock (branchID);
			int ifBranchID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + ifBranchID, 4);
			cfg.addBasicBlock (ifBranchID);
			int mergeID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + mergeID, 4);
			cfg.addBasicBlock (mergeID);
				
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
	}
	
	
	private void buildIndividualIfElseBlocks (int noOfComponents)
	{
		for (int i = 1; i <= noOfComponents; ++i) 
		{
			int branchID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + branchID, 4);
			cfg.addBasicBlock(branchID);
			int ifBranchID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + ifBranchID, 4);
			cfg.addBasicBlock(ifBranchID);
			int elseBranchID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + elseBranchID, 4);
			cfg.addBasicBlock(elseBranchID);
			int mergeID = cfg.getNextVertexID ();
			Debug.debugMessage(getClass(), "Adding basic block " + mergeID, 4);
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
	}
	
	private void connectBranches (int type) 
	{
		int destinationToRemove = 0;
		
		if (type == 4)
		{
			destinationToRemove = random.nextInt (2);
		}
		
		for (int i = 0; i < disconnectedBranches.size () - 3; i = i+2)
		{
			if (random.nextBoolean ()) 
			{
				if (destinationToRemove >= 0)
				{
					int sourceID = disconnectedBranches.get (i);
					int destinationRemovedID = cfg.getVertex (sourceID).getNthSuccessor (destinationToRemove).getVertexID ();
					
					Debug.debugMessage (getClass (), "Removing edge " + sourceID 
							+ "=>" + destinationRemovedID, 4);
					cfg.removeEdge (sourceID, destinationRemovedID); 
					
					int newDestinationID = disconnectedBranches.get (i+2);

					Debug.debugMessage (getClass (), "Adding edge " + sourceID
							+ "=>" + newDestinationID, 4);
					cfg.addEdge (sourceID, newDestinationID, BranchType.TAKEN);
					
					sourceID = disconnectedBranches.get (i+3);
					
					Debug.debugMessage (getClass (), "Adding edge " + sourceID
							+ "=>" + destinationRemovedID, 4);
					cfg.addEdge (sourceID, destinationRemovedID, BranchType.TAKEN);
				}	
			}
			else	
			{	
				Debug.debugMessage (getClass (), "Adding edge " + disconnectedBranches.get (i+1)
						+ "=>" + disconnectedBranches.get (i+2), 4);
				cfg.addEdge (disconnectedBranches.get (i+1), disconnectedBranches.get (i+2), BranchType.TAKEN);
			}	
		}
	}
	
	private int setLoopEntryExit (int n, int position)
	{
		
		System.out.println ("!!!!!" + n);
		int entryID = 0, exitID = 0;
		float decide = random.nextFloat ();
		 
		if (n == 1)
		{
			int id = cfg.getNextVertexID ();
			cfg.addBasicBlock (id);
			return id;
		}
		else if (decide < 0.33 || n < 4)
		{	
			addNonBranchComponents (n);
			entryID = disconnectedVertices.get (0);
			exitID = disconnectedVertices.get (disconnectedVertices.size () - 1);
		}	
		else 
		{	
			if (decide < 0.66)
				addBranchComponents (n, 4);
			else
				addBranchComponents (n, 3);
			entryID = disconnectedBranches.get (0);
			exitID = disconnectedBranches.get (disconnectedBranches.size () - 1);
		}	
		
		if (position == 0)
			return exitID;
		else
			return entryID;
	}
	
	private void addNoLoops (int vertices)
	{
		float decideBody = random.nextFloat ();
		
		if (decideBody < 0.33)
		{	
			addBranchComponents (vertices, 3);
			disconnectedLoopsArray.add (disconnectedBranches.get (0));
		}	
		else if (decideBody < 0.66)
		{	
			addBranchComponents (vertices, 4);
			disconnectedLoopsArray.add (disconnectedBranches.get (0));
		}	
		else
		{	
			addNonBranchComponents (vertices);
			disconnectedLoopsArray.add (disconnectedVertices.get (0));
		}
	}
	
	
	private void addLoops ()
	{	
		int loops = MainProgramGenerator.Globals.getNumberOfLoops ();	
		int vertices = MainProgramGenerator.Globals.getNumberOfVerticesInCFG();
		
		if (loops == 0)
		{
			addNoLoops (vertices);
		}
		
		else
		{
			int entryID, exitID = 0;
			remainingVertices = vertices - 2 * loops - 2;
			
			int loopEntry = random.nextInt (remainingVertices/2 + 1) + 1;
			entryID = setLoopEntryExit (loopEntry, 0);
			
			if (remainingVertices == 1)
			{
				exitID = setLoopEntryExit (1, 1);
			}
			
			buildIndividualLoops (loops);
			setLoopTree (loops);
			connectLoops ();
			
			if (remainingVertices > 0)
			{	
				exitID = setLoopEntryExit (remainingVertices, 1);
			}	
			
			Debug.debugMessage (getClass (), "Adding edge " + entryID + "=>" + disconnectedLoopsArray.get (0), 4);
			cfg.addEdge (entryID, disconnectedLoopsArray.get (0), BranchType.TAKEN);
			Debug.debugMessage (getClass (), "Adding edge " + disconnectedLoopsArray.get (0) + "=>" + exitID, 4);
			cfg.addEdge (disconnectedLoopsArray.get (0), exitID, BranchType.TAKEN);
			//checkLoopHeaders ();
			
			for (Integer h : disconnectedLoopsArray)
			{
				System.out.println ("!! " + h + " has " + cfg.getVertex(h).numOfSuccessors() + " succs.");
			}
		}	
	}

	private void buildIndividualLoops (int loops)
	{
		for (int i = 0; i < loops; ++i)	
		{
			int headerID = cfg.getNextVertexID();	
			Debug.debugMessage(getClass(), "Adding basic block " + headerID, 4);
			cfg.addBasicBlock(headerID);
			int tailID = cfg.getNextVertexID();
			Debug.debugMessage(getClass(), "Adding basic block " + tailID, 4);
			cfg.addBasicBlock (tailID);
			
			if (remainingVertices > 2)
			{		
				int loopBody = random.nextInt (remainingVertices/2 + 1) + 1; 
				remainingVertices -= loopBody;
				int type = decideLoopBody (loopBody);
				boolean check = (type == 3 && loopBody < 3) || (type == 4 && loopBody < 4) || random.nextBoolean ();
				
				if (check)
				{	
					addNonBranchComponents (loopBody);
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
				Debug.debugMessage(getClass(), "Adding edge " + headerID + "=>" + tailID, 4);
				cfg.addEdge (headerID, tailID, BranchType.TAKEN);
			}
			Debug.debugMessage(getClass(), "Adding edge " + tailID + "=>" + headerID, 4);
			cfg.addEdge (tailID, headerID, BranchType.TAKEN);
			disconnectedLoopsArray.add (headerID);
			disconnectedLoops.addVertex (headerID);
		}	
	}
	
	private int decideLoopBody (int loopBody)
	{
		float decideLoopBody = random.nextFloat ();
		int type;
	
		if (decideLoopBody < 0.5)
		{
			addBranchComponents (loopBody, 4);
			type = 4;
		}
		else
		{
			addBranchComponents (loopBody, 3);
			type = 3;
		}
		return type;
	}
	
	private void setLoopTree (int loops)
	{
		for (int i = 1; i < loops; ++i)	
		{
			int source = disconnectedLoopsArray.get (random.nextInt (i));
			int destination = disconnectedLoopsArray.get (i);
			disconnectedLoops.addEdge (source, destination);
		}
		
		disconnectedLoops.setRootID (disconnectedLoopsArray.get (0));
		disconnectedLoops.setHeight ();
	}	
	
	private void connectLoops ()
	{
		for (int i = disconnectedLoops.getHeight() - 1; i > 0; --i)
		{
			Iterator<TreeVertex> it = disconnectedLoops.levelIterator (i);
			
			int childID = it.next().getVertexID (); 
			int parentID = disconnectedLoops.getVertex (childID).getParentID ();
						
			Debug.debugMessage (getClass (), "Adding edge " + parentID
					+ "=>" + childID, 4);
			cfg.addEdge (parentID, childID, BranchType.TAKEN); 
			Debug.debugMessage (getClass (), "Adding edge " + childID + "=>" + cfg.getVertex (parentID).getNthSuccessor (0).getVertexID (), 4);
			cfg.addEdge (childID, cfg.getVertex (parentID).getNthSuccessor (0).getVertexID (), BranchType.TAKEN);
			Debug.debugMessage(getClass (), "Removing edge " + parentID + "=>" + 
					cfg.getVertex (parentID).getNthSuccessor (0).getVertexID (), 4);
			cfg.removeEdge (parentID, cfg.getVertex (parentID).getNthSuccessor (0).getVertexID ());
			
			while (it.hasNext())
			{
				int nextChildID = it.next ().getVertexID ();
				int nextParentID = disconnectedLoops.getVertex (nextChildID).getParentID ();
				
				if (cfg.getVertex (parentID).numOfSuccessors() <= 1)
				{	
					if (parentID == nextParentID)
					{	
						Debug.debugMessage (getClass (), "Adding edge " + parentID
								+ "=>" + nextChildID, 4);
						cfg.addEdge (parentID, nextChildID, BranchType.TAKEN);
						Debug.debugMessage (getClass (), "Adding edge " + nextChildID + "=>" + childID, 4);
						cfg.addEdge (nextChildID, childID, BranchType.TAKEN);
						Debug.debugMessage(getClass (), "Removing edge " + parentID + "=>" + childID, 4);
						cfg.removeEdge (parentID, childID);
					}	
					else
					{
						Debug.debugMessage (getClass (), "Adding edge " + nextParentID
								+ "=>" + nextChildID, 4);
						cfg.addEdge (nextParentID, nextChildID, BranchType.TAKEN); 
						Debug.debugMessage(getClass (), "Adding edge " + nextChildID + "=>" + 
								cfg.getVertex (nextParentID).getNthSuccessor (0).getVertexID (), 4);
						cfg.addEdge (nextChildID, cfg.getVertex (nextParentID).getNthSuccessor (0).getVertexID (), BranchType.TAKEN);
						Debug.debugMessage(getClass(), "Removing edge " + nextParentID + "=>" + 
								cfg.getVertex (nextParentID).getNthSuccessor (0).getVertexID (), 4);
						cfg.removeEdge (nextParentID, cfg.getVertex (nextParentID).getNthSuccessor (0).getVertexID ());
						parentID = nextParentID;
					}	
				}	
				childID = nextChildID;
			}
		}
	}
	
	private void checkLoopHeaders ()
	{
		for (Integer header : disconnectedLoopsArray)
		{
			if (cfg.getVertex (header).numOfSuccessors () < 2)
			{
				int failCaseID = cfg.getNextVertexID ();
				Debug.debugMessage (getClass (), "Adding basic block " + failCaseID, 4);
				cfg.addBasicBlock (failCaseID);
				Debug.debugMessage (getClass (), "Adding edge " + header + "=>" + failCaseID, 4);
				cfg.addEdge (header, failCaseID, BranchType.TAKEN);
				remainingVertices--;
			}
			else if (cfg.getVertex (header).numOfSuccessors () > 2)
			{
				System.out.println ("!!!! more than 2 exits");
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
