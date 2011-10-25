package adam.betts.graphs;

import java.util.ArrayList;
import java.util.Random;

import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.Vertex;

public class CFGGenerator
{
	protected Random random = new Random ();
	protected ControlFlowGraph cfg = new ControlFlowGraph ();
	protected ArrayList <SingleEntrySingleExitComponent> disconnectedComponents = new ArrayList <SingleEntrySingleExitComponent> ();
	protected int remainingVertices;
	protected int numberOfIfThenElseComponents = 0;
	protected int numberOfIfThenComponents = 0;
	protected int numberOfShortCircuitedAndComponents = 0;
	protected int numberOfCaseComponents = 0;

	public CFGGenerator ()
	{
		remainingVertices = random.nextInt (MainProgramGenerator.Globals
				.getNumberOfVerticesInCFG ()) + 1;

		Debug
				.debugMessage (getClass (),
						"Number of vertices in this CFG = " + remainingVertices, 1);

		if (random.nextBoolean ())
		{
			numberOfIfThenElseComponents = setNumberOfComponents (4);
			Debug.debugMessage (getClass (), "Number of if-then-else components = "
					+ numberOfIfThenElseComponents, 1);
		}

		if (random.nextBoolean ())
		{
			numberOfIfThenComponents = setNumberOfComponents (3);
			Debug.debugMessage (getClass (), "Number of if-then components = "
					+ numberOfIfThenComponents, 1);
		}

		if (random.nextBoolean ())
		{
			numberOfShortCircuitedAndComponents = setNumberOfComponents (5);
			Debug.debugMessage (getClass (), "Number of short circuited logical and components = "
					+ numberOfShortCircuitedAndComponents, 1);
		}

		if (MainProgramGenerator.Globals.getFanOut () > 2 && random.nextBoolean ())
		{
			numberOfCaseComponents = setNumberOfComponents (2 + MainProgramGenerator.Globals
					.getFanOut ());

			Debug.debugMessage (getClass (), "Number of case components = "
					+ numberOfCaseComponents, 1);
		}

		Debug.debugMessage (getClass (), "Number of single basic blocks = "
				+ this.remainingVertices, 1);

		while (numberOfIfThenElseComponents > 0 || numberOfIfThenComponents > 0
				|| numberOfShortCircuitedAndComponents > 0 || numberOfCaseComponents > 0)
		{
			if (numberOfIfThenElseComponents > 0 && random.nextBoolean ())
			{
				SingleEntrySingleExitComponent seseComponent = addIfThenElseComponent ();
				disconnectedComponents.add (seseComponent);

				numberOfIfThenElseComponents -= 1;
			}
			if (numberOfIfThenComponents > 0 && random.nextBoolean ())
			{
				SingleEntrySingleExitComponent seseComponent = addIfThenComponent ();
				disconnectedComponents.add (seseComponent);

				numberOfIfThenComponents -= 1;
			}
			if (numberOfShortCircuitedAndComponents > 0 && random.nextBoolean ())
			{
				SingleEntrySingleExitComponent seseComponent = addShortCircuitedAndComponent ();
				disconnectedComponents.add (seseComponent);

				numberOfShortCircuitedAndComponents -= 1;
			}
			if (numberOfCaseComponents > 0 && random.nextBoolean ())
			{
				SingleEntrySingleExitComponent seseComponent = addCaseComponent ();
				disconnectedComponents.add (seseComponent);

				numberOfCaseComponents -= 1;
			}
		}

		if (random.nextBoolean ())
		{
			addLoops ();
		}

		connectRemainingComponentsAndVertices ();
		setEntry ();
		setExit ();

		if (random.nextBoolean ())
		{
			addSelfLoops ();
		}

		cfg.addEdge (cfg.getExitID (), cfg.getEntryID (), BranchType.TAKEN);
	}

	public final ControlFlowGraph getCFG ()
	{
		return cfg;
	}

	private int setNumberOfComponents (final int sizeOfComponent)
	{
		int numOfComponents = 0;

		if (remainingVertices >= sizeOfComponent)
		{
			int rand = random.nextInt (remainingVertices) + sizeOfComponent;
			numOfComponents = rand / sizeOfComponent;
			remainingVertices = (remainingVertices - rand) + (rand % sizeOfComponent);
		}

		return numOfComponents;
	}

	private SingleEntrySingleExitComponent addIfThenElseComponent ()
	{
		Debug.debugMessage (getClass (), "Adding if-then-else component", 2);

		SingleEntrySingleExitComponent seseComponent = new SingleEntrySingleExitComponent ();

		int branchID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding branch vertex " + branchID, 4);
		cfg.addBasicBlock (branchID);

		SingleEntrySingleExitComponent thenBranch = buildSESEComponent ();
		SingleEntrySingleExitComponent elseBranch = buildSESEComponent ();

		cfg.addEdge (branchID, thenBranch.getStartID (), BranchType.TAKEN);
		cfg.addEdge (branchID, elseBranch.getStartID (), BranchType.NOTTAKEN);

		int mergeID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding merge vertex " + mergeID, 4);
		cfg.addBasicBlock (mergeID);

		cfg.addEdge (thenBranch.getExitID (), mergeID, BranchType.TAKEN);
		cfg.addEdge (elseBranch.getExitID (), mergeID, BranchType.TAKEN);

		seseComponent.setStartID (branchID);
		seseComponent.setExitID (mergeID);

		return seseComponent;
	}

	private SingleEntrySingleExitComponent addIfThenComponent ()
	{
		Debug.debugMessage (getClass (), "Adding if-then component", 2);

		SingleEntrySingleExitComponent seseComponent = new SingleEntrySingleExitComponent ();

		int branchID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding branch vertex " + branchID, 4);
		cfg.addBasicBlock (branchID);

		SingleEntrySingleExitComponent thenBranch = buildSESEComponent ();

		cfg.addEdge (branchID, thenBranch.getStartID (), BranchType.TAKEN);

		int mergeID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding merge vertex " + mergeID, 4);
		cfg.addBasicBlock (mergeID);

		cfg.addEdge (thenBranch.getExitID (), mergeID, BranchType.TAKEN);
		cfg.addEdge (branchID, mergeID, BranchType.NOTTAKEN);

		seseComponent.setStartID (branchID);
		seseComponent.setExitID (mergeID);

		return seseComponent;
	}

	private SingleEntrySingleExitComponent addShortCircuitedAndComponent ()
	{
		Debug.debugMessage (getClass (), "Adding short-circuit AND component", 2);

		SingleEntrySingleExitComponent seseComponent = new SingleEntrySingleExitComponent ();

		int outerBranchID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding outer branch vertex " + outerBranchID, 4);
		cfg.addBasicBlock (outerBranchID);

		int innerBranchID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding inner branch vertex " + innerBranchID, 4);
		cfg.addBasicBlock (innerBranchID);

		SingleEntrySingleExitComponent thenBranch = buildSESEComponent ();
		SingleEntrySingleExitComponent elseBranch = buildSESEComponent ();

		cfg.addEdge (outerBranchID, innerBranchID, BranchType.TAKEN);
		cfg.addEdge (outerBranchID, elseBranch.getStartID (), BranchType.NOTTAKEN);
		cfg.addEdge (innerBranchID, thenBranch.getStartID (), BranchType.TAKEN);
		cfg.addEdge (innerBranchID, elseBranch.getStartID (), BranchType.NOTTAKEN);

		int mergeID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding merge vertex " + mergeID, 4);
		cfg.addBasicBlock (mergeID);

		cfg.addEdge (thenBranch.getExitID (), mergeID, BranchType.TAKEN);
		cfg.addEdge (elseBranch.getExitID (), mergeID, BranchType.TAKEN);

		seseComponent.setStartID (outerBranchID);
		seseComponent.setExitID (mergeID);

		return seseComponent;
	}

	private SingleEntrySingleExitComponent addCaseComponent ()
	{
		Debug.debugMessage (getClass (), "Adding case component", 2);

		SingleEntrySingleExitComponent seseComponent = new SingleEntrySingleExitComponent ();

		int branchID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding branch vertex " + branchID, 4);
		cfg.addBasicBlock (branchID);

		int mergeID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding merge vertex " + mergeID, 4);
		cfg.addBasicBlock (mergeID);

		for (int i = 1; i <= MainProgramGenerator.Globals.getFanOut (); ++i)
		{
			SingleEntrySingleExitComponent switchBranch = buildSESEComponent ();

			cfg.addEdge (branchID, switchBranch.getStartID (), BranchType.CASE);
			cfg.addEdge (switchBranch.getExitID (), mergeID, BranchType.TAKEN);
		}

		seseComponent.setStartID (branchID);
		seseComponent.setExitID (mergeID);

		return seseComponent;
	}

	private SingleEntrySingleExitComponent buildSESEComponent ()
	{
		SingleEntrySingleExitComponent seseComponent = new SingleEntrySingleExitComponent ();

		int startID = cfg.getNextVertexID ();
		Debug.debugMessage (getClass (), "Adding vertex " + startID, 4);
		cfg.addBasicBlock (startID);

		seseComponent.setStartID (startID);
		seseComponent.setExitID (startID);

		if (remainingVertices > 0 && random.nextBoolean ())
		{
			int vertexID = cfg.getNextVertexID ();
			Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
			cfg.addBasicBlock (vertexID);

			cfg.addEdge (seseComponent.getExitID (), vertexID, BranchType.TAKEN);

			Debug.debugMessage (getClass (), "Adding edge " + seseComponent.getExitID () + " => "
					+ vertexID, 4);
			seseComponent.setExitID (vertexID);

			remainingVertices -= 1;
		} else if (disconnectedComponents.size () > 0 && random.nextBoolean ())
		{
			SingleEntrySingleExitComponent nestedSeseComponent = disconnectedComponents
					.remove (disconnectedComponents.size () - 1);

			cfg.addEdge (seseComponent.getExitID (), nestedSeseComponent.getStartID (),
					BranchType.TAKEN);

			Debug.debugMessage (getClass (), "Adding edge " + seseComponent.getExitID () + " => "
					+ nestedSeseComponent.getStartID (), 4);
			seseComponent.setExitID (nestedSeseComponent.getExitID ());
		}

		return seseComponent;
	}

	private void connectRemainingComponentsAndVertices ()
	{
		Debug.debugMessage (getClass (), "Connecting remaining components and vertices", 1);

		while (disconnectedComponents.size () > 1)
		{
			SingleEntrySingleExitComponent firstSeseComponent = disconnectedComponents.remove (0);
			SingleEntrySingleExitComponent secondSeseComponent = disconnectedComponents.remove (0);

			int sourceID = firstSeseComponent.getExitID ();

			while (remainingVertices > 0 && random.nextBoolean ())
			{
				int vertexID = cfg.getNextVertexID ();

				Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
				cfg.addBasicBlock (vertexID);
				cfg.addEdge (sourceID, vertexID, BranchType.TAKEN);

				sourceID = vertexID;

				remainingVertices -= 1;
			}

			cfg.addEdge (sourceID, secondSeseComponent.getStartID (), BranchType.TAKEN);
			firstSeseComponent.setExitID (secondSeseComponent.getExitID ());

			disconnectedComponents.add (firstSeseComponent);
		}

		if (disconnectedComponents.isEmpty () == false)
		{
			SingleEntrySingleExitComponent seseComponent = disconnectedComponents.remove (0);

			while (remainingVertices > 0)
			{
				int vertexID = cfg.getNextVertexID ();

				Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
				cfg.addBasicBlock (vertexID);

				if (random.nextBoolean ())
				{
					cfg.addEdge (vertexID, seseComponent.getStartID (), BranchType.TAKEN);
					seseComponent.setStartID (vertexID);
				} else
				{
					cfg.addEdge (seseComponent.getExitID (), vertexID, BranchType.TAKEN);
					seseComponent.setExitID (vertexID);
				}

				remainingVertices -= 1;
			}
		} else
		{
			int predID = Vertex.DUMMY_VERTEX_ID;

			while (remainingVertices > 0)
			{
				int vertexID = cfg.getNextVertexID ();

				Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
				cfg.addBasicBlock (vertexID);

				if (predID != Vertex.DUMMY_VERTEX_ID)
				{
					cfg.addEdge (predID, vertexID, BranchType.TAKEN);
				}

				predID = vertexID;

				remainingVertices -= 1;
			}
		}
	}

	private void addLoops ()
	{
		Debug.debugMessage (getClass (), "Adding loops", 1);
	}

	private void addSelfLoops ()
	{
		Debug.debugMessage (getClass (), "Adding self loops", 1);

		ArrayList <Integer> selfLoopCandidates = new ArrayList <Integer> ();

		for (Vertex v : cfg)
		{
			final int vertexID = v.getVertexID ();

			if (v.numOfSuccessors () == 1 && vertexID != cfg.getEntryID ()
					&& vertexID != cfg.getExitID ())
			{
				selfLoopCandidates.add (vertexID);
			}
		}

		if (MainProgramGenerator.Globals.getNumberOfSelfLoops () > 0
				&& selfLoopCandidates.size () > 0)
		{
			int numberOfSelfLoops = Math.min (selfLoopCandidates.size (),
					MainProgramGenerator.Globals.getNumberOfSelfLoops ());

			while (numberOfSelfLoops > 0)
			{
				int vertexIDIndex = random.nextInt (numberOfSelfLoops);
				int vertexID = selfLoopCandidates.remove (vertexIDIndex);

				cfg.addEdge (vertexID, vertexID, BranchType.TAKEN);

				numberOfSelfLoops -= 1;
			}
		}
	}

	private void setEntry ()
	{
		Debug.debugMessage (getClass (), "Setting the entry vertex", 1);

		ArrayList <Vertex> noPreds = new ArrayList <Vertex> ();
		for (Vertex v : cfg)
		{
			if (v.numOfPredecessors () == 0)
			{
				Debug.debugMessage (getClass (),
						v.getVertexID () + " is currently an entry vertex", 4);
				noPreds.add (v);
			}
		}

		if (noPreds.size () > 1)
		{
			Debug.errorMessage (getClass (),
					"Unable to find entry vertex : too many vertices with no predecessors.");
		} else if (noPreds.size () == 0)
		{
			Debug.errorMessage (getClass (),
					"Unable to find entry vertex : no vertex without predecessors found. ");
		} else
		{
			int entryID = noPreds.get (noPreds.size () - 1).getVertexID ();
			cfg.setEntryID (entryID);
			Debug.debugMessage (getClass (), "Setting entry id to " + entryID, 4);
		}
	}

	private void setExit ()
	{
		Debug.debugMessage (getClass (), "Setting the exit vertex", 1);

		ArrayList <Vertex> noSuccs = new ArrayList <Vertex> ();
		for (Vertex v : cfg)
		{
			if (v.numOfSuccessors () == 0)
			{
				Debug.debugMessage (getClass (), v.getVertexID () + " is currently an exit vertex",
						4);
				noSuccs.add (v);
			}
		}

		if (noSuccs.size () > 1)
		{
			Debug.errorMessage (getClass (),
					"Unable to find exit vertex : too many vertices with no predecessors.");
		} else if (noSuccs.size () == 0)
		{
			Debug.errorMessage (getClass (),
					"Unable to find exit vertex : no vertex without successors found. ");
		} else
		{
			int exitID = noSuccs.get (noSuccs.size () - 1).getVertexID ();
			cfg.setExitID (exitID);
			Debug.debugMessage (getClass (), "Setting exit id to " + exitID, 4);
		}
	}

	private class SingleEntrySingleExitComponent
	{
		private int startID;
		private int exitID;

		public SingleEntrySingleExitComponent ()
		{
		}

		public final void setStartID (int startID)
		{
			this.startID = startID;
		}

		public final void setExitID (int exitID)
		{
			this.exitID = exitID;
		}

		public final int getStartID ()
		{
			return startID;
		}

		public final int getExitID ()
		{
			return exitID;
		}

	}
}
