package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;

import adam.betts.edges.Edge;
import adam.betts.graphs.trees.Tree;
import adam.betts.outputs.OutputGraph;
import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;

public class CFGGenerator
{
	protected Random random = new Random ();
	protected ControlFlowGraph cfg = new ControlFlowGraph ();
	protected ArrayList <SingleEntrySingleExitComponent> disconnectedComponents = new ArrayList <SingleEntrySingleExitComponent> ();
	protected HashMap <Integer, LoopComponent> loops = new HashMap <Integer, LoopComponent> ();
	protected int remainingVertices;
	protected int remainingVerticesInRegion;
	protected int numberOfIfThenElseComponents = 0;
	protected int numberOfIfThenComponents = 0;
	protected int numberOfShortCircuitedAndComponents = 0;
	protected int numberOfCaseComponents = 0;

	public CFGGenerator ()
	{
		remainingVertices = MainProgramGenerator.Globals.getNumberOfVerticesInCFG ();

		Debug.debugMessage (getClass (), "#Vertices in CFG = " + remainingVertices, 1);

		final int numberOfNonLoopVertices = 2;

		Tree lnt = buildLNT ();
		lnt.setHeight ();

		for (int level = lnt.getHeight () - 1; level >= 0; --level)
		{
			Iterator <TreeVertex> levelIt = lnt.levelIterator (level);

			while (levelIt.hasNext ())
			{
				TreeVertex treev = levelIt.next ();

				if (level == 0)
				{
					remainingVerticesInRegion = remainingVertices;

					Debug.debugMessage (getClass (), "#Vertices in loop " + treev.getVertexID ()
							+ " = " + remainingVerticesInRegion, 1);

					decideWhichAcyclicComponents ();
					addAcyclicComponents ();
					connectDisconnectedComponents ();
					findMergeVerticesToRemove ();

					SingleEntrySingleExitComponent seseComponent = connectRemainingVertices ();
					LoopComponent loopComponent = setLoopVertices (seseComponent);
					loops.put (treev.getVertexID (), loopComponent);
				} else
				{
					final int maximumSizeOfLoop = (MainProgramGenerator.Globals
							.getNumberOfVerticesInCFG () - numberOfNonLoopVertices)
							/ MainProgramGenerator.Globals.getNumberOfLoops ();

					remainingVerticesInRegion = 2 + (int) (Math.random () * ((maximumSizeOfLoop - 2) + 1));

					remainingVertices = remainingVertices - remainingVerticesInRegion;

					Debug.debugMessage (getClass (), "#Vertices in loop " + treev.getVertexID ()
							+ " = " + remainingVerticesInRegion, 1);

					decideWhichAcyclicComponents ();
					addAcyclicComponents ();
					connectDisconnectedComponents ();
					findMergeVerticesToRemove ();

					SingleEntrySingleExitComponent seseComponent = connectRemainingVertices ();
					LoopComponent loopComponent = setLoopVertices (seseComponent);
					loops.put (treev.getVertexID (), loopComponent);

					disconnectedComponents.clear ();
				}
			}
		}

		Debug.debugMessage (getClass (), "Connecting loops and adding edges", 1);

		for (int level = lnt.getHeight () - 1; level >= 0; --level)
		{
			Iterator <TreeVertex> levelIt = lnt.levelIterator (level);

			while (levelIt.hasNext ())
			{
				TreeVertex treev = levelIt.next ();

				Debug.debugMessage (getClass (), "Analysing loop " + treev.getVertexID () + " = "
						+ remainingVerticesInRegion, 1);

				if (level == 0)
				{
					connectNestedLoops (treev);
				} else
				{
					addLoopEdges (loops.get (treev.getVertexID ()));

					if (treev.isLeaf () == false)
					{
						connectNestedLoops (treev);
					}
				}
			}
		}

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

	private LoopComponent setLoopVertices (SingleEntrySingleExitComponent seseComponent)
	{
		Debug.debugMessage (getClass (), "Setting loop vertices", 1);

		LoopComponent loopComponent = new LoopComponent ();

		loopComponent.headerID = seseComponent.startID;
		loopComponent.tails.add (seseComponent.exitID);

		if (random.nextBoolean () && cfg.getVertex (seseComponent.startID).numOfSuccessors () == 1)
		{
			// Create a for loop where the exit is the header
			// But only if this vertex has one successor, otherwise, we'll
			// create a header
			// with > 2 sucessors
			loopComponent.exits.add (seseComponent.startID);

		} else
		{
			// Create a do-while loop where the exit is the tail
			loopComponent.exits.add (seseComponent.exitID);
		}

		Debug.debugMessage (getClass (), "Header = " + loopComponent.headerID + ", tails = "
				+ loopComponent.tails + ", exits = " + loopComponent.exits, 4);

		return loopComponent;
	}

	private void addLoopEdges (LoopComponent loopComponent)
	{
		Debug.debugMessage (getClass (), "Adding loop edges with header ID = "
				+ loopComponent.headerID, 1);

		for (int tailID : loopComponent.tails)
		{
			cfg.addEdge (tailID, loopComponent.headerID, BranchType.TAKEN);
		}
	}

	private void connectNestedLoops (TreeVertex treev)
	{
		Debug.debugMessage (getClass (), "Adding nested loops for " + treev.getVertexID (), 1);

		ArrayList <Integer> successors = new ArrayList <Integer> ();

		Iterator <Edge> succIt = treev.successorIterator ();
		while (succIt.hasNext ())
		{
			Edge e = succIt.next ();
			successors.add (e.getVertexID ());
		}

		// The inner nested loops which should be linked to from the
		// outer-enclosing loop
		LoopComponent firstComponent = null;
		LoopComponent lastComponent = null;

		if (successors.size () > 1)
		{
			// The outer loop has several nested loops to link together

			while (successors.size () > 1)
			{
				LoopComponent predLoopComponent = loops.get (successors
						.remove (successors.size () - 1));

				LoopComponent succLoopComponent = loops.get (successors
						.get (successors.size () - 1));

				if (firstComponent == null)
				{
					firstComponent = predLoopComponent;
				}

				lastComponent = succLoopComponent;

				// Sequentially compose two loops at the same nesting level
				for (int exitID : predLoopComponent.exits)
				{
					cfg.addEdge (exitID, succLoopComponent.headerID, BranchType.TAKEN);
				}
			}

			assert (successors.size () == 1);
		} else
		{
			// There is only one nested loop to link to the outer enclosing loop

			firstComponent = loops.get (successors.get (0));
			lastComponent = loops.get (successors.get (0));
		}

		LoopComponent parentComponent = loops.get (treev.getVertexID ());

		final int parentTailID = parentComponent.tails.iterator ().next ();
		final int parentHeaderID = parentComponent.headerID;

		cfg.addEdge (parentHeaderID, firstComponent.headerID, BranchType.TAKEN);

		for (int exitID : lastComponent.exits)
		{
			cfg.addEdge (exitID, parentTailID, BranchType.TAKEN);
		}
	}

	private Tree buildLNT ()
	{
		Tree tree = new Tree ();
		HashMap <Integer, Integer> vertexToLevel = new HashMap <Integer, Integer> ();

		int rootID = Vertex.DUMMY_VERTEX_ID;

		for (int i = 0; i < MainProgramGenerator.Globals.getNumberOfLoops () + 1; ++i)
		{
			int vertexID = tree.getNextVertexID ();
			tree.addVertex (vertexID);

			// Consider every vertex the root of its own tree
			vertexToLevel.put (vertexID, 0);

			// Set the last vertex added as the root of the tree
			rootID = vertexID;
			tree.setRootID (rootID);
		}

		Debug.debugMessage (getClass (), "Root = " + rootID, 1);

		// Begin adding edges from the root of the tree
		int parentID = rootID;
		for (Vertex v : tree)
		{
			int vertexID = v.getVertexID ();

			if (vertexID != rootID)
			{
				int newLevel = vertexToLevel.get (parentID) + 1;

				if (newLevel <= MainProgramGenerator.Globals.getLoopNestingLevelDepth ())
				{
					tree.addEdge (parentID, vertexID);
					vertexToLevel.put (vertexID, newLevel);
					parentID = vertexID;
				} else
				{
					// The level of the LNT exceeds the user-supplied depth
					// Therefore, backtrack to an arbitrary ancestor in the tree

					int ancestorID = parentID;
					boolean ancestorFound = false;

					while (ancestorFound == false)
					{
						TreeVertex treev = tree.getVertex (ancestorID);
						ancestorID = treev.getParentID ();

						// Only stop if the random generator decides to or we
						// reach the root of the LNT
						if (random.nextBoolean () || ancestorID == rootID)
						{
							ancestorFound = true;
						}
					}

					parentID = ancestorID;

					tree.addEdge (parentID, vertexID);
					vertexToLevel.put (vertexID, vertexToLevel.get (parentID) + 1);
					parentID = vertexID;
				}
			}
		}

		return tree;
	}

	private void decideWhichAcyclicComponents ()
	{
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
			final int sizeOfComponent = 2 + MainProgramGenerator.Globals.getFanOut ();
			numberOfCaseComponents = setNumberOfComponents (sizeOfComponent);
			Debug.debugMessage (getClass (), "Number of case components = "
					+ numberOfCaseComponents, 1);
		}

		Debug.debugMessage (getClass (), "Number of single basic blocks = "
				+ remainingVerticesInRegion, 1);
	}

	private int setNumberOfComponents (final int sizeOfComponent)
	{
		int numOfComponents = 0;

		if (remainingVerticesInRegion >= sizeOfComponent)
		{
			numOfComponents = random.nextInt ((int) Math.floor (remainingVerticesInRegion
					/ sizeOfComponent)) + 1;

			remainingVerticesInRegion = remainingVerticesInRegion
					- (numOfComponents * sizeOfComponent);

			Debug.debugMessage (getClass (), "Component size = " + sizeOfComponent
					+ ". #Components = " + numOfComponents + ". #Remaining vertices = "
					+ remainingVerticesInRegion, 4);
		}

		return numOfComponents;
	}

	private void addAcyclicComponents ()
	{
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

		int numberOfSwitchComponents = 2 + (int) (Math.random () * ((MainProgramGenerator.Globals
				.getFanOut () - 2) + 1));

		for (int i = 1; i <= numberOfSwitchComponents; ++i)
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

		if (remainingVerticesInRegion > 0 && random.nextBoolean ())
		{
			int vertexID = cfg.getNextVertexID ();
			Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
			cfg.addBasicBlock (vertexID);

			cfg.addEdge (seseComponent.getExitID (), vertexID, BranchType.TAKEN);

			seseComponent.setExitID (vertexID);

			remainingVerticesInRegion -= 1;
		} else if (disconnectedComponents.size () > 0 && random.nextBoolean ())
		{
			SingleEntrySingleExitComponent nestedSeseComponent = disconnectedComponents
					.remove (disconnectedComponents.size () - 1);

			cfg.addEdge (seseComponent.getExitID (), nestedSeseComponent.getStartID (),
					BranchType.TAKEN);

			seseComponent.setExitID (nestedSeseComponent.getExitID ());
		}

		return seseComponent;
	}

	private void connectDisconnectedComponents ()
	{
		Debug.debugMessage (getClass (), "Connecting disconnected components", 1);

		while (disconnectedComponents.size () > 1)
		{
			SingleEntrySingleExitComponent firstSeseComponent = disconnectedComponents.remove (0);
			SingleEntrySingleExitComponent secondSeseComponent = disconnectedComponents.remove (0);

			int sourceID = firstSeseComponent.getExitID ();

			while (remainingVerticesInRegion > 0 && random.nextBoolean ())
			{
				int vertexID = cfg.getNextVertexID ();

				Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
				cfg.addBasicBlock (vertexID);
				cfg.addEdge (sourceID, vertexID, BranchType.TAKEN);

				sourceID = vertexID;

				remainingVerticesInRegion -= 1;
			}

			cfg.addEdge (sourceID, secondSeseComponent.getStartID (), BranchType.TAKEN);
			firstSeseComponent.setExitID (secondSeseComponent.getExitID ());

			disconnectedComponents.add (firstSeseComponent);
		}
	}

	private void findMergeVerticesToRemove ()
	{
		Debug.debugMessage (getClass (), "Finding potential merge vertices to remove", 1);

		HashMap <Integer, HashSet <Integer>> newSuccessors = new HashMap <Integer, HashSet <Integer>> ();
		HashSet <Integer> toRemove = new HashSet <Integer> ();

		for (Vertex v : cfg)
		{
			if (v.numOfPredecessors () > 1 && v.numOfSuccessors () == 1)
			{
				if (random.nextBoolean ())
				{
					toRemove.add (v.getVertexID ());
				}
			}
		}

		remainingVerticesInRegion += toRemove.size ();

		for (int vertexID : toRemove)
		{
			Debug.debugMessage (getClass (), "Analysing merge vertex " + vertexID, 2);

			HashSet <Integer> predIDs = cfg.getVertex (vertexID).getPredecessorIDs ();
			HashSet <Integer> succIDs = cfg.getVertex (vertexID).getSuccessorIDs ();

			newSuccessors.put (vertexID, new HashSet <Integer> ());
			newSuccessors.get (vertexID).addAll (succIDs);

			for (int predID : predIDs)
			{
				newSuccessors.put (predID, new HashSet <Integer> ());
				newSuccessors.get (predID).addAll (succIDs);

				Debug.debugMessage (getClass (), "newSucc(" + predID + ")"
						+ newSuccessors.get (predID), 4);
			}
		}

		boolean changed = true;
		while (changed)
		{
			Debug.debugMessage (getClass (), "Updating new successors of unlinked vertices", 2);

			changed = false;
			for (int predID : newSuccessors.keySet ())
			{
				for (int succID : newSuccessors.get (predID))
				{
					if (toRemove.contains (succID) && newSuccessors.containsKey (succID))
					{
						newSuccessors.get (predID).remove (succID);
						newSuccessors.get (predID).addAll (newSuccessors.get (succID));

						Debug.debugMessage (getClass (), "Updated: newSucc(" + predID + ")"
								+ newSuccessors.get (predID), 4);

						changed = true;
					}
				}
			}
		}

		for (int vertexID : toRemove)
		{
			Debug.debugMessage (getClass (), "Removing merge vertex " + vertexID, 2);

			cfg.removeVertex (vertexID);
		}

		for (int predID : newSuccessors.keySet ())
		{
			if (toRemove.contains (predID) == false)
			{
				Debug.debugMessage (getClass (), "Updating new successors of vertex " + predID, 3);

				for (int succID : newSuccessors.get (predID))
				{
					if (toRemove.contains (succID) == false)
					{
						cfg.addEdge (predID, succID, BranchType.TAKEN);
					}
				}
			}
		}
	}

	private SingleEntrySingleExitComponent connectRemainingVertices ()
	{
		Debug.debugMessage (getClass (), "Connecting remaining vertices. #Vertices remaining = "
				+ remainingVerticesInRegion, 1);

		SingleEntrySingleExitComponent masterSeseComponent = new SingleEntrySingleExitComponent ();

		if (disconnectedComponents.isEmpty () == false)
		{
			Debug.debugMessage (getClass (), "Disconnected components to connect", 2);

			SingleEntrySingleExitComponent seseComponent = disconnectedComponents.remove (0);

			while (remainingVerticesInRegion > 0)
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

				remainingVerticesInRegion -= 1;
			}

			masterSeseComponent.startID = seseComponent.startID;
			masterSeseComponent.exitID = seseComponent.exitID;
		} else
		{
			Debug.debugMessage (getClass (), "Components ALL connected", 2);

			int predID = Vertex.DUMMY_VERTEX_ID;
			int vertexID = Vertex.DUMMY_VERTEX_ID;

			while (remainingVerticesInRegion > 0)
			{
				vertexID = cfg.getNextVertexID ();

				Debug.debugMessage (getClass (), "Adding vertex " + vertexID, 4);
				cfg.addBasicBlock (vertexID);

				if (predID != Vertex.DUMMY_VERTEX_ID)
				{
					cfg.addEdge (predID, vertexID, BranchType.TAKEN);
				} else
				{
					masterSeseComponent.startID = vertexID;
				}

				predID = vertexID;

				remainingVerticesInRegion -= 1;
			}

			masterSeseComponent.exitID = vertexID;
		}

		Debug.debugMessage (getClass (), "SESE entry = " + masterSeseComponent.startID
				+ ". SESE exit = " + masterSeseComponent.exitID, 3);

		return masterSeseComponent;
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

			Debug.debugMessage (getClass (), "Self-loop candidates = " + selfLoopCandidates, 4);

			while (numberOfSelfLoops > 0)
			{
				int vertexIDIndex = random.nextInt (selfLoopCandidates.size ());
				int vertexID = selfLoopCandidates.remove (vertexIDIndex);

				cfg.addEdge (vertexID, vertexID, BranchType.TAKEN);

				numberOfSelfLoops -= 1;
			}
		}
	}

	private void setEntry ()
	{
		Debug.debugMessage (getClass (), "Setting the entry vertex", 1);

		ArrayList <Integer> noPreds = new ArrayList <Integer> ();
		for (Vertex v : cfg)
		{
			if (v.numOfPredecessors () == 0)
			{
				Debug.debugMessage (getClass (),
						v.getVertexID () + " is currently an entry vertex", 4);
				noPreds.add (v.getVertexID ());
			}
		}

		if (noPreds.size () > 1)
		{
			OutputGraph.output (cfg);
			Debug.errorMessage (getClass (),
					"Unable to find entry vertex. Too many vertices with no predecessors: "
							+ noPreds);
		} else if (noPreds.size () == 0)
		{
			Debug
					.errorMessage (getClass (),
							"Unable to find entry vertex. No vertex without predecessors found: "
									+ noPreds);
		} else
		{
			int entryID = noPreds.get (noPreds.size () - 1);
			cfg.setEntryID (entryID);
			Debug.debugMessage (getClass (), "Setting entry id to " + entryID, 4);
		}
	}

	private void setExit ()
	{
		Debug.debugMessage (getClass (), "Setting the exit vertex", 1);

		ArrayList <Integer> noSuccs = new ArrayList <Integer> ();
		for (Vertex v : cfg)
		{
			if (v.numOfSuccessors () == 0)
			{
				Debug.debugMessage (getClass (), v.getVertexID () + " is currently an exit vertex",
						4);
				noSuccs.add (v.getVertexID ());
			}
		}

		if (noSuccs.size () > 1)
		{
			OutputGraph.output (cfg);
			Debug.errorMessage (getClass (),
					"Unable to find exit vertex. Too many vertices with no successors: " + noSuccs);
		} else if (noSuccs.size () == 0)
		{
			Debug.errorMessage (getClass (),
					"Unable to find exit vertex. No vertex without successors found: " + noSuccs);
		} else
		{
			int exitID = noSuccs.get (noSuccs.size () - 1);
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

	private class LoopComponent
	{
		private int headerID;
		private HashSet <Integer> tails = new HashSet <Integer> ();
		private HashSet <Integer> exits = new HashSet <Integer> ();

		public LoopComponent ()
		{
		}
	}
}
