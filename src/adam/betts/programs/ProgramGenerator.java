package adam.betts.programs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

import adam.betts.graphs.CFGGenerator;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.outputs.UDrawGraph;
import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.vertices.Vertex;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.graphs.utils.StronglyConnectedComponents;


public class ProgramGenerator
{
	protected ArrayList<Subprogram> subprograms = new ArrayList<Subprogram> ();
	protected Program program = new Program ();
	protected ArrayList<Subprogram> levelSet = new ArrayList<Subprogram> ();
	protected CallGraph callgraph = new CallGraph ();
	protected ControlFlowGraph root;
	protected Random gen = new Random();
	protected final int numOfSubprograms = MainProgramGenerator.Globals.getNumberOfSubprograms ();
	protected final int depth = MainProgramGenerator.Globals.getDepthOfCallGraph ();
	protected HashMap<Integer, ArrayList<Integer>> call = new HashMap<Integer, ArrayList<Integer>> ();
	protected int callSiteID;
	
	
	public ProgramGenerator ()
	{
		addSubprograms ();
		addCalls ();
		StronglyConnectedComponents scc = new StronglyConnectedComponents (callgraph);
		
		if (scc.numberOfTrivialSccs() != callgraph.numOfVertices())
		{
			Debug.debugMessage (getClass(), "SCC detected in call graph", 1);
			System.exit (1);
		}
		
		if (Globals.uDrawDirectorySet ())
		{
			UDrawGraph.makeUDrawFile (program.getCallGraph());
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

			Debug.debugMessage (getClass (), "Adding subprogram "
					+ subprogramName, 4);

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
		Debug.debugMessage (getClass (), "Adding calls", 3);
		program.callg = callgraph;
				
		addVerticesInCallGraph ();		
		setRoot ();
		addEdges ();
	}
	
	private void addVerticesInCallGraph ()
	{
		for (Subprogram s : program)	
		{
			callgraph.addVertex (s.getSubprogramID (), s.getSubprogramName ());
		}
		System.out.println ("Vertices added in the call graph");
	}
	
	private void setRoot () 
	{
		root = program.getSubprogram (1).getCFG();					
		root.setSubprogramName (program.getSubprogram (1).getSubprogramName());		
		int rootID = gen.nextInt (MainProgramGenerator.Globals.getNumberOfSubprograms () + 1);
		program.setRootID (rootID);
		
		System.out.println ("Root added - rootID is: " + rootID);
	}
	
	
	private boolean checkCallSite (Subprogram s)
	{
		for (Vertex v : s.getCFG())
		{
			int vertexID = v.getVertexID ();
			callSiteID = vertexID;
			
			System.out.println ("callSiteID: " + callSiteID);
			
			if (!call.get (s.getSubprogramID ()).contains (callSiteID) &&
					s.getCFG().getVertex (callSiteID).numOfSuccessors () == 1)
			{
				
				return true;
			}
		}
		return false;
	}
	
	private void addEdges () 
	{
		int currentLevel = 1;
		int subprogramsLeft = numOfSubprograms - 1;
		Subprogram currentSubprogram = program.getSubprogram (1);
	
		for (int i = 1; i < numOfSubprograms; ++i)
		{
			if (currentLevel < depth && subprogramsLeft > 0)
			{	
				if (subprogramsLeft > 0)
				{
					Subprogram s = program.getSubprogram (i);						
					s.getCFG().setSubprogramName (s.getSubprogramName());
					ArrayList<Integer> callingVertexList = new ArrayList<Integer> ();
					call.put (s.getSubprogramID(), callingVertexList);
					
					int successorsID = gen.nextInt (numOfSubprograms - i + 1) + i;
					
					while (successorsID > i)
					{			
						Subprogram sNext = program.getSubprogram (successorsID);	
						sNext.getCFG ().setSubprogramName (sNext.getSubprogramName ());
						successorsID--;
						
						if (checkCallSite (s))
						{
							System.out.println ("condiiton true and callSiteID = " + callSiteID);
							call.get (s.getSubprogramID ()).add (callSiteID);
							Debug.debugMessage (getClass (), "Adding call " + Integer.toString(callSiteID), 4);	
							callgraph.addCall (s.getSubprogramName (), sNext.getSubprogramName (), callSiteID);
						}		
						
						if (!subprograms.contains (sNext))
						{
							subprogramsLeft--;
							subprograms.add (sNext);
							currentSubprogram = sNext;
						}	
					}
					currentLevel++;
				}
			}	
			else
			{	
				break;
			}	
		}
		
		if (subprogramsLeft > 0)
		{
			connectVerticesLeft ();
		}
		
		checkLevel (currentLevel, currentSubprogram);
	}	
	
	private void connectVerticesLeft ()
	{
		ArrayList<Integer> callingVertexInRoot = new ArrayList<Integer> ();
		int rootID = callgraph.getVertex(root.getSubprogramName()).getVertexID();
		call.put (rootID, callingVertexInRoot);
		int callSiteID = 0;
		
		for (int i = 2; i <= numOfSubprograms; ++i)
		{
			Subprogram s = program.getSubprogram (i);
			
			if (!subprograms.contains (s))
			{
				subprograms.add (s);
				int subprogramID = gen.nextInt(numOfSubprograms - i + 1) + i;
				Subprogram subprogram = program.getSubprogram (subprogramID); 
				
				ArrayList<Integer> callingVertexList = new ArrayList<Integer> ();
				call.put (subprogram.getSubprogramID(), callingVertexList);
													
				if (!callgraph.isLeaf (subprogramID))
				{
					if (checkCallSite (subprogram))
					{	
						call.get (subprogram.getSubprogramID ()).add (callSiteID);
						Debug.debugMessage (getClass (), "Adding call " + Integer.toString (callSiteID), 4);	
						callgraph.addCall (subprogram.getSubprogramName (), s.getSubprogramName (), callSiteID);
					}
				}
				else
				{	
					if (checkCallSite (program.getSubprogram (rootID)))
					{
						call.get (rootID).add (callSiteID);
						Debug.debugMessage (getClass (), "Adding call " + Integer.toString (callSiteID), 4);	
						callgraph.addCall (root.getSubprogramName (), s.getSubprogramName (), callSiteID);
					}
				}
			}
		}
	}	
		
	private void checkLevel (int currentLevel, Subprogram currentSubprogram)
	{
		if (currentLevel < depth)
		{
			int levelsToBeAdded = depth - currentLevel;			
			for (Subprogram s : subprograms)
			{	
				if (levelsToBeAdded > 0)
				{	
					if (callgraph.isLeaf (s.getSubprogramID ()))
					{						
						callgraph.removeAllPredecessorEdges (s.getSubprogramID());
						callgraph.addCall (currentSubprogram.getSubprogramName (), s.getSubprogramName (), numOfSubprograms);
						Debug.debugMessage (getClass (), "Adding call " + Integer.toString (numOfSubprograms), 4);	
						currentSubprogram = s;
						levelsToBeAdded--;
						currentLevel++;
					}
				}	
				else
				{	
					break;
				}
			}	
		}
	}
}	

