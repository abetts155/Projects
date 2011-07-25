package adam.betts.programs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Random;

import adam.betts.edges.Edge;
import adam.betts.graphs.CFGGenerator;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.outputs.OutputGraph;
import adam.betts.outputs.UDrawGraph;
import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.graphs.CallGraph;


public class ProgramGenerator
{
	protected ArrayList<Subprogram> subprograms = new ArrayList<Subprogram> ();
	protected Program program = new Program ();
	protected ArrayList<Subprogram> levelSet = new ArrayList<Subprogram> ();
	protected HashMap<Integer, ArrayList<Subprogram>> levelMap = new HashMap<Integer, ArrayList<Subprogram>> ();
	protected CallGraph callgraph = new CallGraph ();
	protected ControlFlowGraph root;
	protected Random gen = new Random();
	protected final int numOfSubprograms = MainProgramGenerator.Globals.getNumberOfSubprograms ();
	protected final int depth = MainProgramGenerator.Globals.getDepthOfCallGraph ();

	public ProgramGenerator ()
	{
		addSubprograms ();
		addCalls ();
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
		}
	}

	private void addCalls ()
	{
		Debug.debugMessage (getClass (), "Adding calls", 3);
		program.callg = callgraph;
				
		for (int i = 1; i <= numOfSubprograms; ++i)	
		{
			Subprogram s = program.getSubprogram (i);
			callgraph.addVertex (s.getSubprogramID (), s.getSubprogramName ());
		}
				
		root = program.getSubprogram (1).getCFG();					
		root.setSubprogramName (program.getSubprogram (1).getSubprogramName());		
		int id = gen.nextInt (MainProgramGenerator.Globals.getNumberOfSubprograms () + 1);
		program.setRootID (id);
		
		add ();
	}
	
	private void add () 
	{
		int currentLevel = 1;
		int subprogramsLeft = numOfSubprograms - 1;
		Subprogram currentSubprogram = program.getSubprogram(1);
	
		for (int i = 1; i < numOfSubprograms; ++i)
		{
			if (currentLevel < depth && subprogramsLeft > 0)
			{	
				if (subprogramsLeft > 0)
				{
					Subprogram s = program.getSubprogram (i);						
					s.getCFG().setSubprogramName (s.getSubprogramName());
					
					int successorID = gen.nextInt (numOfSubprograms - i + 1) + i;
					
					while (successorID > i)
					{			
						Subprogram sNext = program.getSubprogram (successorID);	
						sNext.getCFG ().setSubprogramName (sNext.getSubprogramName ());
						successorID--;
						
						callgraph.addCall (s.getSubprogramName (), sNext.getSubprogramName (), i);
						Debug.debugMessage (getClass (), "Adding call " + Integer.toString (i), 4);
												
						if (!subprograms.contains (sNext))
						{
							subprogramsLeft--;
							subprograms.add (sNext);
							currentSubprogram = sNext;
							//System.out.println ("current subprogram: " + currentSubprogram.getSubprogramName());
						}	
					}
					currentLevel++;
				}
			}	
			else
			{	
				i = numOfSubprograms + 1;
				//currentLevel--;
				//System.out.println ("currentLevel < depth failed");
			}	
			
		}	
		
		if (subprogramsLeft > 0)
			for (int i = 2; i <= numOfSubprograms; ++i)
			{
				Subprogram s = program.getSubprogram (i);
				if (!subprograms.contains (s))
				{
					subprograms.add (s);
					int subprogramID = gen.nextInt(numOfSubprograms - i + 1) + i;
					Subprogram subprogram = program.getSubprogram (subprogramID); 
					if (!callgraph.isLeaf (subprogramID))
						callgraph.addCall (subprogram.getSubprogramName (), s.getSubprogramName (), i);
					else
						callgraph.addCall (root.getSubprogramName (), s.getSubprogramName (), i);
					Debug.debugMessage (getClass (), "Adding call " + Integer.toString(i), 4);	
				}
			}
		
		//System.out.println ("current level: " + currentLevel);
		//System.out.println ("current subprogram: " + currentSubprogram.getSubprogramName());
		
		
		if (currentLevel < depth)
		{
			int levelsToBeAdded = depth - currentLevel;			
			for (Subprogram s : subprograms)
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
			//System.out.println ("levels: " + currentLevel);
		}
	}
}
