package adam.betts.programs;

import java.util.Random;

import adam.betts.graphs.CFGGenerator;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.outputs.UDrawGraph;
import adam.betts.tools.MainProgramGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.graphs.CallGraph;

public class ProgramGenerator
{
	protected Program program = new Program ();
	
	public ProgramGenerator ()
	{
		addSubprograms ();
		addCalls ();
	}

	public final Program getProgram ()
	{
		return program;
	}

	private void addSubprograms ()
	{
		Debug.debugMessage (getClass (), "Adding subprograms", 3);

		for (int i = 1; i <= MainProgramGenerator.Globals
				.getNumberOfSubprograms (); ++i)
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
		Random gen = new Random();
		
		Debug.debugMessage (getClass (), "Adding calls", 3);
		CallGraph callgraph = new CallGraph ();
				
		for (int i = 1; i <= MainProgramGenerator.Globals.getNumberOfSubprograms (); ++i)
		{
			Subprogram s = program.getSubprogram (i);
			callgraph.addVertex (s.getSubprogramID (), s.getSubprogramName ());
		}
				
		ControlFlowGraph root = program.getSubprogram (1).getCFG();
		root.setSubprogramName (program.getSubprogram (1).getSubprogramName());
		int id = gen.nextInt(MainProgramGenerator.Globals.getNumberOfSubprograms () + 1);
		program.setRootID(id);
		
		for (int i = 2; i<= MainProgramGenerator.Globals.getNumberOfSubprograms (); ++i)
		{
			Subprogram s = program.getSubprogram(i);
			s.getCFG().setSubprogramName (s.getSubprogramName ());
			callgraph.addCall (root.getSubprogramName (), s.getCFG().getSubprogramName (), i);
			Debug.debugMessage (getClass (), "Adding call " + Integer.toString(i), 4);
		}
	}
}
