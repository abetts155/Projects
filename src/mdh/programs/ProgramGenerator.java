package se.mdh.programs;

import se.mdh.graphs.CFGGenerator;
import se.mdh.graphs.ControlFlowGraph;
import se.mdh.outputs.UDrawGraph;
import se.mdh.tools.MainProgramGenerator;
import se.mdh.utilities.Debug;
import se.mdh.utilities.Globals;

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
		Debug.debugMessage (getClass (), "Adding calls", 3);
	}
}
