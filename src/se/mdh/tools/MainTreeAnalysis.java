package se.mdh.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import se.mdh.calculations.CFGDatabase;
import se.mdh.calculations.CalculationEngineAST;
import se.mdh.calculations.CalculationEngineCFG;
import se.mdh.outputs.AnalysisOutput;
import se.mdh.programs.Program;
import se.mdh.utilities.Debug;
import se.mdh.utilities.DefaultOptions;

public class MainTreeAnalysis
{
	private static Options options;

	private static void addOptions ()
	{
		options = new Options ();
		DefaultOptions.addDefaultOptions (options);
		DefaultOptions.addProgramOption (options);
		DefaultOptions.addRootOption (options);
		DefaultOptions.addTraceFileOption (options);
		DefaultOptions.addUDrawDirectoryOption (options);
		DefaultOptions.addIPETOptions (options);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "tree.jar";
		CommandLineParser parser = new GnuParser ();
		HelpFormatter formatter = new HelpFormatter ();
		formatter.setWidth (128);
		CommandLine line = null;
		try
		{
			line = parser.parse (options, args);

			if (line.hasOption (DefaultOptions.helpOption.getOpt ()))
			{
				formatter.printHelp (toolName, options);
				System.exit (1);
			}
			else
			{
				DefaultOptions.setDefaultOptions (line);
				DefaultOptions.setProgramOption (line);
				DefaultOptions.setRootOption (line);
				DefaultOptions.setTraceFileOption (line);
				DefaultOptions.setUDrawDirectoryOption (line);
				DefaultOptions.setIPETOptions (line);
			}
		}
		catch (ParseException e)
		{
			System.out.println (e.getMessage ());
			formatter.printHelp (toolName, options);
			System.exit (1);
		}
	}

	private static void run ()
	{
		Debug.verboseMessage ("Reading program");
		Program program = new Program ();
		program.buildSyntaxTrees ();
		program.inline ();
		CFGDatabase database = new CFGDatabase (program);
		CalculationEngineCFG cfgCalculations = new CalculationEngineCFG (
				program, database);
		CalculationEngineAST astCalculations = new CalculationEngineAST (
				program, database);
		new AnalysisOutput (program, database, cfgCalculations, astCalculations);
	}

	public static void main (String[] args)
	{
		addOptions ();
		parseCommandLine (args);
		run ();
	}
}
