package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.calculations.Database;
import adam.betts.programs.Program;
import adam.betts.utilities.Debug;
import adam.betts.utilities.DefaultOptions;

public class MainProgramAnalyser
{
	private static Options options;
	private static Option inlineOption;
	private static Option loopsOption;
	private static Option syntaxTreesOption;

	private static void addOptions ()
	{
		options = new Options ();
		DefaultOptions.addDefaultOptions (options);
		DefaultOptions.addProgramOption (options);
		DefaultOptions.addUDrawDirectoryOption (options);
		DefaultOptions.addIPETOptions (options);

		inlineOption = new Option ("I", "inline", false,
				"Inline all control flow graphs to create a single monolothic graph.");
		inlineOption.setRequired (false);
		options.addOption (inlineOption);

		loopsOption = new Option ("l", "loops", false,
				"Generate the loop-nesting trees in the program.");
		loopsOption.setRequired (false);
		options.addOption (loopsOption);

		syntaxTreesOption = new Option ("s", "syntax-trees", false,
				"Generate the syntax trees of the control flow graphs.");
		syntaxTreesOption.setRequired (false);
		options.addOption (syntaxTreesOption);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "program-analyser.jar";
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
			} else
			{
				DefaultOptions.setDefaultOptions (line);
				DefaultOptions.setProgramOption (line);
				DefaultOptions.setUDrawDirectoryOption (line);
				DefaultOptions.setIPETOptions (line);

				Globals.inline = line.hasOption (inlineOption.getOpt ());

				Globals.LNTs = line.hasOption (loopsOption.getOpt ());

				Globals.ASTs = line.hasOption (syntaxTreesOption.getOpt ());
			}
		} catch (ParseException e)
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
		program.setRootID ();

		if (Globals.LNTs)
		{
			Debug.verboseMessage ("Building loop-nesting trees");
			program.buildLNTs ();
		}

		if (Globals.ASTs)
		{
			Debug.verboseMessage ("Building abstract syntax trees");
			program.buildSyntaxTrees ();
		}

		Database database = new Database (program);
		database.generateData ();
	}

	public static void main (String[] args)
	{
		addOptions ();
		parseCommandLine (args);
		run ();
	}

	public static class Globals
	{
		protected static boolean inline;
		protected static boolean LNTs;
		protected static boolean ASTs;
	}
}
