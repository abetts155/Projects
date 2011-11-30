package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.calculations.LoopBoundDatabase;
import adam.betts.outputs.AnalysisOutput;
import adam.betts.programs.Program;
import adam.betts.utilities.Debug;
import adam.betts.utilities.DefaultOptions;

public class MainLoopAnalyser
{
	private static Options options;
	private static Option boundLevelOption;

	private static int boundLevel = 1;

	private static void addOptions ()
	{
		options = new Options ();
		DefaultOptions.addDefaultOptions (options);
		DefaultOptions.addProgramOption (options);
		DefaultOptions.addRootOption (options, true);
		DefaultOptions.addTraceFileOption (options);
		DefaultOptions.addUDrawDirectoryOption (options);
		DefaultOptions.addInstrumentationProfileOption (options, true, 1);

		boundLevelOption = new Option ("b", "bound-level", true,
				"Obtain bounds up to this number of outer-nested loops. Default is " + boundLevel
						+ ".");
		boundLevelOption.setRequired (false);
		options.addOption (boundLevelOption);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "loop.jar";
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
				DefaultOptions.setRootOption (line);
				DefaultOptions.setTraceFileOption (line);
				DefaultOptions.setUDrawDirectoryOption (line);
				DefaultOptions.setInstrumentationProfileOption (line);

				if (line.hasOption (boundLevelOption.getOpt ()))
				{
					String arg = line.getOptionValue (boundLevelOption.getOpt ());
					if (arg != null)
					{
						try
						{
							int boundLevel = Integer.parseInt (arg);
							if (boundLevel < 1)
							{
								throw new IllegalArgumentException ();
							} else
							{
								MainLoopAnalyser.boundLevel = boundLevel;
							}
						} catch (NumberFormatException e)
						{
							System.err.println ("'" + arg
									+ "' is not a valid argument to the option -"
									+ boundLevelOption.getOpt () + ".");
							System.exit (1);
						} catch (IllegalArgumentException e)
						{
							System.err
									.println (arg
											+ " is not a valid bound level. It should be a positive number greater than 1.");
							System.exit (1);
						}
					} else
					{
						System.err
								.println ("You must specify the bound level as an argument to the option -"
										+ boundLevelOption.getOpt () + ".");
						System.exit (1);
					}
				}
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
		program.insertVirtualIpoints ();
		program.buildIPGS (true);
		LoopBoundDatabase database = new LoopBoundDatabase (program);
		new AnalysisOutput (program, database);
	}

	public static void main (String[] args)
	{
		addOptions ();
		parseCommandLine (args);
		run ();
	}

	public final static int getBoundLevel ()
	{
		return boundLevel;
	}
}
