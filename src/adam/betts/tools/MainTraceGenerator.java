package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.programs.Program;
import adam.betts.traces.GenerateTrace;
import adam.betts.utilities.DefaultOptions;
import adam.betts.utilities.Globals;

public class MainTraceGenerator
{
	private static Options options;
	private static Option runsOption;
	private static Option addressTraceOption;

	protected static long runs = 1;
	protected static boolean addressTrace;

	public static void main (String[] args)
	{
		addOptions ();
		parseCommandLine (args);
		run ();
	}

	private static void addOptions ()
	{
		options = new Options ();
		DefaultOptions.addDefaultOptions (options);
		DefaultOptions.addProgramOption (options);
		DefaultOptions.addRootOption (options, false);
		DefaultOptions.addInstrumentationProfileOption (options, true, 1);
		DefaultOptions.addUDrawDirectoryOption (options);
		DefaultOptions.addOutFileOption (options);

		runsOption = new Option ("n", "runs", true,
				"Number of runs of root to be generated. Default is " + MainTraceGenerator.runs
						+ ".");
		runsOption.setRequired (false);
		options.addOption (runsOption);

		addressTraceOption = new Option ("a", "address-trace", false,
				"Generate an address trace rather than a timing trace.");
		addressTraceOption.setRequired (false);
		options.addOption (addressTraceOption);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "trace-generator.jar";
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
				/*
				 * Set the global variables according to the command-line
				 * parameters
				 */
				DefaultOptions.setDefaultOptions (line);
				DefaultOptions.setProgramOption (line);
				DefaultOptions.setRootOption (line);
				DefaultOptions.setInstrumentationProfileOption (line);
				DefaultOptions.setUDrawDirectoryOption (line);
				DefaultOptions.setOutFileOption (line);

				addressTrace = line.hasOption (addressTraceOption.getOpt ());

				if (!addressTrace && Globals.getInstrumentationProfile () == null)
				{
					System.err.println ("You must specify an instrumentation profile.");
					System.exit (1);
				}

				String arg = line.getOptionValue (runsOption.getOpt ());
				if (arg != null)
				{
					try
					{
						long runs = Long.parseLong (arg);
						if (runs < 1 || runs > Long.MAX_VALUE)
						{
							throw new IllegalArgumentException ();
						} else
						{
							MainTraceGenerator.runs = runs;
						}
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ runsOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err.println (arg
								+ " is not a valid number of runs. It should be in the range: 1.."
								+ Long.MAX_VALUE);
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
		Program program = new Program ();
		new GenerateTrace (program);
	}

	public final static long getNumberOfRuns ()
	{
		return runs;
	}

	public final static boolean addressTrace ()
	{
		return addressTrace;
	}
}
