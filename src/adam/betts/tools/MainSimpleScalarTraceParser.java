package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.programs.Program;
import adam.betts.simplescalar.TraceParser;
import adam.betts.utilities.Debug;
import adam.betts.utilities.DefaultOptions;
import adam.betts.utilities.Enums.IProfile;

public class MainSimpleScalarTraceParser
{
	private static Options options;
	private static Option basicBlockIDTraceOption;
	private static Option outputHexAddressesOption;

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
		DefaultOptions.addRootOption (options, true);
		DefaultOptions.addInstrumentationProfileOption (options, true, IProfile.values ().length);
		DefaultOptions.addUDrawDirectoryOption (options);
		DefaultOptions.addTraceFileOption (options);

		basicBlockIDTraceOption = new Option ("b", "basic-block", false,
				"Write basic block ids to the trace files instead of addresses.");
		basicBlockIDTraceOption.setRequired (false);
		options.addOption (basicBlockIDTraceOption);

		outputHexAddressesOption = new Option ("H", "hex", false,
				"Output hexadecimal addresses to the trace files instead of integers.");
		outputHexAddressesOption.setRequired (false);
		options.addOption (outputHexAddressesOption);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "simplescalar.jar";
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
				DefaultOptions.setTraceFileOption (line);
				Globals.outputBasicBlockIds = line.hasOption (basicBlockIDTraceOption.getOpt ());
				Globals.outputHexAddresses = line.hasOption (outputHexAddressesOption.getOpt ());
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
		Debug.verboseMessage ("Parsing trace file");
		new TraceParser (program);
	}

	public static class Globals
	{
		protected static boolean outputBasicBlockIds;
		protected static boolean outputHexAddresses;

		public final static boolean outputBasicBlockIds ()
		{
			return outputBasicBlockIds;
		}

		public final static boolean outputHexAddresses ()
		{
			return outputHexAddresses;
		}
	}
}
