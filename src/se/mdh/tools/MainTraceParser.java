package se.mdh.tools;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import se.mdh.calculations.DatabaseWithoutProgram;
import se.mdh.outputs.WCETOutput;
import se.mdh.utilities.Debug;
import se.mdh.utilities.DefaultOptions;
import se.mdh.utilities.Enums;
import se.mdh.utilities.Enums.LpSolveVerbosity;

public class MainTraceParser
{
	private static Options options;
	private static Option lpSolveDirectoryOption;
	private static Option incrementalWCETOption;
	private static Option lpSolveVerbosityOption;
	private static Option modelIdentificationOption;

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
		DefaultOptions.addUDrawDirectoryOption (options);
		DefaultOptions.addTraceFileOption (options);

		incrementalWCETOption = new Option ("I", "incremental", false,
				"Do a WCET computation after each run in the trace.");
		incrementalWCETOption.setRequired (false);
		options.addOption (incrementalWCETOption);

		modelIdentificationOption = new Option ("m", "model", false,
				"Write out the model identification matrices.");
		modelIdentificationOption.setRequired (false);
		options.addOption (modelIdentificationOption);

		lpSolveDirectoryOption = new Option ("L", "lpsolve", true,
				"Write the lp_solve models to files inside this directory.");
		lpSolveDirectoryOption.setRequired (false);
		options.addOption (lpSolveDirectoryOption);

		lpSolveVerbosityOption = new Option (
				"l",
				"lpsolve-verbose",
				true,
				"Force the verbosity of lp_solve to the selected level.\nSupported arguments are: "
						+ Arrays.toString (LpSolveVerbosity.values ()).replace (
								"[", "").replace ("]", ""));
		lpSolveVerbosityOption.setRequired (false);
		options.addOption (lpSolveVerbosityOption);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "trace-parser.jar";
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
				DefaultOptions.setUDrawDirectoryOption (line);
				DefaultOptions.setTraceFileOption (line);

				Globals.incrementalWCET = line.hasOption (incrementalWCETOption
						.getOpt ());
				Globals.doModelIdentification = line
						.hasOption (modelIdentificationOption.getOpt ());

				String arg = line.getOptionValue (lpSolveDirectoryOption
						.getOpt ());
				if (arg != null)
				{
					Globals.lpSolveDirectory = arg;
				}

				arg = line.getOptionValue (lpSolveVerbosityOption.getOpt ());
				if (arg != null)
				{
					try
					{
						Globals.lpVerbosity = LpSolveVerbosity.valueOf (arg
								.toUpperCase ());
					}
					catch (IllegalArgumentException e)
					{
						System.err
								.println (arg
										+ " is not a valid verbosity level of lp_solve.");
						System.exit (1);
					}
				}
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
		Debug.verboseMessage ("Parsing trace");
		try
		{
			WCETOutput
					.openFileHandles ();
			WCETOutput.writeTableHeader ();
			new DatabaseWithoutProgram ();
			WCETOutput.closeFileHandles ();
		}
		catch (IOException e)
		{
			e.printStackTrace ();
			System.exit (1);
		}
	}

	public static class Globals
	{
		protected static String lpSolveDirectory = null;
		protected static Enums.LpSolveVerbosity lpVerbosity = LpSolveVerbosity.CRITICAL;
		protected static boolean incrementalWCET;
		protected static boolean doModelIdentification;

		public final static boolean lpSolveDirectorySet ()
		{
			return lpSolveDirectory != null;
		}

		public final static String getLpSolveDirectory ()
		{
			File dir = new File (lpSolveDirectory);
			if (!dir.exists ())
			{
				dir.mkdir ();
			}
			return lpSolveDirectory;
		}

		public final static int getLpSolveVerbosity ()
		{
			switch (lpVerbosity)
			{
				case CRITICAL:
					return 1;
				case SEVERE:
					return 2;
				case IMPORTANT:
					return 3;
				case NORMAL:
					return 4;
				case DETAILED:
					return 5;
				case FULL:
					return 6;
			}
			return 0;
		}

		public final static boolean doIncrementalWCET ()
		{
			return incrementalWCET;
		}

		public final static boolean writeModelIdentificationData ()
		{
			return doModelIdentification;
		}
	}
}
