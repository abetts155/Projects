package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.outputs.WriteProgram;
import adam.betts.programs.Program;
import adam.betts.programs.ProgramGenerator;
import adam.betts.utilities.DefaultOptions;

public class MainProgramGenerator
{
	private static Options options;
	private static Option fanOutOption;
	private static Option loopsOption;
	private static Option loopsDepthOption;
	private static Option selfLoopsOption;
	private static Option subprogramsOption;
	private static Option depthOption;
	private static Option breaksOption;
	private static Option continuesOption;
	private static Option indirectRecursionOption;
	private static Option directRecursionOption;
	private static Option numberOfVerticesOption;

	private final static int minVertices = 10;
	private final static int maxVertices = 10000;

	private final static int minFanOut = 2;
	private final static int maxFanOut = 10;

	private final static int maxNumberOfLoops = 100;
	private final static int maxNumberOfSelfLoops = 5;

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

		fanOutOption = new Option ("F", "fan-out", true,
				"Maximum number of successors of a vertex in range [" + minFanOut + ".."
						+ maxFanOut + "]. Default is " + Globals.fanOut + ".");
		fanOutOption.setRequired (false);
		options.addOption (fanOutOption);

		loopsOption = new Option ("l", "loops", true,
				"Maximum number of loops in a single graph in range [0.." + maxNumberOfLoops
						+ "]. Default is " + Globals.loops + ".");
		loopsOption.setRequired (false);
		options.addOption (loopsOption);

		loopsDepthOption = new Option ("L", "loop-depth", true,
				"Maximum loop-nesting level depth. Default is " + Globals.loopsDepth + ".");
		loopsDepthOption.setRequired (false);
		options.addOption (loopsDepthOption);

		selfLoopsOption = new Option ("S", "self-loops", true,
				"Maximum number of self-loops in a single graph in range [0.."
						+ maxNumberOfSelfLoops + "]. Default is " + Globals.selfLoops + ".");
		selfLoopsOption.setRequired (false);
		options.addOption (selfLoopsOption);

		subprogramsOption = new Option ("s", "subprograms", true,
				"Maximum number of sub-programs in the program.");
		subprogramsOption.setRequired (true);
		options.addOption (subprogramsOption);

		depthOption = new Option ("D", "depth", true,
				"Maximum depth of the call graph. Default is " + Globals.depth + ".");
		depthOption.setRequired (false);
		options.addOption (depthOption);

		directRecursionOption = new Option ("R", "direct-recursion", true,
				"Number of direct recursive calls. Default is " + Globals.directRecursiveCalls
						+ ".");
		directRecursionOption.setRequired (false);
		options.addOption (directRecursionOption);

		indirectRecursionOption = new Option ("I", "indirect-recursion", true,
				"Number of indirect recursive calls. Default is " + Globals.indirectRecursiveCalls
						+ ".");
		indirectRecursionOption.setRequired (false);
		options.addOption (indirectRecursionOption);

		breaksOption = new Option ("b", "breaks", false, "Include break-like structures in loops.");
		breaksOption.setRequired (false);
		options.addOption (breaksOption);

		continuesOption = new Option ("C", "continue", false,
				"Include continue-like structures in loops.");
		continuesOption.setRequired (false);
		options.addOption (continuesOption);

		numberOfVerticesOption = new Option ("V", "vertices", true,
				"Maximum number of vertices in a control flow graph. Default is "
						+ Globals.vertices + ".");
		numberOfVerticesOption.setRequired (false);
		options.addOption (numberOfVerticesOption);
	}

	private static void parseCommandLine (String[] args)
	{
		final String toolName = "program-generator.jar";
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
				DefaultOptions.setUDrawDirectoryOption (line);

				Globals.breaks = line.hasOption (breaksOption.getOpt ());
				Globals.continues = line.hasOption (continuesOption.getOpt ());

				if (line.hasOption (numberOfVerticesOption.getOpt ()))
				{
					String arg = line.getOptionValue (numberOfVerticesOption.getOpt ());
					try
					{
						int vertices = Integer.parseInt (arg);
						if (vertices < minVertices || vertices > maxVertices)
						{
							throw new IllegalArgumentException (vertices
									+ " is not a valid number of vertices. "
									+ "It must be a positive integer in the range [" + minVertices
									+ ".." + maxVertices + "].");
						}
						if (vertices < 2 * Globals.loops + 2)
						{
							throw new IllegalArgumentException (vertices
									+ " is not a valid number of vertices. "
									+ "It must be grater or equal to " + (2 * Globals.loops + 2)
									+ " to be able to generate " + Globals.loops + " loops.");
						}
						Globals.vertices = vertices;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ numberOfVerticesOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err.println (e.getMessage ());
						System.exit (1);
					}
				}

				if (line.hasOption (fanOutOption.getOpt ()))
				{
					String arg = line.getOptionValue (fanOutOption.getOpt ());
					try
					{
						int fanOut = Integer.parseInt (arg);
						if (fanOut < minFanOut || fanOut > maxFanOut)
						{
							throw new IllegalArgumentException ();
						}
						Globals.fanOut = fanOut;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ fanOutOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err.println (arg
								+ " is not a valid fan out. It must be in the range [" + minFanOut
								+ ".." + maxFanOut + "].");
						System.exit (1);
					}
				}

				if (line.hasOption (loopsOption.getOpt ()))
				{
					String arg = line.getOptionValue (loopsOption.getOpt ());
					try
					{
						int loops = Integer.parseInt (arg);
						if (loops < 0 || loops > maxNumberOfLoops)
						{
							throw new IllegalArgumentException ();
						} else if (loops > (Globals.vertices - 2) / 2)
						{
							System.err.println ("You need at least " + (loops * 2 + 2)
									+ " vertices to have " + loops + " loops");
							System.exit (1);
						}

						Globals.loops = loops;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ loopsOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err.println (arg
								+ " is not a valid number of loops. It must be in the range [0.."
								+ maxNumberOfLoops + "].");
						System.exit (1);
					}
				}

				if (line.hasOption (loopsDepthOption.getOpt ()))
				{
					if (line.hasOption (loopsOption.getOpt ()) == false)
					{
						System.err
								.println ("The loop-nesting depth option must be used in conjunction with the number of loops option, otherwise there are no loops generated.");
						System.exit (1);
					}

					String arg = line.getOptionValue (loopsDepthOption.getOpt ());
					try
					{
						int depth = Integer.parseInt (arg);
						if (depth < 1 || depth > Globals.loops)
						{
							throw new IllegalArgumentException ();
						}
						Globals.loopsDepth = depth;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ loopsDepthOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err
								.println (arg
										+ " is not a valid loop-nesting depth. It must be in the range [1..#Loops].");
						System.exit (1);
					}
				}

				if (line.hasOption (selfLoopsOption.getOpt ()))
				{
					String arg = line.getOptionValue (selfLoopsOption.getOpt ());
					try
					{
						int selfLoops = Integer.parseInt (arg);
						if (selfLoops < 0 || selfLoops > maxNumberOfSelfLoops)
						{
							throw new IllegalArgumentException ();
						}
						Globals.selfLoops = selfLoops;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ selfLoopsOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err
								.println (arg
										+ " is not a valid number of self loops. It must be in the range [0.."
										+ maxNumberOfSelfLoops + "]");
						System.exit (1);
					}
				}

				if (line.hasOption (subprogramsOption.getOpt ()))
				{
					String arg = line.getOptionValue (subprogramsOption.getOpt ());
					try
					{
						int subprograms = Integer.parseInt (arg);
						if (subprograms < 1)
						{
							throw new IllegalArgumentException ();
						}
						Globals.subprograms = subprograms;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ subprogramsOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err
								.println (arg
										+ " is not a valid number of subprograms. It must be a positive integer in the range 1.."
										+ Integer.MAX_VALUE);
					}
				}

				if (line.hasOption (directRecursionOption.getOpt ()))
				{
					String arg = line.getOptionValue (directRecursionOption.getOpt ());
					try
					{
						int calls = Integer.parseInt (arg);
						if (calls < 0)
						{
							throw new IllegalArgumentException (
									calls
											+ " is not a valid number of direct recursive calls. Negative numbers not permitted.");
						} else if (calls > Globals.subprograms)
						{
							throw new IllegalArgumentException (
									calls
											+ " is not a valid number of direct recursive calls. There must be at least as many subprograms as the number of direct recursive calls.");
						}
						Globals.directRecursiveCalls = calls;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ subprogramsOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err.println (e.getMessage ());
						System.exit (1);
					}
				}

				if (line.hasOption (indirectRecursionOption.getOpt ()))
				{
					String arg = line.getOptionValue (indirectRecursionOption.getOpt ());
					try
					{
						int calls = Integer.parseInt (arg);
						if (calls < 0)
						{
							throw new IllegalArgumentException (
									calls
											+ " is not a valid number of indirect recursive calls. Negative numbers not permitted.");
						}
						Globals.indirectRecursiveCalls = calls;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ subprogramsOption.getLongOpt ());
						System.exit (1);
					} catch (IllegalArgumentException e)
					{
						System.err.println (e.getMessage ());
						System.exit (1);
					}
				}

				if (line.hasOption (depthOption.getOpt ()))
				{
					String arg = line.getOptionValue (depthOption.getOpt ());
					try
					{
						int depth = Integer.parseInt (arg);
						Globals.depth = depth;
					} catch (NumberFormatException e)
					{
						System.err.println ("'" + arg + "' is not a valid argument to "
								+ depthOption.getLongOpt ());
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
		Program program = new ProgramGenerator ().getProgram ();
		new WriteProgram (program);
	}

	public static class Globals
	{
		protected static int subprograms;
		protected static int fanOut = minFanOut;
		protected static int loops = 0;
		protected static int loopsDepth = 1;
		protected static int selfLoops = 0;
		protected static int depth = 7;
		protected static boolean breaks = false;
		protected static boolean continues = false;
		protected static int vertices = 50;
		protected static int directRecursiveCalls = 0;
		protected static int indirectRecursiveCalls = 0;

		public final static int getFanOut ()
		{
			return fanOut;
		}

		public final static int getNumberOfLoops ()
		{
			return loops;
		}

		public final static int getLoopNestingLevelDepth ()
		{
			return loopsDepth;
		}

		public final static int getNumberOfSelfLoops ()
		{
			return selfLoops;
		}

		public final static int getNumberOfSubprograms ()
		{
			return subprograms;
		}

		public final static int getDepthOfCallGraph ()
		{
			return depth;
		}

		public final static boolean breaksAllowed ()
		{
			return breaks;
		}

		public final static boolean continuesAllowed ()
		{
			return continues;
		}

		public final static int getNumberOfVerticesInCFG ()
		{
			return vertices;
		}

		public final static int getNumberOfDirectRecursiveCalls ()
		{
			return directRecursiveCalls;
		}

		public final static int getNumberOfIndirectRecursiveCalls ()
		{
			return indirectRecursiveCalls;
		}
	}
}
