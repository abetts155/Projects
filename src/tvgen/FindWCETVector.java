package tvgen;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import tvgen.random.RandomWCETFinder;
import tvgen.util.*;

public class FindWCETVector {

	private static Options options;
	private static Option helpOption;
	private static Option debugOption;
	private static Option randomOption;
	private static Option numRandom;
	private static Option programOption;
	private static Option vectorLengthOption;
	private static Option upBoundOption;
	private static Option lowBoundOption;
	private static Option cpuTypeOption;
	private static Option threadsOption;
	
	private static String worstCaseVector = "";
	private static double worstCaseTime;
	private static String bestCaseVector = "";
	private static double bestCaseTime;
	
	private static void addOptions()
	{
		options = new Options ();
		
		helpOption = new Option ("h", "help", false, "Display this message.");
		options.addOption (helpOption);
		
		debugOption = new Option ("d", "debug", false, "Debug mode.");
		options.addOption (debugOption);

		randomOption = new Option ("r", "random", false,
				"Use random test vector generation rather than systamatic");
		randomOption.setRequired (false);
		options.addOption (randomOption);
		
		numRandom = new Option ("n", "num-random", true,
				"The number of random vectors to test");
		numRandom.setRequired (false);
		options.addOption (numRandom);
		
		programOption = new Option ("p", "program", true,
				"The name of the program binary to be tested");
		programOption.setRequired (true);
		options.addOption (programOption);
		
		vectorLengthOption = new Option ("v", "vector-length", true,
				"The length of the vectors generated");
		vectorLengthOption.setRequired (true);
		options.addOption (vectorLengthOption);
		
		upBoundOption = new Option ("u", "upper-bound", true,
				"The upper bound of values in the vector");
		upBoundOption.setRequired (false);
		options.addOption (upBoundOption);
		
		lowBoundOption = new Option ("l", "lower-bound", true,
				"The lower bound of values in the vector");
		lowBoundOption.setRequired (false);
		options.addOption (lowBoundOption);
		
		cpuTypeOption = new Option ("c", "cpu-type", true,
				"The type of cpu to use in the gem5 simulation" +
				"(atomic, timing, detailed, inorder)");
		cpuTypeOption.setRequired (true);
		options.addOption (cpuTypeOption);
		
		threadsOption = new Option ("t", "num-threads", true,
				"The number of evaluator threads to use (default is 1)");
		threadsOption.setRequired (false);
		options.addOption (threadsOption);
	}
	
	private static CommandLine parseCommandLine(String[] args)
	{
		final String toolName = "tv-generator.jar";
		CommandLineParser parser = new GnuParser ();
		HelpFormatter formatter = new HelpFormatter ();
		formatter.setWidth (80);
		CommandLine line = null;
		try
		{
			line = parser.parse (options, args);

			if (line.hasOption (helpOption.getOpt ()))
			{
				formatter.printHelp (toolName, options);
				System.exit (1);
			} 
			else
			{
				if (line.hasOption (debugOption.getOpt ()))
					SystemOutput.debugMode = true;
			}
		}
		catch (ParseException e)
		{
			System.out.println (e.getMessage ());
			formatter.printHelp (toolName, options);
			System.exit (1);
		}
		
		return line;
	}
	
	/**
	 * Finds the test vector with the WCET using brute force
	 * @param args
	 */
	public static void main(String[] args) {
		addOptions();
		CommandLine line = parseCommandLine(args);
		
		worstCaseTime = Double.NEGATIVE_INFINITY;
		bestCaseTime = Double.POSITIVE_INFINITY;
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {printScores();}
		});
		
		int numThreads = 1;
		if(line.hasOption (threadsOption.getOpt())) {
			numThreads = Integer.parseInt(
					line.getOptionValue(threadsOption.getOpt()));
		}
		WCETFinder[] finders = new WCETFinder[numThreads];
		Thread[] threads = new Thread[numThreads];
		
		String programName = line.getOptionValue(programOption.getOpt());
		String cpuType = line.getOptionValue(cpuTypeOption.getOpt());
		
		int upBound = Integer.MAX_VALUE;
		int lowBound = Integer.MIN_VALUE;
		int vectorLength = Integer.parseInt(
				line.getOptionValue(vectorLengthOption.getOpt()));
		if(line.hasOption (lowBoundOption.getOpt())) {
			lowBound = Integer.parseInt(
					line.getOptionValue(lowBoundOption.getOpt()));
		}
		if(line.hasOption (upBoundOption.getOpt())) {
			upBound = Integer.parseInt(
					line.getOptionValue(upBoundOption.getOpt()));
		}
		
		if(line.hasOption (randomOption.getOpt())) {
			if(!line.hasOption (numRandom.getOpt())) {
				SystemOutput.exitWithError("Missing option " + numRandom.getOpt());
			}
			int numRand = Integer.parseInt(
					line.getOptionValue(numRandom.getOpt()));
			for(int i = 0; i < numThreads; i++) {
				finders[i] = new RandomWCETFinder(i, programName,
						cpuType, numRand / numThreads);
			}
		} else {
			for(int i = 0; i < numThreads; i++) {
				int threadLowRange = lowBound +
						(int)(((float)i / (float)numThreads) * (upBound - lowBound));
				int threadUpRange = lowBound +
						(int)(((float)(i + 1) / (float)numThreads) * (upBound - lowBound));
				
				//If last thread increment upper range to ensure upperbound is tested too
				if(i == numThreads - 1) {
					threadUpRange++;
				}
				
				finders[i] = new SystematicWCETFinder(i, programName,
						cpuType, threadUpRange, threadLowRange);
			}
		}
		
		for(int i = 0; i < numThreads; i++) {
			finders[i].setVectorLength(vectorLength);
			finders[i].setUpperBound(upBound);
			finders[i].setLowerBound(lowBound);
			threads[i] = new Thread(finders[i]);
			threads[i].start();
		}
		
		for(int i = 0; i < numThreads; i++) {
			try {
				threads[i].join();
			} catch (Exception e) {
				SystemOutput.exitWithError("Error when running threads");
			}
		}
	}
	
	public static synchronized void logVector(TestVector vector) {
		if(vector.hasBeenScored()) {
			double score = vector.getScore();
			if(score > worstCaseTime) {
				worstCaseTime = score;
				worstCaseVector = vector.toString();
				SystemOutput.printMessage("New worst case: " + worstCaseTime + 
						" " + worstCaseVector);
			}
			if(score < bestCaseTime) {
				bestCaseTime = score;
				bestCaseVector = vector.toString();
				SystemOutput.printMessage("New best case: " + bestCaseTime + 
						" " + bestCaseVector);
			}
		}
	}
	
	public static void printScores() {
		SystemOutput.printMessage("Worst case vector: " + worstCaseVector);
		SystemOutput.printMessage("Worst case time: " + worstCaseTime);
		SystemOutput.printMessage("Best case vector: " + bestCaseVector);
		SystemOutput.printMessage("Best case time: " + bestCaseTime);
	}

}
