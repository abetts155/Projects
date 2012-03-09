package tvgen;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import java.io.File;

import tvgen.ga.*;
import tvgen.random.*;
import tvgen.util.SystemOutput;

public class GenerateTestVectors {
	
	private static Options options;
	private static Option helpOption;
	private static Option debugOption;
	private static Option typeOption;
	private static Option outputOption;
	private static Option programOption;
	private static Option vectorLengthOption;
	private static Option upBoundOption;
	private static Option lowBoundOption;
	private static Option numGensOption;
	private static Option cpuTypeOption;
	private static Option mutRateOption;
	private static Option crossRateOption;
	
	private static void addOptions ()
	{
		options = new Options ();
		
		helpOption = new Option ("h", "help", false, "Display this message.");
		options.addOption (helpOption);
		
		debugOption = new Option ("d", "debug", false, "Debug mode.");
		options.addOption (debugOption);

		typeOption = new Option ("t", "gen-type", true,
				"The type of generator to use (rand or ga)");
		typeOption.setRequired (true);
		options.addOption (typeOption);
		
		outputOption = new Option ("o", "output-file", true,
				"The output file to save the vectors to");
		outputOption.setRequired (true);
		options.addOption (outputOption);
		
		programOption = new Option ("p", "program", true,
				"The name of the program binary to be tested");
		programOption.setRequired (false);
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
		
		numGensOption = new Option ("n", "num-generations", true,
				"The number of generations to use with ga or total number of vectors to" +
				"generate with rand");
		numGensOption.setRequired (true);
		options.addOption (numGensOption);
		
		cpuTypeOption = new Option ("c", "cpu-type", true,
				"The type of cpu to use in the gem5 simulation" +
				"(atomic, timing, detailed, inorder)");
		cpuTypeOption.setRequired (false);
		options.addOption (cpuTypeOption);
		
		mutRateOption = new Option ("m", "mutation-rate", true,
				"The mutation rate to use in the genetic algorithm (default = 0.05)");
		mutRateOption.setRequired (false);
		options.addOption (mutRateOption);
		
		crossRateOption = new Option ("x", "crossover-rate", true,
				"The crossover rate to use in the genetic algorithm (default = 0.9)");
		crossRateOption.setRequired (false);
		options.addOption (crossRateOption);
	}

	private static CommandLine parseCommandLine (String[] args)
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
	 * Generates test vectors for programs
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		addOptions();
		CommandLine line = parseCommandLine(args);
		
		TestVectorGenerator generator;
		File outputFile = new File( line.getOptionValue(outputOption.getOpt()) );
		
		int numVecsOrGens = Integer.parseInt(
				line.getOptionValue(numGensOption.getOpt()));
		
		// Select type of generator to create
		String genType = line.getOptionValue(typeOption.getOpt()); 
		if (genType.equals("rand"))
		{
			generator = createRandomGenerator(outputFile, numVecsOrGens);
		}
		else if (genType.equals("ga"))
		{
			TVSelector selec = createSelector("elite");
			TVCrossover cross = createCrossover("2point");
			Mutator mut = createMutator("rand");
			
			TVEvaluator eval = createEvaluator("gem5Time", line);
			
			generator = createGAGenerator(outputFile, numVecsOrGens, eval, cross, selec, mut);
			
			if (line.hasOption (mutRateOption.getOpt ()))
			{
				double mutRate = Integer.parseInt(
						line.getOptionValue(mutRateOption.getOpt()));
				((GaTVGenerator)generator).setMutationRate(mutRate);
			}
			
			if (line.hasOption (crossRateOption.getOpt ()))
			{
				double crossRate = Integer.parseInt(
						line.getOptionValue(crossRateOption.getOpt()));
				((GaTVGenerator)generator).setCrossoverRate(crossRate);
			}
		}
		else
		{
			SystemOutput.errorMessage("Error: Invalid type of generator");
			return;
		}
		
		int vectorLength = Integer.parseInt(
				line.getOptionValue(vectorLengthOption.getOpt()));
		generator.setVectorLength(vectorLength);
		
		// Set upper and lower bound if specified
		if (line.hasOption (upBoundOption.getOpt ()))
		{
			int upperBound = Integer.parseInt(
					line.getOptionValue(upBoundOption.getOpt()));
			generator.setUpperBound(upperBound);
		}
		if (line.hasOption (lowBoundOption.getOpt ()))
		{
			int lowerBound = Integer.parseInt(
					line.getOptionValue(lowBoundOption.getOpt()));
			generator.setLowerBound(lowerBound);
		}
		
		generator.generate();
	}
	
	private static TestVectorGenerator createRandomGenerator(File outputFile, int numVectors) {
		RandomTVGenerator gen = new RandomTVGenerator(outputFile);
		gen.setNumberOfVectors(numVectors);
		return gen;
	}
	
	private static TestVectorGenerator createGAGenerator(File outputFile, int numGens,
			TVEvaluator eval, TVCrossover cross, TVSelector selec, Mutator mut) {
		GaTVGenerator gen = new GaTVGenerator(outputFile);
		gen.setNumGenerations(numGens);
		gen.setEvaluator(eval);
		gen.setCrossover(cross);
		gen.setSelector(selec);
		gen.setMutator(mut);
		return gen;
	}
	
	private static TVEvaluator createEvaluator(String type, CommandLine line) {
		if(type.equals("gem5Time"))
		{
			if (!line.hasOption (programOption.getOpt ()))
				SystemOutput.exitWithError("Error: missing option " + programOption.getOpt());
			
			if (!line.hasOption (cpuTypeOption.getOpt ()))
				SystemOutput.exitWithError("Error: missing option " + cpuTypeOption.getOpt());
			
			String programName = line.getOptionValue(programOption.getOpt());
			String cpuType = line.getOptionValue(cpuTypeOption.getOpt());
			
			return new Gem5TimeEvaluator(programName, cpuType);
		}
		else if(type.equals("test"))
		{
			return new TestEvaluator();
		}
		SystemOutput.exitWithError("Error: invaild evaluator type: " + type);
		return null;
	}
	
	private static TVCrossover createCrossover(String type) {
		if(type.equals("1point"))
		{
			return new OnePointCrossover();
		}
		else if(type.equals("2point"))
		{
			return new TwoPointCrossover();
		}
		SystemOutput.exitWithError("Error: invaild crossover type: " + type);
		return null;
	}

	private static TVSelector createSelector(String type) {
		if(type.equals("prob"))
		{
			return new ProbabilisticSelector();
		}
		else if(type.equals("elite"))
		{
			return new ElitistSelector();
		}
		else if(type.equals("rand"))
		{
			return new RandomSelector();
		}
		SystemOutput.exitWithError("Error: invaild evaluator type: " + type);
		return null;
	}

	private static Mutator createMutator(String type) {
		if(type.equals("rand"))
		{
			return new RandomMutator();
		}
		SystemOutput.exitWithError("Error: invaild mutator type: " + type);
		return null;
	}

}
