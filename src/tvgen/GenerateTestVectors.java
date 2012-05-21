package tvgen;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import gem5.Gem5CovCountEval;
import gem5.Gem5CovFractionEval;
import gem5.Gem5TimeEvaluator;
import gem5.Gem5Tools;

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
	private static Option entryOption;
	private static Option vectorLengthOption;
	private static Option upBoundOption;
	private static Option lowBoundOption;
	private static Option numGensOption;
	private static Option configFlagsOption;
	private static Option crossoverOption;
	private static Option selectOption;
	private static Option evaluatorOption;
	private static Option mutRateOption;
	private static Option crossRateOption;
	private static Option selectRateOption;
	private static Option threadsOption;
	private static Option seedOption;
	private static Option populationOption;
	
	private static void addOptions ()
	{
		options = new Options ();
		
		helpOption = new Option ("h", "help", false, "Display this message.");
		options.addOption (helpOption);
		
		debugOption = new Option ("d", "debug", false, "Debug mode.");
		options.addOption (debugOption);

		typeOption = new Option ("g", "gen-type", true,
				"The type of generator to use (rand or ga) (default is ga)");
		typeOption.setRequired (false);
		options.addOption (typeOption);
		
		outputOption = new Option ("o", "output-file", true,
				"The output file to save the vectors to");
		outputOption.setRequired (true);
		options.addOption (outputOption);
		
		programOption = new Option ("p", "program", true,
				"The name of the program binary to be tested");
		programOption.setRequired (false);
		options.addOption (programOption);
		
		entryOption = new Option ("r", "entry-point", true,
				"The name of the function to use as the entry point of the program");
		entryOption.setRequired (false);
		options.addOption (entryOption);
		
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
				"The number of generations to use with ga or total number of vectors to " +
				"generate with rand");
		numGensOption.setRequired (true);
		options.addOption (numGensOption);
		
		configFlagsOption = new Option ("c", "config-file", true,
				"The file specifying the flags to be used with gem5 config file");
		configFlagsOption.setRequired (false);
		options.addOption (configFlagsOption);
		
		crossoverOption = new Option ("C", "crossover", true,
				"The type of crossover to use (1point, 2point) (default is 1point)");
		crossoverOption.setRequired (false);
		options.addOption (crossoverOption);
		
		selectOption = new Option ("S", "selector", true,
				"The type of selector to use (elite, prob, rand) (default is elite)");
		selectOption.setRequired (false);
		options.addOption (selectOption);
		
		evaluatorOption = new Option ("E", "evaluator", true,
				"The type of evaluator to use (gem5Time, gem5CovFrac, gem5CovCount)");
		evaluatorOption.setRequired (false);
		options.addOption (evaluatorOption);
		
		mutRateOption = new Option ("m", "mutation-rate", true,
				"The mutation rate to use in the genetic algorithm (default = 0.05)");
		mutRateOption.setRequired (false);
		options.addOption (mutRateOption);
		
		crossRateOption = new Option ("x", "crossover-rate", true,
				"The crossover rate to use in the genetic algorithm (default = 0.9)");
		crossRateOption.setRequired (false);
		options.addOption (crossRateOption);
		
		selectRateOption = new Option ("s", "selection-rate", true,
				"The fraction of a generation selected for crossover/mutation " +
				"(default = 0.1)");
		selectRateOption.setRequired (false);
		options.addOption (selectRateOption);
		
		threadsOption = new Option ("t", "num-threads", true,
				"The number of evaluator threads to use (default is 1)");
		threadsOption.setRequired (false);
		options.addOption (threadsOption);
		
		populationOption = new Option ("P", "pop-size", true,
				"The size of the population in each generation (default is 100)");
		populationOption.setRequired (false);
		options.addOption (populationOption);
		
		seedOption = new Option ("R", "seed-rand", false,
				"Seed each generation with random vector");
		seedOption.setRequired (false);
		options.addOption (seedOption);
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
		if (genType == null) { genType = "ga"; }
		if (genType.equals("rand"))
		{
			generator = createRandomGenerator(outputFile, numVecsOrGens);
		}
		else if (genType.equals("ga"))
		{
			TVSelector selec = createSelector(
					line.getOptionValue(selectOption.getOpt()));
			TVCrossover cross = createCrossover(
					line.getOptionValue(crossoverOption.getOpt()));
			Mutator mut = createMutator("rand");
			
			GenerationEvaluator eval = createEvaluator(
					line.getOptionValue(evaluatorOption.getOpt()), line);
			
			generator = createGAGenerator(outputFile, numVecsOrGens, eval, cross, selec, mut);
			
			if (line.hasOption (mutRateOption.getOpt ()))
			{
				double mutRate = Double.parseDouble(
						line.getOptionValue(mutRateOption.getOpt()));
				((GaTVGenerator)generator).setMutationRate(mutRate);
			}
			
			if (line.hasOption (crossRateOption.getOpt ()))
			{
				double crossRate = Double.parseDouble(
						line.getOptionValue(crossRateOption.getOpt()));
				((GaTVGenerator)generator).setCrossoverRate(crossRate);
			}
			if (line.hasOption (selectRateOption.getOpt ()))
			{
				double selectRate = Double.parseDouble(
						line.getOptionValue(selectRateOption.getOpt()));
				((GaTVGenerator)generator).setSelectionRate(selectRate);
			}
			if (line.hasOption (populationOption.getOpt ()))
			{
				int popSize = Integer.parseInt(
						line.getOptionValue(populationOption.getOpt()));
				((GaTVGenerator)generator).setPopulationSize(popSize);
			}
			((GaTVGenerator)generator).setSeedWithRand(
					line.hasOption (seedOption.getOpt ()));
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
			GenerationEvaluator eval, TVCrossover cross, TVSelector selec, Mutator mut) {
		GaTVGenerator gen = new GaTVGenerator(outputFile);
		gen.setNumGenerations(numGens);
		gen.setEvaluator(eval);
		gen.setCrossover(cross);
		gen.setSelector(selec);
		gen.setMutator(mut);
		return gen;
	}
	
	private static GenerationEvaluator createEvaluator(String type, CommandLine line) {
		if (type == null)
		{
			SystemOutput.exitWithError("Error: need to specify evaluator type");
		}
		int numThreads = 1;
		if(line.hasOption (threadsOption.getOpt ())) {
			numThreads = Integer.parseInt(
					line.getOptionValue(threadsOption.getOpt()));
		}
		
		TVEvaluator[] evals = new TVEvaluator[numThreads];
		
		if(type.equals("gem5Time") || type.equals("gem5CovFrac")
				|| type.equals("gem5CovCount"))
		{
			if (!line.hasOption (programOption.getOpt ()))
				SystemOutput.exitWithError("Error: missing option " +programOption.getOpt());
			
			String entryPoint = "";
			if (!line.hasOption (entryOption.getOpt ())) {
				SystemOutput.exitWithError("Error: missing option " +
						entryOption.getOpt());
			}
			entryPoint = line.getOptionValue(entryOption.getOpt());
			
			String programName = line.getOptionValue(programOption.getOpt());
			String configFileName = line.getOptionValue(configFlagsOption.getOpt());
			
			Gem5Tools g5tools = new Gem5Tools(programName, configFileName);
			
			for(int i = 0; i < numThreads; i++) {
				if(type.equals("gem5Time")) {
					evals[i] = new Gem5TimeEvaluator(i, programName, g5tools, entryPoint);
				} else if(type.equals("gem5CovFrac")) {
					evals[i] = new Gem5CovFractionEval(i, programName, g5tools, entryPoint);
				} else if(type.equals("gem5CovCount")) {
					evals[i] = new Gem5CovCountEval(i, programName, g5tools, entryPoint);
				}
			}
			return new GenerationEvaluator(evals);
		}
		else if(type.equals("test"))
		{
			for(int i = 0; i < numThreads; i++) {
				evals[i] = new TestEvaluator(i);
			}
			return new GenerationEvaluator(evals);
		}
		SystemOutput.exitWithError("Error: invaild evaluator type: " + type);
		return null;
	}
	
	private static TVCrossover createCrossover(String type) {
		if (type == null)
		{
			return new OnePointCrossover();
		}
		else if(type.equals("1point"))
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
		if (type == null)
		{
			return new ElitistSelector();
		}
		else if(type.equals("prob"))
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
		SystemOutput.exitWithError("Error: invaild selector type: " + type);
		return null;
	}

	private static Mutator createMutator(String type) {
		if (type == null)
		{
			SystemOutput.exitWithError("Error: need to specify mutator type");
		}
		else if(type.equals("rand"))
		{
			return new RandomMutator();
		}
		SystemOutput.exitWithError("Error: invaild mutator type: " + type);
		return null;
	}

}
