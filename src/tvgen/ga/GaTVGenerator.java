package tvgen.ga;

import java.io.File;

import tvgen.TestVectorGenerator;
import tvgen.util.*;

public class GaTVGenerator extends TestVectorGenerator {
	
	private int numGenerations;
	private int populationSize;
	
	private double mutationRate;
	private double crossoverRate;
	private double selectionRate;
	
	private boolean seedWithRand;
	
	private GenerationEvaluator evaluator = null;
	private TVSelector selector = null;
	private TVCrossover crossover = null;
	private Mutator mutator = null;
	
	public GaTVGenerator(File outFile) {
		super(outFile);
		
		numGenerations = 100;
		populationSize = 100;
		mutationRate = 0.05;
		crossoverRate = 0.9;
		selectionRate = 0.1;
		seedWithRand = false;
	}

	@Override
	public void generate() {
		if(!readyToGenerate()){
			return;
		}
		
		SystemOutput.debugMessage("Generating test vectors");
		
		RandomGenerator rand = TestVectorGenerator.getRandomGenerator();
		TestVector[] generation = rand.generateRandomVectors(
				populationSize, getVectorLength(), getLowerBound(), getUpperBound());
		
		// Add test vectors to list
		for(int i = 0; i < generation.length; i++) {
			TVs.add(generation[i]);
		}
		
		for(int i = 0; i < numGenerations; i++) {
			SystemOutput.debugMessage("Start generation " + i);
			evaluator.evaluateGeneration(generation);
			
			double bestScore = Double.NEGATIVE_INFINITY;
			double worstScore = Double.POSITIVE_INFINITY;
			double averageScore = 0.0;
			
			// Evaluate each chromosome in the generation
			for(TestVector chromosome : generation) {
				bestScore = Math.max(bestScore, chromosome.getScore());
				worstScore = Math.min(worstScore, chromosome.getScore());
				averageScore += chromosome.getScore();
				
				String message = chromosome.toString() + ": " + chromosome.getScore();
				SystemOutput.debugMessage(message);
			}
			averageScore /= generation.length;
			SystemOutput.printMessage("Generation " + i + " Best/Average/Worst Scores: " + 
					bestScore + " / " + averageScore + " / " + worstScore);
			
			// Select which chromosomes will be used to generate next generation
			TestVector[] selected = selector.selectVectors(generation,
					(int)(generation.length * selectionRate));
			
			// Generate the next generation
			generation = createNextGeneration(selected, rand);
			
			SystemOutput.debugMessage("End generation " + i);
			
		}
	}
	
	private TestVector[] createNextGeneration(TestVector[] selected, RandomGenerator rand) {
		TestVector[] newGeneration = new TestVector[populationSize];
		
		int startIndex = 0;
		if(seedWithRand) {
			SystemOutput.debugMessage("Seeding with random vector");
			startIndex = 1;
			newGeneration[0] = createTV( rand.generateRandomArray(getVectorLength(),
					getLowerBound(), getUpperBound()) );
		}
		
		for(int i = startIndex; i < newGeneration.length; i++) {
			int[] crossed;
			if(rand.generateRandomDouble() < crossoverRate) {
				TestVector t1 = selected[rand.generateRandomInt(0, selected.length - 1)];
				TestVector t2 = selected[rand.generateRandomInt(0, selected.length - 1)];
				
				crossed = crossover.crossVectors(t1.getVector(), t2.getVector());
			} else {
				crossed = selected[rand.generateRandomInt(0, selected.length - 1)].getVector();
			}
			
			// Mutate crossed vector
			for(int j = 0; j < crossed.length; j++) {
				if(rand.generateRandomDouble() < mutationRate) {
					crossed[j] = mutator.mutateValue(crossed[j],
							getLowerBound(), getUpperBound());
				}
			}
			
			newGeneration[i] = createTV(crossed);
		}
		
		return newGeneration;
	}
	
	/**
	 * Checks to see if test vector is already in list
	 * Creates new Test Vector if not
	 * @param v
	 * @return
	 */
	private TestVector createTV(int[] v) {
		for(TestVector tv : TVs) {
			if(tv.equalsVector(v)) {
				return tv;
			}
		}
		TestVector newVector = new TestVector(v);
		TVs.add(newVector);
		return newVector;
	}
	
	public void setNumGenerations(int numGenerations) {
		this.numGenerations = numGenerations;
	}

	public void setPopulationSize(int populationSize) {
		this.populationSize = populationSize;
	}
	
	public void setMutationRate(double mutationRate) {
		this.mutationRate = mutationRate;
	}

	public void setCrossoverRate(double crossoverRate) {
		this.crossoverRate = crossoverRate;
	}

	public void setSelectionRate(double selectionRate) {
		this.selectionRate = selectionRate;
	}

	public void setSeedWithRand(boolean seedWithRand) {
		this.seedWithRand = seedWithRand;
	}

	public void setEvaluator(GenerationEvaluator evaluator) {
		this.evaluator = evaluator;
	}

	public void setSelector(TVSelector selector) {
		this.selector = selector;
	}

	public void setCrossover(TVCrossover crossover) {
		this.crossover = crossover;
	}

	public void setMutator(Mutator mutator) {
		this.mutator = mutator;
	}
	
	private boolean readyToGenerate() {
		boolean ready = true;
		if(evaluator == null) {
			SystemOutput.errorMessage("Error: no evaluator selected");
			ready = false;
		}
		if(selector == null) {
			SystemOutput.errorMessage("Error: no selector selected");
			ready = false;
		}
		if(crossover == null) {
			SystemOutput.errorMessage("Error: no crossover selected");
			ready = false;
		}
		if(mutator == null) {
			SystemOutput.errorMessage("Error: no mutator selected");
			ready = false;
		}
		return ready;
	}

}
