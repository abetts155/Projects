package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.calculations.CalculationEngineCFG;
import adam.betts.calculations.Database;
import adam.betts.programs.Program;
import adam.betts.programs.ProgramReader;
import adam.betts.utilities.Debug;
import adam.betts.utilities.DefaultOptions;

public class MainProgramAnalyser
{

    private static Options options;
    private static Option inlineOption;
    private static Option loopsOption;
    private static Option syntaxTreesOption;
    private static Option controlDependenceOption;
    private static Option dominatorTreesOption;
    private static Option timingAnalysisOption;

    protected static boolean inline;
    protected static boolean LNTs;
    protected static boolean ASTs;
    protected static boolean dominators;
    protected static boolean controlDependences;
    protected static boolean timingAnalysis;
    protected static String programFileName;

    private static void addOptions ()
    {
        options = new Options();
        DefaultOptions.addDefaultOptions(options);
        DefaultOptions.addProgramOption(options);
        DefaultOptions.addUDrawDirectoryOption(options);
        DefaultOptions.addIPEOptions(options);

        inlineOption = new Option("I", "inline", false,
                "Inline all control flow graphs to create a single monolothic graph.");
        inlineOption.setRequired(false);
        options.addOption(inlineOption);

        loopsOption = new Option("l", "loops", false,
                "Generate the loop-nesting trees in the program.");
        loopsOption.setRequired(false);
        options.addOption(loopsOption);

        syntaxTreesOption = new Option("s", "syntax-trees", false,
                "Generate the syntax trees of the control flow graphs.");
        syntaxTreesOption.setRequired(false);
        options.addOption(syntaxTreesOption);

        controlDependenceOption = new Option("C", "control-dependence", false,
                "Generate the control dependence graphs of the control flow graphs.");
        controlDependenceOption.setRequired(false);
        options.addOption(controlDependenceOption);

        dominatorTreesOption = new Option("D", "dominator-trees", false,
                "Generate the dominator trees of the control flow graphs.");
        dominatorTreesOption.setRequired(false);
        options.addOption(dominatorTreesOption);

        timingAnalysisOption = new Option("T", "timing-analysis", false,
                "Generate (random) WCET data for the program and do a WCET computation.");
        timingAnalysisOption.setRequired(false);
        options.addOption(timingAnalysisOption);
    }

    private static void parseCommandLine (String[] args)
    {
        final String toolName = "program-analyser.jar";
        CommandLineParser parser = new GnuParser();
        HelpFormatter formatter = new HelpFormatter();
        formatter.setWidth(128);
        CommandLine line = null;
        try
        {
            line = parser.parse(options, args);

            if (line.hasOption(DefaultOptions.helpOption.getOpt()))
            {
                formatter.printHelp(toolName, options);
                System.exit(1);
            }
            else
            {
                DefaultOptions.setDefaultOptions(line);
                DefaultOptions.setUDrawDirectoryOption(line);
                DefaultOptions.setIPETOptions(line);

                programFileName = line
                        .getOptionValue(DefaultOptions.programFileOption
                                .getOpt());

                inline = line.hasOption(inlineOption.getOpt());
                LNTs = line.hasOption(loopsOption.getOpt());
                ASTs = line.hasOption(syntaxTreesOption.getOpt());
                dominators = line.hasOption(dominatorTreesOption.getOpt());
                controlDependences = line.hasOption(controlDependenceOption
                        .getOpt());
                timingAnalysis = line.hasOption(timingAnalysisOption.getOpt());
            }
        }
        catch (ParseException e)
        {
            System.out.println(e.getMessage());
            formatter.printHelp(toolName, options);
            System.exit(1);
        }
    }

    private static void run ()
    {
        Debug.verboseMessage("Reading program");

        Program program = new Program();
        new ProgramReader(program, programFileName, true);

        if (LNTs)
        {
            Debug.verboseMessage("Building loop-nesting trees");
            program.buildLNTs();
        }

        if (ASTs)
        {
            Debug.verboseMessage("Building abstract syntax trees");
            program.buildSyntaxTrees();
        }

        if (dominators)
        {
            Debug.verboseMessage("Building pre- and post-dominator trees");
            program.buildDominatorTrees();
        }

        if (controlDependences)
        {
            Debug.verboseMessage("Building control dependence graphs");
            program.buildControlDependenceGraphs();
        }

        if (timingAnalysis)
        {
            Debug.verboseMessage("Doing timing analysis");
            Database database = new Database(program);
            database.generateData(false);
            CalculationEngineCFG calc = new CalculationEngineCFG(program,
                    database);
            calc.printResults();
        }
    }

    public static void main (String[] args)
    {
        addOptions();
        parseCommandLine(args);
        run();
    }
}
