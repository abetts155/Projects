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
import adam.betts.programs.ProgramReader;
import adam.betts.utilities.DefaultOptions;
import adam.betts.utilities.Globals;

public class MainDisassembler
{

    private static Options options;
    private static Option basicBlocksOption;

    private static String programFileName;
    private static boolean basicBlocks;

    private static void addOptions ()
    {
        options = new Options();
        DefaultOptions.addDefaultOptions(options);
        DefaultOptions.addProgramOption(options);
        DefaultOptions.addRootOption(options, true);
        DefaultOptions.addUDrawDirectoryOption(options);
        DefaultOptions.addOutFileOption(options);

        basicBlocksOption = new Option("B", "basic-blocks", false,
                "Only output basic blocks and neither edges nor call information.");
        basicBlocksOption.setRequired(false);
        options.addOption(basicBlocksOption);
    }

    private static void parseCommandLine (String[] args)
    {
        final String toolName = "disassemble.jar";
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
                DefaultOptions.setRootOption(line);
                DefaultOptions.setUDrawDirectoryOption(line);
                DefaultOptions.setOutFileOption(line);

                basicBlocks = line.hasOption(basicBlocksOption.getOpt());

                programFileName = line
                        .getOptionValue(DefaultOptions.programFileOption
                                .getOpt());
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
        Program program = new Program();
        new ProgramReader(program, programFileName, !basicBlocks);
        new WriteProgram(program, Globals.getOutputFileName(), true);
    }

    public static void main (String[] args)
    {
        addOptions();
        parseCommandLine(args);
        run();
    }
}
