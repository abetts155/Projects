package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.outputs.WriteProgram;
import adam.betts.programs.Program;
import adam.betts.utilities.DefaultOptions;

public class MainProgramBuilder
{

    private static Options options;

    private static String programFileName;

    private static void addOptions ()
    {
        options = new Options();
        DefaultOptions.addDefaultOptions(options);
        DefaultOptions.addProgramOption(options);
        DefaultOptions.addRootOption(options, true);
        DefaultOptions.addUDrawDirectoryOption(options);
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
        Program program = new Program(programFileName);
        new WriteProgram(program, "program.xml");
    }

    public static void main (String[] args)
    {
        addOptions();
        parseCommandLine(args);
        run();
    }
}
