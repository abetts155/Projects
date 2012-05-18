package adam.betts.tools;

import java.io.IOException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.calculations.DatabaseWithoutProgram;
import adam.betts.outputs.WCETOutput;
import adam.betts.utilities.DefaultOptions;

public class MainTraceParser
{

    private static Options options;
    private static Option incrementalWCETOption;

    protected static boolean incrementalWCET;

    public static void main (String[] args)
    {
        addOptions();
        parseCommandLine(args);
        run();
    }

    private static void addOptions ()
    {
        options = new Options();
        DefaultOptions.addDefaultOptions(options);
        DefaultOptions.addUDrawDirectoryOption(options);
        DefaultOptions.addTraceFileOption(options);
        DefaultOptions.addIPETOptions(options);

        incrementalWCETOption = new Option("I", "incremental", false,
                "Do a WCET computation after each run in the trace.");
        incrementalWCETOption.setRequired(false);
        options.addOption(incrementalWCETOption);
    }

    private static void parseCommandLine (String[] args)
    {
        final String toolName = "trace-parser.jar";
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
                DefaultOptions.setTraceFileOption(line);
                DefaultOptions.setIPETOptions(line);

                MainTraceParser.incrementalWCET = line
                        .hasOption(incrementalWCETOption.getOpt());
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
        try
        {
            WCETOutput.openFileHandles();
            WCETOutput.writeTableHeader();
            new DatabaseWithoutProgram();
            WCETOutput.closeFileHandles();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }

    public final static boolean doIncrementalWCET ()
    {
        return incrementalWCET;
    }
}
