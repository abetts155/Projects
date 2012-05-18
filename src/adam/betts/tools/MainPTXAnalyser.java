package adam.betts.tools;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import adam.betts.programs.Program;
import adam.betts.utilities.DefaultOptions;

public class MainPTXAnalyser
{

    private static Options options;
    private static Option ptxFileOption;

    private static String ptxFileName;

    private static void addOptions ()
    {
        options = new Options();

        DefaultOptions.addDefaultOptions(options);
        DefaultOptions.addUDrawDirectoryOption(options);

        ptxFileOption = new Option("p", "ptx", true,
                "File containing PTX code.");
        ptxFileOption.setRequired(true);
        options.addOption(ptxFileOption);
    }

    private static void parseCommandLine (String[] args)
    {
        final String toolName = "ptx-analyser.jar";
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

                ptxFileName = line
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
        new Program(ptxFileName);
    }

    public static void main (String[] args)
    {
        addOptions();
        parseCommandLine(args);
        run();
    }
}
