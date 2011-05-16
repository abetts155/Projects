package adam.betts.outputs;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import adam.betts.utilities.Globals;
import adam.betts.utilities.Enums.IProfile;

public class WCETOutput
{
	/*
	 * File handles to be written
	 */
	private static BufferedWriter textOut;
	private static BufferedWriter xyPlotOut;

	private static DecimalFormat df = new DecimalFormat ("###.##");
	private final static String subprogramColName = "Subprogram";

	public final static void openFileHandles () throws IOException
	{
		String fileName1 = null;
		String fileName2 = null;

		for (IProfile iprofile: IProfile.values ())
		{
			Pattern pattern = Pattern.compile (".*" + iprofile.toString ()
					+ ".*", Pattern.CASE_INSENSITIVE);
			Matcher fit = pattern.matcher (Globals.getTraceFileName ());
			if (fit.matches ())
			{
				fileName1 = "report." + iprofile.toString () + ".txt";
				fileName2 = "xy." + iprofile.toString () + ".txt";
				break;
			}
		}

		if (fileName1 == null)
		{
			fileName1 = "report.txt";
		}
		if (fileName2 == null)
		{
			fileName2 = "xy.txt";
		}

		textOut = new BufferedWriter (new FileWriter (Globals.getOutputFileName ()));
		xyPlotOut = new BufferedWriter (new FileWriter (fileName2));
	}

	public final static void closeFileHandles () throws IOException
	{
		textOut.close ();
		xyPlotOut.close ();
	}

	public final static void writeSubprogramTableHeader () throws IOException
	{
		textOut.write ("Subprogram \t#Tests \tMET \tWCET \tCoverage\n");
		writeRunDelimiter ();
	}

	public final static void writeTableHeader () throws IOException
	{
		textOut.write ("#Tests \tMET \tWCET\n");
		writeRunDelimiter ();
	}

	public final static void writeTimingData (String subprogramName,
			long tests,
			long met,
			long wcet,
			double coverage) throws IOException
	{
		textOut.write (subprogramName);
		for (int i = 0; i < subprogramColName.length ()
				- subprogramName.length (); ++i)
		{
			textOut.write (" ");
		}
		textOut.write ("\t" + Long.toString (tests) + " \t"
				+ Long.toString (met) + " \t" + Long.toString (wcet) + " \t"
				+ df.format (coverage) + "\n");
	}

	public final static void writeTimingData (long tests, long met, long wcet) throws IOException
	{
		textOut.write (Long.toString (tests) + " \t" + Long.toString (met)
				+ " \t" + Long.toString (wcet) + "\n");

	}

	public final static void writeToGNUPlotFile (long tests,
			long met,
			long wcet_All,
			long wcet_DFS,
			long highestMET) throws IOException
	{
		xyPlotOut.write (Long.toString (tests) + " \t" + Long.toString (met)
				+ " \t" + Long.toString (wcet_All) + "\t "
				+ Long.toString (wcet_DFS) + " \t" + Long.toString (highestMET)
				+ "\n");
	}

	public final static void writeToGNUPlotFile (long tests, long met, long wcet) throws IOException
	{
		xyPlotOut.write (Long.toString (tests) + " \t" + Long.toString (met)
				+ " \t" + Long.toString (wcet) + "\n");
	}

	public final static void writeRunDelimiter () throws IOException
	{
		textOut.write ("------------------------------------------------\n");
	}
}
