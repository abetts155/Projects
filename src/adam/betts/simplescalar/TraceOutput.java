package adam.betts.simplescalar;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;

import adam.betts.tools.MainSimpleScalarTraceParser;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.utilities.Enums.IProfile;

public class TraceOutput
{
	/*
	 * One file handle per instrumentation profile
	 */
	private static HashMap <IProfile, BufferedWriter> files = new HashMap <IProfile, BufferedWriter> ();

	public final static void openFileHandles ()
	{
		String traceFileName = Globals.getTraceFileName ();
		int i = traceFileName.indexOf (".");
		if (i == -1)
		{
			i = 0;
		}

		for (IProfile iprofile : Globals.getInstrumentationProfiles ())
		{
			try
			{
				files.put (iprofile, new BufferedWriter (new FileWriter (traceFileName.substring (
						0, i)
						+ "." + iprofile.toString () + ".txt")));
			} catch (IOException e)
			{
				Debug.errorMessage (TraceOutput.class, e.getMessage ());
			}
		}
	}

	public final static void closeFileHandles ()
	{
		for (IProfile iprofile : Globals.getInstrumentationProfiles ())
		{
			try
			{
				files.get (iprofile).close ();
			} catch (IOException e)
			{
				Debug.errorMessage (TraceOutput.class, e.getMessage ());
			}
		}
	}

	public final static void writeTuple (IProfile iprofile, long ipointID, long time)
	{
		try
		{
			BufferedWriter out = files.get (iprofile);

			out.write (Long.toString (ipointID));
			if (MainSimpleScalarTraceParser.outputTimestamps () == false)
			{
				out.write (" " + time);
			}
			out.write ("\n");
		} catch (IOException e)
		{
			Debug.errorMessage (TraceOutput.class, e.getMessage ());
		}

	}

	public final static void writeTuple (IProfile iprofile, String ipointID, long time)
	{
		try
		{
			BufferedWriter out = files.get (iprofile);

			out.write (ipointID);
			if (MainSimpleScalarTraceParser.outputTimestamps () == false)
			{
				out.write (" " + time);
			}
			out.write ("\n");
		} catch (IOException e)
		{
			Debug.errorMessage (TraceOutput.class, e.getMessage ());
		}

	}
}
