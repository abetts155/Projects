package adam.betts.simplescalar;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;

import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.utilities.Enums.IProfile;

public class TraceOutput
{
	/*
	 * One file handle per instrumentation profile
	 */
	private static HashMap<IProfile, BufferedWriter> files = new HashMap<IProfile, BufferedWriter> ();

	public final static void openFileHandles ()
	{
		String traceFileName = Globals.getTraceFileName ();
		int i = traceFileName.indexOf (".");
		if (i == -1)
		{
			i = 0;
		}

		for (IProfile iprofile: Globals.getInstrumentationProfiles ())
		{
			try
			{
				files.put (iprofile, new BufferedWriter (new FileWriter (
						traceFileName.substring (0, i) + "."
								+ iprofile.toString () + ".txt")));
			}
			catch (IOException e)
			{
				Debug.debugMessage (TraceOutput.class, e.getMessage (), 1);
				System.exit (1);
			}
		}
	}

	public final static void closeFileHandles ()
	{
		for (IProfile iprofile: Globals.getInstrumentationProfiles ())
		{
			try
			{
				files.get (iprofile).close ();
			}
			catch (IOException e)
			{
				Debug.debugMessage (TraceOutput.class, e.getMessage (), 1);
				System.exit (1);
			}
		}
	}

	public final static void writeTuple (IProfile iprofile, long ipointID,
			long time)
	{
		try
		{
			files.get (iprofile).write (ipointID + " " + time + "\n");
		}
		catch (IOException e)
		{
			Debug.debugMessage (TraceOutput.class, e.getMessage (), 1);
			System.exit (1);
		}

	}

	public final static void writeTuple (IProfile iprofile, String ipointID,
			long time)
	{
		try
		{
			files.get (iprofile).write (ipointID + " " + time + "\n");
		}
		catch (IOException e)
		{
			Debug.debugMessage (TraceOutput.class, e.getMessage (), 1);
			System.exit (1);
		}

	}
}
