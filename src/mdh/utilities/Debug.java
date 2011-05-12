package se.mdh.utilities;

public class Debug
{
	private static boolean verbose;
	private static boolean debug = false;
	private static int debugLevel = 1;
	public static final int LOWEST_DEBUG = 1;
	public static final int HIGHEST_DEBUG = 4;

	public final static void setVerbose (boolean verbose)
	{
		Debug.verbose = verbose;
	}

	public final static void setDebugLevel (int level)
	{
		debug = true;
		debugLevel = level;
	}

	public final static int getDebugLevel ()
	{
		return debugLevel;
	}

	public final static void verboseMessage (String message)
	{
		if (verbose || debug)
		{
			System.err.println ("***** " + message + ". *****");
		}
	}

	public final static void debugMessage (Class<?> c, String message, int level)
	{
		if (debug && level <= debugLevel)
		{
			System.err.println ("[" + c.getSimpleName () + "] " + message);
		}
	}

	public final static void debugMessage (CallBack callBack, int level)
	{
		if (debug && level <= debugLevel)
		{
			callBack.doJob ();
		}
	}
}
