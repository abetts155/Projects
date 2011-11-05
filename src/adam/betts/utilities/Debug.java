package adam.betts.utilities;

public class Debug
{
	public static final int LOWEST_LEVEL = 0;
	public static final int CONSTRUCTOR_LEVEL = 1;
	public static final int FUNCTION_LEVEL = 2;
	public static final int LOOP_LEVEL_1 = 3;
	public static final int LOOP_LEVEL_2 = 4;
	public static final int LOOP_LEVEL_3 = 5;
	public static final int LOOP_LEVEL_4 = 6;
	public static final int HIGHEST_LEVEL = 7;

	private static boolean verbose;
	private static boolean debug = false;
	private static int debugLevel = CONSTRUCTOR_LEVEL;

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

	public final static void errorMessage (Class <?> c, String message)
	{
		System.err.println ("[" + c.getSimpleName () + "] " + message);
		System.exit (1);
	}

	public final static void debugMessage (Class <?> c, String message, int level)
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
