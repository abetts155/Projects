package se.mdh.outputs;

public class Output
{
	public static void outputPadderString (int length, char c)
	{
		outputPadderString (0, length, c);
	}

	public static void outputPadderString (int start, int last, char c)
	{
		for (int i = start; i < last; ++i)
		{
			System.out.print (c);
		}
		System.out.println ("");
	}

	public static String getPadderString (int length, char c)
	{
		return getPadderString (0, length, c);
	}

	public static String getPadderString (int start, int last, char c)
	{
		StringBuffer buffer = new StringBuffer ();
		for (int i = start; i < last; ++i)
		{
			buffer.append (c);
		}
		buffer.append ("\n");
		return buffer.toString ();
	}
}
