package se.mdh.graphs.utils;

import java.util.ArrayList;

public class BracketList
{
	protected ArrayList<Integer> brackets = new ArrayList<Integer> ();

	public BracketList ()
	{
	}

	public final int size ()
	{
		return brackets.size ();
	}

	public final void push (Integer edgeID)
	{
		brackets.add (brackets.size () - 1, edgeID);
	}

	public final int top ()
	{
		return brackets.get (brackets.size () - 1);
	}

	public final void delete (Integer edgeID)
	{
		brackets.remove (edgeID);
	}

	public final void concatenate (BracketList bracketList)
	{
		brackets.addAll (bracketList.brackets);
	}
}
