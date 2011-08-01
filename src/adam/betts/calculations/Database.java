package adam.betts.calculations;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Random;

import adam.betts.edges.Edge;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.outputs.OutputGraph;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.vertices.Vertex;

public class Database
{
	/*
	 * The program used during trace parsing to populate the database
	 */
	protected Program program;

	/*
	 * The calculation engine used after parsing
	 */
	protected CalculationEngine engine;

	/*
	 * The WCETs of the units of computation: either edges or vertices in the
	 * graph
	 */
	protected HashMap<Integer, HashMap<Integer, Long>> unitWCETs = new HashMap<Integer, HashMap<Integer, Long>> ();

	/*
	 * Loop bounds for each subprogram
	 */
	protected HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> loopBounds = new HashMap<Integer, HashMap<Integer, HashMap<Integer, Integer>>> ();

	/*
	 *	The bounds set for each proper ancestor 
	 */
	protected HashMap<Integer, Integer> properAncestorBounds = new HashMap<Integer, Integer> ();
	
	/*
	 * The bounds of each header
	 */
	protected HashMap<Integer, HashMap<Integer, Integer>> headerBounds = new HashMap<Integer, HashMap<Integer, Integer>> ();
	
	/*
	 * Infeasible path data for each subprogram
	 */
	protected HashMap<Integer, HashMap<Integer, HashSet<Integer>>> observedPaths = new HashMap<Integer, HashMap<Integer, HashSet<Integer>>> ();

	/*
	 * To record number of tests for each subprogram
	 */
	protected HashMap<Integer, Long> tests = new HashMap<Integer, Long> ();

	/*
	 * METs of each subprogram
	 */
	protected HashMap<Integer, Long> mets = new HashMap<Integer, Long> ();

	Random random = new Random (0);
	
	public Database (final Program program)
	{
		this.program = program;
	}
	
	public Database (Program program, boolean gen)
	{
		for (Subprogram s: program) 
		{
			int subprogramID = s.getSubprogramID ();
			unitWCETs.put (subprogramID, new HashMap<Integer, Long> ());
			ControlFlowGraph cfg = s.getCFG();
			for (Vertex v : cfg)
			{
				long data = Math.abs(random.nextLong ());
				unitWCETs.get(subprogramID).put (v.getVertexID (), data);
				//System.out.println(unitWCETs.get(subprogramID).get(v.getVertexID()));
			}	
		}
	}
	
	public Database (Program program, int n)
	{
		for (Subprogram s : program)
		{
			int subprogramID = s.getSubprogramID ();		
			LoopNests loop = s.getCFG ().getLNT ();
			
			OutputGraph.output(loop);
			
			Iterator<Integer> it = loop.headerIterator ();
			while (it.hasNext ())
			{
				int headerID = it.next ();
				ArrayList<Integer> properAncestors = getProperAncestors (loop, headerID);
				
				System.out.println ("headerID: " + headerID);
				for (int x : properAncestors)
				{
					System.out.print (x + " ");
				}
				
				Iterator<Integer> listIterator = properAncestors.listIterator ();
				
				int properAncestor = listIterator.next ();
				int bound = Math.abs (random.nextInt ());
				properAncestorBounds.put (properAncestor, bound);	
				
				while (listIterator.hasNext ())
				{
					int nextProperAncestor = listIterator.next ();
					int nextBound = random.nextInt () + bound;
					properAncestorBounds.put (nextProperAncestor, nextBound);		
					properAncestor = nextProperAncestor;
					bound = nextBound;
				}
				headerBounds.put(headerID, properAncestorBounds);
			}
			loopBounds.put (subprogramID, headerBounds);
		}
	}

	private final ArrayList<Integer> getProperAncestors (LoopNests loop, int headerID)
	{
		ArrayList<Integer> properAncestors = new ArrayList<Integer> ();
		int parentID = loop.getVertex (headerID).getParentID ();
		properAncestors.add (parentID);
		int rootID = loop.getRootID ();
		
		while (parentID != rootID)
		{
			int nextParentID = loop.getVertex(parentID).getParentID();
			properAncestors.add (nextParentID);
			parentID = nextParentID;
		}
		
		return properAncestors;
	}
	
	public final long getUnitWCET (int subprogramID, int unitID)
	{
		return unitWCETs.get (subprogramID).get (unitID);
	}

	public final int getLoopBound (int subprogramID,
			int headerID,
			int ancestorID)
	{
		return loopBounds.get (subprogramID).get (headerID).get (ancestorID);
	}

	public Set<Integer> getInfeasibleUnits (int subprogramID, int unitID)
	{
		Set<Integer> infeasible = new HashSet<Integer> ();
		for (int ID: unitWCETs.get (subprogramID).keySet ())
		{
			/*
			 * Add the vertex or edge ID to the set of infeasible units only if
			 * it was not seen during its execution
			 */
			if (!observedPaths.get (subprogramID).get (unitID).contains (ID))
			{
				infeasible.add (ID);
			}
		}
		return infeasible;
	}

	public final int unitsCovered (int subprogramID)
	{
		int count = 0;
		for (int edgeID: unitWCETs.get (subprogramID).keySet ())
		{
			if (unitWCETs.get (subprogramID).get (edgeID) != 0)
			{
				count++;
			}
		}
		return count;
	}

	public final boolean isCovered (int subprogramID, int unitID)
	{
		return unitWCETs.get (subprogramID).get (unitID) > 0;
	}

	public final long getTests (int subprogramID)
	{
		return tests.get (subprogramID);
	}

	public final long getMET (int subprogramID)
	{
		return mets.get (subprogramID);
	}
}
