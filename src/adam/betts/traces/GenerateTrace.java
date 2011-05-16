package adam.betts.traces;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;
import java.util.Stack;

import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.edges.IPGEdge;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.instructions.Instruction;
import adam.betts.outputs.UDrawGraph;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainTraceGenerator;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.vertices.BasicBlock;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;

public class GenerateTrace
{
	protected Program program;
	protected CallGraph callg;
	protected Stack<Integer> callStack = new Stack<Integer> ();
	protected Stack<Vertex> returnStack = new Stack<Vertex> ();
	protected Random random = new Random ();
	protected HashMap<Integer, LoopNests> subprogramToLNT = new HashMap<Integer, LoopNests> ();
	protected HashMap<Integer, HashMap<Integer, Integer>> subprogramToExecutionCounts = new HashMap<Integer, HashMap<Integer, Integer>> ();

	public GenerateTrace (Program program)
	{
		this.program = program;
		this.callg = program.getCallGraph ();

		initialise ();

		if (MainTraceGenerator.Globals.addressTrace ())
		{
			generateAddressTrace ();
		}
		else
		{
			program.insertVirtualIpoints ();
			program.buildIPGS (false);
			generateTimingTrace ();
		}
	}

	private void initialise ()
	{
		for (Subprogram subprogram: program)
		{
			int subprogramID = subprogram.getSubprogramID ();
			subprogramToExecutionCounts.put (subprogramID,
					new HashMap<Integer, Integer> ());

			ControlFlowGraph cfg = subprogram.getCFG ();
			cfg.addEntryAndExitEdges ();
			Debug.debugMessage (getClass (), "Building LNT of "
					+ subprogram.getSubprogramName (), 3);
			LoopNests lnt = new LoopNests (cfg, cfg.getEntryID ());
			subprogramToLNT.put (subprogramID, lnt);

			if (Globals.uDrawDirectorySet ())
			{
				UDrawGraph.makeUDrawFile (lnt, subprogram.getSubprogramName ());
			}

			for (Vertex v: lnt)
			{
				int vertexID = v.getVertexID ();
				if (lnt.isLoopHeader (vertexID))
				{
					subprogramToExecutionCounts.get (subprogramID).put (
							vertexID, 0);
				}
			}
		}
	}

	private void generateAddressTrace ()
	{
		try
		{
			BufferedWriter out = new BufferedWriter (new FileWriter (
					adam.betts.utilities.Globals.getOutputFileName ()));

			for (long i = 1; i <= MainTraceGenerator.Globals.getNumberOfRuns (); ++i)
			{
				Debug.debugMessage (getClass (), "Run #" + i, 3);

				boolean finished = false;
				final int loopHeaderBudget = 2;
				int subprogramID = program.getRootID ();
				ControlFlowGraph cfg = program.getSubprogram (subprogramID)
						.getCFG ();
				int vertexID = cfg.getEntryID ();
				BasicBlock bb = cfg.getBasicBlock (vertexID);

				Debug.debugMessage (getClass (), "In subprogram "
						+ program.getSubprogramName (subprogramID), 3);

				while (!finished)
				{
					Debug.debugMessage (getClass (), "At basic block "
							+ vertexID + " in "
							+ program.getSubprogramName (subprogramID), 4);

					/*
					 * Output all the addresses inside the basic block
					 */
					Iterator<Instruction> instrIt = bb.instructionIterator ();
					while (instrIt.hasNext ())
					{
						Instruction instr = instrIt.next ();
						out
								.write ("0x"
										+ Long
												.toHexString (instr
														.getAddress ()) + "\n");
					}

					int calleeID = callg.isCallSite (subprogramID, vertexID);

					/*
					 * If this basic block is a call site then make the function
					 * call
					 */
					if (calleeID != Vertex.DUMMY_VERTEX_ID)
					{
						callStack.push (subprogramID);
						returnStack.push (bb);
						subprogramID = calleeID;
						cfg = program.getSubprogram (calleeID).getCFG ();
						vertexID = cfg.getEntryID ();
						bb = cfg.getBasicBlock (vertexID);

						Debug.debugMessage (getClass (), "Calling subprogram "
								+ program.getSubprogramName (subprogramID), 3);
					}
					/*
					 * If this basic block is a return vertex of the CFG, unwind
					 * the stack or reset to the root
					 */
					else if (cfg.isExit (vertexID))
					{
						if (subprogramID == program.getRootID ())
						{
							finished = true;
						}
						else
						{
							subprogramID = callStack.pop ();
							cfg = program.getSubprogram (subprogramID)
									.getCFG ();
							bb = (BasicBlock) returnStack.pop ();
							Edge e = bb.getNthSuccessor (0);
							vertexID = e.getVertexID ();
							bb = cfg.getBasicBlock (vertexID);

							Debug
									.debugMessage (
											getClass (),
											"Returning to subprogram "
													+ program
															.getSubprogramName (subprogramID),
											3);
						}
					}
					/*
					 * Otherwise, arbitrarily select a successor
					 */
					else
					{
						int numOfSucc = bb.numOfSuccessors ();
						int nthSucc = 0;
						if (numOfSucc > 1)
						{
							LoopNests lnt = subprogramToLNT.get (subprogramID);
							boolean ok = false;

							while (!ok)
							{
								ok = true;
								nthSucc = random.nextInt (numOfSucc);
								Edge e = bb.getNthSuccessor (nthSucc);
								int succID = e.getVertexID ();

								if (lnt.isLoopHeader (succID))
								{
									int count = subprogramToExecutionCounts
											.get (subprogramID).get (succID);

									if (lnt.isLoopTail (succID, vertexID))
									{
										if (count > loopHeaderBudget)
										{
											ok = false;
											Debug
													.debugMessage (
															getClass (),
															"Execution count budget for "
																	+ succID
																	+ " in "
																	+ program
																			.getSubprogramName (subprogramID)
																	+ " exceeded",
															4);
										}
									}

									subprogramToExecutionCounts.get (
											subprogramID).put (succID,
											count + 1);
								}
								else if (lnt.isLoopHeader (vertexID))
								{
									int count = subprogramToExecutionCounts
											.get (subprogramID).get (vertexID);

									if (lnt.inLoopBody (vertexID, succID)
											&& count > loopHeaderBudget)
									{
										ok = false;
									}

									subprogramToExecutionCounts.get (
											subprogramID).put (vertexID,
											count + 1);
								}
							}
						}

						Debug.debugMessage (getClass (), "Selecting successor "
								+ nthSucc, 4);
						Edge e = bb.getNthSuccessor (nthSucc);
						vertexID = e.getVertexID ();
						bb = cfg.getBasicBlock (vertexID);
					}

				}
			}

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}

	private void generateTimingTrace ()
	{
		try
		{
			BufferedWriter out = new BufferedWriter (new FileWriter (
					adam.betts.utilities.Globals.getOutputFileName ()));

			for (int i = 1; i <= MainTraceGenerator.Globals.getNumberOfRuns (); ++i)
			{
				long timestamp = 0;
				boolean finished = false;

				int subprogramID = program.getRootID ();

				LoopNests lnt = program.getSubprogram (subprogramID)
						.getCFGStar (Globals.getInstrumentationProfile ())
						.getLNT ();

				IpointGraph ipg = program.getSubprogram (subprogramID).getIPG (
						Globals.getInstrumentationProfile ());

				HashMap<Integer, Integer> edgeCounts = new HashMap<Integer, Integer> ();

				Ipoint v = ipg.getVertex (ipg.getEntryID ());

				while (!finished)
				{
					int vertexID = v.getVertexID ();

					if (vertexID == ipg.getEntryID ())
					{
						Edge succEdge = v.getNthSuccessor (0);
						v = ipg.getVertex (succEdge.getVertexID ());
					}
					else if (vertexID == ipg.getExitID ())
					{
						finished = true;
					}
					else
					{
						timestamp += random.nextInt (50) + 1;

						out.write (Long.toString (v.getIpointID ()) + " "
								+ Long.toString (timestamp) + "\n");

						if (v.numOfSuccessors () > 1)
						{
							IPGEdge iterationEdge = null;
							IPGEdge exitEdge = null;

							Iterator<Edge> succIt = v.successorIterator ();
							while (succIt.hasNext ())
							{
								IPGEdge succEdge = (IPGEdge) succIt.next ();
								if (succEdge.isIterationEdge ())
								{
									iterationEdge = succEdge;
								}
								else if (succEdge.isExitEdge ())
								{
									exitEdge = succEdge;
								}
							}

							if (iterationEdge == null && exitEdge == null)
							{
								int nthSucc = random.nextInt (v
										.numOfSuccessors ());
								FlowEdge succEdge = (FlowEdge) v
										.getNthSuccessor (nthSucc);
								vertexID = succEdge.getVertexID ();
								v = ipg.getVertex (vertexID);
							}
							else
							{
								if (iterationEdge != null)
								{
									int edgeID = iterationEdge.getEdgeID ();
									if (!edgeCounts.containsKey (edgeID))
									{
										edgeCounts.put (edgeID, 1);
									}

									int bound = 2;

									if (edgeCounts.get (edgeID) < bound)
									{
										edgeCounts.put (edgeID, edgeCounts
												.get (edgeID) + 1);
										vertexID = iterationEdge.getVertexID ();
										v = ipg.getVertex (vertexID);
									}
									else
									{
										edgeCounts.put (iterationEdge
												.getEdgeID (), 1);
										vertexID = exitEdge.getVertexID ();
										v = ipg.getVertex (vertexID);
									}
								}
							}
						}
						else
						{
							int nthSucc = random.nextInt (v.numOfSuccessors ());
							FlowEdge succEdge = (FlowEdge) v
									.getNthSuccessor (nthSucc);
							vertexID = succEdge.getVertexID ();
							v = ipg.getVertex (vertexID);
						}

					}
				}
			}

			out.close ();
		}
		catch (IOException e)
		{
			System.err.println ("Error: " + e.getMessage ());
			System.exit (1);
		}
	}
}
