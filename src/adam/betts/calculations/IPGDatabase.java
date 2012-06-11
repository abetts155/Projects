package adam.betts.calculations;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Stack;

import adam.betts.edges.IPGEdge;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.utils.LeastCommonAncestor;
import adam.betts.outputs.UDrawGraph;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainWCETAnalyser;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.IPGEdgeType;
import adam.betts.utilities.Globals;
import adam.betts.vertices.Ipoint;

public class IPGDatabase extends Database
{

    final int rootID = 1;
    final protected IpointGraph ipg;
    protected HashMap <Integer, Long> observedWCETs = new HashMap <Integer, Long>();
    protected HashMap <Integer, HashMap <Integer, Integer>> unitBounds = new HashMap <Integer, HashMap <Integer, Integer>>();

    public IPGDatabase (final Program program)
    {
        super(program);

        ControlFlowGraph cfg = program.getSubprogram(Globals.getRoot())
                .getCFG();
        ipg = new IpointGraph();
        Ipoint startv = new Ipoint(ipg.getNextVertexID(), cfg.getFirstAddress());
        ipg.addIpoint(startv);
        ipg.setEntryID(startv.getVertexID());

        TraceParser parser = new TraceParser(rootID, ipg, startv);
        initialise(rootID, parser);

        Debug.verboseMessage("Parsing trace file " + Globals.getTraceFileName());
        parser.doParsing(false);

        if (MainWCETAnalyser.Globals.doObservedWCET())
        {
            Debug.verboseMessage("Reparsing for Observed WCETs");
            parser.doParsing(true);
        }
    }

    protected long doWCET ()
    {
        UDrawGraph.makeUDrawFile(Globals.getInstrumentationProfile(), ipg,
                "inlined");
        IPETModelIPG ipet = new IPETModelIPG(this, ipg, rootID);
        return ipet.wcet;
    }

    public final int getBound (int subprogramID, int unitID)
    {
        return unitBounds.get(subprogramID).get(unitID);
    }

    public final long getObservedWCET (int subprogramID)
    {
        return observedWCETs.get(subprogramID);
    }

    private void initialise (int rootID, TraceParser parser)
    {
        unitWCETs.put(rootID, new HashMap <Integer, Long>());
        unitBounds.put(rootID, new HashMap <Integer, Integer>());
        parser.tempUnitBounds.put(rootID, new HashMap <Integer, Integer>());
        parser.properDescendants.put(rootID,
                new HashMap <Integer, HashSet <Integer>>());
        mets.put(rootID, (long) 0);
        observedWCETs.put(rootID, (long) 0);
        tests.put(rootID, (long) 0);
    }

    private class TraceParser
    {

        /*
         * Cached LCA data structures
         */
        private HashMap <Integer, LeastCommonAncestor> lcas = new HashMap <Integer, LeastCommonAncestor>();

        /*
         * Stacks needed during trace parsing
         */
        private Stack <Integer> callStack = new Stack <Integer>();
        private Stack <Ipoint> ipointStack = new Stack <Ipoint>();
        private Stack <Long> metStack = new Stack <Long>();
        private Stack <Long> observedWCETStack = new Stack <Long>();

        /*
         * To keep track of the subprogram and where we are in the trace
         */
        private final int rootID;
        private int subprogramID;
        private final Ipoint startv;
        private Subprogram subprogram;
        private IpointGraph ipg;
        private Ipoint source;
        private Ipoint destination;
        private IPGEdge transition;
        private int edgeID;
        boolean newRun = true;

        private int testCounter = 0;

        /*
         * To keep track of times
         */
        private long observedWCET = 0;
        private long T1 = 0;
        private long T2 = 0;

        protected HashMap <Integer, HashMap <Integer, Integer>> tempUnitBounds = new HashMap <Integer, HashMap <Integer, Integer>>();
        protected HashMap <Integer, HashMap <Integer, HashSet <Integer>>> properDescendants = new HashMap <Integer, HashMap <Integer, HashSet <Integer>>>();

        private TraceParser (int rootID, IpointGraph ipg, Ipoint startv)
        {
            this.rootID = rootID;
            this.ipg = ipg;
            this.startv = startv;
            subprogramID = rootID;
        }

        private void doParsing (boolean doObservedWCET)
        {
            try
            {
                source = startv;

                BufferedReader in = new BufferedReader(new FileReader(
                        Globals.getTraceFileName()));
                String str;
                while ((str = in.readLine()) != null)
                {
                    if (!str.startsWith("//") && !str.startsWith("/*"))
                    {
                        final String[] lexemes = str.split("\\s+");
                        long ipointID;

                        if (lexemes[0].startsWith("0x"))
                        {
                            ipointID = Long.parseLong(lexemes[0].substring(2));
                        }
                        else
                        {
                            ipointID = Long.parseLong(lexemes[0]);
                        }

                        long timeStamp = Long.parseLong(lexemes[1]);

                        Debug.debugMessage(getClass(),
                                "Ipoint = 0x" + Long.toHexString(ipointID)
                                        + ", t = " + timeStamp, 3);

                        if (source.hasTraceSuccessor(ipointID))
                        {
                            if (newRun)
                            {
                                startNewRun(doObservedWCET, ipointID, timeStamp);
                            }
                            else
                            {
                                continueRun(doObservedWCET, ipointID, timeStamp);
                            }
                        }
                        else
                        {
                            Debug.debugMessage(
                                    getClass(),
                                    "Unable to find successor of ipoint "
                                            + source.getVertexID()
                                            + " with ipoint ID 0x"
                                            + Long.toHexString(ipointID), 1);

                            if (ipg.hasIpointID(ipointID) == false)
                            {
                                Ipoint newv = new Ipoint(ipg.getNextVertexID(),
                                        ipointID);
                                ipg.addIpoint(newv);
                            }

                            Ipoint destination = ipg.getVertex(ipointID);
                            int newEdgeID = ipg.getNextEdgeID();

                            source.addSuccessor(destination.getVertexID(),
                                    destination.getIpointID(), newEdgeID,
                                    IPGEdgeType.TRACE_EDGE);
                            destination.addPredecessor(source.getVertexID(),
                                    newEdgeID, IPGEdgeType.TRACE_EDGE);

                            Debug.debugMessage(getClass(), "Adding edge "
                                    + source.getVertexID() + " => "
                                    + destination.getVertexID(), 1);

                            unitWCETs.get(subprogramID)
                                    .put(newEdgeID, (long) 0);
                            unitBounds.get(subprogramID).put(newEdgeID, 0);
                            tempUnitBounds.get(subprogramID).put(newEdgeID, 0);

                            continueRun(doObservedWCET, ipointID, timeStamp);
                        }
                    }
                    else
                    {
                        Debug.debugMessage(getClass(), "New run detected", 1);
                        newRun = true;
                        source = ipg.getVertex(ipg.getEntryID());
                        commitTempBounds();
                    }
                }
            }
            catch (Exception e)
            {
                Debug.errorMessage(getClass(), e.getMessage());
            }

            if (!doObservedWCET)
            {
                long wcet = doWCET();
                System.out.println("WCET at trace #" + testCounter + " = "
                        + wcet);
            }
        }

        private void startNewRun (boolean doObservedWCET, long ipointID,
                long timeStamp)
        {
            source = startv;
            newRun = false;
            testCounter++;

            if (!doObservedWCET)
            {
                if (MainWCETAnalyser.Globals.doIncrementalWCET())
                {
                    Debug.debugMessage(getClass(), "Doing incremental WCET", 1);

                    /*
                     * Only do an incremental WCET computation if we have
                     * processed at least one run
                     */
                    if (testCounter > 1)
                    {
                        long wcet = doWCET();
                        System.out.println("WCET at trace #" + testCounter
                                + " = " + wcet);
                    }
                }

                T1 = timeStamp;
                metStack.push(T1);
                tests.put(subprogramID, tests.get(subprogramID) + 1);
            }
            else
            {
                observedWCET = 0;
            }
        }

        private void continueRun (boolean doObservedWCET, long ipointID,
                long timeStamp)
        {
            transition = source.getTraceSuccessor(ipointID);
            destination = ipg.getVertex(transition.getVertexID());
            edgeID = transition.getEdgeID();

            Debug.debugMessage(getClass(), "Analysing " + source.getVertexID()
                    + " => " + destination.getVertexID(), 1);

            int boundSoFar = tempUnitBounds.get(subprogramID).get(edgeID) + 1;
            tempUnitBounds.get(subprogramID).put(edgeID, boundSoFar);

            if (!doObservedWCET)
            {
                T2 = timeStamp;
                long transitionWCET = T2 - T1;

                if (transitionWCET > unitWCETs.get(subprogramID).get(edgeID))
                {
                    unitWCETs.get(subprogramID).put(edgeID, transitionWCET);
                }

                T1 = T2;
            }
            else
            {
                observedWCET += unitWCETs.get(subprogramID).get(edgeID);
            }

            source = destination;
        }

        private void commitTempBounds ()
        {
            for (int subprogramID : tempUnitBounds.keySet())
            {
                for (int edgeID : tempUnitBounds.get(subprogramID).keySet())
                {
                    int boundOnThisRun = tempUnitBounds.get(subprogramID).get(
                            edgeID);
                    if (boundOnThisRun > unitBounds.get(subprogramID).get(
                            edgeID))
                    {
                        unitBounds.get(subprogramID)
                                .put(edgeID, boundOnThisRun);
                        Debug.debugMessage(getClass(), "Edge " + edgeID
                                + ", bound = " + boundOnThisRun, 1);
                    }

                    tempUnitBounds.get(subprogramID).put(edgeID, 0);
                }
            }
        }
    }

}
