package adam.betts.calculations;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import adam.betts.edges.FlowEdge;
import adam.betts.edges.IPGEdge;
import adam.betts.graphs.IpointGraph;
import adam.betts.outputs.UDrawGraph;
import adam.betts.outputs.WCETOutput;
import adam.betts.tools.MainTraceParser;
import adam.betts.utilities.CallBack;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.utilities.Enums.IPGEdgeType;
import adam.betts.utilities.Enums.IProfile;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;

public class DatabaseWithoutProgram
{

    protected IpointGraph ipg = new IpointGraph();
    protected int nextVertexID = Vertex.FIRST_VERTEX_ID;
    protected int nextEdgeID = FlowEdge.FIRST_EDGE_ID;
    protected long newRunDelimiter;
    protected Ipoint startv;
    protected HashMap <Integer, Integer> edgeCounts = new HashMap <Integer, Integer>();
    protected HashMap <Integer, Integer> temporaryEdgeCounts = new HashMap <Integer, Integer>();
    protected HashMap <Integer, Long> edgeToMin = new HashMap <Integer, Long>();
    protected HashMap <Integer, Long> edgeToMax = new HashMap <Integer, Long>();
    protected HashMap <Long, Ipoint> ipointIDToIpoint = new HashMap <Long, Ipoint>();
    protected long highestMET = 0;
    protected long lowestMET = Long.MAX_VALUE;
    protected long testCounter;

    /*
     * Variables used to extract matrices and vectors for model identification
     */
    protected HashMap <Long, HashMap <Integer, Integer>> runToEdgeCounts = new HashMap <Long, HashMap <Integer, Integer>>();
    protected HashMap <Long, Long> runToMET = new HashMap <Long, Long>();
    protected HashMap <Long, Long> runToWCET_All = new HashMap <Long, Long>();
    protected HashMap <Long, Long> runToWCET_DFS = new HashMap <Long, Long>();

    public DatabaseWithoutProgram ()
    {
        setNewRunDelimiter();
        testCounter = 0;
        parseRandomTrace();
        enforceSingleExit();
        writeTimingDataFiles();

        for (IProfile iprofile : IProfile.values())
        {
            Pattern pattern = Pattern
                    .compile(".*" + iprofile.toString() + ".*",
                            Pattern.CASE_INSENSITIVE);
            Matcher fit = pattern.matcher(Globals.getTraceFileName());
            if (fit.matches())
            {
                UDrawGraph.makeUDrawFile(iprofile, ipg, "parsedIPG");
            }
        }

        Debug.debugMessage(new CallBack()
        {

            public void doJob ()
            {
                Debug.debugMessage(DatabaseWithoutProgram.class, "MET = "
                        + highestMET, 4);

                for (int edgeID : edgeCounts.keySet())
                {
                    Debug.debugMessage(DatabaseWithoutProgram.class,
                            "Edge " + edgeID + " has execution count = "
                                    + edgeCounts.get(edgeID), 4);
                }

                for (int edgeID : edgeToMax.keySet())
                {
                    Debug.debugMessage(DatabaseWithoutProgram.class, "Edge "
                            + edgeID + " has WCET = " + edgeToMax.get(edgeID),
                            4);
                }
            }
        }, 4);
    }

    public final IpointGraph getIPG ()
    {
        return ipg;
    }

    public final long getTests ()
    {
        return testCounter;
    }

    public final long getEdgeWCET (int edgeID)
    {
        return edgeToMax.get(edgeID);
    }

    public final int getEdgeCount (int edgeID)
    {
        return edgeCounts.get(edgeID);
    }

    public final long getMinMET ()
    {
        return lowestMET;
    }

    public final long geMaxMET ()
    {
        return highestMET;
    }

    private void setNewRunDelimiter ()
    {
        try
        {
            RandomAccessFile raf = new RandomAccessFile(
                    Globals.getTraceFileName(), "r");
            String str;

            /*
             * Loop past any comments in the trace file
             */
            while ((str = raf.readLine()).startsWith("//"))
            {
                ;
            }

            final String[] lexemes = str.split("\\s+");
            if (lexemes[0].startsWith("0x"))
            {
                newRunDelimiter = Long.parseLong(lexemes[0].substring(2));
            }
            else
            {
                newRunDelimiter = Long.parseLong(lexemes[0]);
            }

            startv = new Ipoint(nextVertexID, newRunDelimiter);
            nextVertexID++;
            ipg.addIpoint(startv);
            ipg.setEntryID(startv.getVertexID());
            ipointIDToIpoint.put(newRunDelimiter, startv);

            Debug.debugMessage(getClass(),
                    "Start ipoint is " + startv.getVertexID()
                            + " with ipoint id = " + newRunDelimiter, 3);
        }
        catch (Exception e)
        {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void parseRandomTrace ()
    {
        try
        {
            Ipoint source = startv;
            Ipoint destination;
            long T1 = 0;
            long T2 = 0;
            long startOfRun = 0;

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

                    Debug.debugMessage(getClass(), "Ipoint = " + ipointID
                            + ", t = " + timeStamp, 3);

                    if (ipointID == startv.getIpointID())
                    {
                        Debug.debugMessage(getClass(),
                                "=== New run detected @ " + timeStamp + " ===",
                                4);

                        if (testCounter > 0)
                        {
                            /*
                             * Only commit the edge counts if we have seen at
                             * least one run
                             */
                            commitEdgeCounts();
                            setMETs(T2 - startOfRun);

                            if (MainTraceParser.doIncrementalWCET())
                            {
                                doWCETComputation();
                            }
                        }
                        testCounter++;

                        source = startv;
                        startOfRun = timeStamp;
                        T1 = timeStamp;
                    }
                    else
                    {
                        if (!source.hasTraceSuccessor(ipointID))
                        {
                            if (!ipointIDToIpoint.containsKey(ipointID))
                            {
                                destination = new Ipoint(nextVertexID, ipointID);
                                nextVertexID++;
                                ipg.addIpoint(destination);
                                Debug.debugMessage(
                                        getClass(),
                                        "Adding new ipoint with id " + ipointID,
                                        4);

                                source.addSuccessor(destination.getVertexID(),
                                        ipointID, nextEdgeID,
                                        IPGEdgeType.TRACE_EDGE);
                                destination.addPredecessor(
                                        source.getVertexID(), nextEdgeID,
                                        IPGEdgeType.TRACE_EDGE);
                                ipointIDToIpoint.put(ipointID, destination);
                                nextEdgeID++;

                                Debug.debugMessage(getClass(), "Adding edge "
                                        + source.getVertexID() + " => "
                                        + destination.getVertexID(), 4);
                            }
                            else
                            {
                                destination = ipointIDToIpoint.get(ipointID);
                                source.addSuccessor(destination.getVertexID(),
                                        ipointID, nextEdgeID,
                                        IPGEdgeType.TRACE_EDGE);
                                destination.addPredecessor(
                                        source.getVertexID(), nextEdgeID,
                                        IPGEdgeType.TRACE_EDGE);
                                nextEdgeID++;

                                Debug.debugMessage(getClass(), "Adding edge "
                                        + source.getVertexID() + " => "
                                        + destination.getVertexID(), 4);
                            }
                        }
                        else
                        {
                            IPGEdge transition = source
                                    .getTraceSuccessor(ipointID);
                            destination = ipg.getVertex(transition
                                    .getVertexID());
                        }

                        T2 = timeStamp;
                        long transitionWCET = T2 - T1;

                        IPGEdge transition = source.getTraceSuccessor(ipointID);
                        int edgeID = transition.getEdgeID();

                        Debug.debugMessage(getClass(),
                                "Edge " + source.getVertexID() + " => "
                                        + destination.getVertexID()
                                        + " (edge id = " + edgeID
                                        + ") traversed", 4);

                        if (edgeToMax.containsKey(edgeID))
                        {
                            if (transitionWCET < edgeToMin.get(edgeID))
                            {
                                edgeToMin.put(edgeID, transitionWCET);
                            }

                            if (transitionWCET > edgeToMax.get(edgeID))
                            {
                                edgeToMax.put(edgeID, transitionWCET);
                            }
                        }
                        else
                        {
                            edgeToMin.put(edgeID, transitionWCET);
                            edgeToMax.put(edgeID, transitionWCET);
                        }

                        if (temporaryEdgeCounts.containsKey(edgeID))
                        {
                            int edgeCount = temporaryEdgeCounts.get(edgeID);
                            temporaryEdgeCounts.put(edgeID, edgeCount + 1);
                        }
                        else
                        {
                            temporaryEdgeCounts.put(edgeID, 1);
                        }

                        /*
                         * Advance the transition
                         */
                        T1 = T2;
                        source = destination;
                    }
                }
                else
                {
                    Debug.debugMessage(getClass(), str, 4);
                }
            }

            /*
             * The entire file has now been parsed
             */
            commitEdgeCounts();
            doWCETComputation();
            setMETs(T2 - startOfRun);
        }
        catch (Exception e)
        {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void commitEdgeCounts ()
    {
        runToEdgeCounts.put(testCounter, new HashMap <Integer, Integer>());

        for (int edgeID : temporaryEdgeCounts.keySet())
        {
            int edgeCount = temporaryEdgeCounts.get(edgeID);
            runToEdgeCounts.get(testCounter).put(edgeID, edgeCount);

            if (edgeCounts.containsKey(edgeID))
            {
                if (edgeCount > edgeCounts.get(edgeID))
                {
                    edgeCounts.put(edgeID, edgeCount);
                }
            }
            else
            {
                edgeCounts.put(edgeID, edgeCount);
            }

            temporaryEdgeCounts.put(edgeID, 0);
        }
    }

    private void setMETs (long time)
    {
        runToMET.put(testCounter, time);

        if (time > highestMET)
        {
            highestMET = time;
            Debug.debugMessage(getClass(), "Max MET = " + highestMET, 4);
        }
        if (time < lowestMET)
        {
            lowestMET = time;
            Debug.debugMessage(getClass(), "Min MET = " + lowestMET, 4);
        }
    }

    private void doWCETComputation ()
    {
    }

    private void enforceSingleExit ()
    {
        ArrayList <Integer> noSuccs = new ArrayList <Integer>();
        for (Vertex v : ipg)

        {
            if (v.numOfSuccessors() == 0)
            {
                noSuccs.add(v.getVertexID());
            }
        }

        if (noSuccs.size() == 1)
        {
            int vertexID = noSuccs.get(noSuccs.size() - 1);
            ipg.setExitID(vertexID);
        }
        else
        {
            Ipoint exitv = new Ipoint(nextVertexID, Ipoint.GHOST_IPOINT_ID);
            ipg.addIpoint(exitv);
            ipg.setExitID(exitv.getVertexID());

            for (int vertexID : noSuccs)
            {
                Ipoint u = ipg.getVertex(vertexID);
                u.addSuccessor(exitv.getVertexID(), Ipoint.GHOST_IPOINT_ID,
                        nextEdgeID, IPGEdgeType.GHOST_EDGE);
                exitv.addPredecessor(vertexID, nextEdgeID,
                        IPGEdgeType.GHOST_EDGE);
                nextEdgeID++;
            }
        }
    }

    private void writeTimingDataFiles ()
    {
        for (long test : new TreeSet <Long>(runToMET.keySet()))
        {
            try
            {
                WCETOutput.writeRunDelimiter();
                WCETOutput.writeToGNUPlotFile(test, runToMET.get(test),
                        runToWCET_All.get(test), runToWCET_DFS.get(test),
                        highestMET);
            }
            catch (IOException e)
            {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }
}
