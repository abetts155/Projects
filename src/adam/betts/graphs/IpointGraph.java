package adam.betts.graphs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.edges.IPGEdge;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.utilities.CallBack;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.IPGEdgeType;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class IpointGraph extends FlowGraph
{

    protected CFGStar cfgStar;
    protected LoopNests lnt;
    protected HashMap <Integer, Integer> edgeToSource = new LinkedHashMap <Integer, Integer>();
    protected HashMap <Integer, Integer> edgeToDestination = new LinkedHashMap <Integer, Integer>();

    public IpointGraph (CFGStar cfgStar, LoopNests lnt)
    {
        this.cfgStar = cfgStar;
        this.lnt = lnt;

        Debug.debugMessage(getClass(), "Initialising", 2);
        addIpoints();
        final AuxiliaryData auxiliaryData = new AuxiliaryData();
        initialise(auxiliaryData);

        Debug.debugMessage(getClass(), "Computing topological sorts", 2);
        Set <Integer> visited = new HashSet <Integer>();
        doDFS(auxiliaryData, visited, cfgStar.getEntryID());

        Debug.debugMessage(new CallBack()
        {

            public void doJob ()
            {
                for (int headerID : auxiliaryData.hToTopSorts.keySet())
                {
                    Debug.debugMessage(IpointGraph.class, "topsort(" + headerID
                            + ") = " + auxiliaryData.hToTopSorts.get(headerID),
                            4);
                }
            }
        }, 4);

        Debug.debugMessage(getClass(), "Adding edges using LNT", 2);
        addEdges(auxiliaryData);

        Debug.debugMessage(new CallBack()
        {

            public void doJob ()
            {
                for (int vertexID : auxiliaryData.vToReachable.keySet())
                {
                    for (int keyID : auxiliaryData.vToReachable.get(vertexID)
                            .keySet())
                    {
                        Debug.debugMessage(
                                IpointGraph.class,
                                "Path("
                                        + keyID
                                        + " => "
                                        + vertexID
                                        + ") = "
                                        + auxiliaryData.vToReachable.get(
                                                vertexID).get(keyID), 4);
                    }
                }
            }
        }, 4);
    }

    public IpointGraph (CFGStar cfgStar)
    {
        this.cfgStar = cfgStar;

        Debug.debugMessage(getClass(), "Initialising", 2);
        addIpoints();

        Debug.debugMessage(getClass(), "Adding edges using data-flow analysis",
                2);
        addEdgesThroughDataFlow();
    }

    public IpointGraph ()
    {
    }

    public void addIpoint (Ipoint v)
    {
        idToVertex.put(v.getVertexID(), v);
    }

    public Ipoint getVertex (int vertexID)
    {
        return (Ipoint) idToVertex.get(vertexID);
    }

    public Ipoint getVertex (long ipointID)
    {
        Ipoint returnv = null;

        for (Vertex v : this)
        {
            Ipoint u = (Ipoint) v;
            /*
             * Disregard all dummy ipoints and all inlined ones
             */
            if (!u.isGhostIpoint() && !u.isInlined())
            {
                if (u.getIpointID() == ipointID)
                {
                    returnv = u;
                    break;
                }
            }
        }

        assert returnv != null : "Cannot find Ipoint with ID 0x"
                + Long.toHexString(ipointID);
        return returnv;
    }

    public boolean hasIpointID (long ipointID)
    {
        Debug.debugMessage(getClass(),
                "Looking for Ipoint " + Long.toHexString(ipointID), 4);
        for (Vertex v : this)
        {
            Ipoint u = (Ipoint) v;
            /*
             * Disregard all dummy ipoints and all inlined ones
             */
            if (!u.isGhostIpoint() && !u.isInlined())
            {
                if (u.getIpointID() == ipointID)
                {
                    return true;
                }
            }
        }
        return false;
    }

    public final int numberOfTraceEdges ()
    {
        int count = 0;
        for (Vertex v : this)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                if (e.getEdgeType() == IPGEdgeType.TRACE_EDGE)
                {
                    count++;
                }
            }
        }
        return count;
    }

    public final int getSourceID (int edgeID)
    {
        return edgeToSource.get(edgeID);
    }

    public final int getDestinationID (int edgeID)
    {
        return edgeToDestination.get(edgeID);
    }

    public final boolean isMasterEntry (int vertexID)
    {
        Vertex v = idToVertex.get(entryID);
        return v.hasSuccessor(vertexID);
    }

    public final boolean isMasterExit (int vertexID)
    {
        Vertex v = idToVertex.get(exitID);
        return v.hasPredecessor(vertexID);
    }

    public final Set <Integer> masterEntries ()
    {
        Set <Integer> vertices = new HashSet <Integer>();
        Ipoint v = (Ipoint) idToVertex.get(entryID);
        Iterator <Edge> succIt = v.successorIterator();
        while (succIt.hasNext())
        {
            Edge e = succIt.next();
            int succID = e.getVertexID();
            if (succID != exitID)
            {
                vertices.add(succID);
            }
        }
        return Collections.unmodifiableSet(vertices);
    }

    public final Set <Integer> masterExits ()
    {
        Set <Integer> vertices = new HashSet <Integer>();
        Ipoint v = (Ipoint) idToVertex.get(exitID);
        Iterator <Edge> predIt = v.predecessorIterator();
        while (predIt.hasNext())
        {
            Edge e = predIt.next();
            int predID = e.getVertexID();
            if (predID != entryID)
            {
                vertices.add(predID);
            }
        }
        return Collections.unmodifiableSet(vertices);
    }

    private void addIpoints ()
    {
        for (Vertex v : cfgStar)
        {
            int vertexID = v.getVertexID();

            if (cfgStar.isIpoint(vertexID))
            {
                Debug.debugMessage(getClass(), "Adding ipoint " + vertexID, 4);
                idToVertex.put(vertexID, ((Ipoint) v).clone());
            }

            if (vertexID == cfgStar.getEntryID())
            {
                entryID = cfgStar.getEntryID();
            }
            else if (vertexID == cfgStar.getExitID())
            {
                exitID = cfgStar.getExitID();
            }
        }
    }

    private void initialise (AuxiliaryData auxiliaryData)
    {
        for (Vertex v : cfgStar)
        {
            int vertexID = v.getVertexID();
            auxiliaryData.vToReachable.put(vertexID,
                    new HashMap <Integer, HashSet <Integer>>());

            if (cfgStar.isIpoint(vertexID))
            {
                if (lnt.isLoopHeader(vertexID))
                {
                    auxiliaryData.ipointToHeader.put(vertexID, vertexID);
                }
                else
                {
                    int headerID = lnt.getVertex(vertexID).getParentID();
                    HeaderVertex headerv = (HeaderVertex) lnt
                            .getVertex(headerID);
                    auxiliaryData.ipointToHeader.put(vertexID,
                            headerv.getHeaderID());
                }
            }

            if (lnt.isLoopHeader(vertexID))
            {
                if (!lnt.isSelfLoop(vertexID))
                {
                    auxiliaryData.hToReachable.put(vertexID,
                            new HashMap <Integer, HashSet <Integer>>());
                    auxiliaryData.hToTopSorts.put(vertexID,
                            new ArrayList <Integer>());
                    auxiliaryData.hToIterationEdges.put(vertexID,
                            new HashSet <Integer>());
                    auxiliaryData.hToIpoints.put(vertexID,
                            new HashSet <Integer>());

                    Iterator <Integer> bodyIt = lnt.bodyIterator(vertexID);
                    while (bodyIt.hasNext())
                    {
                        int bodyID = bodyIt.next();
                        if (cfgStar.isIpoint(bodyID))
                        {
                            auxiliaryData.hToIpoints.get(vertexID).add(bodyID);
                        }
                    }
                }
            }
        }
    }

    private void doDFS (AuxiliaryData auxiliaryData, Set <Integer> visited,
            int vertexID)
    {
        Debug.debugMessage(getClass(), "Visiting " + vertexID, 4);
        Vertex v = cfgStar.getVertex(vertexID);
        visited.add(vertexID);

        Iterator <Edge> succIt = v.successorIterator();
        while (succIt.hasNext())
        {
            Edge e = succIt.next();
            int succID = e.getVertexID();
            if (!visited.contains(succID))
            {
                doDFS(auxiliaryData, visited, succID);
            }
        }

        HeaderVertex rootv = (HeaderVertex) lnt.getVertex(lnt.getRootID());
        if (vertexID != rootv.getHeaderID())
        {
            TreeVertex treev = lnt.getVertex(vertexID);
            int headerID = treev.getParentID();
            HeaderVertex headerv = (HeaderVertex) lnt.getVertex(headerID);
            ArrayList <Integer> topSort = auxiliaryData.hToTopSorts.get(headerv
                    .getHeaderID());
            topSort.add(0, vertexID);

            Debug.debugMessage(getClass(), "Adding " + vertexID + " to "
                    + headerv.getHeaderID(), 4);

            if (lnt.isLoopHeader(vertexID))
            {
                int enclosingHeaderID = headerv.getParentID();
                HeaderVertex enclosingHeaderv = (HeaderVertex) lnt
                        .getVertex(enclosingHeaderID);
                ArrayList <Integer> enclosingTopSort = auxiliaryData.hToTopSorts
                        .get(enclosingHeaderv.getHeaderID());
                enclosingTopSort.add(0, vertexID);

                Debug.debugMessage(getClass(),
                        "Adding " + vertexID + " to enclosing header "
                                + enclosingHeaderv.getHeaderID(), 4);
            }
        }
        else
        {
            ArrayList <Integer> topSort = auxiliaryData.hToTopSorts
                    .get(vertexID);
            topSort.add(0, vertexID);
        }
    }

    private void addEdges (AuxiliaryData auxiliaryData)
    {
        for (int level = lnt.getHeight() - 1; level >= 0; --level)
        {
            Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
            while (levelIt.hasNext())
            {
                TreeVertex v = levelIt.next();
                int vertexID = v.getVertexID();

                if (lnt.isLoopHeader(vertexID) && !lnt.isSelfLoop(vertexID))
                {
                    Debug.debugMessage(getClass(),
                            "Building IPG transitions in CFG* header "
                                    + vertexID, 3);

                    if (!cfgStar.isIpoint(vertexID))
                    {
                        HashSet <Integer> path = new HashSet <Integer>();
                        path.add(vertexID);
                        auxiliaryData.vToReachable.get(vertexID).put(vertexID,
                                path);
                    }

                    auxiliaryData.headerID = vertexID;
                    solveDFF(auxiliaryData, vertexID);
                    collapseInnerLoops(auxiliaryData, vertexID);
                }
            }
        }
    }

    private void solveDFF (AuxiliaryData auxiliaryData, int headerID)
    {
        auxiliaryData.changed = true;
        auxiliaryData.it = 0;

        while (auxiliaryData.changed)
        {
            auxiliaryData.changed = false;
            auxiliaryData.it = auxiliaryData.it + 1;

            for (int vertexID : auxiliaryData.hToTopSorts.get(headerID))
            {
                Vertex v = cfgStar.getVertex(vertexID);
                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    Edge e = predIt.next();
                    int predID = e.getVertexID();

                    boolean analyseEdge = false;
                    if (vertexID == headerID)
                    {
                        if (lnt.isLoopTail(headerID, predID)
                                && auxiliaryData.it > 1)
                        {
                            analyseEdge = true;
                        }
                    }
                    else if (!lnt.isLoopHeader(vertexID))
                    {
                        analyseEdge = true;
                    }
                    else if (!lnt.isLoopTail(vertexID, predID))
                    {
                        analyseEdge = true;
                    }

                    if (analyseEdge)
                    {
                        Debug.debugMessage(getClass(), "It " + auxiliaryData.it
                                + ": CFG* edge " + predID + " => " + vertexID,
                                4);

                        if (cfgStar.isIpoint(predID))
                        {
                            update(auxiliaryData, predID, vertexID, headerID,
                                    predID);
                        }
                        else
                        {
                            for (int keyID : auxiliaryData.vToReachable.get(
                                    predID).keySet())
                            {
                                if (cfgStar.isIpoint(keyID))
                                {
                                    update(auxiliaryData, keyID, vertexID,
                                            headerID, predID);
                                }
                            }

                            if (auxiliaryData.it == 1
                                    && auxiliaryData.vToReachable.get(predID)
                                            .containsKey(headerID))
                            {
                                updateLoopReachability(auxiliaryData, headerID,
                                        vertexID, predID);
                            }
                        }
                    }
                }
            }
        }
    }

    private void update (AuxiliaryData auxiliaryData, int uID, int vID,
            int headerID, int predID)
    {
        Ipoint u = (Ipoint) idToVertex.get(uID);

        if (cfgStar.isIpoint(vID))
        {
            if (!u.hasSuccessor(vID))
            {
                auxiliaryData.changed = true;
                addIPGEdge(auxiliaryData, uID, vID);

                /*
                 * When the predecessor and the key are equal it means that
                 * there are no basic blocks between the ipoints
                 */
                if (predID != uID)
                {
                    addEdgeLabel(auxiliaryData, uID, vID,
                            auxiliaryData.vToReachable.get(predID).get(uID));
                }

                if (auxiliaryData.it == 2)
                {
                    setIterationEdge(auxiliaryData, uID, vID);
                }
                else
                {
                    determineAcyclicProperties(auxiliaryData, uID, vID);
                }
            }
            else if (predID != uID)
            {
                addEdgeLabel(auxiliaryData, uID, vID,
                        auxiliaryData.vToReachable.get(predID).get(uID));
            }
        }
        else
        {
            if (lnt.isLoopHeader(vID) && !lnt.isSelfLoop(vID)
                    && vID != headerID)
            {
                updateLoopExits(auxiliaryData, vID, uID, predID);
                addLoopEntryEdges(auxiliaryData, uID, vID, predID);
            }
            else
            {
                if (!auxiliaryData.vToReachable.get(vID).containsKey(uID))
                {
                    auxiliaryData.changed = true;

                    HashSet <Integer> path = new HashSet <Integer>();
                    path.add(vID);
                    if (predID != uID)
                    {
                        path.addAll(auxiliaryData.vToReachable.get(predID).get(
                                uID));
                    }

                    Debug.debugMessage(getClass(), "Added " + path
                            + " to the path " + uID + " to " + vID, 4);
                    auxiliaryData.vToReachable.get(vID).put(uID, path);
                }
                else if (predID != uID)
                {
                    int oldSize = auxiliaryData.vToReachable.get(vID).get(uID)
                            .size();
                    auxiliaryData.vToReachable
                            .get(vID)
                            .get(uID)
                            .addAll(auxiliaryData.vToReachable.get(predID).get(
                                    uID));
                    int newSize = auxiliaryData.vToReachable.get(vID).get(uID)
                            .size();

                    if (oldSize != newSize)
                    {
                        Debug.debugMessage(
                                getClass(),
                                "Added "
                                        + auxiliaryData.vToReachable
                                                .get(predID).get(uID)
                                        + " to the path " + uID + " to " + vID,
                                4);
                        auxiliaryData.changed = true;
                    }
                }
            }
        }
    }

    private void addIPGEdge (AuxiliaryData auxiliaryData, int sourceID,
            int destinationID)
    {
        Debug.debugMessage(getClass(), "Adding edge " + sourceID + " => "
                + destinationID, 3);

        Ipoint u = (Ipoint) idToVertex.get(sourceID);
        Ipoint v = (Ipoint) idToVertex.get(destinationID);

        if (sourceID == entryID || destinationID == exitID
                || sourceID == exitID)
        {
            u.addSuccessor(destinationID, v.getIpointID(),
                    auxiliaryData.edgeID, IPGEdgeType.GHOST_EDGE);
            v.addPredecessor(sourceID, auxiliaryData.edgeID,
                    IPGEdgeType.GHOST_EDGE);
        }
        else if (u.isInlinedEntry() && v.isInlinedExit())
        {
            u.addSuccessor(destinationID, v.getIpointID(),
                    auxiliaryData.edgeID, IPGEdgeType.INLINED_EDGE);
            v.addPredecessor(sourceID, auxiliaryData.edgeID,
                    IPGEdgeType.INLINED_EDGE);
        }
        else
        {
            u.addSuccessor(destinationID, v.getIpointID(),
                    auxiliaryData.edgeID, IPGEdgeType.TRACE_EDGE);
            v.addPredecessor(sourceID, auxiliaryData.edgeID,
                    IPGEdgeType.TRACE_EDGE);
        }

        edgeToSource.put(auxiliaryData.edgeID, sourceID);
        edgeToDestination.put(auxiliaryData.edgeID, destinationID);
        auxiliaryData.edgeID += 1;
    }

    private void addEdgeLabel (AuxiliaryData auxiliaryData, int sourceID,
            int destinationID, HashSet <Integer> label)
    {
        Ipoint u = (Ipoint) idToVertex.get(sourceID);
        Ipoint v = (Ipoint) idToVertex.get(destinationID);
        IPGEdge succEdge = (IPGEdge) u.getSuccessor(destinationID);
        IPGEdge predEdge = (IPGEdge) v.getPredecessor(sourceID);
        int oldSize = succEdge.getEdgeLabel().size();
        label.add(sourceID);
        label.add(destinationID);
        succEdge.addToEdgeLabel(label);
        predEdge.addToEdgeLabel(label);
        int newSize = succEdge.getEdgeLabel().size();

        if (oldSize != newSize)
        {
            Debug.debugMessage(getClass(), "Added " + label
                    + " to the IPG edge " + sourceID + " => " + destinationID,
                    4);
            auxiliaryData.changed = true;
        }
    }

    private void setIterationEdge (AuxiliaryData auxiliaryData, int sourceID,
            int destinationID)
    {
        Debug.debugMessage(getClass(), "Iteration edge: " + sourceID + " => "
                + destinationID + " for " + auxiliaryData.headerID, 3);

        Ipoint u = (Ipoint) idToVertex.get(sourceID);
        Ipoint v = (Ipoint) idToVertex.get(destinationID);
        IPGEdge succEdge = (IPGEdge) u.getSuccessor(destinationID);
        IPGEdge predEdge = (IPGEdge) v.getPredecessor(sourceID);
        succEdge.setIterationHeaderID(auxiliaryData.headerID);
        predEdge.setIterationHeaderID(auxiliaryData.headerID);
    }

    private void determineAcyclicProperties (AuxiliaryData auxiliaryData,
            int sourceID, int destinationID)
    {
        Debug.debugMessage(getClass(), "Determining acyclic properties of "
                + sourceID + " => " + destinationID, 3);

        Ipoint u = (Ipoint) idToVertex.get(sourceID);
        Ipoint v = (Ipoint) idToVertex.get(destinationID);
        IPGEdge succEdge = (IPGEdge) u.getSuccessor(destinationID);
        IPGEdge predEdge = (IPGEdge) v.getPredecessor(sourceID);
        int sourceHeaderID = auxiliaryData.ipointToHeader.get(sourceID);
        int destinationHeaderID = auxiliaryData.ipointToHeader
                .get(destinationID);

        HeaderVertex sourceHeaderv = (HeaderVertex) lnt.getVertex(lnt
                .getVertex(sourceHeaderID).getParentID());
        HeaderVertex destinationHeaderv = (HeaderVertex) lnt.getVertex(lnt
                .getVertex(destinationHeaderID).getParentID());

        if (sourceHeaderv.getHeaderID() != destinationHeaderv.getHeaderID())
        {
            if (lnt.isNested(sourceHeaderv.getVertexID(),
                    destinationHeaderv.getVertexID()))
            {
                succEdge.setExitEdge(sourceHeaderv.getHeaderID());
                predEdge.setExitEdge(sourceHeaderv.getHeaderID());
            }
            else if (lnt.isNested(destinationHeaderv.getVertexID(),
                    sourceHeaderv.getVertexID()))
            {
                succEdge.setEntryEdge(destinationHeaderv.getHeaderID());
                predEdge.setEntryEdge(destinationHeaderv.getHeaderID());
            }
            else
            {
                succEdge.setExitEdge(sourceHeaderv.getHeaderID());
                predEdge.setExitEdge(sourceHeaderv.getHeaderID());
                succEdge.setEntryEdge(destinationHeaderv.getHeaderID());
                predEdge.setEntryEdge(destinationHeaderv.getHeaderID());
            }
        }
    }

    private void addLoopEntryEdges (AuxiliaryData auxiliaryData, int sourceID,
            int headerID, int predID)
    {
        Debug.debugMessage(getClass(),
                "Adding loop entry edges of " + headerID, 3);

        Ipoint u = (Ipoint) idToVertex.get(sourceID);
        for (int ipointID : auxiliaryData.hToReachable.get(headerID).keySet())
        {
            if (!u.hasSuccessor(ipointID))
            {
                auxiliaryData.changed = true;
                addIPGEdge(auxiliaryData, sourceID, ipointID);

                HashSet <Integer> path = new HashSet <Integer>();
                if (predID != sourceID)
                {
                    path.addAll(auxiliaryData.vToReachable.get(predID).get(
                            sourceID));
                }
                path.addAll(auxiliaryData.hToReachable.get(headerID).get(
                        ipointID));
                addEdgeLabel(auxiliaryData, sourceID, ipointID, path);

                if (auxiliaryData.it == 2)
                {
                    setIterationEdge(auxiliaryData, sourceID, ipointID);
                }
                else
                {
                    determineAcyclicProperties(auxiliaryData, sourceID,
                            ipointID);
                }
            }
            else
            {
                if (auxiliaryData.it == 2)
                {
                    IPGEdge succEdge = (IPGEdge) u.getSuccessor(ipointID);
                    if (succEdge.isIterationEdge())
                    {
                        setIterationEdge(auxiliaryData, sourceID, ipointID);
                    }
                }
            }
        }
    }

    private void updateLoopReachability (AuxiliaryData auxiliaryData,
            int headerID, int vertexID, int predID)
    {
        Debug.debugMessage(getClass(), "Updating loop " + headerID + " from "
                + vertexID, 3);

        if (cfgStar.isIpoint(vertexID))
        {
            if (!auxiliaryData.hToReachable.get(headerID).containsKey(vertexID))
            {
                auxiliaryData.changed = true;
                HashSet <Integer> path = new HashSet <Integer>();
                path.addAll(auxiliaryData.vToReachable.get(predID)
                        .get(headerID));
                auxiliaryData.hToReachable.get(headerID).put(vertexID, path);

                Debug.debugMessage(getClass(), "Ipoint " + vertexID
                        + " is reachable on ipoint-free path " + path, 4);
            }
            else
            {
                int oldSize = auxiliaryData.hToReachable.get(headerID)
                        .get(vertexID).size();
                auxiliaryData.hToReachable
                        .get(headerID)
                        .get(vertexID)
                        .addAll(auxiliaryData.vToReachable.get(predID).get(
                                headerID));
                int newSize = auxiliaryData.hToReachable.get(headerID)
                        .get(vertexID).size();

                if (oldSize != newSize)
                {
                    Debug.debugMessage(getClass(),
                            "Added "
                                    + auxiliaryData.vToReachable.get(predID)
                                            .get(headerID) + " to the path "
                                    + headerID + " to " + vertexID, 4);
                    auxiliaryData.changed = true;
                }
            }
        }
        else
        {
            if (lnt.isLoopHeader(vertexID) && !lnt.isSelfLoop(vertexID)
                    && vertexID != headerID)
            {
                updateLoopExits(auxiliaryData, vertexID, headerID, predID);

                for (int ipointID : auxiliaryData.hToReachable.get(vertexID)
                        .keySet())
                {
                    if (!auxiliaryData.hToReachable.get(headerID).containsKey(
                            ipointID))
                    {
                        auxiliaryData.changed = true;

                        HashSet <Integer> path = new HashSet <Integer>();
                        path.addAll(auxiliaryData.hToReachable.get(vertexID)
                                .get(ipointID));
                        path.addAll(auxiliaryData.vToReachable.get(predID).get(
                                headerID));
                        auxiliaryData.hToReachable.get(headerID).put(ipointID,
                                path);
                    }
                    else
                    {
                        int oldSize = auxiliaryData.hToReachable.get(headerID)
                                .get(ipointID).size();
                        auxiliaryData.hToReachable
                                .get(headerID)
                                .get(ipointID)
                                .addAll(auxiliaryData.vToReachable.get(predID)
                                        .get(headerID));
                        int newSize = auxiliaryData.hToReachable.get(headerID)
                                .get(ipointID).size();

                        if (oldSize != newSize)
                        {
                            auxiliaryData.changed = true;
                        }
                    }
                }
            }
            else
            {
                if (!auxiliaryData.vToReachable.get(vertexID).containsKey(
                        headerID))
                {
                    auxiliaryData.changed = true;

                    HashSet <Integer> path = new HashSet <Integer>();
                    path.addAll(auxiliaryData.vToReachable.get(predID).get(
                            headerID));
                    path.add(vertexID);
                    auxiliaryData.vToReachable.get(vertexID)
                            .put(headerID, path);
                }
                else
                {
                    int oldSize = auxiliaryData.vToReachable.get(vertexID)
                            .get(headerID).size();
                    auxiliaryData.vToReachable
                            .get(vertexID)
                            .get(headerID)
                            .addAll(auxiliaryData.vToReachable.get(predID).get(
                                    headerID));
                    int newSize = auxiliaryData.vToReachable.get(vertexID)
                            .get(headerID).size();
                    if (oldSize != newSize)
                    {
                        auxiliaryData.changed = true;
                    }
                }
            }
        }
    }

    private void updateLoopExits (AuxiliaryData auxiliaryData, int headerID,
            int keyID, int predID)
    {
        Debug.debugMessage(getClass(), "Updating exits in " + headerID
                + " with key " + keyID, 3);

        Iterator <Integer> exitIt = lnt.exitIterator(headerID);
        while (exitIt.hasNext())
        {
            int exitID = exitIt.next();
            if (auxiliaryData.vToReachable.get(exitID).containsKey(headerID))
            {
                if (!auxiliaryData.vToReachable.get(exitID).containsKey(keyID))
                {
                    auxiliaryData.changed = true;

                    HashSet <Integer> path = new HashSet <Integer>();
                    if (predID != keyID)
                    {
                        path.addAll(auxiliaryData.vToReachable.get(predID).get(
                                keyID));
                    }
                    path.addAll(auxiliaryData.vToReachable.get(exitID).get(
                            headerID));
                    auxiliaryData.vToReachable.get(exitID).put(keyID, path);
                }
                else if (predID != keyID)
                {
                    int oldSize = auxiliaryData.vToReachable.get(exitID)
                            .get(keyID).size();
                    auxiliaryData.vToReachable
                            .get(exitID)
                            .get(keyID)
                            .addAll(auxiliaryData.vToReachable.get(predID).get(
                                    keyID));
                    int newSize = auxiliaryData.vToReachable.get(exitID)
                            .get(keyID).size();

                    if (oldSize != newSize)
                    {
                        auxiliaryData.changed = true;
                    }
                }
            }
        }
    }

    private void collapseInnerLoops (AuxiliaryData auxiliaryData, int headerID)
    {
        Debug.debugMessage(getClass(), "Collapsing inner loops in " + headerID,
                3);

        int parentID = lnt.getVertex(headerID).getParentID();
        HeaderVertex headerv = (HeaderVertex) lnt.getVertex(parentID);
        Iterator <Edge> succIt = headerv.successorIterator();
        while (succIt.hasNext())
        {
            Edge e = succIt.next();
            int succID = e.getVertexID();

            if (lnt.isLoopHeader(succID) && !lnt.isSelfLoop(succID))
            {
                HashSet <Integer> ipoints = auxiliaryData.hToIpoints
                        .get(succID);
                auxiliaryData.hToIpoints.get(headerID).addAll(ipoints);

                for (int ipointID : ipoints)
                {
                    auxiliaryData.ipointToHeader.put(ipointID, headerID);
                }
            }
        }
    }

    private void addEdgesThroughDataFlow ()
    {
        boolean changed = true;
        DepthFirstTree dfs = new DepthFirstTree(cfgStar, cfgStar.getEntryID());
        HashMap <Integer, HashMap <Integer, HashSet <Integer>>> vToIpoints = new HashMap <Integer, HashMap <Integer, HashSet <Integer>>>();

        for (Vertex v : cfgStar)
        {
            int vertexID = v.getVertexID();
            vToIpoints
                    .put(vertexID, new HashMap <Integer, HashSet <Integer>>());
        }

        while (changed)
        {
            changed = false;

            for (int i = dfs.numOfVertices(); i >= 1; --i)
            {
                int vertexID = dfs.getPostVertexID(i);
                Vertex v = cfgStar.getVertex(vertexID);

                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    Edge e = predIt.next();
                    int predID = e.getVertexID();

                    if (cfgStar.isIpoint(predID))
                    {
                        if (!vToIpoints.get(vertexID).containsKey(predID))
                        {
                            changed = true;
                            vToIpoints.get(vertexID).put(predID,
                                    new HashSet <Integer>());

                            if (!cfgStar.isIpoint(vertexID))
                            {
                                vToIpoints.get(vertexID).get(predID)
                                        .add(vertexID);
                            }
                        }
                    }
                    else
                    {

                        for (int keyID : vToIpoints.get(predID).keySet())
                        {
                            if (!vToIpoints.get(vertexID).containsKey(keyID))
                            {
                                changed = true;
                                vToIpoints.get(vertexID).put(keyID,
                                        new HashSet <Integer>());

                                Set <Integer> pathFromKeyToPred = vToIpoints
                                        .get(predID).get(keyID);
                                vToIpoints.get(vertexID).get(keyID)
                                        .addAll(pathFromKeyToPred);

                                if (!cfgStar.isIpoint(vertexID))
                                {
                                    vToIpoints.get(vertexID).get(keyID)
                                            .add(vertexID);
                                }
                            }
                            else
                            {
                                int oldSize = vToIpoints.get(vertexID)
                                        .get(keyID).size();
                                Set <Integer> pathFromKeyToPred = vToIpoints
                                        .get(predID).get(keyID);
                                vToIpoints.get(vertexID).get(keyID)
                                        .addAll(pathFromKeyToPred);
                                int newSize = vToIpoints.get(vertexID)
                                        .get(keyID).size();

                                if (oldSize != newSize)
                                {
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        int edgeID = 1;

        for (Vertex cfgv : cfgStar)
        {
            int destinationID = cfgv.getVertexID();
            if (cfgStar.isIpoint(destinationID))
            {
                for (int sourceID : vToIpoints.get(destinationID).keySet())
                {
                    Ipoint u = (Ipoint) idToVertex.get(sourceID);
                    Ipoint v = (Ipoint) idToVertex.get(destinationID);
                    u.addSuccessor(destinationID, v.getIpointID(), edgeID,
                            IPGEdgeType.GHOST_EDGE);
                    v.addPredecessor(sourceID, edgeID, IPGEdgeType.GHOST_EDGE);

                    IPGEdge succEdge = (IPGEdge) u.getSuccessor(destinationID);
                    IPGEdge predEdge = (IPGEdge) v.getPredecessor(sourceID);

                    HashSet <Integer> label = vToIpoints.get(destinationID)
                            .get(sourceID);

                    label.add(sourceID);
                    label.add(destinationID);

                    succEdge.addToEdgeLabel(label);
                    predEdge.addToEdgeLabel(label);

                    edgeID++;
                }
            }
        }
    }

    private class AuxiliaryData
    {

        protected boolean changed;
        protected int headerID;
        protected int it;
        protected int edgeID = 1;
        protected HashMap <Integer, HashMap <Integer, HashSet <Integer>>> vToReachable = new LinkedHashMap <Integer, HashMap <Integer, HashSet <Integer>>>();
        protected HashMap <Integer, HashMap <Integer, HashSet <Integer>>> hToReachable = new LinkedHashMap <Integer, HashMap <Integer, HashSet <Integer>>>();
        protected HashMap <Integer, ArrayList <Integer>> hToTopSorts = new LinkedHashMap <Integer, ArrayList <Integer>>();
        protected HashMap <Integer, HashSet <Integer>> hToIterationEdges = new LinkedHashMap <Integer, HashSet <Integer>>();
        protected HashMap <Integer, HashSet <Integer>> hToIpoints = new LinkedHashMap <Integer, HashSet <Integer>>();
        protected HashMap <Integer, Integer> ipointToHeader = new LinkedHashMap <Integer, Integer>();

        private AuxiliaryData ()
        {
        }
    }
}