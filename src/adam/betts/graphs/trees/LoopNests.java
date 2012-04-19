package adam.betts.graphs.trees;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.DirectedGraph;
import adam.betts.graphs.FlowGraph;
import adam.betts.outputs.OutputGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DFSEdgeType;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class LoopNests extends Tree
{

    protected final DirectedGraph directedg;
    protected DepthFirstTree dfs;
    protected HashMap <Integer, Set <Integer>> headerToLoop = new LinkedHashMap <Integer, Set <Integer>>();
    protected HashMap <Integer, Set <Integer>> headerToTails = new LinkedHashMap <Integer, Set <Integer>>();
    protected HashMap <Integer, Set <Integer>> headerToExits = new LinkedHashMap <Integer, Set <Integer>>();
    private HashMap <Integer, Integer> parent = new LinkedHashMap <Integer, Integer>();
    private HashMap <Integer, Integer> cfgIDToTreeID = new LinkedHashMap <Integer, Integer>();

    public LoopNests (DirectedGraph directedg, int rootID)
    {
        this.directedg = directedg;

        initialise();
        findLoops(rootID);
        setRoot();
        identifyLoopExits();
        setHeight();
    }

    public final boolean isLoopHeader (int vertexID)
    {
        return headerToLoop.containsKey(vertexID);
    }

    public final HeaderVertex getHeaderVertex (int headerID)
    {
        return (HeaderVertex) idToVertex.get(cfgIDToTreeID.get(headerID));
    }

    public final int getLoopHeader (int vertexID)
    {
        if (headerToLoop.containsKey(vertexID))
        {
            return vertexID;
        }
        else
        {
            TreeVertex v = (TreeVertex) idToVertex.get(vertexID);
            return v.getParentID();
        }
    }

    public final boolean isSelfLoop (int vertexID)
    {
        if (!headerToLoop.containsKey(vertexID))
        {
            return false;
        }
        else
        {
            return headerToLoop.get(vertexID).size() == 1;
        }
    }

    public final int numOfSelfLoops ()
    {
        int total = 0;
        for (int headerID : headerToLoop.keySet())
        {
            if (isSelfLoop(headerID))
            {
                total++;
            }
        }
        return total;
    }

    public final boolean hasSelfLoops ()
    {
        return numOfSelfLoops() > 0;
    }

    public final int numOfLoops ()
    {
        return headerToLoop.size();
    }

    public final Iterator <Integer> bodyIterator (int headerID)
    {
        return headerToLoop.get(headerID).iterator();
    }

    public boolean inLoopBody (int headerID, int vertexID)
    {
        for (int ancestorID : getAncestors(vertexID))
        {
            TreeVertex ancestorv = (TreeVertex) idToVertex.get(ancestorID);
            if (ancestorv instanceof HeaderVertex)
            {
                HeaderVertex headerv = (HeaderVertex) ancestorv;

                if (headerv.getHeaderID() == headerID)
                {
                    return true;
                }
            }
        }

        return false;
    }

    public final Iterator <Integer> tailIterator (int headerID)
    {
        return headerToTails.get(headerID).iterator();
    }

    public final Set <Integer> getTails (int headerID)
    {
        return headerToTails.get(headerID);
    }

    public final int numOfTails (int headerID)
    {
        return headerToTails.get(headerID).size();
    }

    public final boolean isLoopTail (int headerID, int vertexID)
    {
        return headerToTails.get(headerID).contains(vertexID);
    }

    public final Iterator <Integer> exitIterator (int headerID)
    {
        return headerToExits.get(headerID).iterator();
    }

    public final int numOfExits (int headerID)
    {
        return headerToExits.get(headerID).size();
    }

    public final boolean isLoopExit (int headerID, int vertexID)
    {
        return headerToExits.get(headerID).contains(vertexID);
    }

    public final boolean isDowhileLoop (int headerID)
    {
        assert isLoopHeader(headerID);

        Set <Integer> exits = headerToExits.get(headerID);
        Set <Integer> tails = headerToTails.get(headerID);

        Debug.debugMessage(getClass(), "tails(" + headerID + ") = " + tails
                + ", exits(" + headerID + ") = " + exits, Debug.HIGHEST_LEVEL);

        if (exits.size() != tails.size())
        {
            return false;
        }

        return exits.containsAll(tails) && tails.containsAll(exits);
    }

    public final Iterator <Integer> headerIterator ()
    {
        return headerToLoop.keySet().iterator();
    }

    public final boolean isNested (int left, int right)
    {
        return isProperAncestor(right, left);
    }

    public FlowGraph induceSubraph (HeaderVertex headerv)
    {
        System.out.println("Header " + headerv.getHeaderID());

        FlowGraph flowg = new FlowGraph();

        ArrayList <Integer> workList = new ArrayList <Integer>();
        workList.addAll(getTails(headerv.getHeaderID()));

        HashMap <Integer, HashSet <Integer>> edges = new HashMap <Integer, HashSet <Integer>>();

        // Add vertices to the flow graph
        while (!workList.isEmpty())
        {
            int vertexID = workList.remove(workList.size() - 1);

            if (flowg.hasVertex(vertexID) == false)
            {
                flowg.addVertex(vertexID);
                edges.put(vertexID, new HashSet <Integer>());

                if (isLoopHeader(vertexID) && vertexID != headerv.getHeaderID())
                {
                    if (isDowhileLoop(vertexID))
                    {
                        flowg.getVertex(vertexID).setDummy();
                    }
                }

                Vertex cfgv = directedg.getVertex(vertexID);
                Iterator <Edge> predIt = cfgv.predecessorIterator();
                while (predIt.hasNext())
                {
                    Edge e = predIt.next();
                    int predID = e.getVertexID();
                    TreeVertex treePredv = getVertex(predID);
                    HeaderVertex predHeaderv = (HeaderVertex) getVertex(treePredv
                            .getParentID());
                    int predHeaderID = predHeaderv.getHeaderID();

                    if (dfs.getEdgeType(predID, vertexID) != DFSEdgeType.BACK_EDGE)
                    {
                        if (predHeaderID == headerv.getHeaderID())
                        {
                            workList.add(predID);
                            edges.get(vertexID).add(predID);

                        }
                        else if (isNested(predHeaderv.getVertexID(),
                                headerv.getVertexID()))
                        {
                            if (isDowhileLoop(predHeaderID))
                            {
                                Debug.debugMessage(getClass(), predHeaderID
                                        + " is a do-while loop", 4);

                                workList.add(predHeaderID);
                                edges.get(vertexID).add(predHeaderID);
                            }
                            else
                            {
                                workList.add(predID);
                                edges.get(vertexID).add(predID);
                            }
                        }
                    }
                }
            }
        }

        // Now add edges to the induced flow graph
        for (int vertexID : edges.keySet())
        {
            for (int predID : edges.get(vertexID))
            {
                flowg.addEdge(predID, vertexID);
            }
        }

        // Add exit vertex to the induced subgraph
        addExitVertexToInducedSubgraph(flowg);

        // The entry vertex of the induced subgraph is the loop header
        flowg.setEntryID(headerv.getHeaderID());

        return flowg;
    }

    private void addExitVertexToInducedSubgraph (FlowGraph flowg)
    {
        ArrayList <Integer> noSuccs = new ArrayList <Integer>();

        for (Vertex v : flowg)
        {
            if (v.numOfSuccessors() == 0)
            {
                noSuccs.add(v.getVertexID());
            }
        }

        // Either add another vertex to ensure there is a unique exit (when
        // there are multiple tails) or set it to the unique tail
        if (noSuccs.size() != 1)
        {
            int exitID = flowg.getNextVertexID();
            flowg.addExit(exitID);
            flowg.getVertex(exitID).setDummy();
            Debug.debugMessage(getClass(), "Adding exit vertex " + exitID, 3);

            for (int vertexID : noSuccs)
            {
                flowg.addEdge(vertexID, exitID);
            }
        }
        else
        {
            int exitID = noSuccs.get(noSuccs.size() - 1);
            flowg.setExitID(exitID);
        }
    }

    private void initialise ()
    {
        Debug.debugMessage(getClass(), "Initialising", 3);

        for (Vertex v : directedg)
        {
            int vertexID = v.getVertexID();
            parent.put(vertexID, vertexID);
            addVertex(vertexID);
        }
    }

    private void findLoops (int rootID)
    {
        dfs = new DepthFirstTree(directedg, rootID);
        for (int i = dfs.numOfVertices(); i >= 1; --i)
        {
            Integer vertexID = dfs.getPreVertexID(i);
            Vertex v = directedg.getVertex(vertexID);

            ArrayList <Integer> workList = new ArrayList <Integer>();

            Iterator <Edge> predIt = v.predecessorIterator();
            while (predIt.hasNext())
            {
                Edge e = predIt.next();
                int predID = e.getVertexID();

                Debug.debugMessage(getClass(), "Analysing " + predID + " => "
                        + vertexID, 4);

                if (dfs.getEdgeType(predID, vertexID) == DFSEdgeType.BACK_EDGE)
                {
                    if (predID == vertexID)
                    {
                        Debug.debugMessage(getClass(), vertexID
                                + " is self-loop", 3);
                        addSelfLoop(vertexID);
                    }
                    else
                    {
                        workList.add(parent.get(predID));
                    }
                }
            }
            workList.remove(vertexID);

            if (!workList.isEmpty())
            {
                findLoop(dfs, workList, vertexID);
            }
        }
    }

    private void addSelfLoop (int headerID)
    {
        Debug.debugMessage(getClass(), "Header " + headerID + " is self-loop",
                3);

        int vertexID = getNextVertexID();
        HeaderVertex header = new HeaderVertex(vertexID, headerID);
        idToVertex.put(vertexID, header);
        addEdge(header.getVertexID(), headerID);
        cfgIDToTreeID.put(headerID, vertexID);

        HashSet <Integer> loopBody = new HashSet <Integer>();
        loopBody.add(headerID);
        headerToLoop.put(headerID, loopBody);

        HashSet <Integer> tails = new HashSet <Integer>();
        tails.add(headerID);
        headerToTails.put(headerID, tails);
    }

    private HeaderVertex findHeader (int headerID)
    {
        for (Vertex v : this)
        {
            if (v instanceof HeaderVertex)
            {
                HeaderVertex headerv = (HeaderVertex) v;
                if (headerv.getHeaderID() == headerID)
                {
                    return headerv;
                }
            }
        }
        return null;
    }

    private void findLoop (DepthFirstTree dfsTree,
            ArrayList <Integer> workList, int headerID)
    {
        Debug.debugMessage(getClass(), "Header " + headerID + " tails = "
                + workList, 1);

        if (headerToTails.containsKey(headerID))
        {
            headerToTails.get(headerID).addAll(workList);
        }
        else
        {
            HashSet <Integer> tails = new HashSet <Integer>();
            tails.addAll(workList);
            headerToTails.put(headerID, tails);

            /*
             * Add the internal vertex to the tree representing this header
             */
            int vertexID = getNextVertexID();
            HeaderVertex header = new HeaderVertex(vertexID, headerID);
            idToVertex.put(vertexID, header);
            cfgIDToTreeID.put(headerID, vertexID);
        }

        HashSet <Integer> loopBody = new HashSet <Integer>();
        while (!workList.isEmpty())
        {
            int listID = workList.remove(workList.size() - 1);
            loopBody.add(listID);

            Vertex v = directedg.getVertex(listID);
            Iterator <Edge> predIt = v.predecessorIterator();
            while (predIt.hasNext())
            {
                Edge e = predIt.next();
                int predID = e.getVertexID();

                if (dfsTree.getEdgeType(predID, listID) != DFSEdgeType.BACK_EDGE)
                {
                    int repID = parent.get(predID);

                    if (!workList.contains(repID) && !loopBody.contains(repID)
                            && repID != headerID)
                    {
                        workList.add(repID);
                    }
                }
            }
        }

        if (!loopBody.isEmpty())
        {
            HeaderVertex headerv = findHeader(headerID);

            for (int vertexID : loopBody)
            {
                Debug.debugMessage(getClass(), vertexID + " is in loop body", 4);

                parent.put(vertexID, headerID);
                if (isLoopHeader(vertexID))
                {
                    HeaderVertex innerHeaderv = findHeader(vertexID);
                    addEdge(headerv.getVertexID(), innerHeaderv.getVertexID());
                }
                else
                {
                    addEdge(headerv.getVertexID(), vertexID);
                }
            }

            loopBody.add(headerID);
            addEdge(headerv.getVertexID(), headerID);

            if (!headerToLoop.containsKey(headerID))
            {
                headerToLoop.put(headerID, loopBody);
            }
            else
            {
                headerToLoop.get(headerID).addAll(loopBody);
            }
        }
    }

    private void setRoot ()
    {
        for (Vertex v : this)
        {
            if (v.numOfPredecessors() == 0)
            {
                this.rootID = v.getVertexID();
            }
        }
    }

    private void identifyLoopExits ()
    {
        Debug.debugMessage(getClass(), "Identifying loop exits", 3);

        for (int headerID : headerToLoop.keySet())
        {
            headerToExits.put(headerID, new HashSet <Integer>());

            for (int vertexID : headerToLoop.get(headerID))
            {
                Vertex v = directedg.getVertex(vertexID);
                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    Edge e = succIt.next();
                    int succID = e.getVertexID();

                    if (!headerToLoop.get(headerID).contains(succID))
                    {
                        if (headerID != vertexID && isLoopHeader(vertexID))
                        {
                            if (!headerToLoop.get(vertexID).contains(succID))
                            {
                                headerToExits.get(headerID).add(vertexID);
                            }
                        }
                        else
                        {
                            headerToExits.get(headerID).add(vertexID);
                        }
                    }
                }
            }
        }
    }
}
