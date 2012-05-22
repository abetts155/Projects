package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.Stack;

import adam.betts.edges.CallEdge;
import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.outputs.OutputGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.BasicBlock;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.call.CallVertex;

public class ControlFlowGraph extends FlowGraph implements Cloneable
{

    /*
     * To ensure edge IDs are unique across all CFGs
     */
    private static int edgeID = 1;

    protected String subprogramName;
    protected Long firstAddress;
    protected Long lastAddress;
    protected LoopNests lnt;

    public ControlFlowGraph ()
    {
    }

    public ControlFlowGraph clone ()
    {
        ControlFlowGraph cfg = new ControlFlowGraph();
        for (Vertex v : this)
        {
            cfg.addBasicBlock(v.getVertexID());
        }

        for (Vertex v : this)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                int succID = e.getVertexID();
                BranchType type = e.getBranchType();
                cfg.addEdge(v.getVertexID(), succID, type);
            }
        }

        cfg.entryID = entryID;
        cfg.exitID = exitID;

        return cfg;
    }

    public void makeVertexAndEdgeNumbersDistinct (CallVertex callv)
    {
        Debug.debugMessage(getClass(),
                "Renumbering vertices in  " + callv.getSubprogramName() + " "
                        + callv.numOfSuccessors(), 1);

        HashMap <Integer, Integer> oldIdToNewID = new LinkedHashMap <Integer, Integer>();

        int newID = 0;
        for (Vertex v : this)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                newID++;

                FlowEdge succe = (FlowEdge) succIt.next();
                int succID = succe.getVertexID();
                FlowEdge prede = (FlowEdge) getVertex(succID).getPredecessor(
                        v.getVertexID());

                succe.setEdgeID(newID);
                prede.setEdgeID(newID);
            }
        }

        for (Vertex v : this)
        {
            newID++;
            Integer oldVertexID = v.getVertexID();
            v.setVertexID(newID);
            oldIdToNewID.put(oldVertexID, newID);

            if (oldVertexID == entryID)
            {
                entryID = newID;
            }
            if (oldVertexID == exitID)
            {
                exitID = newID;
            }

            Debug.debugMessage(getClass(), "Renumbering vertex " + oldVertexID
                    + " to new ID " + newID, 1);
        }

        HashSet <Vertex> vertices = new HashSet <Vertex>();
        for (int oldVertexID : oldIdToNewID.keySet())
        {
            Vertex v = idToVertex.remove(oldVertexID);
            vertices.add(v);
        }

        // Renumber the call sites on the call graph edges
        Iterator <Edge> callSuccIt = callv.successorIterator();
        while (callSuccIt.hasNext())
        {
            CallEdge calle = (CallEdge) callSuccIt.next();
            Set <Integer> newIDs = new HashSet <Integer>();

            for (int oldVertexID : oldIdToNewID.keySet())
            {
                if (calle.callSites().contains(oldVertexID))
                {
                    newIDs.add(oldIdToNewID.get(oldVertexID));
                }
            }

            calle.changeCallSiteIDs(newIDs);
        }

        for (int oldVertexID : oldIdToNewID.keySet())
        {
            int newVertexID = oldIdToNewID.get(oldVertexID);
            for (Vertex v : vertices)
            {
                if (v.getVertexID() == newVertexID)
                {
                    idToVertex.put(newVertexID, v);
                }
            }
        }

        for (Vertex v : this)
        {
            Debug.debugMessage(getClass(), "Analysing " + v.getVertexID(), 4);

            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge succe = (FlowEdge) succIt.next();
                int oldSuccID = succe.getVertexID();
                int newSuccID = oldIdToNewID.get(oldSuccID);
                succe.setVertexID(newSuccID);

                Debug.debugMessage(getClass(), "Renumbering succ " + oldSuccID
                        + " to " + newSuccID, 4);
            }

            Iterator <Edge> predIt = v.predecessorIterator();
            while (predIt.hasNext())
            {
                FlowEdge prede = (FlowEdge) predIt.next();
                int oldPredID = prede.getVertexID();
                int newPredID = oldIdToNewID.get(oldPredID);
                prede.setVertexID(oldIdToNewID.get(oldPredID));

                Debug.debugMessage(getClass(), "Renumbering pred " + oldPredID
                        + " to " + newPredID, 4);
            }
        }
    }

    public void inline (ControlFlowGraph cfg, String subprogramName)
    {
        for (Vertex v : cfg)
        {
            BasicBlock bb = (BasicBlock) v;
            BasicBlock clone = bb.clone();
            clone.setSubprogramName(subprogramName);
            idToVertex.put(v.getVertexID(), clone);
        }

        for (Vertex v : cfg)
        {
            int vertexID = v.getVertexID();
            if (vertexID != cfg.getExitID())
            {
                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    FlowEdge e = (FlowEdge) succIt.next();
                    int succID = e.getVertexID();
                    BranchType type = e.getBranchType();

                    this.addEdge(v.getVertexID(), succID, type);
                }
            }
        }
    }

    public void addEdge (int sourceID, int destinationID, BranchType type)
    {
        if (!idToVertex.containsKey(sourceID))
        {
            addBasicBlock(sourceID);
        }
        if (!idToVertex.containsKey(destinationID))
        {
            addBasicBlock(destinationID);
        }

        BasicBlock u = (BasicBlock) idToVertex.get(sourceID);
        BasicBlock v = (BasicBlock) idToVertex.get(destinationID);
        u.addSuccessor(destinationID, type, edgeID);
        v.addPredecessor(sourceID, type, edgeID);
        edgeID++;

        Debug.debugMessage(getClass(), "Adding edge " + sourceID + " => "
                + destinationID + " (" + type + ")", 4);
    }

    public final void addAllPredecessorEdges ()
    {
        for (Vertex v : this)
        {
            int sourceID = v.getVertexID();
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                int destinationID = e.getVertexID();
                BasicBlock destination = getBasicBlock(destinationID);
                if (!destination.hasPredecessor(sourceID))
                {
                    destination.addPredecessor(sourceID, e.getBranchType(),
                            e.getEdgeID());
                }
            }
        }
    }

    public final void addBasicBlock (int vertexID)
    {
        idToVertex.put(vertexID, new BasicBlock(vertexID));
    }

    public final BasicBlock getBasicBlock (int vertexID)
    {
        assert idToVertex.containsKey(vertexID) : "Cannot find vertex "
                + vertexID;
        return (BasicBlock) idToVertex.get(vertexID);
    }

    public final BasicBlock getBasicBlock (long address)
    {
        for (Vertex v : this)
        {
            if (((BasicBlock) v).hasAddress(address))
            {
                return (BasicBlock) v;
            }
        }
        return null;
    }

    public final BasicBlock getSuccessor (BasicBlock u, Long address)
    {
        Iterator <Edge> succIt = u.successorIterator();
        while (succIt.hasNext())
        {
            Edge e = succIt.next();
            BasicBlock v = getBasicBlock(e.getVertexID());
            if (v.getFirstAddress() == address)
            {
                return v;
            }
        }
        return null;
    }

    public final void setEntry ()
    {
        long firstAddress = Long.MAX_VALUE;
        for (Vertex v : this)
        {
            long address = ((BasicBlock) v).getFirstAddress();
            if (address < firstAddress && address != BasicBlock.DUMMY_ADDRESS)
            {
                firstAddress = address;
                entryID = v.getVertexID();
            }
        }

        if (entryID == Vertex.DUMMY_VERTEX_ID)
        {
            Debug.debugMessage(
                    getClass(),
                    "Could not find a unique entry point using basic block addresses",
                    1);

            ArrayList <Vertex> noPreds = new ArrayList <Vertex>();
            for (Vertex v : this)
            {
                if (v.numOfPredecessors() == 0)
                {
                    noPreds.add(v);
                }
            }

            if (noPreds.size() == 1)
            {
                entryID = noPreds.get(noPreds.size() - 1).getVertexID();

                Debug.debugMessage(getClass(), "Found entry vertex " + entryID
                        + " as it has no predecessors", 1);

            }
            else
            {
                Debug.errorMessage(
                        getClass(),
                        "Could not find a unique entry point through predecessor inspection either. Giving up. Potential entry points found: "
                                + noPreds);
            }
        }
    }

    public final void setExit ()
    {
        ArrayList <Vertex> noSuccs = new ArrayList <Vertex>();
        for (Vertex v : this)
        {
            if (v.numOfSuccessors() == 0)
            {
                noSuccs.add(v);
            }
        }

        if (noSuccs.size() == 1)
        {
            exitID = noSuccs.get(noSuccs.size() - 1).getVertexID();

            Debug.debugMessage(getClass(), "Found exit vertex " + exitID
                    + " as it has no successors", 1);
        }
        else
        {
            Debug.errorMessage(getClass(),
                    "Could not find a unique exit point. Giving up. Potential exit points found: "
                            + noSuccs);
        }
    }

    public final boolean isExit (int vertexID)
    {
        return idToVertex.get(vertexID).numOfSuccessors() == 0;
    }

    public final void setSubprogramName (String subprogramName)
    {
        this.subprogramName = subprogramName;
    }

    public final String getSubprogramName ()
    {
        return subprogramName;
    }

    public final void setFirstAndLastAddress ()
    {
        firstAddress = Long.MAX_VALUE;
        lastAddress = Long.MIN_VALUE;

        for (Vertex v : this)
        {
            if (v instanceof BasicBlock)
            {
                long bbFirstAddress = ((BasicBlock) v).getFirstAddress();
                long bbLastAddress = ((BasicBlock) v).getLastAddress();
                if (bbFirstAddress < firstAddress)
                {
                    firstAddress = bbFirstAddress;
                }
                if (bbLastAddress > lastAddress)
                {
                    lastAddress = bbLastAddress;
                }
            }
        }
    }

    public final void setFirstAddress (long firstAddress)
    {
        this.firstAddress = firstAddress;
    }

    public final long getFirstAddress ()
    {
        return firstAddress;
    }

    public final void setLastAddress (long lastAddress)
    {
        this.lastAddress = lastAddress;
    }

    public final long getLastAddress ()
    {
        return lastAddress;
    }

    public final boolean hasAddress (long address)
    {
        return address >= firstAddress && address <= lastAddress;
    }

    public final void removeDeadCode ()
    {
        HashSet <Integer> toRemove = new HashSet <Integer>(idToVertex.keySet());
        HashSet <Integer> visited = new HashSet <Integer>();

        Stack <Integer> stack = new Stack <Integer>();
        stack.push(entryID);
        while (!stack.isEmpty())
        {
            int vertexID = stack.pop();
            toRemove.remove(vertexID);
            visited.add(vertexID);

            Vertex v = idToVertex.get(vertexID);
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                Edge succEdge = succIt.next();
                int succID = succEdge.getVertexID();
                if (!visited.contains(succID))
                {
                    stack.push(succID);
                }
            }
        }

        for (int vertexID : toRemove)
        {
            Debug.debugMessage(getClass(), "Removing basic block " + vertexID,
                    3);
            Debug.debugMessage(getClass(), idToVertex.get(vertexID).toString(),
                    4);
            removeVertex(vertexID);
        }
    }

    public void addEntryAndExitEdges ()
    {
        Debug.debugMessage(getClass(), "Adding entry and exit edges",
                Debug.FUNCTION_LEVEL);

        if (entryID == Vertex.DUMMY_VERTEX_ID)
        {
            ArrayList <Integer> noPreds = new ArrayList <Integer>();

            for (Vertex v : this)
            {
                int vertexID = v.getVertexID();
                if (v.numOfPredecessors() == 0)
                {
                    noPreds.add(vertexID);
                }
            }

            if (noPreds.size() == 1)
            {
                Debug.debugMessage(getClass(), "Setting entry to " + entryID, 3);
                this.entryID = noPreds.get(noPreds.size() - 1);
            }
            else
            {
                this.entryID = getNextVertexID();
                Debug.debugMessage(getClass(), "Adding entry " + entryID, 3);
                addBasicBlock(entryID);

                for (int vertexID : noPreds)
                {
                    addEdge(entryID, vertexID, BranchType.UNKNOWN);
                }
            }
        }

        if (exitID == Vertex.DUMMY_VERTEX_ID)
        {

            ArrayList <Integer> noSuccs = new ArrayList <Integer>();
            for (Vertex v : this)
            {
                int vertexID = v.getVertexID();
                if (v.numOfSuccessors() == 0)
                {
                    noSuccs.add(vertexID);
                }
            }

            if (noSuccs.size() == 1)
            {
                Debug.debugMessage(getClass(), "Setting exit to " + entryID, 3);
                this.exitID = noSuccs.get(noSuccs.size() - 1);
            }
            else
            {
                this.exitID = getNextVertexID();
                Debug.debugMessage(getClass(), "Adding exit " + exitID, 3);
                addBasicBlock(exitID);

                for (int vertexID : noSuccs)
                {
                    addEdge(vertexID, exitID, BranchType.UNKNOWN);
                }
            }
        }

        if (getVertex(entryID).hasPredecessor(exitID) == false
                && getVertex(exitID).hasSuccessor(entryID) == false)
        {

            addEdge(exitID, entryID, BranchType.UNKNOWN);
        }
    }

    public final void generateLNT ()
    {
        this.lnt = new LoopNests(this, this.entryID);
    }

    public final LoopNests getLNT ()
    {
        if (lnt == null)
        {
            generateLNT();
        }
        return lnt;
    }
}
