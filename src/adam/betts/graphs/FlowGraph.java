package adam.betts.graphs;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.vertices.FlowVertex;
import adam.betts.vertices.Vertex;

public class FlowGraph extends DirectedGraph
{

    protected int entryID = Vertex.DUMMY_VERTEX_ID;
    protected int exitID = Vertex.DUMMY_VERTEX_ID;

    public FlowGraph ()
    {
    }

    public void addVertex (int vertexID)
    {
        idToVertex.put(vertexID, new FlowVertex(vertexID));
    }

    public FlowVertex getVertex (int vertexID)
    {
        return (FlowVertex) super.getVertex(vertexID);
    }

    public void addEntry (int entryID)
    {
        setEntryID(entryID);
        idToVertex.put(entryID, new FlowVertex(entryID));
    }

    public final void setEntryID (int entryID)
    {
        this.entryID = entryID;
    }

    public final int getEntryID ()
    {
        return entryID;
    }

    public final boolean hasEntry ()
    {
        return entryID != Vertex.DUMMY_VERTEX_ID;
    }

    public void addExit (int exitID)
    {
        setExitID(exitID);
        idToVertex.put(exitID, new FlowVertex(exitID));
    }

    public final void setExitID (int exitID)
    {
        this.exitID = exitID;
    }

    public final int getExitID ()
    {
        return exitID;
    }

    public final boolean hasExit ()
    {
        return exitID != Vertex.DUMMY_VERTEX_ID;
    }

    public final void reverseGraph (FlowGraph reverseGraph)
    {
        assert entryID != Vertex.DUMMY_VERTEX_ID;
        assert exitID != Vertex.DUMMY_VERTEX_ID;

        reverseGraph.entryID = exitID;
        reverseGraph.exitID = entryID;
        super.reverseGraph(reverseGraph);
    }

    public final int getNextEdgeID ()
    {
        int nextID = Integer.MAX_VALUE;
        boolean stop = false;

        Set <Integer> edgeIDs = new HashSet <Integer>();
        for (Vertex v : this)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                edgeIDs.add(e.getEdgeID());
            }
        }

        while (nextID > 1 && !stop)
        {
            if (!edgeIDs.contains(nextID))
            {
                stop = true;
            }
            else
            {
                --nextID;
            }
        }
        
        return nextID;
    }
}
