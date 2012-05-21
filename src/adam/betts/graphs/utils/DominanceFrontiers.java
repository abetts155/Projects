package adam.betts.graphs.utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;

public class DominanceFrontiers
{

    protected FlowGraph flowg;
    protected DominatorTree dominatort;
    protected HashMap <Integer, Set <Integer>> vToDF = new LinkedHashMap <Integer, Set <Integer>>();

    public DominanceFrontiers (FlowGraph flowg, DominatorTree dominatort)
    {
        this.flowg = flowg;
        this.dominatort = dominatort;

        initialise();
        compute();
    }

    public final Iterator <Integer> dfIterator (int vertexID)
    {
        return vToDF.get(vertexID).iterator();
    }

    public final Set <Integer> dfSet (int vertexID)
    {
        return Collections.unmodifiableSet(vToDF.get(vertexID));
    }

    public final int size (int vertexID)
    {
        return vToDF.get(vertexID).size();
    }

    public final boolean contains (int vertexID, int elementID)
    {
        return vToDF.get(vertexID).contains(elementID);
    }

    private void initialise ()
    {
        for (Vertex v : flowg)
        {
            int vertexID = v.getVertexID();
            vToDF.put(vertexID, new HashSet <Integer>());
        }
    }

    private void compute ()
    {
        Debug.debugMessage(getClass(), "Computing dominance frontiers", 4);
        for (Vertex v : flowg)
        {
            int vertexID = v.getVertexID();

            if (v.numOfPredecessors() > 1)
            {
                int idomID = dominatort.getImmediateDominator(vertexID);

                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    Edge e = predIt.next();
                    int predID = e.getVertexID();
                    int runnerID = predID;

                    while (runnerID != idomID)
                    {
                        if (!vToDF.get(runnerID).contains(vertexID))
                        {
                            vToDF.get(runnerID).add(vertexID);
                        }
                        runnerID = dominatort.getImmediateDominator(runnerID);
                    }
                }
            }
        }
    }
}
