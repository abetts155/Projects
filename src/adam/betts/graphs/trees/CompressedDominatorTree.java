package adam.betts.graphs.trees;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import adam.betts.edges.Edge;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.utils.LeastCommonAncestor;
import adam.betts.utilities.Debug;
import adam.betts.vertices.trees.TreeVertex;

public class CompressedDominatorTree extends Tree
{

    protected final FlowGraph flowg;
    protected final DominatorTree dominatort;
    protected final LeastCommonAncestor lca;

    public CompressedDominatorTree (FlowGraph flowg, DominatorTree dominatort,
            LeastCommonAncestor lca, int branchID)
    {
        Debug.debugMessage(getClass(),
                "Building compressed dominator tree for branch " + branchID, 4);

        this.flowg = flowg;
        this.dominatort = dominatort;
        this.lca = lca;

        build(branchID);
        this.rootID = dominatort.getImmediateDominator(branchID);
        setHeight();
    }

    private void build (int branchID)
    {
        HashMap <Integer, Integer> vToLca = new HashMap <Integer, Integer>();
        HashSet <Integer> querySet = new HashSet <Integer>();
        HashSet <Integer> newQuerySet = new HashSet <Integer>();
        Iterator <Edge> succIt = flowg.getVertex(branchID).successorIterator();
        while (succIt.hasNext())
        {
            Edge succEdge = succIt.next();
            int succID = succEdge.getVertexID();
            querySet.add(succID);
        }

        while (!querySet.isEmpty())
        {
            for (int e1 : querySet)
            {
                for (int e2 : querySet)
                {
                    if (e1 != e2)
                    {
                        int lcaID = lca.getLCA(e1, e2);
                        TreeVertex lcav = dominatort.getVertex(lcaID);

                        Debug.debugMessage(getClass(), "lca(" + e1 + "," + e2
                                + ") = " + lcaID, 4);

                        if (vToLca.containsKey(e1))
                        {
                            int oldLcaID = vToLca.get(e1);
                            if (lcav.getLevel() > dominatort
                                    .getVertex(oldLcaID).getLevel()
                                    && lcaID != e1)
                            {
                                vToLca.put(e1, lcaID);
                            }
                        }
                        else
                        {
                            vToLca.put(e1, lcaID);
                        }

                        if (vToLca.containsKey(e2))
                        {
                            int oldLcaID = vToLca.get(e2);
                            if (lcav.getLevel() > dominatort
                                    .getVertex(oldLcaID).getLevel()
                                    && lcaID != e2)
                            {
                                vToLca.put(e2, lcaID);
                            }
                        }
                        else
                        {
                            vToLca.put(e2, lcaID);
                        }
                    }
                }
            }

            newQuerySet.clear();
            for (int vertexID : vToLca.keySet())
            {
                int lcaID = vToLca.get(vertexID);
                if (!querySet.contains(lcaID))
                {
                    Debug.debugMessage(getClass(), "Adding " + lcaID
                            + " to query set", 4);
                    newQuerySet.add(lcaID);
                }
            }

            querySet.clear();
            querySet.addAll(newQuerySet);
        }

        for (int vertexID : vToLca.keySet())
        {
            Debug.debugMessage(getClass(), "parent(" + vertexID + ") = "
                    + vToLca.get(vertexID), 3);
            addEdge(vToLca.get(vertexID), vertexID);
        }
    }
}
