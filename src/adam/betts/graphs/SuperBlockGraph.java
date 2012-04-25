package adam.betts.graphs;

import java.util.Iterator;

import adam.betts.edges.Edge;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.graphs.utils.StronglyConnectedComponents;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.vertices.SuperBlockVertex;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;

public class SuperBlockGraph extends DirectedGraph
{

    protected final FlowGraph flowg;
    protected int rootID;

    public SuperBlockGraph (FlowGraph flowg)
    {
        this.flowg = flowg;

        DominatorGraph dominatorg = new DominatorGraph();
        StronglyConnectedComponents scc = new StronglyConnectedComponents(
                dominatorg);

        addVertices(scc);
        addEdges(scc, dominatorg);
        setRoot();
    }

    public final SuperBlockVertex getVertex (int vertexID)
    {
        return (SuperBlockVertex) idToVertex.get(vertexID);
    }

    private void addVertices (StronglyConnectedComponents scc)
    {
        Debug.debugMessage(getClass(), "Adding vertices", 3);

        for (int i = 1; i <= scc.numberOfSccs(); ++i)
        {
            idToVertex.put(i, new SuperBlockVertex(i));
        }

        for (Vertex v : flowg)
        {
            int vertexID = v.getVertexID();
            int sccID = scc.getSCCID(vertexID);
            SuperBlockVertex superv = (SuperBlockVertex) idToVertex.get(sccID);
            superv.addBasicBlock(vertexID);
        }
    }

    private void addEdges (StronglyConnectedComponents scc,
            DominatorGraph dominatorg)
    {
        Debug.debugMessage(getClass(), "Adding edges", 3);

        for (Vertex v : this)
        {
            for (int bbID : ((SuperBlockVertex) v).basicBlockIDs())
            {
                Vertex dominatorv = dominatorg.getVertex(bbID);
                Iterator <Edge> succIt = dominatorv.successorIterator();
                while (succIt.hasNext())
                {
                    Edge e = succIt.next();
                    int succID = e.getVertexID();
                    SuperBlockVertex superv = findSuperBlockVertex(succID);

                    /*
                     * Make sure that the basic blocks are not contained in the
                     * same super block vertex
                     */
                    if (superv.getVertexID() != v.getVertexID())
                    {
                        /*
                         * And now make sure that this edge has not already been
                         * added
                         */
                        if (!v.hasSuccessor(superv.getVertexID()))
                        {
                            addEdge(v.getVertexID(), superv.getVertexID());
                        }
                    }
                }
            }
        }
    }

    private void setRoot ()
    {
        Debug.debugMessage(getClass(), "Setting root", 3);

        for (Vertex v : this)
        {
            if (v.numOfPredecessors() == 0)
            {
                rootID = v.getVertexID();
                Debug.debugMessage(getClass(), "Root = " + rootID, 4);
            }
        }
    }

    private SuperBlockVertex findSuperBlockVertex (int bbID)
    {
        for (Vertex v : this)
        {
            SuperBlockVertex superv = (SuperBlockVertex) v;
            if (superv.containsBasicBlock(bbID))
            {
                return superv;
            }
        }
        return null;
    }

    private class DominatorGraph extends DirectedGraph
    {

        private DominatorGraph ()
        {
            Debug.debugMessage(getClass(), "Creating dominator graph", 3);

            /*
             * Make sure that the CFG has a unique entry and a unique exit.
             * Since the CFG is cloned, it does not matter that we change its
             * shape by adding vertices and edges; that is, the original CFG
             * remains the same
             */
            addVertices();
            addEdges();
        }

        private void addVertices ()
        {
            for (Vertex v : flowg)
            {
                int vertexID = v.getVertexID();
                this.addVertex(vertexID);
            }
        }

        private void addEdges ()
        {
            DominatorTree predomt = new DominatorTree(flowg,
                    flowg.getEntryID(), DominatorTreeType.PRE_DOMINATOR);
            FlowGraph reverseg = new FlowGraph();
            flowg.reverseGraph(reverseg);
            DominatorTree postdomt = new DominatorTree(reverseg,
                    flowg.getExitID(), DominatorTreeType.POST_DOMINATOR);

            for (Vertex v : predomt)
            {
                int vertexID = v.getVertexID();
                if (vertexID != predomt.getRootID())
                {
                    TreeVertex treev = (TreeVertex) v;
                    int parentID = treev.getParentID();
                    this.addEdge(parentID, vertexID);
                }
            }

            for (Vertex v : postdomt)
            {
                int vertexID = v.getVertexID();
                if (vertexID != postdomt.getRootID())
                {
                    TreeVertex treev = (TreeVertex) v;
                    int parentID = treev.getParentID();
                    /*
                     * Must check that this edge has not already been added to
                     * the dominator graph when adding the pre-dominator tree
                     * edges
                     */
                    if (!this.idToVertex.get(parentID).hasSuccessor(vertexID))
                    {
                        this.addEdge(parentID, vertexID);
                    }
                }
            }
        }
    }
}
