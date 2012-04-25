package adam.betts.graphs;

import java.util.Iterator;

import adam.betts.edges.Edge;
import adam.betts.edges.SuperBlockCFGStructureEdge;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.outputs.OutputGraph;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.utilities.Enums.SuperBlockCFGStructureEdgeType;
import adam.betts.vertices.SuperBlockVertex;
import adam.betts.vertices.Vertex;

public class SuperBlockCFGStructureGraph extends DirectedGraph
{

    private final ControlFlowGraph cfg;
    private final SuperBlockGraph superblockg;

    public SuperBlockCFGStructureGraph (ControlFlowGraph cfg)
    {
        this.cfg = cfg;

        OutputGraph.output(cfg);
        superblockg = new SuperBlockGraph(cfg);

        addVertices();
        addEdges();
    }

    public final SuperBlockVertex getVertex (int vertexID)
    {
        return (SuperBlockVertex) idToVertex.get(vertexID);
    }

    private void addVertices ()
    {
        for (Vertex v : superblockg)
        {
            SuperBlockVertex superv = (SuperBlockVertex) v;
            SuperBlockVertex clonev = SuperBlockVertex.copy(superv);
            idToVertex.put(clonev.getVertexID(), clonev);
        }
    }

    private SuperBlockVertex getSuperBlockVertexWithBasicBlock (int bbID)
    {
        for (Vertex v : this)
        {
            SuperBlockVertex superv = (SuperBlockVertex) v;
            if (superv.containsBasicBlock(bbID))
            {
                return superv;
            }
        }

        assert false : "Unable to find basic block ID " + bbID;
        return null;
    }

    private void addEdges ()
    {
        DominatorTree predomt = new DominatorTree(cfg, cfg.getEntryID(),
                DominatorTreeType.PRE_DOMINATOR);
        FlowGraph reverseg = new FlowGraph();
        cfg.reverseGraph(reverseg);
        DominatorTree postdomt = new DominatorTree(reverseg, cfg.getExitID(),
                DominatorTreeType.POST_DOMINATOR);

        for (Vertex v : cfg)
        {
            SuperBlockVertex sourcev = getSuperBlockVertexWithBasicBlock(v
                    .getVertexID());
            int sourceID = sourcev.getVertexID();

            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                Edge cfge = succIt.next();

                SuperBlockVertex destinationv = getSuperBlockVertexWithBasicBlock(cfge
                        .getVertexID());
                int destinationID = destinationv.getVertexID();

                // Make sure that the source super block vertex does not already
                // have that destination
                // and that they are not the same vertex
                if (sourcev.hasSuccessor(destinationID) == false
                        && sourceID != destinationID)
                {
                    // Only add an edge if the basic block dominates its
                    // successor
                    if (predomt.dominates(v.getVertexID(), cfge.getVertexID()))
                    {
                        SuperBlockCFGStructureEdge succe;
                        SuperBlockCFGStructureEdge prede;

                        if (postdomt.dominates(cfge.getVertexID(),
                                v.getVertexID()) == false)
                        {
                            succe = new SuperBlockCFGStructureEdge(
                                    destinationID,
                                    SuperBlockCFGStructureEdgeType.NORMAL);
                            prede = new SuperBlockCFGStructureEdge(sourceID,
                                    SuperBlockCFGStructureEdgeType.NORMAL);
                        }
                        else
                        {
                            succe = new SuperBlockCFGStructureEdge(
                                    destinationID,
                                    SuperBlockCFGStructureEdgeType.ACYCLIC_IRREDUCIBLE);
                            prede = new SuperBlockCFGStructureEdge(
                                    sourceID,
                                    SuperBlockCFGStructureEdgeType.ACYCLIC_IRREDUCIBLE);

                        }

                        sourcev.addSuccessor(succe);
                        destinationv.addPredecessor(prede);
                    }
                }
            }
        }
    }
}
