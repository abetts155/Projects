package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.edges.SuperBlockCFGStructureEdge;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.SuperBlockCFGStructureEdgeType;
import adam.betts.vertices.SuperBlockVertex;
import adam.betts.vertices.Vertex;

public class SuperBlockCFGStructureGraph extends DirectedGraph
{

    private final FlowGraph flowg;
    private final SuperBlockGraph superblockg;
    protected int rootID;
    private ArrayList <SuperBlockVertex> disconnectedVertices = new ArrayList <SuperBlockVertex>();

    public SuperBlockCFGStructureGraph (FlowGraph flowg)
    {
        this.flowg = flowg;
        superblockg = new SuperBlockGraph(flowg);
        rootID = Vertex.DUMMY_VERTEX_ID;

        addVertices();
        addEdges();

        assert disconnectedVertices.size() == 0 : "Dummy super block vertices added for branches with direct edge to post dominator are not empty";

        setRoot();
    }

    public final SuperBlockVertex getVertex (int vertexID)
    {
        return (SuperBlockVertex) idToVertex.get(vertexID);
    }

    public final int getRootID ()
    {
        assert rootID != Vertex.DUMMY_VERTEX_ID;
        return rootID;
    }

    public HashMap <Integer, Set <SuperBlockCFGStructureEdge>> partitionEdges (
            SuperBlockVertex superv)
    {
        assert superv.numOfSuccessors() > 1 : "Paritioning of super block edges only works for branch vertices";

        HashMap <Integer, Set <SuperBlockCFGStructureEdge>> theMap = new HashMap <Integer, Set <SuperBlockCFGStructureEdge>>();

        Iterator <Edge> succIt = superv.successorIterator();
        while (succIt.hasNext())
        {
            Edge succe = succIt.next();
            SuperBlockCFGStructureEdge supere = (SuperBlockCFGStructureEdge) succe;
            int bbID = supere.getBasicBlockID();

            if (theMap.containsKey(bbID) == false)
            {
                theMap.put(bbID, new HashSet <SuperBlockCFGStructureEdge>());
            }

            theMap.get(bbID).add(supere);
        }

        return theMap;
    }

    private void addVertices ()
    {
        for (Vertex v : superblockg)
        {
            SuperBlockVertex superv = (SuperBlockVertex) v;
            SuperBlockVertex clonev = SuperBlockVertex.copy(superv);
            idToVertex.put(clonev.getVertexID(), clonev);
        }

        for (Vertex v : flowg)
        {
            if (v.numOfSuccessors() > 1)
            {
                int postDomID = superblockg.getPostdominatorTree()
                        .getImmediateDominator(v.getVertexID());

                if (v.hasSuccessor(postDomID))
                {
                    int nextVertexID = getNextVertexID();
                    SuperBlockVertex superv = new SuperBlockVertex(nextVertexID);
                    superv.addBasicBlock(Vertex.DUMMY_VERTEX_ID);
                    idToVertex.put(nextVertexID, superv);
                    disconnectedVertices.add(superv);
                }
            }
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
        int edgeID = 1;

        for (Vertex v : flowg)
        {
            if (v.numOfSuccessors() > 1)
            {
                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    Edge cfge = succIt.next();

                    SuperBlockVertex sourcev = getSuperBlockVertexWithBasicBlock(v
                            .getVertexID());
                    SuperBlockVertex destinationv = getSuperBlockVertexWithBasicBlock(cfge
                            .getVertexID());

                    if (superblockg.getPostdominatorTree().dominates(
                            cfge.getVertexID(), v.getVertexID()) == false)
                    {
                        if (sourcev.hasSuccessor(destinationv.getVertexID()) == false)
                        {
                            SuperBlockCFGStructureEdge succe = new SuperBlockCFGStructureEdge(
                                    destinationv.getVertexID(), edgeID);
                            SuperBlockCFGStructureEdge prede = new SuperBlockCFGStructureEdge(
                                    sourcev.getVertexID(), edgeID);

                            sourcev.addSuccessor(succe);
                            destinationv.addPredecessor(prede);

                            edgeID++;
                        }

                        SuperBlockCFGStructureEdge succe = (SuperBlockCFGStructureEdge) sourcev
                                .getSuccessor(destinationv.getVertexID());
                        SuperBlockCFGStructureEdge prede = (SuperBlockCFGStructureEdge) destinationv
                                .getPredecessor(sourcev.getVertexID());

                        succe.setBasicBlockID(v.getVertexID());
                        prede.setBasicBlockID(v.getVertexID());
                    }
                    else
                    {
                        SuperBlockVertex newDestinationv = disconnectedVertices
                                .remove(disconnectedVertices.size() - 1);

                        SuperBlockCFGStructureEdge succe = new SuperBlockCFGStructureEdge(
                                newDestinationv.getVertexID(), edgeID);
                        SuperBlockCFGStructureEdge prede = new SuperBlockCFGStructureEdge(
                                sourcev.getVertexID(), edgeID);

                        sourcev.addSuccessor(succe);
                        newDestinationv.addPredecessor(prede);

                        succe.setBasicBlockID(v.getVertexID());
                        prede.setBasicBlockID(v.getVertexID());

                        edgeID++;
                    }
                }
            }

            if (v.numOfPredecessors() > 1)
            {
                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    Edge cfge = predIt.next();

                    SuperBlockVertex sourcev = getSuperBlockVertexWithBasicBlock(cfge
                            .getVertexID());
                    SuperBlockVertex destinationv = getSuperBlockVertexWithBasicBlock(v
                            .getVertexID());

                    if (sourcev.getVertexID() != destinationv.getVertexID())
                    {
                        if (superblockg.getPostdominatorTree().dominates(
                                v.getVertexID(), cfge.getVertexID()) == false)
                        {
                            Debug.debugMessage(
                                    getClass(),
                                    v.getVertexID()
                                            + " does NOT post-dominate "
                                            + cfge.getVertexID(), 4);

                            if (sourcev
                                    .hasSuccessor(destinationv.getVertexID()) == false)
                            {
                                SuperBlockCFGStructureEdge succe = new SuperBlockCFGStructureEdge(
                                        destinationv.getVertexID(), edgeID);
                                SuperBlockCFGStructureEdge prede = new SuperBlockCFGStructureEdge(
                                        sourcev.getVertexID(), edgeID);

                                sourcev.addSuccessor(succe);
                                destinationv.addPredecessor(prede);

                                edgeID++;
                            }

                            SuperBlockCFGStructureEdge succe = (SuperBlockCFGStructureEdge) sourcev
                                    .getSuccessor(destinationv.getVertexID());
                            SuperBlockCFGStructureEdge prede = (SuperBlockCFGStructureEdge) destinationv
                                    .getPredecessor(sourcev.getVertexID());

                            succe.setEdgeType(SuperBlockCFGStructureEdgeType.ACYCLIC_IRREDUCIBLE);
                            prede.setEdgeType(SuperBlockCFGStructureEdgeType.ACYCLIC_IRREDUCIBLE);
                        }
                    }
                }
            }
        }
    }

    private void setRoot ()
    {
        for (Vertex v : this)
        {
            if (v.numOfPredecessors() == 0)
            {
                assert rootID == Vertex.DUMMY_VERTEX_ID : "Multiple roots found in super block graph";
                rootID = v.getVertexID();
            }
        }
    }
}
