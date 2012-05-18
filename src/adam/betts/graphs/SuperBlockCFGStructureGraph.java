package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.edges.SuperBlockCFGStructureEdge;
import adam.betts.graphs.utils.DominanceFrontiers;
import adam.betts.outputs.UDrawGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.vertices.SuperBlockVertex;
import adam.betts.vertices.Vertex;

public class SuperBlockCFGStructureGraph extends DirectedGraph
{

    private final FlowGraph flowg;
    protected int rootID = Vertex.DUMMY_VERTEX_ID;
    private ArrayList <SuperBlockVertex> disconnectedVertices = new ArrayList <SuperBlockVertex>();

    public SuperBlockCFGStructureGraph (FlowGraph flowg)
    {
        this.flowg = flowg;

        SuperBlockGraph superblockg = new SuperBlockGraph(flowg);
        addVertices(superblockg);
        addEdges(superblockg);

        assert disconnectedVertices.size() == 0 : "Dummy super block vertices added for branches with direct edge to post dominator are not empty";

        setRoot();
        checkForSelfLoops();
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

    private void addVertices (SuperBlockGraph superblockg)
    {
        Debug.debugMessage(getClass(), "Adding vertices", 4);

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

    private void addEdges (SuperBlockGraph superblockg)
    {
        Debug.debugMessage(getClass(), "Adding edges", 4);

        int edgeID = 1;

        DominanceFrontiers postDominanceFrontiers = new DominanceFrontiers(
                superblockg.getReverseFlowGraph(),
                superblockg.getPostdominatorTree());

        for (Vertex v : flowg)
        {
            int vertexID = v.getVertexID();
            Debug.debugMessage(getClass(), "Vertex " + vertexID, 4);

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
                            cfge.getVertexID(), vertexID) == false)
                    {
                        if (sourcev.hasSuccessor(destinationv.getVertexID()) == false)
                        {
                            SuperBlockCFGStructureEdge succe = new SuperBlockCFGStructureEdge(
                                    destinationv.getVertexID(), edgeID);
                            SuperBlockCFGStructureEdge prede = new SuperBlockCFGStructureEdge(
                                    sourcev.getVertexID(), edgeID);

                            sourcev.addSuccessor(succe);
                            destinationv.addPredecessor(prede);

                            if (sourcev.getVertexID() == destinationv
                                    .getVertexID())
                            {
                                Debug.debugMessage(getClass(),
                                        "Adding self-loop due to " + vertexID
                                                + " => " + cfge.getVertexID(),
                                        1);
                            }

                            edgeID++;
                        }

                        SuperBlockCFGStructureEdge succe = (SuperBlockCFGStructureEdge) sourcev
                                .getSuccessor(destinationv.getVertexID());
                        SuperBlockCFGStructureEdge prede = (SuperBlockCFGStructureEdge) destinationv
                                .getPredecessor(sourcev.getVertexID());

                        succe.setBasicBlockID(vertexID);
                        prede.setBasicBlockID(vertexID);
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

                        succe.setBasicBlockID(vertexID);
                        prede.setBasicBlockID(vertexID);

                        edgeID++;
                    }
                }
            }

            if (v.numOfPredecessors() > 1)
            {
                int predomID = superblockg.getPredominatorTree()
                        .getImmediateDominator(vertexID);

                if (superblockg.getPostdominatorTree().getImmediateDominator(
                        predomID) != vertexID)
                {
                    if (postDominanceFrontiers.dfSet(vertexID).size() > 1)
                    {
                        Debug.debugMessage(
                                getClass(),
                                "Acyclic IRREDUCIBLE merge found "
                                        + vertexID
                                        + " ipre("
                                        + vertexID
                                        + ") = "
                                        + predomID
                                        + ", |DF("
                                        + vertexID
                                        + ")| = "
                                        + postDominanceFrontiers
                                                .dfSet(vertexID), 4);

                        SuperBlockVertex destinationv = getSuperBlockVertexWithBasicBlock(v
                                .getVertexID());
                        destinationv.setIsUnstructuredMerge();

                        Iterator <Edge> predIt = v.predecessorIterator();
                        while (predIt.hasNext())
                        {
                            Edge cfge = predIt.next();

                            SuperBlockVertex sourcev = getSuperBlockVertexWithBasicBlock(cfge
                                    .getVertexID());

                            assert sourcev.getVertexID() != destinationv
                                    .getVertexID() : "Super blocks are the same for acyclic irreducible merge";

                            if (sourcev
                                    .hasSuccessor(destinationv.getVertexID()) == false)
                            {
                                SuperBlockCFGStructureEdge succe = new SuperBlockCFGStructureEdge(
                                        destinationv.getVertexID(), edgeID);
                                SuperBlockCFGStructureEdge prede = new SuperBlockCFGStructureEdge(
                                        sourcev.getVertexID(), edgeID);

                                sourcev.addSuccessor(succe);
                                destinationv.addPredecessor(prede);

                                succe.setBasicBlockID(cfge.getVertexID());
                                prede.setBasicBlockID(cfge.getVertexID());

                                edgeID++;
                            }
                        }
                    }
                    else
                    {
                        Debug.debugMessage(
                                getClass(),
                                "Acyclic REDUCIBLE Merge found "
                                        + vertexID
                                        + " ipre("
                                        + vertexID
                                        + ") = "
                                        + predomID
                                        + ", |DF("
                                        + vertexID
                                        + ")| = "
                                        + postDominanceFrontiers
                                                .dfSet(vertexID), 4);

                        SuperBlockVertex destinationv = getSuperBlockVertexWithBasicBlock(v
                                .getVertexID());

                        Iterator <Edge> predIt = v.predecessorIterator();
                        while (predIt.hasNext())
                        {
                            Edge cfge = predIt.next();

                            SuperBlockVertex sourcev = getSuperBlockVertexWithBasicBlock(cfge
                                    .getVertexID());

                            assert sourcev.getVertexID() != destinationv
                                    .getVertexID() : "Super blocks are the same for acyclic irreducible merge";

                            if (sourcev
                                    .hasSuccessor(destinationv.getVertexID()) == false)
                            {
                                SuperBlockCFGStructureEdge succe = new SuperBlockCFGStructureEdge(
                                        destinationv.getVertexID(), edgeID);
                                SuperBlockCFGStructureEdge prede = new SuperBlockCFGStructureEdge(
                                        sourcev.getVertexID(), edgeID);

                                sourcev.addSuccessor(succe);
                                destinationv.addPredecessor(prede);

                                succe.setBasicBlockID(cfge.getVertexID());
                                prede.setBasicBlockID(cfge.getVertexID());

                                edgeID++;
                            }
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
                if (rootID != Vertex.DUMMY_VERTEX_ID)
                {

                    Debug.debugMessage(getClass(), "Vertex " + v.getVertexID()
                            + " (" + this.getVertex(rootID).basicBlockIDs()
                            + ") is potential root", 4);

                    Debug.debugMessage(getClass(), "Vertex " + v.getVertexID()
                            + " ("
                            + this.getVertex(v.getVertexID()).basicBlockIDs()
                            + ") is potential root", 4);
                }

                assert rootID == Vertex.DUMMY_VERTEX_ID : "Multiple roots found in super block graph";
                rootID = v.getVertexID();
            }
        }

        if (rootID == Vertex.DUMMY_VERTEX_ID)
        {
            Debug.debugMessage(getClass(), "Unable to find root", 4);
            UDrawGraph.makeUDrawFile(this, "debug");

            for (Vertex v : this)
            {
                SuperBlockVertex superv = (SuperBlockVertex) v;

                if (superv.basicBlockIDs().contains(flowg.getEntryID()))
                {
                    Debug.debugMessage(getClass(),
                            "Vertex " + superv.basicBlockIDs(), 4);
                }
            }
        }
    }

    private void checkForSelfLoops ()
    {
        for (Vertex v : this)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                Edge succe = succIt.next();

                assert succe.getVertexID() != v.getVertexID() : "Found erroneous self-loop edge "
                        + v.getVertexID() + " => " + succe.getVertexID();
            }
        }
    }
}
