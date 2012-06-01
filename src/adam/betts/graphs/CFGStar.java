package adam.betts.graphs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Random;

import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.outputs.UDrawGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.utilities.Enums.IProfile;
import adam.betts.vertices.BasicBlock;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.SuperBlockVertex;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.TreeVertex;

public class CFGStar extends ControlFlowGraph
{

    protected final ControlFlowGraph cfg;
    protected final String subprogramName;

    public CFGStar (ControlFlowGraph cfg, IProfile profile,
            String subprogramName)
    {
        this.cfg = cfg;
        this.subprogramName = subprogramName;

        for (Vertex v : cfg)
        {
            BasicBlock bb = (BasicBlock) v;
            addBasicBlock(bb.getVertexID());
        }

        if (cfg.getEntryID() != Vertex.DUMMY_VERTEX_ID
                && cfg.getExitID() != Vertex.DUMMY_VERTEX_ID)
        {
            if (cfg.getVertex(cfg.getEntryID()).hasPredecessors())
            {
                Debug.debugMessage(
                        getClass(),
                        "Removing edge " + cfg.getExitID() + " => "
                                + cfg.getEntryID(), 4);
                cfg.removeEdge(cfg.getExitID(), cfg.getEntryID());
            }
        }

        switch (profile)
        {
            case BASIC_BLOCK:
                instrumentBasicBlocks();
                break;
            case BRANCH:
                instrumentTakenBranches();
                break;
            case PRE_DOMINATOR:
                instrumentPreDominatorLeaves();
                break;
            case SUPER_BLOCK:
                instrumentSuperBlocks();
                break;
            case RANDOM:
                instrumentRandomly();
                break;
            default:
                Debug.errorMessage(getClass(), "Unhandled CFG* instrumentation");
        }

        addEntryAndExitEdges();
    }

    public final ControlFlowGraph getCFG ()
    {
        return cfg;
    }

    public final void addIpoint (int vertexID, long ipointID)
    {
        idToVertex.put(vertexID, new Ipoint(vertexID, ipointID));
    }

    public final void addIpoint (int vertexID, long ipointID,
            String subprogramName)
    {
        idToVertex
                .put(vertexID, new Ipoint(vertexID, ipointID, subprogramName));
    }

    public final Ipoint getIpoint (int vertexID)
    {
        return (Ipoint) idToVertex.get(vertexID);
    }

    public final boolean isIpoint (int vertexID)
    {
        return idToVertex.get(vertexID) instanceof Ipoint;
    }

    public final boolean isBasicBlock (int vertexID)
    {
        return idToVertex.get(vertexID) instanceof BasicBlock;
    }

    public final void addInline (int siteID, IpointGraph ipg, String calleeName)
    {
        Debug.debugMessage(getClass(), "Analsying call site " + siteID, 1);

        BasicBlock bb = getBasicBlock(siteID);
        HashSet <Integer> predIDs = new HashSet <Integer>();
        HashSet <Integer> succIDs = new HashSet <Integer>();

        /*
         * First add the master entry vertices to the CFG*
         */
        for (Integer entryID : ipg.masterEntries())
        {
            int ipointID = getNextVertexID();
            addIpoint(ipointID, ipg.getVertex(entryID).getIpointID(),
                    calleeName);
            Ipoint duplicate = getIpoint(ipointID);
            duplicate.setInlinedEntry();

            Debug.debugMessage(getClass(), "Adding master entry " + ipointID, 4);

            /*
             * Link the master entry to each of the predecessors of the call
             * site
             */
            Iterator <Edge> predIt = bb.predecessorIterator();
            while (predIt.hasNext())
            {
                Edge e = predIt.next();
                int predID = e.getVertexID();

                boolean addEdge = false;
                if (isBasicBlock(predID))
                {
                    addEdge = true;
                }
                else
                {
                    Ipoint p = (Ipoint) idToVertex.get(predID);
                    if (!p.isInlinedEntry())
                    {
                        addEdge = true;
                    }
                }

                if (addEdge)
                {
                    predIDs.add(predID);
                    addEdge(predID, ipointID);
                    Debug.debugMessage(getClass(), "Adding edge " + predID
                            + " => " + ipointID, 4);
                }
            }

            /*
             * Link the master entry to the call site itself
             */
            addEdge(ipointID, siteID);
            Debug.debugMessage(getClass(), "Adding edge " + ipointID + " => "
                    + siteID, 4);
        }

        for (Integer exitID : ipg.masterExits())
        {
            int ipointID = getNextVertexID();
            addIpoint(ipointID, ipg.getVertex(exitID).getIpointID(), calleeName);
            Ipoint duplicate = getIpoint(ipointID);
            duplicate.setInlinedExit();
            Debug.debugMessage(getClass(), "Adding master exit " + ipointID, 4);

            /*
             * Link the master exit to each of the successors of the call site
             */
            Iterator <Edge> succIt = bb.successorIterator();
            while (succIt.hasNext())
            {
                Edge e = succIt.next();
                int succID = e.getVertexID();

                boolean addEdge = false;
                if (isBasicBlock(succID))
                {
                    addEdge = true;
                }
                else
                {
                    Ipoint s = (Ipoint) idToVertex.get(succID);
                    if (!s.isInlinedExit())
                    {
                        addEdge = true;
                    }
                }

                if (addEdge)
                {
                    succIDs.add(succID);
                    addEdge(ipointID, succID);
                    Debug.debugMessage(getClass(), "Adding edge " + ipointID
                            + " => " + succID, 4);
                }
            }

            /*
             * Link the call site to the master exit
             */
            addEdge(siteID, ipointID);
            Debug.debugMessage(getClass(), "Adding edge " + siteID + " => "
                    + ipointID, 4);
        }

        /*
         * Finally unlink the old predecessors and successors of the call site
         */
        for (int predID : predIDs)
        {
            removeEdge(predID, siteID);
            Debug.debugMessage(getClass(), "Removing edge " + predID + " => "
                    + siteID, 4);
        }

        for (int succID : succIDs)
        {
            removeEdge(siteID, succID);
            Debug.debugMessage(getClass(), "Removing edge " + siteID + " => "
                    + succID, 4);
        }
    }

    private void instrumentBasicBlocks ()
    {
        HashMap <Integer, Integer> instrumented = new HashMap <Integer, Integer>();

        for (Vertex v : cfg)
        {
            BasicBlock bb = (BasicBlock) v;
            int ipointID = getNextVertexID();
            addIpoint(ipointID, bb.getFirstAddress());
            addEdge(ipointID, bb.getVertexID());

            instrumented.put(v.getVertexID(), ipointID);

            Debug.debugMessage(getClass(), "Adding ipoint " + ipointID
                    + " for bb " + bb.getVertexID(), 4);
            Debug.debugMessage(getClass(), "Adding edge " + ipointID + " => "
                    + bb.getVertexID(), 4);
        }

        instrumentFunctionLimits(instrumented);
        addRemainingEdges(instrumented);
    }

    private void instrumentTakenBranches ()
    {
        HashMap <Integer, Integer> instrumented = new HashMap <Integer, Integer>();

        for (Vertex v : cfg)
        {
            BasicBlock bb = (BasicBlock) v;

            Iterator <Edge> succIt = bb.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                int succID = e.getVertexID();

                if (e.getBranchType() == BranchType.TAKEN)
                {
                    BasicBlock succ = cfg.getBasicBlock(succID);

                    if (!instrumented.containsKey(succID))
                    {
                        int ipointID = getNextVertexID();
                        /*
                         * The first address of the successor is seen in the
                         * trace when the branch is taken
                         */
                        addIpoint(ipointID, succ.getFirstAddress());
                        addEdge(ipointID, succID);
                        instrumented.put(succ.getVertexID(), ipointID);

                        Debug.debugMessage(getClass(), "Adding ipoint "
                                + ipointID + " for edge " + v.getVertexID()
                                + "=>" + succID, 4);
                        Debug.debugMessage(getClass(), "Adding edge "
                                + ipointID + " => " + succID, 4);
                    }
                }
            }
        }

        instrumentFunctionLimits(instrumented);
        addRemainingEdges(instrumented);
    }

    private void instrumentFunctionLimits (
            HashMap <Integer, Integer> instrumented)
    {
        for (Vertex v : cfg)
        {
            int vertexID = v.getVertexID();
            BasicBlock bb = (BasicBlock) v;

            /*
             * Only instrument the entry vertex if not already done so. This
             * could happen, for example, if the sub-program only contains a
             * single vertex
             */
            if (bb.numOfPredecessors() == 0
                    && !instrumented.containsKey(vertexID))
            {
                int ipointID = getNextVertexID();
                addIpoint(ipointID, bb.getFirstAddress());
                addEdge(ipointID, vertexID);

                Debug.debugMessage(getClass(), "Adding ipoint " + ipointID
                        + " for entry vertex " + vertexID, 4);
                Debug.debugMessage(getClass(), "Adding edge " + ipointID
                        + " => " + vertexID, 4);
            }

            /*
             * Only instrument an exit vertex if not already done so and it has
             * more than one instruction
             */
            if (bb.numOfSuccessors() == 0
                    && !instrumented.containsKey(vertexID))
            {
                int ipointID = getNextVertexID();
                addIpoint(ipointID, bb.getLastAddress());
                addEdge(bb.getVertexID(), ipointID);

                Debug.debugMessage(getClass(), "Adding ipoint " + ipointID
                        + " for exit vertex " + vertexID, 4);
                Debug.debugMessage(getClass(),
                        "Adding edge " + bb.getVertexID() + " => " + ipointID,
                        4);
            }
        }
    }

    private void addRemainingEdges (HashMap <Integer, Integer> instrumented)
    {
        for (Vertex v : cfg)
        {
            int vertexID = v.getVertexID();
            BasicBlock bb = (BasicBlock) v;
            Iterator <Edge> succIt = bb.successorIterator();
            while (succIt.hasNext())
            {
                Edge e = succIt.next();
                int succID = e.getVertexID();

                if (instrumented.containsKey(succID))
                {
                    addEdge(vertexID, instrumented.get(succID));
                    Debug.debugMessage(getClass(), "Adding edge " + vertexID
                            + " => " + instrumented.get(succID), 4);
                }
                else
                {
                    addEdge(vertexID, succID);
                    Debug.debugMessage(getClass(), "Adding edge " + vertexID
                            + " => " + succID, 4);
                }
            }
        }
    }

    private void instrumentPreDominatorLeaves ()
    {
        HashMap <Integer, Integer> instrumented = new HashMap <Integer, Integer>();

        /*
         * Make sure the CFG has its entry ID set to build the pre-dominator
         * tree
         */
        cfg.setEntry();

        DominatorTree predomT = new DominatorTree(cfg, cfg.getEntryID(),
                DominatorTreeType.PRE_DOMINATOR);

        UDrawGraph.makeUDrawFile(predomT, DominatorTreeType.PRE_DOMINATOR,
                subprogramName);

        for (Vertex v : predomT)
        {
            int vertexID = v.getVertexID();
            BasicBlock bb = cfg.getBasicBlock(vertexID);
            TreeVertex treev = (TreeVertex) v;

            if (vertexID != cfg.getExitID() && vertexID != cfg.getEntryID()
                    && treev.isLeaf())
            {
                int ipointID = getNextVertexID();
                addIpoint(ipointID, bb.getFirstAddress());
                addEdge(ipointID, vertexID);
                instrumented.put(vertexID, ipointID);

                Debug.debugMessage(getClass(), "Adding ipoint " + ipointID
                        + " for bb " + vertexID, 4);
                Debug.debugMessage(getClass(), "Adding edge " + ipointID
                        + " => " + vertexID, 4);
            }
        }

        instrumentFunctionLimits(instrumented);
        addRemainingEdges(instrumented);
    }

    private void instrumentSuperBlocks ()
    {
        SuperBlockGraph superg = new SuperBlockGraph(cfg);

        HashMap <Integer, Integer> instrumented = new HashMap <Integer, Integer>();

        for (Vertex v : superg)
        {
            SuperBlockVertex superv = (SuperBlockVertex) v;
            List <Integer> bbIDs = superv.basicBlockIDs();

            if (v.numOfSuccessors() == 0)
            {
                int bbID = bbIDs.get(0);
                BasicBlock bb = cfg.getBasicBlock(bbID);
                int ipointID = getNextVertexID();
                addIpoint(ipointID, bb.getFirstAddress());
                addEdge(ipointID, bb.getVertexID());

                instrumented.put(bbID, ipointID);

                Debug.debugMessage(getClass(), "Adding ipoint " + ipointID
                        + " for bb " + bb.getVertexID(), 4);
            }
        }

        instrumentFunctionLimits(instrumented);
        addRemainingEdges(instrumented);
    }

    private void instrumentRandomly ()
    {
        Random random = new Random();
        int totalIpoints = random.nextInt(cfg.numOfVertices()) + 1;
        ArrayList <Integer> uninstrumented = new ArrayList <Integer>(
                idToVertex.keySet());
        HashMap <Integer, Integer> vToIpoint = new LinkedHashMap <Integer, Integer>();

        /*
         * First link ipoints to basic blocks
         */
        while (totalIpoints > 0)
        {
            int i = random.nextInt(uninstrumented.size());
            int vertexID = uninstrumented.get(i);
            uninstrumented.remove(i);
            BasicBlock bb = cfg.getBasicBlock(vertexID);
            int ipointID = getNextVertexID();
            addIpoint(ipointID, bb.getFirstAddress());
            addEdge(ipointID, vertexID);
            vToIpoint.put(vertexID, ipointID);
            totalIpoints--;
            Debug.debugMessage(getClass(), "Adding ipoint " + ipointID
                    + " for bb " + bb.getVertexID(), 4);
        }

        for (Vertex v : cfg)
        {
            int vertexID = v.getVertexID();
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                int succID = e.getVertexID();

                /*
                 * Check whether the basic block was selected for
                 * instrumentation
                 */
                if (vToIpoint.containsKey(succID))
                {
                    /*
                     * If so, link to the ipoint
                     */
                    addEdge(vertexID, vToIpoint.get(succID));
                    Debug.debugMessage(getClass(), "Adding edge " + vertexID
                            + " => " + vToIpoint.get(succID), 4);
                }
                else
                {
                    addEdge(vertexID, succID);
                    Debug.debugMessage(getClass(), "Adding edge " + vertexID
                            + " => " + succID, 4);
                }
            }
        }
    }

    public void addEntryAndExitEdges ()
    {
        entryID = getNextVertexID();
        addIpoint(entryID, Ipoint.GHOST_IPOINT_ID);
        exitID = getNextVertexID();
        addIpoint(exitID, Ipoint.GHOST_IPOINT_ID);

        for (Vertex v : this)
        {
            int vertexID = v.getVertexID();
            if (vertexID != entryID && vertexID != exitID)
            {
                if (v.numOfPredecessors() == 0)
                {
                    addEdge(entryID, vertexID);
                }
                if (v.numOfSuccessors() == 0)
                {
                    addEdge(vertexID, exitID);
                }
            }
        }

        addEdge(exitID, entryID);
    }
}
