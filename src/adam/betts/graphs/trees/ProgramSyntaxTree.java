package adam.betts.graphs.trees;

import java.util.HashMap;
import java.util.Iterator;

import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.DirectedGraph;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.utils.AcyclicReducibility;
import adam.betts.graphs.utils.LeastCommonAncestor;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.AlternativeVertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.LeafVertex;
import adam.betts.vertices.trees.LoopVertex;
import adam.betts.vertices.trees.ProgramSyntaxVertex;
import adam.betts.vertices.trees.SequenceVertex;
import adam.betts.vertices.trees.TreeVertex;

public class ProgramSyntaxTree extends DirectedGraph
{

    protected final ControlFlowGraph cfg;
    protected final String subprogramName;
    protected final DepthFirstTree dfs;
    protected final LoopNests lnt;
    protected DominatorTree preTree;
    protected DominatorTree postTree;
    protected LeastCommonAncestor lca;
    protected HashMap <Integer, LoopVertex> headerToSyntaxTree = new HashMap <Integer, LoopVertex>();
    protected HashMap <Integer, CompressedDominatorTree> branchToCompressedDominatorTree = new HashMap <Integer, CompressedDominatorTree>();
    protected int rootID;

    public ProgramSyntaxTree (ControlFlowGraph cfg, String subprogramName)
    {
        this.cfg = cfg;
        this.subprogramName = subprogramName;
        dfs = new DepthFirstTree(cfg, cfg.getEntryID());
        lnt = new LoopNests(cfg, cfg.getEntryID());

        build();
    }

    public final ProgramSyntaxVertex getVertex (int vertexID)
    {
        return (ProgramSyntaxVertex) idToVertex.get(vertexID);
    }

    public final LoopVertex getLoopVertex (int headerID)
    {
        for (Vertex v : this)
        {
            if (v instanceof LoopVertex)
            {
                LoopVertex loop = (LoopVertex) v;
                if (loop.getHeaderID() == headerID)
                {
                    return loop;
                }
            }
        }
        return null;
    }

    public final int getRootID ()
    {
        return rootID;
    }

    public void outputStats ()
    {
        int numOfALT = 0;
        int numOfSEQ = 0;
        int numOfLOOP = 0;
        int numOfLeaves = 0;
        int numOfLambda = 0;
        int numOfMerges = 0;
        int numOfBranches = 0;
        int numOfHeaders = 0;
        int numOfBasicBlocks = 0;

        for (Vertex v : this)
        {
            if (v instanceof LoopVertex)
            {
                numOfLOOP++;
            }
            else if (v instanceof AlternativeVertex)
            {
                numOfALT++;
            }
            else if (v instanceof SequenceVertex)
            {
                numOfSEQ++;
            }
            else if (v instanceof LeafVertex)
            {
                LeafVertex leafv = (LeafVertex) v;

                if (leafv.isLambdaVertex())
                {
                    numOfLambda++;
                }
                else
                {
                    numOfLeaves++;
                }
            }
        }

        for (Vertex v : cfg)
        {
            numOfBasicBlocks++;

            if (lnt.isLoopHeader(v.getVertexID()))
            {
                numOfHeaders++;
            }
            else if (v.numOfSuccessors() > 1)
            {
                numOfBranches++;
            }

            if (v.numOfPredecessors() > 1
                    && lnt.isSelfLoop(v.getVertexID()) == false)
            {
                numOfMerges++;
            }
        }

        System.out.println("#ALT      = " + numOfALT);
        System.out.println("#SEQ      = " + numOfSEQ);
        System.out.println("#LOOP     = " + numOfLOOP);
        System.out.println("#Lambda   = " + numOfLambda);
        System.out.println("#Leaves   = " + numOfLeaves);

        System.out.println("#Headers  = " + numOfHeaders);
        System.out.println("#Branches = " + numOfBranches);
        System.out.println("#Merges   = " + numOfMerges);
        System.out.println("#Blocks   = " + numOfBasicBlocks);
    }

    private void build ()
    {
        for (int level = lnt.getHeight() - 1; level >= 0; --level)
        {
            Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
            while (levelIt.hasNext())
            {
                TreeVertex v = levelIt.next();

                if (v instanceof HeaderVertex)
                {
                    HeaderVertex headerv = (HeaderVertex) v;
                    branchToCompressedDominatorTree.clear();

                    Debug.debugMessage(getClass(),
                            "Building portion of tree inside CFG header "
                                    + headerv.getHeaderID(), 3);

                    FlowGraph flowg = lnt.induceSubraph(headerv);
                    FlowGraph reverseg = new FlowGraph();
                    flowg.reverseGraph(reverseg);

                    Debug.debugMessage(getClass(),
                            "Building pre-dominator tree", 4);
                    preTree = new DominatorTree(flowg, flowg.getEntryID(),
                            DominatorTreeType.PRE_DOMINATOR);

                    Debug.debugMessage(getClass(),
                            "Building post-dominator tree", 4);
                    postTree = new DominatorTree(reverseg, flowg.getExitID(),
                            DominatorTreeType.POST_DOMINATOR);
                    lca = new LeastCommonAncestor(postTree);

                    analyseAcyclicSubgraph(flowg, reverseg, preTree, postTree);

                    ProgramSyntaxVertex rootVertex = whichSubTree(flowg,
                            flowg.getEntryID(), flowg.getExitID(), headerv);

                    if (headerv.getHeaderID() == cfg.getEntryID())
                    {
                        this.rootID = rootVertex.getVertexID();
                        setHeight();
                    }
                    else
                    {
                        LoopVertex loop = addLoopVertex(headerv.getHeaderID());
                        addEdge(loop.getVertexID(), rootVertex.getVertexID());
                        headerToSyntaxTree.put(headerv.getHeaderID(), loop);
                    }
                }
            }
        }
    }

    private void setHeight ()
    {

    }

    private LoopVertex addLoopVertex (int headerID)
    {
        int vertexID = getNextVertexID();
        LoopVertex loop = new LoopVertex(vertexID, headerID);
        idToVertex.put(vertexID, loop);
        return loop;
    }

    private LeafVertex addLeafVertex (int cfgVertexID)
    {
        int vertexID = getNextVertexID();
        LeafVertex leaf = new LeafVertex(vertexID);
        leaf.setCFGVertexID(cfgVertexID);
        idToVertex.put(vertexID, leaf);
        return leaf;
    }

    private LeafVertex addLambdaLeafVertex ()
    {
        int vertexID = getNextVertexID();
        LeafVertex leaf = new LeafVertex(vertexID);
        leaf.setLambdaVertex();
        idToVertex.put(vertexID, leaf);
        return leaf;
    }

    private AlternativeVertex addALTVertex ()
    {
        int vertexID = getNextVertexID();
        AlternativeVertex alt = new AlternativeVertex(vertexID);
        idToVertex.put(vertexID, alt);
        return alt;
    }

    private SequenceVertex addSEQVertex ()
    {
        int vertexID = getNextVertexID();
        SequenceVertex seq = new SequenceVertex(vertexID);
        idToVertex.put(vertexID, seq);
        return seq;
    }

    private SequenceVertex buildSEQLambda ()
    {
        SequenceVertex seq = addSEQVertex();
        LeafVertex leaf = addLambdaLeafVertex();
        addEdge(seq.getVertexID(), leaf.getVertexID());
        return seq;
    }

    private void analyseAcyclicSubgraph (FlowGraph flowg, FlowGraph reverseg,
            DominatorTree preTree, DominatorTree postTree)
    {
        AcyclicReducibility acyclicReducibility = new AcyclicReducibility(
                flowg, reverseg, preTree, postTree);

        DepthFirstTree dfs = new DepthFirstTree(flowg, flowg.getEntryID());

        for (int postID = flowg.numOfVertices(); postID >= 1; --postID)
        {
            int vertexID = dfs.getPostVertexID(postID);

            if (flowg.getVertex(vertexID).numOfPredecessors() > 1)
            {
                if (acyclicReducibility.isReducibleMerge(vertexID) == false)
                {
                    Debug.debugMessage(getClass(), vertexID
                            + " is an acyclic irreducible merge vertex",
                            Debug.FUNCTION_LEVEL);
                }
            }
        }
    }

    private ProgramSyntaxVertex whichSubTree (FlowGraph flowg, int sourceID,
            int destinationID, HeaderVertex headerv)
    {
        if (flowg.getVertex(sourceID).numOfSuccessors() == 0)
        {
            return buildSEQ(flowg, sourceID, sourceID, headerv);
        }
        else
        {
            SequenceVertex seq = buildSEQ(flowg, sourceID, destinationID,
                    headerv);
            if (!flowg.getVertex(destinationID).isDummy())
            {
                LeafVertex leaf = addLeafVertex(destinationID);
                addEdge(seq.getVertexID(), leaf.getVertexID());
            }
            return seq;
        }
    }

    private AlternativeVertex buildALT (FlowGraph flowg, int branchID,
            HeaderVertex headerv)
    {
        Debug.debugMessage(getClass(), "Building ALT for branch vertex "
                + branchID, 1);

        CompressedDominatorTree comt;
        if (branchToCompressedDominatorTree.containsKey(branchID))
        {
            comt = branchToCompressedDominatorTree.get(branchID);
        }
        else
        {
            comt = new CompressedDominatorTree(flowg, postTree, lca, branchID,
                    flowg.getVertex(branchID).successorIterator());
            branchToCompressedDominatorTree.put(branchID, comt);
        }

        HashMap <Integer, AlternativeVertex> mergeRoots = new HashMap <Integer, AlternativeVertex>();
        int ipostID = postTree.getImmediateDominator(branchID);

        for (int level = comt.getHeight() - 1; level >= 1; --level)
        {
            Iterator <TreeVertex> vertexIt = comt.levelIterator(level);
            while (vertexIt.hasNext())
            {
                TreeVertex comtreev = vertexIt.next();
                int cfgvertexID = comtreev.getVertexID();
                int parentID = comtreev.getParentID();
                SequenceVertex seq = buildSEQ(flowg, cfgvertexID, parentID,
                        headerv);

                if (!comtreev.isLeaf())
                {
                    AlternativeVertex alt = mergeRoots.get(cfgvertexID);
                    addEdge(seq.getVertexID(), alt.getVertexID());
                }

                if (!mergeRoots.containsKey(parentID))
                {
                    mergeRoots.put(parentID, addALTVertex());
                }
                AlternativeVertex alt = mergeRoots.get(parentID);
                addEdge(alt.getVertexID(), seq.getVertexID());
            }
        }

        /*
         * If there is an edge from the branch to its ipost then add a lambda
         * sequence
         */
        Vertex branch = flowg.getVertex(branchID);
        if (branch.hasSuccessor(ipostID))
        {
            SequenceVertex seq = buildSEQLambda();
            AlternativeVertex alt = mergeRoots.get(ipostID);
            addEdge(alt.getVertexID(), seq.getVertexID());
        }

        return mergeRoots.get(ipostID);
    }

    private SequenceVertex buildSEQ (FlowGraph flowg, int sourceID,
            int destinationID, HeaderVertex headerv)
    {
        Debug.debugMessage(getClass(), "Building SEQ from " + sourceID + " to "
                + destinationID, 1);

        SequenceVertex seq = addSEQVertex();

        int cfgVertexID = sourceID;
        do
        {
            if (lnt.isLoopHeader(cfgVertexID)
                    && cfgVertexID != headerv.getHeaderID())
            {
                LoopVertex loop = headerToSyntaxTree.get(cfgVertexID);

                if (loop.numOfPredecessors() == 0)
                {
                    addEdge(seq.getVertexID(), loop.getVertexID());
                }
            }

            if (flowg.getVertex(cfgVertexID).isDummy() == false)
            {
                LeafVertex leaf = addLeafVertex(cfgVertexID);
                addEdge(seq.getVertexID(), leaf.getVertexID());
            }

            Vertex cfgv = flowg.getVertex(cfgVertexID);
            if (cfgv.numOfSuccessors() > 1)
            {
                AlternativeVertex alt = buildALT(flowg, cfgVertexID, headerv);
                addEdge(seq.getVertexID(), alt.getVertexID());
            }

            cfgVertexID = postTree.getImmediateDominator(cfgVertexID);
        }
        while (cfgVertexID != destinationID);

        return seq;
    }

    public final int countLeaves ()
    {
        int count = 0;
        for (Vertex v : this)
        {
            if (v instanceof LeafVertex)
            {
                count++;
            }
        }
        return count;
    }

    public final int countLambdaLeaves ()
    {
        int count = 0;
        for (Vertex v : this)
        {
            if (v instanceof LeafVertex)
            {
                LeafVertex leafv = (LeafVertex) v;
                if (leafv.isLambdaVertex())
                {
                    count++;
                }
            }
        }
        return count;
    }

    public final int countALTVertices ()
    {
        int count = 0;
        for (Vertex v : this)
        {
            if (v instanceof AlternativeVertex)
            {
                count++;
            }
        }
        return count;
    }

    public final int countSEQVertices ()
    {
        int count = 0;
        for (Vertex v : this)
        {
            if (v instanceof SequenceVertex)
            {
                count++;
            }
        }
        return count;
    }
}