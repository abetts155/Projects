package adam.betts.programs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;

import adam.betts.edges.CallEdge;
import adam.betts.edges.Edge;
import adam.betts.graphs.CFGStar;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.CallLoopGraph;
import adam.betts.graphs.ContextGraph;
import adam.betts.graphs.ControlDependenceGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.graphs.trees.SyntaxTree;
import adam.betts.outputs.UDrawGraph;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.utilities.Enums.DominatorTreeType;
import adam.betts.utilities.Enums.IProfile;
import adam.betts.utilities.Globals;
import adam.betts.vertices.BasicBlock;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.call.CallVertex;

public class Program implements Iterable <Subprogram>
{

    protected String programName = "program";
    protected int rootID;
    protected CallGraph callg = new CallGraph();
    protected ContextGraph contextg = null;
    protected CallLoopGraph clg = null;
    protected ControlFlowGraph inlinedCFG = null;
    protected HashMap <Integer, Subprogram> idToSubprogram = new LinkedHashMap <Integer, Subprogram>();
    protected HashMap <String, Integer> nameToId = new LinkedHashMap <String, Integer>();

    public Program (String programFileName)
    {
        new ProgramReader(this, programFileName);

        if (Globals.hasRoot())
        {
            rootID = nameToId.get(Globals.getRoot());
        }
        else
        {
            setRootID();
        }

        UDrawGraph.makeUDrawFile(callg, rootID);

        for (int subprogramID : idToSubprogram.keySet())
        {
            final Subprogram subprogram = idToSubprogram.get(subprogramID);
            final ControlFlowGraph cfg = subprogram.getCFG();
            cfg.addEntryAndExitEdges();
            UDrawGraph.makeUDrawFile(cfg, subprogram.getSubprogramName());
        }
    }

    public Program ()
    {
    }

    public final String getName ()
    {
        return programName;
    }

    public final void addSubprogram (int subprogramID, String subprogramName)
    {
        Subprogram subprogram = new Subprogram(subprogramID, subprogramName);
        idToSubprogram.put(subprogramID, subprogram);
        nameToId.put(subprogramName, subprogramID);
        callg.addVertex(subprogramID, subprogramName);
    }

    public final CallGraph getCallGraph ()
    {
        return callg;
    }

    public final void setRootID (int rootID)
    {
        this.rootID = rootID;
    }

    public final int getRootID ()
    {
        return rootID;
    }

    public final String getRootName ()
    {
        return idToSubprogram.get(rootID).getSubprogramName();
    }

    public Iterator <Subprogram> iterator ()
    {
        return idToSubprogram.values().iterator();
    }

    public final Subprogram getSubprogram (int subprogramID)
    {
        return idToSubprogram.get(subprogramID);
    }

    public final Subprogram getSubprogram (String subprogramName)
    {
        int subprogramID = nameToId.get(subprogramName);
        return idToSubprogram.get(subprogramID);
    }

    public final int getSubprogramID (String subprogramName)
    {
        return nameToId.get(subprogramName);
    }

    public final String getSubprogramName (int subprogramID)
    {
        return idToSubprogram.get(subprogramID).getSubprogramName();
    }

    public final Subprogram getSubprogram (long address)
    {
        for (Subprogram subprogram : idToSubprogram.values())
        {
            ControlFlowGraph cfg = subprogram.getCFG();
            if (address >= cfg.getFirstAddress()
                    && address <= cfg.getLastAddress())
            {
                return subprogram;
            }
        }

        assert false : "Unable to find subprogram with address " + address;
        return null;
    }

    public final void inline ()
    {
        Debug.debugMessage(getClass(), "Building inlined CFG", 3);

        inlinedCFG = new ControlFlowGraph();
        for (Subprogram subprogram : idToSubprogram.values())
        {
            Debug.debugMessage(
                    getClass(),
                    "Adding basic blocks and edges from "
                            + subprogram.getSubprogramName(), 3);

            ControlFlowGraph cfg = subprogram.getCFG();
            inlinedCFG.inline(cfg, subprogram.getSubprogramName());
        }

        DepthFirstTree dfs = new DepthFirstTree(callg, rootID);
        for (int i = 1; i <= dfs.numOfVertices(); ++i)
        {
            int calleeID = dfs.getPostVertexID(i);
            Subprogram calleeS = getSubprogram(calleeID);
            CallVertex callv = callg.getVertex(calleeID);

            if (calleeID == rootID)
            {
                int entryID = calleeS.getCFG().getEntryID();
                int exitID = calleeS.getCFG().getExitID();

                /*
                 * Add the dummy exit to entry edge to make the entire inlined
                 * CFG a strongly connected component
                 */
                inlinedCFG.addEdge(exitID, entryID, BranchType.UNKNOWN);

                /*
                 * Set the same vertices as the entry and exit of the inlined
                 * CFG
                 */
                inlinedCFG.setEntryID(entryID);
                inlinedCFG.setExitID(exitID);
            }

            Iterator <Edge> predIt = callv.predecessorIterator();
            while (predIt.hasNext())
            {
                CallEdge calle = (CallEdge) predIt.next();
                int callerID = calle.getVertexID();
                Subprogram callerS = getSubprogram(callerID);

                Iterator <Integer> callSiteIt = calle.iterator();
                while (callSiteIt.hasNext())
                {
                    int callSiteID = callSiteIt.next();

                    Debug.debugMessage(getClass(), "Analysing call "
                            + getSubprogram(callerID).getSubprogramName()
                            + " to "
                            + getSubprogram(calleeID).getSubprogramName()
                            + " @ call site " + callSiteID, 3);

                    /*
                     * Add an edge from the call site to the entry basic block
                     * of the callee
                     */
                    inlinedCFG.addEdge(callSiteID, calleeS.getCFG()
                            .getEntryID(), BranchType.CALL);

                    /*
                     * Then add edges from the exit basic block of the callee to
                     * each successor of the call site
                     */
                    BasicBlock callSite = callerS.getCFG().getBasicBlock(
                            callSiteID);
                    HashSet <Integer> succIDs = new HashSet <Integer>();
                    Iterator <Edge> succIt = callSite.successorIterator();
                    while (succIt.hasNext())
                    {
                        Edge succEdge = succIt.next();
                        int succID = succEdge.getVertexID();
                        succIDs.add(succID);

                        inlinedCFG.addEdge(calleeS.getCFG().getExitID(),
                                succID, BranchType.RETURN);
                    }

                    /*
                     * Remove the edges from the call site to its old successors
                     */
                    for (int succID : succIDs)
                    {
                        inlinedCFG.removeEdge(callSiteID, succID);
                    }
                }
            }
        }
        UDrawGraph.makeUDrawFile(inlinedCFG, "inlined");
    }

    public final void insertVirtualIpoints ()
    {
        Debug.verboseMessage("Adding virtual instrumentation");

        for (final Subprogram subprogram : idToSubprogram.values())
        {
            subprogram.getCFG().addAllPredecessorEdges();
            final String subprogramName = subprogram.getSubprogramName();

            for (IProfile iprofile : Globals.getInstrumentationProfiles())
            {
                Debug.debugMessage(getClass(), "Instrumenting "
                        + subprogramName + " with " + iprofile, 2);
                subprogram.buildCFGStar(iprofile);
                Debug.debugMessage(getClass(), "DONE", 2);
            }
        }
    }

    public final void buildIPGS (boolean structureOnly)
    {
        Debug.debugMessage(getClass(), "Building IPGs", 2);
        Debug.debugMessage(getClass(), "Building DFS tree of call graph", 4);

        DepthFirstTree dfs = new DepthFirstTree(callg, rootID);
        for (int i = 1; i <= callg.numOfVertices(); ++i)
        {
            int subprogramID = dfs.getPostVertexID(i);
            final Subprogram subprogram = idToSubprogram.get(subprogramID);

            Debug.debugMessage(getClass(), subprogram.getSubprogramName(), 4);

            for (IProfile iprofile : Globals.getInstrumentationProfiles())
            {
                final CFGStar cfgStar = subprogram.getCFGStar(iprofile);
                CallVertex callv = callg.getVertex(subprogramID);

                Iterator <Edge> succIt = callv.successorIterator();
                while (succIt.hasNext())
                {
                    CallEdge e = (CallEdge) succIt.next();
                    int calleeID = e.getVertexID();
                    String calleeName = idToSubprogram.get(calleeID)
                            .getSubprogramName();
                    IpointGraph calleeIPG = idToSubprogram.get(calleeID)
                            .getIPG(iprofile);

                    Debug.debugMessage(getClass(), callv.getSubprogramName()
                            + " => " + calleeName, 4);

                    if (calleeIPG.numOfVertices() > 2)
                    {
                        for (int siteID : e)
                        {
                            Debug.debugMessage(getClass(),
                                    "Adding inline from " + calleeName + " to "
                                            + subprogram.getSubprogramName()
                                            + " @ basic block " + siteID, 3);
                            cfgStar.addInline(siteID, calleeIPG, calleeName);
                        }
                    }
                }

                UDrawGraph.makeUDrawFile(iprofile, cfgStar,
                        subprogram.getSubprogramName());

                Debug.debugMessage(getClass(),
                        "Building LNT of " + subprogram.getSubprogramName(), 2);
                final LoopNests lnt = new LoopNests(cfgStar,
                        cfgStar.getEntryID());

                UDrawGraph.makeUDrawFile(iprofile, lnt,
                        subprogram.getSubprogramName());

                Debug.debugMessage(getClass(),
                        "Building IPG of " + subprogram.getSubprogramName(), 2);

                IpointGraph ipg;
                if (structureOnly)
                {
                    ipg = new IpointGraph(cfgStar);
                }
                else
                {
                    ipg = new IpointGraph(cfgStar, lnt);
                }
                subprogram.setIPG(iprofile, ipg);

                UDrawGraph.makeUDrawFile(iprofile, ipg,
                        subprogram.getSubprogramName());
            }
        }
    }

    public final void buildLNTs ()
    {
        Debug.debugMessage(getClass(), "Building LNTs", Debug.FUNCTION_LEVEL);

        for (int subprogramID : idToSubprogram.keySet())
        {
            final Subprogram subprogram = idToSubprogram.get(subprogramID);

            Debug.debugMessage(getClass(),
                    "Building LNT of " + subprogram.getSubprogramName(),
                    Debug.LOOP_LEVEL_1);

            final ControlFlowGraph cfg = subprogram.getCFG();
            cfg.generateLNT();

            UDrawGraph.makeUDrawFile(cfg.getLNT(),
                    subprogram.getSubprogramName());
        }
    }

    public final void buildControlDependenceGraphs ()
    {
        Debug.debugMessage(getClass(), "Building control dependence graphs",
                Debug.FUNCTION_LEVEL);

        for (int subprogramID : idToSubprogram.keySet())
        {
            final Subprogram subprogram = idToSubprogram.get(subprogramID);

            Debug.debugMessage(getClass(), "Control dependence of "
                    + subprogram.getSubprogramName(), Debug.LOOP_LEVEL_1);

            final ControlFlowGraph cfg = subprogram.getCFG();
            ControlDependenceGraph controlg = new ControlDependenceGraph(cfg);
            UDrawGraph.makeUDrawFile(controlg, subprogram.getSubprogramName());
        }
    }

    public final void buildDominatorTrees ()
    {
        Debug.debugMessage(getClass(), "Building Dominator Trees",
                Debug.FUNCTION_LEVEL);

        for (int subprogramID : idToSubprogram.keySet())
        {
            final Subprogram subprogram = idToSubprogram.get(subprogramID);
            final ControlFlowGraph cfg = subprogram.getCFG();

            Debug.debugMessage(getClass(), "Building pre-dominator tree of "
                    + subprogram.getSubprogramName(), Debug.LOOP_LEVEL_1);

            DominatorTree preTree = new DominatorTree(cfg, cfg.getEntryID(),
                    DominatorTreeType.PRE_DOMINATOR);
            UDrawGraph.makeUDrawFile(preTree, DominatorTreeType.PRE_DOMINATOR,
                    subprogram.getSubprogramName());

            Debug.debugMessage(getClass(), "Building post-dominator tree of "
                    + subprogram.getSubprogramName(), Debug.LOOP_LEVEL_1);

            FlowGraph reverseg = new FlowGraph();
            cfg.reverseGraph(reverseg);

            DominatorTree postTree = new DominatorTree(reverseg,
                    cfg.getExitID(), DominatorTreeType.POST_DOMINATOR);

            UDrawGraph.makeUDrawFile(postTree,
                    DominatorTreeType.POST_DOMINATOR,
                    subprogram.getSubprogramName());
        }
    }

    public final void buildSyntaxTrees ()
    {
        Debug.debugMessage(getClass(), "Building Syntax Trees",
                Debug.FUNCTION_LEVEL);

        for (int subprogramID : idToSubprogram.keySet())
        {
            final Subprogram subprogram = idToSubprogram.get(subprogramID);

            Debug.debugMessage(getClass(), "Building syntax tree of "
                    + subprogram.getSubprogramName(), Debug.LOOP_LEVEL_1);

            final ControlFlowGraph cfg = subprogram.getCFG();
            final SyntaxTree stree = new SyntaxTree(cfg,
                    subprogram.getSubprogramName());
            subprogram.setSyntaxTree(stree);
            stree.outputStats();
            UDrawGraph.makeUDrawFile(stree, subprogram.getSubprogramName());
        }
    }

    public final ContextGraph getContextGraph ()
    {
        if (contextg == null)
        {
            Debug.verboseMessage("Creating context graph");
            contextg = new ContextGraph(this);
            UDrawGraph.makeUDrawFile(contextg);
        }
        return contextg;
    }

    public final CallLoopGraph getCallLoopGraph ()
    {
        if (clg == null)
        {
            Debug.verboseMessage("Creating call loop graph");
            clg = new CallLoopGraph(this);
            UDrawGraph.makeUDrawFile(clg);
        }
        return clg;
    }

    private final void setRootID ()
    {
        ArrayList <String> rootIDs = new ArrayList <String>();

        for (Vertex v : callg)
        {
            if (v.numOfPredecessors() == 0)
            {
                rootIDs.add(((CallVertex) v).getSubprogramName());
            }
        }

        if (rootIDs.size() == 0)
        {
            Debug.errorMessage(getClass(), "Unable to find root. None found.");
        }
        else if (rootIDs.size() > 1)
        {
            Debug.errorMessage(getClass(),
                    "Unable to find root. Too many found: " + rootIDs);
        }
        else
        {
            CallVertex rootv = callg.getVertex(rootIDs.get(rootIDs.size() - 1));
            this.rootID = rootv.getVertexID();
            Debug.debugMessage(getClass(), "Root ID set to " + this.rootID, 4);
        }
    }
}
