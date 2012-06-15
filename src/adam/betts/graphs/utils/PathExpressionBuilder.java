package adam.betts.graphs.utils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.CFGStar;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.trees.CompressedDominatorTree;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.DominatorTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class PathExpressionBuilder
{

    protected final CFGStar cfgStar;
    protected final LoopNests lnt;
    protected final HashMap <Integer, AuxiliaryData> headerToAuxiliaryData = new LinkedHashMap <Integer, AuxiliaryData>();
    protected final HashMap <Integer, PathExpression> headerToPathExpression = new LinkedHashMap <Integer, PathExpression>();

    protected AuxiliaryData auxiliaryData;
    protected FlowGraph flowg;
    protected AcyclicReducibility acyclicReducibility;
    protected DominatorTree predomt;
    protected DominatorTree postdomt;
    protected LeastCommonAncestor lcaPredomt;
    protected int currentHeaderID;

    public PathExpressionBuilder (CFGStar cfgStar)
    {
        this.cfgStar = cfgStar;
        lnt = cfgStar.getLNT();

        for (int level = lnt.getHeight() - 1; level >= 0; --level)
        {
            Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
            while (levelIt.hasNext())
            {
                TreeVertex v = levelIt.next();

                if (v instanceof HeaderVertex)
                {
                    HeaderVertex headerv = (HeaderVertex) v;
                    currentHeaderID = headerv.getHeaderID();

                    Debug.debugMessage(getClass(),
                            "Analysing portion of CFG in " + currentHeaderID, 1);

                    flowg = lnt.induceSubraph(headerv);
                    auxiliaryData = new AuxiliaryData(flowg);
                    auxiliaryData.beginPathExpression(currentHeaderID);

                    acyclicReducibility = new AcyclicReducibility(flowg);
                    predomt = acyclicReducibility.predomt;
                    postdomt = acyclicReducibility.postdomt;
                    lcaPredomt = new LeastCommonAncestor(predomt);

                    doDataFlowAnalysis();

                    headerToAuxiliaryData.put(currentHeaderID, auxiliaryData);
                    headerToPathExpression.put(currentHeaderID, auxiliaryData
                            .getPathExpression(flowg.getExitID(),
                                    flowg.getEntryID()));
                }
            }
        }
    }

    public void outputPathExpression (int ancestorID, int descendantID)
    {
        Debug.debugMessage(
                getClass(),
                ancestorID
                        + " => "
                        + descendantID
                        + " :: "
                        + auxiliaryData.getPathExpression(descendantID,
                                ancestorID).toString(), 1);
    }

    private void doDataFlowAnalysis ()
    {
        DepthFirstTree dfs = new DepthFirstTree(flowg, flowg.getEntryID());

        for (int postID = flowg.numOfVertices() - 1; postID >= 1; --postID)
        {
            int vertexID = dfs.getPostVertexID(postID);
            Vertex v = flowg.getVertex(vertexID);

            if (v.numOfPredecessors() > 1)
            {
                if (acyclicReducibility.isReducibleMerge(vertexID))
                {
                    handleAcyclicReducibleMerge(v);

                }
                else
                {
                    handleAcyclicIrreducibleMerge(v);
                }
            }
            else
            {
                int predID = v.getNthPredecessor(0).getVertexID();
                Vertex predv = flowg.getVertex(predID);

                if (predv.numOfSuccessors() > 1)
                {
                    handleBranchEdge(predID, vertexID);
                }
                else
                {
                    handleSequence(predID, vertexID);
                }
            }
        }
    }

    private void handleBranchEdge (int predID, int vertexID)
    {
        Debug.debugMessage(getClass(), "Branch edge detected: " + predID
                + " => " + vertexID, 2);

        PathExpression newe = new PathExpression();
        if (lnt.isLoopHeader(vertexID))
        {
            handleLoopHeader(newe, vertexID);
        }
        else
        {
            newe.append(Integer.toString(vertexID));
        }

        auxiliaryData.addPathExpression(vertexID, predID, newe);
    }

    private void handleSequence (int predID, int vertexID)
    {
        Debug.debugMessage(getClass(), "Sequential composition: " + predID
                + " => " + vertexID, 1);

        if (cfgStar.isIpoint(vertexID))
        {
            handleIpointEdge(predID, vertexID);
        }
        else
        {
            for (int reachableID : auxiliaryData.getReachableVertices(predID))
            {
                PathExpression pathe = auxiliaryData.getPathExpression(predID,
                        reachableID);
                PathExpression newe = PathExpression.copy(pathe);

                if (newe.isEmpty() == false)
                {
                    String lastElement = newe.getLastNonSpaceElement();
                    if (lastElement.equals(PathExpression.zeroOrMoreOperator) == false
                            && lastElement
                                    .equals(PathExpression.oneOrMoreOperator) == false)
                    {
                        newe.append(PathExpression.concatenationOperator);
                    }
                }

                if (lnt.isLoopHeader(vertexID))
                {
                    handleLoopHeader(newe, vertexID);
                }
                else
                {
                    newe.append(Integer.toString(vertexID));
                }

                auxiliaryData.addPathExpression(vertexID, reachableID, newe);
            }
        }
    }

    private void handleAcyclicIrreducibleMerge (Vertex v)
    {
        int mergeID = v.getVertexID();

        Debug.debugMessage(getClass(), "Merge " + mergeID
                + " is NOT acyclic reducible", 2);
    }

    private void handleAcyclicReducibleMerge (Vertex v)
    {
        int mergeID = v.getVertexID();

        Debug.debugMessage(getClass(), "Merge " + mergeID
                + " is acyclic reducible", 2);

        HashMap <Integer, PathExpression> vertexToTempPathExpression = new LinkedHashMap <Integer, PathExpression>();

        CompressedDominatorTree comt = new CompressedDominatorTree(flowg,
                predomt, lcaPredomt, mergeID, v.predecessorIterator());
        // Traverse up each level of the compressed tree until the root
        for (int level = comt.getHeight() - 1; level >= 0; --level)
        {
            Iterator <TreeVertex> vertexIt = comt.levelIterator(level);
            while (vertexIt.hasNext())
            {
                TreeVertex comtreev = vertexIt.next();
                int vertexID = comtreev.getVertexID();

                // If a leaf is found then just buffer its path expression into
                // the temporary data structure
                if (comtreev.isLeaf())
                {
                    for (int reachableID : auxiliaryData
                            .getReachableVertices(vertexID))
                    {
                        vertexToTempPathExpression.put(vertexID, auxiliaryData
                                .getPathExpression(vertexID, reachableID));
                    }
                }
                // Otherwise build the path expression from the children
                else
                {
                    PathExpression newe = new PathExpression();

                    // If this is not the root then start the path expression at
                    // this level with the branch vertex
                    if (comtreev.getVertexID() != comt.getRootID())
                    {
                        newe.append(Integer.toString(comtreev.getVertexID()));
                    }

                    newe.append(PathExpression.space);
                    newe.append(PathExpression.openParenthesis);
                    Iterator <Edge> succIt = comtreev.successorIterator();
                    while (succIt.hasNext())
                    {
                        Edge succe = succIt.next();
                        int succID = succe.getVertexID();

                        newe.append(vertexToTempPathExpression.get(succID));
                        newe.append(PathExpression.alernateOperator);
                    }

                    if (flowg.getVertex(comtreev.getVertexID()).hasSuccessor(
                            mergeID))
                    {
                        newe.append(PathExpression.nullExpression);
                    }
                    else
                    {
                        newe.removeLastElement();
                    }

                    newe.append(PathExpression.closeParenthesis);
                    newe.append(PathExpression.space);

                    vertexToTempPathExpression.put(vertexID, newe);
                }
            }
        }

        int preID = predomt.getImmediateDominator(mergeID);
        assert vertexToTempPathExpression.containsKey(preID) : "Unable to find key for ipre("
                + mergeID + ") = " + preID;

        PathExpression pathe = vertexToTempPathExpression.get(preID);
        for (int reachableID : auxiliaryData.getReachableVertices(preID))
        {
            PathExpression newe = new PathExpression();

            if (postdomt.getImmediateDominator(preID) == mergeID)
            {
                newe.append(PathExpression.copy(auxiliaryData
                        .getPathExpression(preID, reachableID)));
            }

            newe.append(PathExpression.copy(pathe));
            newe.append(Integer.toString(mergeID));

            auxiliaryData.addPathExpression(mergeID, reachableID, newe);
        }
    }

    private void handleLoopHeader (PathExpression pathe, int headerID)
    {
        Debug.debugMessage(getClass(), "Handling header " + headerID, 2);

        if (lnt.isDowhileLoop(headerID))
        {
            pathe.append(PathExpression.space);
            pathe.append(PathExpression.openParenthesis);
            pathe.append(PathExpression.copy(headerToPathExpression
                    .get(headerID)));
            pathe.append(PathExpression.closeParenthesis);
            pathe.append(PathExpression.oneOrMoreOperator);
        }
        else
        {
            pathe.append(PathExpression.space);
            pathe.append(PathExpression.openParenthesis);
            pathe.append(PathExpression.copy(headerToPathExpression
                    .get(headerID)));
            pathe.append(PathExpression.closeParenthesis);
            pathe.append(PathExpression.zeroOrMoreOperator);
            pathe.append(Integer.toString(headerID));
        }
    }

    private void handleIpointEdge (int predID, int vertexID)
    {
        auxiliaryData.beginPathExpression(vertexID);

        for (int reachableID : auxiliaryData.getReachableVertices(predID))
        {
            PathExpression pathe = auxiliaryData.getPathExpression(predID,
                    reachableID);
            PathExpression newe = PathExpression.copy(pathe);
            newe.append(PathExpression.concatenationOperator);
            newe.append(Integer.toString(vertexID));

            auxiliaryData.addPathExpression(vertexID, reachableID, newe);

            outputPathExpression(reachableID, vertexID);
        }
    }

    private class AuxiliaryData
    {

        protected final HashMap <Integer, HashMap <Integer, PathExpression>> vertexToVertexMap = new LinkedHashMap <Integer, HashMap <Integer, PathExpression>>();

        public AuxiliaryData (FlowGraph flowg)
        {
            for (Vertex v : flowg)
            {
                vertexToVertexMap.put(v.getVertexID(),
                        new HashMap <Integer, PathExpression>());
            }
        }

        public void beginPathExpression (int vertexID)
        {
            PathExpression pathe = new PathExpression();
            pathe.append(Integer.toString(vertexID));
            vertexToVertexMap.get(vertexID).put(vertexID, pathe);
        }

        public void addPathExpression (int vertexID, int reachableID,
                PathExpression pathe)
        {
            vertexToVertexMap.get(vertexID).put(reachableID, pathe);
        }

        public PathExpression getPathExpression (int vertexID, int reachableID)
        {
            assert vertexToVertexMap.get(vertexID).containsKey(reachableID) : "Cannot find the reachable key "
                    + reachableID + " for the vertex " + vertexID;
            return vertexToVertexMap.get(vertexID).get(reachableID);
        }

        public Set <Integer> getReachableVertices (int vertexID)
        {
            return vertexToVertexMap.get(vertexID).keySet();
        }
    }
}
