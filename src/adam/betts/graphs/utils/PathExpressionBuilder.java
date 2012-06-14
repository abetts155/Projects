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
import adam.betts.outputs.OutputGraph;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class PathExpressionBuilder
{

    protected final HashMap <Integer, AuxiliaryData> headerToPathExpressions = new LinkedHashMap <Integer, PathExpressionBuilder.AuxiliaryData>();

    protected AuxiliaryData auxiliaryData;
    protected FlowGraph flowg;
    protected AcyclicReducibility acyclicReducibility;
    protected DominatorTree predomt;
    protected DominatorTree postdomt;
    protected LeastCommonAncestor lcaPredomt;

    public PathExpressionBuilder (CFGStar cfgStar)
    {
        LoopNests lnt = cfgStar.getLNT();
        for (int level = lnt.getHeight() - 1; level >= 0; --level)
        {
            Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
            while (levelIt.hasNext())
            {
                TreeVertex v = levelIt.next();

                if (v instanceof HeaderVertex)
                {
                    HeaderVertex headerv = (HeaderVertex) v;

                    Debug.debugMessage(
                            getClass(),
                            "Analysing portion of CFG in "
                                    + headerv.getHeaderID(), 1);

                    flowg = lnt.induceSubraph(headerv);
                    auxiliaryData = new AuxiliaryData(flowg);
                    initialise();

                    acyclicReducibility = new AcyclicReducibility(flowg);
                    predomt = acyclicReducibility.predomt;
                    postdomt = acyclicReducibility.postdomt;
                    lcaPredomt = new LeastCommonAncestor(predomt);

                    doDataFlowAnalysis();

                    headerToPathExpressions.put(headerv.getVertexID(),
                            auxiliaryData);

                    outputPathExpression(flowg.getEntryID(), flowg.getExitID());
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

    private void initialise ()
    {
        for (Vertex v : flowg)
        {
            if (v.getVertexID() == flowg.getEntryID() || v instanceof Ipoint)
            {
                auxiliaryData.beginPathExpression(v.getVertexID());
            }
        }
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

        PathExpression pathe = new PathExpression();
        pathe.append(Integer.toString(vertexID));
        auxiliaryData.addPathExpression(vertexID, predID, pathe);
    }

    private void handleSequence (int predID, int vertexID)
    {
        Debug.debugMessage(getClass(), "Sequential composition: " + predID
                + " => " + vertexID, 1);

        for (int reachableID : auxiliaryData.getReachableVertices(predID))
        {
            PathExpression pathe = auxiliaryData.getPathExpression(predID,
                    reachableID);
            PathExpression newe = PathExpression.copy(pathe);

            if (newe.isEmpty() == false)
            {
                newe.append(PathExpression.concatenationOperator);
            }
            newe.append(Integer.toString(vertexID));

            auxiliaryData.addPathExpression(vertexID, reachableID, newe);

            Debug.debugMessage(getClass(), "Path expression (" + reachableID
                    + ", " + vertexID + ") = " + newe.toString(), 4);
        }
    }

    private void handleAcyclicIrreducibleMerge (Vertex v)
    {
        Debug.debugMessage(getClass(), "Merge " + v.getVertexID()
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
        OutputGraph.output(comt);
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
                    PathExpression pathe = new PathExpression();

                    // If this is not the root then start the path expression at
                    // this level with the branch vertex
                    if (comtreev.getVertexID() != comt.getRootID())
                    {
                        pathe.append(Integer.toString(comtreev.getVertexID()));
                    }

                    pathe.append(PathExpression.openParenthesis);
                    Iterator <Edge> succIt = comtreev.successorIterator();
                    while (succIt.hasNext())
                    {
                        Edge succe = succIt.next();
                        int succID = succe.getVertexID();
                        
                        Debug.debugMessage(getClass(), "Vertex " + succID + " at vertex " + comtreev.getVertexID(), 2);  

                        pathe.append(vertexToTempPathExpression.get(succID));
                        pathe.append(PathExpression.alernateOperator);
                    }

                    if (flowg.getVertex(comtreev.getVertexID()).hasSuccessor(
                            mergeID))
                    {
                        pathe.append(PathExpression.nullExpression);
                    }
                    else
                    {
                        pathe.removeLastElement();
                    }

                    pathe.append(PathExpression.closeParenthesis);

                    vertexToTempPathExpression.put(vertexID, pathe);
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
