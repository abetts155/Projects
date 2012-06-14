package adam.betts.graphs.utils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

import adam.betts.edges.Edge;
import adam.betts.graphs.CFGStar;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class PathExpressionBuilder
{

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

                    FlowGraph flowg = lnt.induceSubraph(headerv);
                    doDataFlowAnalysis(flowg);
                }
            }
        }
    }

    private void doDataFlowAnalysis (FlowGraph flowg)
    {
        AuxiliaryData auxiliaryData = new AuxiliaryData(flowg);
        AcyclicReducibility acyclicReducibility = new AcyclicReducibility(flowg);
        DepthFirstTree dfs = new DepthFirstTree(flowg, flowg.getEntryID());

        for (int postID = flowg.numOfVertices() - 1; postID >= 1; --postID)
        {
            int vertexID = dfs.getPostVertexID(postID);
            Vertex v = flowg.getVertex(vertexID);

            if (v.numOfPredecessors() > 1)
            {
                if (acyclicReducibility.isReducibleMerge(vertexID))
                {
                    handleAcyclicIrreducibleMerge(auxiliaryData, v);
                }
                else
                {
                    handleAcyclicReducibleMerge(auxiliaryData, v);
                }
            }
            else
            {
                int predID = v.getNthPredecessor(0).getVertexID();
                Vertex predv = flowg.getVertex(predID);

                if (predv.numOfSuccessors() > 1)
                {
                    Debug.debugMessage(getClass(), "Branch edge detected: "
                            + predID + " => " + vertexID, 2);

                    auxiliaryData.addReachableVertexToVertex(vertexID, predID);

                    PathExpression pathe = auxiliaryData.getPathExpression(
                            vertexID, predID);
                    pathe.concatenate(vertexID);
                }
                else
                {
                    Debug.debugMessage(getClass(), "Sequential composition: "
                            + predID + " => " + vertexID, 2);
                }
            }

            if (v instanceof Ipoint)
            {
                auxiliaryData.addReachableVertexToVertex(vertexID, vertexID);
            }
        }
    }

    private void handleAcyclicIrreducibleMerge (AuxiliaryData auxiliaryData,
            Vertex v)
    {
        Debug.debugMessage(getClass(), "Merge " + v.getVertexID()
                + " is NOT acyclic irreducible", 2);

    }

    private void handleAcyclicReducibleMerge (AuxiliaryData auxiliaryData,
            Vertex v)
    {
        Debug.debugMessage(getClass(), "Merge " + v.getVertexID()
                + " is acyclic irreducible", 2);

        int mergeID = v.getVertexID();

        Iterator <Edge> predIt = v.predecessorIterator();
        while (predIt.hasNext())
        {
            Edge prede = predIt.next();
            int predID = prede.getVertexID();

            for (int reachableID : auxiliaryData.getReachableVertices(predID))
            {
                auxiliaryData.addReachableVertexToVertex(mergeID, reachableID);
                PathExpression pathe = auxiliaryData.getPathExpression(mergeID,
                        reachableID);

                if (pathe.isEmpty())
                {
                    pathe.concatenate(reachableID);
                    pathe.concatenate(PathExpression.openParenthesis);
                }

                pathe.concatenate(auxiliaryData.getPathExpression(predID,
                        reachableID));
                pathe.concatenate(PathExpression.alernateOperator);
            }
        }

        predIt = v.predecessorIterator();
        while (predIt.hasNext())
        {
            Edge prede = predIt.next();
            int predID = prede.getVertexID();

            for (int reachableID : auxiliaryData.getReachableVertices(predID))
            {
                PathExpression pathe = auxiliaryData.getPathExpression(mergeID,
                        reachableID);

                pathe.concatenate(PathExpression.closeParenthesis);
                pathe.concatenate(mergeID);
            }
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

        public void addReachableVertexToVertex (int vertexID, int reachableID)
        {
            vertexToVertexMap.get(vertexID).put(reachableID,
                    new PathExpression());
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
