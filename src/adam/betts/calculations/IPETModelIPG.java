package adam.betts.calculations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;

import lpsolve.LpSolve;
import lpsolve.LpSolveException;
import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.utilities.Debug;
import adam.betts.vertices.Vertex;

public class IPETModelIPG extends IPETModel
{

    protected final IpointGraph ipg;
    protected final IPGDatabase database;
    protected final int subprogramID;

    public IPETModelIPG (IPGDatabase database, IpointGraph ipg, int subprogramID)
    {
        this.ipg = ipg;
        this.database = database;
        this.subprogramID = subprogramID;

        final String fileName = "F" + Integer.toString(subprogramID)
                + ".ipg.lp";
        try
        {

            final File file = new File(ILPdirectory, fileName);

            BufferedWriter out = new BufferedWriter(new FileWriter(
                    file.getAbsolutePath()));

            writeObjectiveFunction(out);
            writeFlowConstraints(out);
            writeLoopConstraints(out);
            writeIntegerConstraints(out);

            out.close();

            try
            {
                /*
                 * The Linear Program Will Always Have at Least |V| Structural
                 * Constraints (Equivalent To Rows) and exactly |V| Variables
                 * (equivalent to Columns).
                 */
                numOfColumns = ipg.numOfEdges();
                Debug.debugMessage(getClass(), "About to solve 1", 1);
                lp = LpSolve.makeLp(numOfColumns, numOfColumns);
                Debug.debugMessage(getClass(), "About to solve 2", 1);

                lp = LpSolve.readLp(file.getAbsolutePath(),
                        IPETModel.getLpSolveVerbosity(), null);
                Debug.debugMessage(getClass(), "About to solve 3", 1);
                try
                {
                    solve();
                }
                catch (SolutionException e)
                {
                    System.exit(1);
                }
            }
            catch (LpSolveException e)
            {
                e.printStackTrace();
                System.exit(1);
            }
        }
        catch (IOException e)
        {
            Debug.errorMessage(getClass(), "Problem with file " + fileName);
        }
    }

    protected void solve () throws LpSolveException, SolutionException
    {
        Debug.debugMessage(getClass(), "Solving ILP", 3);

        long stamp1 = System.nanoTime();
        int solution = lp.solve();
        long stamp2 = System.nanoTime();

        switch (solution)
        {
            case LpSolve.OPTIMAL:
                Debug.debugMessage(getClass(),
                        "Optimal solution found " + lp.getObjective(), 3);
                wcet = Math.round(lp.getObjective());

                break;
            default:
                Debug.debugMessage(getClass(), "Problem with the LP model: "
                        + solution, 2);
                throw new SolutionException(solution);
        }
    }

    private void writeObjectiveFunction (BufferedWriter out) throws IOException
    {
        Debug.debugMessage(getClass(), "Writing objective function", 3);

        out.write(createComment("Objective function"));
        out.write(maxString);

        int num = 1;
        int numOfEdges = ipg.numOfEdges();
        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                Debug.debugMessage(getClass(), "Edge " + e.getEdgeID(), 1);
                long wcet = database.getUnitWCET(subprogramID, e.getEdgeID());

                out.write(Long.toString(wcet) + space
                        + createEdgeVariable(e.getEdgeID()));

                if (num < numOfEdges)
                {
                    out.write(plus);
                }
                if (num % 10 == 0)
                {
                    out.newLine();
                }
                num++;
            }
        }

        out.write(statementTerminator + newLine + newLine);
    }

    private void writeFlowConstraints (BufferedWriter out) throws IOException
    {
        Debug.debugMessage(getClass(), "Writing flow constraints", 3);
        for (Vertex v : ipg)
        {
            if (v.hasPredecessors() && v.hasSuccessors())
            {
                out.write(createComment("Vertex "
                        + Integer.toString(v.getVertexID())));

                out.write(createVertexVariable(v.getVertexID()) + equals);

                int num = 1;
                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    FlowEdge e = (FlowEdge) predIt.next();
                    int edgeID = e.getEdgeID();
                    out.write(createEdgeVariable(edgeID));

                    if (num++ < v.numOfPredecessors())
                    {
                        out.write(plus);
                    }
                }
                out.write(statementTerminator + newLine);

                writeEdgeConstraints(v, out);
            }
        }
    }

    private void writeEdgeConstraints (Vertex v, BufferedWriter out)
            throws IOException
    {
        int num = 1;
        Iterator <Edge> succIt = v.successorIterator();
        while (succIt.hasNext())
        {
            FlowEdge e = (FlowEdge) succIt.next();
            int edgeID = e.getEdgeID();
            out.write(createEdgeVariable(edgeID));

            if (num++ < v.numOfSuccessors())
            {
                out.write(plus);
            }
        }

        out.write(equals);

        num = 1;
        Iterator <Edge> predIt = v.predecessorIterator();
        while (predIt.hasNext())
        {
            FlowEdge e = (FlowEdge) predIt.next();
            int edgeID = e.getEdgeID();
            out.write(createEdgeVariable(edgeID));

            if (num++ < v.numOfPredecessors())
            {
                out.write(plus);
            }
        }

        out.write(statementTerminator + newLine + newLine);
    }

    private void writeLoopConstraints (BufferedWriter out) throws IOException
    {
        Debug.debugMessage(getClass(), "Writing loop constraints", 3);

        LoopNests lnt = new LoopNests(ipg, ipg.getEntryID());

        for (Vertex v : ipg)
        {
            if (v.getVertexID() == ipg.getEntryID())
            {
                out.write(createVertexVariable(v.getVertexID()) + equals + " 1"
                        + statementTerminator + newLine);
            }
            else
            {
                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    FlowEdge e = (FlowEdge) predIt.next();
                    int edgeID = e.getEdgeID();

                    if (lnt.isLoopHeader(v.getVertexID()))
                    {
                        if (lnt.isLoopTail(v.getVertexID(), e.getVertexID()))
                        {
                            int bound = database.getBound(subprogramID, edgeID);

                            out.write(createEdgeVariable(edgeID)
                                    + lessThanOrEquals
                                    + Integer.toString(bound)
                                    + statementTerminator + newLine);

                        }
                    }
                }

            }
        }

        out.append(newLine);
    }

    private void writeIntegerConstraints (BufferedWriter out)
            throws IOException
    {
        Debug.debugMessage(getClass(), "Writing integer constraints", 3);

        StringBuffer buffer = new StringBuffer();

        out.write(createComment("Integer constraints"));
        out.write(intString);
        for (Vertex v : ipg)
        {
            buffer.append(createVertexVariable(v.getVertexID())
                    + variableSeparator);

            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                buffer.append(createEdgeVariable(e.getEdgeID())
                        + variableSeparator);
            }
        }

        buffer.delete(buffer.length() - 2, buffer.length() - 1);
        buffer.append(statementTerminator + newLine);

        out.write(buffer.toString());
    }
}
