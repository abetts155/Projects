package adam.betts.calculations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import lpsolve.LpSolve;
import lpsolve.LpSolveException;
import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.edges.IPGEdge;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.DFSEdgeType;
import adam.betts.utilities.Enums.IPGEdgeType;
import adam.betts.utilities.Enums.IProfile;
import adam.betts.utilities.Globals;
import adam.betts.vertices.Ipoint;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class IPETModelIPG extends IPETModel
{

    protected final IpointGraph ipg;

    public IPETModelIPG (CalculationEngine engine, Database database,
            IpointGraph ipg, LoopNests lnt, int subprogramID,
            String subprogramName)
    {
        Debug.debugMessage(getClass(), "Building IPET model with LNT for "
                + subprogramName, 2);

        this.ipg = ipg;

        assert IPETModel.lpSolveDirectorySet() : "ILP directory not set";
        final String fileName = subprogramName + ".lp";

        try
        {
            initialise();
            partitionIPGEdges(lnt);

            final File file = new File(IPETModel.getILPDirectory(), fileName);
            BufferedWriter out = new BufferedWriter(new FileWriter(
                    file.getAbsolutePath()));

            writeObjectiveFunction(engine, database, subprogramID, out);
            writeFlowContraints(ipg, out);
            writeLoopConstraints(database, subprogramID, lnt, out);
            writeIntegerConstraints(ipg, out);
            out.close();

            lp = LpSolve.readLp(file.getAbsolutePath(), getLpSolveVerbosity(),
                    null);
            solve();
        }
        catch (IOException e)
        {
            Debug.errorMessage(getClass(), "Problem creating file " + fileName);
        }
        catch (LpSolveException e)
        {
            Debug.errorMessage(getClass(), e.getMessage());
        }
        catch (SolutionException e)
        {
            Debug.errorMessage(getClass(), e.getMessage());
        }
    }

    public IPETModelIPG (DatabaseWithoutProgram database, IpointGraph ipg,
            boolean allConstraints)
    {
        Debug.debugMessage(getClass(), "Building IPET model without LNT", 3);

        this.ipg = ipg;

        try
        {
            initialise();

            if (lpSolveDirectorySet())
            {
                String fileName = null;

                for (IProfile iprofile : IProfile.values())
                {
                    Pattern pattern = Pattern.compile(
                            ".*" + iprofile.toString() + ".*",
                            Pattern.CASE_INSENSITIVE);
                    Matcher fit = pattern.matcher(Globals.getTraceFileName());
                    if (fit.matches())
                    {
                        fileName = iprofile.toString() + ".lp"
                                + database.getTests();
                        break;
                    }
                }

                if (fileName == null)
                {
                    fileName = "parsedIPG.lp" + database.getTests();
                    Debug.debugMessage(getClass(),
                            "Unable to determine instrumentation profile from '"
                                    + Globals.getTraceFileName() + "'. Using "
                                    + fileName + " instead", 4);
                }

                final File file = new File(getILPDirectory(), fileName);

                try
                {
                    BufferedWriter out = new BufferedWriter(new FileWriter(
                            file.getAbsolutePath()));

                    Debug.debugMessage(getClass(), "Writing ILP model to "
                            + file.getCanonicalPath(), 3);

                    Debug.debugMessage(getClass(),
                            "Writing objective function", 3);
                    writeObjectiveFunction(database, out);

                    Debug.debugMessage(getClass(), "Writing flow constraints",
                            3);
                    writeFlowContraints(ipg, out);

                    Debug.debugMessage(getClass(),
                            "Writing capacity constraints", 3);
                    writeCapacityConstraints(database, out, allConstraints);

                    Debug.debugMessage(getClass(),
                            "Writing integer constraints", 3);
                    writeIntegerConstraints(ipg, out);
                    out.close();
                }
                catch (IOException e)
                {
                    System.err.println("Problem with file " + fileName);
                    System.exit(1);
                }

                try
                {
                    lp = LpSolve.readLp(file.getAbsolutePath(),
                            getLpSolveVerbosity(), null);
                    solve();
                }
                catch (SolutionException e)
                {
                    System.exit(1);
                }
            }
            else
            {
                Debug.debugMessage(getClass(), "Adding columns", 3);
                addColumns();

                lp.setAddRowmode(true);

                Debug.debugMessage(getClass(), "Adding flow constraints", 3);
                addEdgeContraints();

                Debug.debugMessage(getClass(), "Adding capacity constraints", 3);
                addCapacityConstraints(database);

                lp.setAddRowmode(false);

                Debug.debugMessage(getClass(), "Adding objective function", 3);
                addObjectiveFunction(database);

                lp.setVerbose(getLpSolveVerbosity());

                lp.setMaxim();

                try
                {
                    Debug.debugMessage(getClass(), "Solving linear program", 3);
                    solve();
                }
                catch (SolutionException e)
                {
                    final String fileName = "parsedIPG.lp"
                            + database.getTests();
                    Debug.debugMessage(getClass(), "Writing LP model to "
                            + fileName, 1);
                    lp.writeLp(fileName);
                    System.exit(1);
                }
            }
        }
        catch (LpSolveException e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void initialise () throws LpSolveException
    {
        /*
         * The Linear Program Will Always Have at Least |V| Structural
         * Constraints (Equivalent To Rows) and exactly |E| Variables
         * (equivalent to Columns).
         */
        numOfColumns = ipg.numOfEdges();
        colArray = new int[numOfColumns];
        rowArray = new double[numOfColumns];

        lp = LpSolve.makeLp(ipg.numOfVertices(), numOfColumns);
    }

    private void partitionIPGEdges (LoopNests lnt)
    {
        Debug.debugMessage(getClass(),
                "Partitioning IPG edges into loop categories", 3);

        for (int level = 0; level < lnt.getHeight(); ++level)
        {
            Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
            while (levelIt.hasNext())
            {
                TreeVertex v = levelIt.next();
                int vertexID = v.getVertexID();

                if (v.numOfSuccessors() > 0)
                {
                    backEdges.put(vertexID, new ArrayList <Integer>());
                    entryEdges.put(vertexID, new ArrayList <Integer>());
                    exitEdges.put(vertexID, new ArrayList <Integer>());
                    ancestors.put(vertexID, new ArrayList <Integer>());
                    ancestors.get(vertexID).add(vertexID);

                    if (vertexID != lnt.getRootID())
                    {
                        ancestors.get(vertexID).addAll(
                                ancestors.get(v.getParentID()));
                    }
                }
            }
        }

        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();

                if (e.isIterationEdge())
                {
                    for (int vertexID : e.getEdgeLabel())
                    {
                        if (lnt.isLoopHeader(vertexID))
                        {
                            HeaderVertex headerv = lnt
                                    .getHeaderVertex(vertexID);

                            backEdges.get(headerv.getVertexID()).add(edgeID);
                        }
                    }
                }
                else if (e.isExitEdge())
                {
                    HeaderVertex headerv = lnt.getHeaderVertex(e
                            .getExitHeaderID());

                    exitEdges.get(headerv.getVertexID()).add(edgeID);
                }
                else if (e.isEntryEdge())
                {
                    HeaderVertex headerv = lnt.getHeaderVertex(e
                            .getEntryHeaderID());

                    entryEdges.get(headerv.getVertexID()).add(edgeID);
                }
            }
        }
    }

    private void writeFlowContraints (IpointGraph ipg, BufferedWriter out)
            throws IOException
    {
        Debug.debugMessage(getClass(), "Writing flow constraints", 3);

        for (Vertex v : ipg)
        {
            /*
             * Only add the flow constraint for a vertex if it has both
             * successors and predecessors
             */
            if (v.hasPredecessors() && v.hasSuccessors())
            {
                out.write("// Vertex " + Integer.toString(v.getVertexID())
                        + "\n");

                int num = 1;
                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    IPGEdge e = (IPGEdge) succIt.next();
                    int edgeID = e.getEdgeID();
                    out.write(edgePrefix + Integer.toString(edgeID));

                    if (num++ < v.numOfSuccessors())
                    {
                        out.write(" + ");
                    }
                }

                out.write(" = ");

                num = 1;
                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    IPGEdge e = (IPGEdge) predIt.next();
                    int edgeID = e.getEdgeID();
                    out.write(edgePrefix + Integer.toString(edgeID));

                    if (num++ < v.numOfPredecessors())
                    {
                        out.write(" + ");
                    }
                }

                out.write(";\n\n");
            }
        }
    }

    private void writeIntegerConstraints (IpointGraph ipg, BufferedWriter out)
            throws IOException
    {
        Debug.debugMessage(getClass(), "Writing integer constraints", 3);

        int num = 1;
        int numOfEdges = ipg.numOfEdges();

        out.write("// Integer constraints\n");
        out.write("int ");
        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();

                out.write(edgePrefix + Integer.toString(edgeID));

                if (num++ < numOfEdges)
                {
                    out.write(", ");
                }
            }
        }

        out.write(";\n");
    }

    private void addColumns () throws LpSolveException
    {
        int columnNum = 0;
        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();
                columnNum += 1;
                unitToColumn.put(edgeID, columnNum);
                columnToUnit.put(columnNum, edgeID);

                /*
                 * Set the column name to the edge id
                 */
                lp.setColName(columnNum, edgePrefix + edgeID);

                /*
                 * Each variable (= column) must be integral
                 */
                lp.setInt(columnNum, true);
            }
        }
    }

    private void addEdgeContraints () throws LpSolveException
    {
        for (Vertex v : ipg)
        {
            /*
             * Only add the flow constraint for a vertex if it has both
             * successors and predecessors
             */
            if (v.hasPredecessors() && v.hasSuccessors())
            {
                int index = 0;

                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    IPGEdge e = (IPGEdge) succIt.next();
                    int edgeID = e.getEdgeID();
                    colArray[index] = unitToColumn.get(edgeID);
                    rowArray[index] = 1;
                    index++;
                }

                Iterator <Edge> predIt = v.predecessorIterator();
                while (predIt.hasNext())
                {
                    IPGEdge e = (IPGEdge) predIt.next();
                    int edgeID = e.getEdgeID();
                    colArray[index] = unitToColumn.get(edgeID);
                    rowArray[index] = -1;
                    index++;
                }

                /*
                 * Add the constraint to the model. Note that the last 2
                 * parameters states that flow in = flow out
                 */
                lp.addConstraintex(v.numOfPredecessors() + v.numOfSuccessors(),
                        rowArray, colArray, LpSolve.EQ, 0);
            }
        }
    }

    private void addObjectiveFunction (DatabaseWithoutProgram database)
            throws LpSolveException
    {
        int index = 0;
        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();
                long wcet = 0;

                switch (e.getEdgeType())
                {
                    case GHOST_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is ghost edge", 4);
                        wcet = 0;
                        break;
                    case TRACE_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is trace edge", 4);
                        wcet = database.getEdgeWCET(edgeID);
                        break;
                }

                Debug.debugMessage(getClass(), "WCET(e_" + edgeID + ") = "
                        + wcet, 3);

                colArray[index] = unitToColumn.get(edgeID);
                rowArray[index] = wcet;
                index++;
            }
        }

        lp.setObjFnex(ipg.numOfEdges(), rowArray, colArray);
    }

    private void writeObjectiveFunction (CalculationEngine engine,
            Database database, int subprogramID, BufferedWriter out)
            throws IOException
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
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();
                long wcet = 0;

                switch (e.getEdgeType())
                {
                    case GHOST_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is ghost edge", 4);
                        wcet = 0;
                        break;
                    case INLINED_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is inlined edge", 4);
                        Ipoint s = ipg.getVertex(e.getVertexID());
                        wcet = engine.getWCET(s.getSubprogramName());
                        break;
                    case TRACE_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is trace edge", 4);
                        wcet = database.getUnitWCET(subprogramID, edgeID);
                        break;
                }

                Debug.debugMessage(getClass(), "WCET("
                        + createEdgeVariable(edgeID) + ") = " + wcet, 3);

                out.write(Long.toString(wcet) + createEdgeVariable(edgeID));

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

    private void writeObjectiveFunction (DatabaseWithoutProgram database,
            BufferedWriter out) throws IOException
    {
        out.write(createComment("Objective function"));
        out.write(maxString);

        int num = 1;
        int numOfEdges = ipg.numOfEdges();
        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();
                long wcet = 0;

                switch (e.getEdgeType())
                {
                    case GHOST_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is ghost edge", 4);
                        wcet = 0;
                        break;
                    case TRACE_EDGE:
                        Debug.debugMessage(getClass(), edgeID
                                + " is trace edge", 4);
                        wcet = database.getEdgeWCET(edgeID);
                        break;
                }

                Debug.debugMessage(getClass(), "WCET("
                        + createEdgeVariable(edgeID) + ") = " + wcet, 3);

                out.write(Long.toString(wcet) + " "
                        + createEdgeVariable(edgeID));

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

    private void writeLoopConstraints (Database database, int subprogramID,
            LoopNests lnt, BufferedWriter out) throws IOException
    {
        Debug.debugMessage(getClass(), "Writing loop constraints", 3);

        for (int level = lnt.getHeight() - 1; level >= 0; --level)
        {
            Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
            while (levelIt.hasNext())
            {
                TreeVertex v = levelIt.next();
                int vertexID = v.getVertexID();

                if (v.numOfSuccessors() > 0)
                {
                    Debug.debugMessage(getClass(), "Analsying header "
                            + vertexID, 2);

                    if (vertexID == lnt.getRootID())
                    {
                        out.write("// Header " + Integer.toString(vertexID)
                                + "\n");

                        Vertex entryv = ipg.getVertex(ipg.getEntryID());

                        Iterator <Edge> predIt = entryv.predecessorIterator();
                        while (predIt.hasNext())
                        {
                            FlowEdge e = (FlowEdge) predIt.next();

                            out.write(edgePrefix
                                    + Integer.toString(e.getEdgeID()));
                        }

                        out.write(" <= 1;\n");
                    }

                    if (backEdges.get(vertexID).size() > 0)
                    {
                        out.write("// Header " + Integer.toString(vertexID)
                                + "\n");

                        writeInnerLoopConstraints(database, subprogramID, lnt,
                                vertexID, out);

                        out.newLine();
                    }
                }
            }
        }
    }

    private void writeIterationEdges (BufferedWriter out, int headerID)
            throws IOException
    {
        Debug.debugMessage(getClass(),
                "Wrting iteration edges for " + headerID, 2);

        int num = 1;
        for (int edgeID : backEdges.get(headerID))
        {
            out.write(edgePrefix + Integer.toString(edgeID));

            if (num++ < backEdges.get(headerID).size())
            {
                out.write(" + ");
            }
        }
    }

    private void writeInnerLoopConstraints (Database database,
            int subprogramID, LoopNests lnt, int headerID, BufferedWriter out)
            throws IOException
    {
        for (int ancestorID : lnt.getProperAncestors(headerID))
        {
            HeaderVertex headerv = (HeaderVertex) lnt.getVertex(headerID);

            HeaderVertex ancestorv = (HeaderVertex) lnt.getVertex(ancestorID);

            Debug.debugMessage(getClass(), "Wrting iteration edges for "
                    + headerv.getHeaderID() + " with respect to outer loop "
                    + ancestorv.getHeaderID(), 2);

            if (headerv.getLevel() - ancestorv.getLevel() <= loopConstraintLevel)
            {
                out.write("//...with respect to "
                        + Integer.toString(ancestorv.getHeaderID()) + "\n");

                int bound = database.getLoopBound(subprogramID, headerID,
                        ancestorID);

                Debug.debugMessage(getClass(), "Adding constraint on loop "
                        + headerv.getHeaderID() + " relative to loop "
                        + ancestorv.getHeaderID() + ". Bound = " + bound, 4);

                writeIterationEdges(out, headerID);
                out.write(" <= ");
                writeRelativeEdges(out, headerID, bound);
                out.write(";\n");
            }
        }
    }

    private void writeRelativeEdges (BufferedWriter out, int headerID, int bound)
            throws IOException
    {
        if (entryEdges.get(headerID).size() > 0)
        {
            Debug.debugMessage(getClass(), "Found entry edges to " + headerID,
                    4);

            int num = 1;
            for (int edgeID : entryEdges.get(headerID))
            {
                out.write(Integer.toString(bound) + " " + edgePrefix
                        + Integer.toString(edgeID));

                if (num++ < entryEdges.get(headerID).size())
                {
                    out.write(" + ");
                }
            }
        }
        else if (exitEdges.get(headerID).size() > 0)
        {
            Debug.debugMessage(getClass(), "Found exit edges to " + headerID, 4);

            int num = 1;
            for (int edgeID : exitEdges.get(headerID))
            {
                out.write(Integer.toString(bound) + " " + edgePrefix
                        + Integer.toString(edgeID));

                if (num++ < exitEdges.get(headerID).size())
                {
                    out.write(" + ");
                }
            }
        }
        else
        {
            Debug.errorMessage(getClass(),
                    "Could not find entry/exit edges for " + headerID);
        }
    }

    private void addCapacityConstraints (DatabaseWithoutProgram database)
            throws LpSolveException
    {
        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                IPGEdge e = (IPGEdge) succIt.next();
                int edgeID = e.getEdgeID();
                int count = database.getEdgeCount(edgeID);

                if (e.getEdgeType() == IPGEdgeType.TRACE_EDGE)
                {
                    Debug.debugMessage(getClass(), "Count(e_" + edgeID + ") = "
                            + count, 3);
                    lp.setUpbo(edgeID, count);
                }
            }
        }
    }

    private void writeCapacityConstraints (DatabaseWithoutProgram database,
            BufferedWriter out, boolean allConstraints) throws IOException
    {
        out.write("// Capacity constraints\n");

        DepthFirstTree dfs = new DepthFirstTree(ipg, ipg.getEntryID());

        for (Vertex v : ipg)
        {
            Iterator <Edge> succIt = v.successorIterator();

            if (v.getVertexID() == ipg.getEntryID())
            {
                int count = 1;
                while (succIt.hasNext())
                {
                    IPGEdge e = (IPGEdge) succIt.next();
                    int edgeID = e.getEdgeID();

                    out.write(edgePrefix + Integer.toString(edgeID));

                    if (count++ < v.numOfSuccessors())
                    {
                        out.write(" + ");
                    }
                }

                out.write(" <= " + Integer.toString(1) + ";\n");
            }
            else
            {
                while (succIt.hasNext())
                {
                    IPGEdge e = (IPGEdge) succIt.next();
                    int succID = e.getVertexID();
                    int edgeID = e.getEdgeID();
                    int count = database.getEdgeCount(edgeID);

                    Debug.debugMessage(getClass(), "Count(e_" + edgeID + ") = "
                            + count, 3);

                    if (allConstraints)
                    {
                        out.write(edgePrefix + Integer.toString(edgeID)
                                + " <= " + Integer.toString(count) + ";\n");
                    }
                    else if (dfs.getEdgeType(v.getVertexID(), succID) == DFSEdgeType.BACK_EDGE)
                    {

                        out.write(edgePrefix + Integer.toString(edgeID)
                                + " <= " + Integer.toString(count) + ";\n");
                    }
                }
            }
        }

        out.write("\n");
    }

    private void solve () throws LpSolveException, SolutionException
    {
        int solution = lp.solve();
        switch (solution)
        {
            case LpSolve.OPTIMAL:
                Debug.debugMessage(getClass(),
                        "Optimal solution found " + lp.getObjective(), 3);
                wcet = Math.round(lp.getObjective());

                lp.getVariables(rowArray);
                for (int i = 0; i < rowArray.length; ++i)
                {
                    executionCounts.put(columnToUnit.get(i + 1),
                            (int) rowArray[i]);
                }
                break;

            default:
                Debug.debugMessage(getClass(), "Problem with the LP model: "
                        + solution, 2);
                throw new SolutionException(solution);
        }
    }
}
