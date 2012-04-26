package adam.betts.calculations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import lpsolve.LpSolve;
import lpsolve.LpSolveException;
import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.edges.SuperBlockCFGStructureEdge;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.FlowGraph;
import adam.betts.graphs.SuperBlockCFGStructureGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.graphs.trees.LoopNests;
import adam.betts.outputs.UDrawGraph;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.tools.MainTraceParser;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.SuperBlockCFGStructureEdgeType;
import adam.betts.utilities.Globals;
import adam.betts.vertices.SuperBlockVertex;
import adam.betts.vertices.Vertex;
import adam.betts.vertices.trees.HeaderVertex;
import adam.betts.vertices.trees.TreeVertex;

public class CalculationEngineCFG
{

    protected final Program program;
    protected final CallGraph callg;
    protected final Database database;
    protected final DepthFirstTree dfs;

    protected final HashMap <String, IPETModelCFGInFile> ILPsInFile = new HashMap <String, IPETModelCFGInFile>();
    protected final HashMap <String, IPETModelCFGInMemory> ILPsInMemory = new HashMap <String, IPETModelCFGInMemory>();
    protected final HashMap <String, IPETModelCFGInFileWithSuperBlocks> ILPsInFileSuperBlocks = new HashMap <String, IPETModelCFGInFileWithSuperBlocks>();
    protected final HashMap <String, IPETModelCFGInMemoryWithSuperBlocks> ILPsInMemorySuperBlocks = new HashMap <String, IPETModelCFGInMemoryWithSuperBlocks>();

    public CalculationEngineCFG (Program program, Database database)
    {
        this.program = program;
        this.callg = program.getCallGraph();
        this.database = database;
        dfs = new DepthFirstTree(callg, program.getRootID());

        for (int i = 1; i <= dfs.numOfVertices(); ++i)
        {
            int subprogramID = dfs.getPostVertexID(i);
            Subprogram subprogram = program.getSubprogram(subprogramID);
            String subprogramName = subprogram.getSubprogramName();

            Debug.debugMessage(getClass(),
                    "Building IPET of " + subprogramName, 3);

            ControlFlowGraph cfg = subprogram.getCFG();
            IPETModelCFGInFile ilp = new IPETModelCFGInFile(cfg, cfg.getLNT(),
                    subprogramID, subprogramName);
            ILPsInFile.put(subprogramName, ilp);

            Debug.debugMessage(getClass(), "CFG-ILP: WCET(" + subprogramName
                    + ") = " + ilp.wcet, 3);

            IPETModelCFGInMemory ilp2 = new IPETModelCFGInMemory(cfg,
                    cfg.getLNT(), subprogramID, subprogramName);
            ILPsInMemory.put(subprogramName, ilp2);

            assert ilp2.wcet == ilp.wcet : "Disparity between WCETs found: "
                    + ilp2.wcet + " and " + ilp.wcet;

            assert ilp2.flowConstraints == ilp.flowConstraints : "Different number of flow constraints found "
                    + ilp2.flowConstraints + " and " + ilp.flowConstraints;

            assert ilp2.loopConstraints == ilp.loopConstraints : "Different number of loop constraints found "
                    + ilp2.loopConstraints + " and " + ilp.loopConstraints;

            assert ilp2.numberOfVariables == ilp.numberOfVariables : "Different number of variables found "
                    + ilp2.numberOfVariables + " and " + ilp.numberOfVariables;

            IPETModelCFGInFileWithSuperBlocks ilp3 = new IPETModelCFGInFileWithSuperBlocks(
                    cfg, cfg.getLNT(), subprogramID, subprogramName);
            ILPsInFileSuperBlocks.put(subprogramName, ilp3);
        }
    }

    public final String pad (String rowEntry, int lengthOfColumn)
    {
        StringBuffer paddedString = new StringBuffer();
        for (int i = rowEntry.length(); i < lengthOfColumn; ++i)
        {
            paddedString.append(" ");
        }
        return paddedString.toString();
    }

    public final void printResults ()
    {
        final int numOfColumns = 4;
        final String column1Heading = "Subprogram";
        final String column2Heading = "WCET";
        final String column3Heading = "#Constraints";
        final String column4Heading = "#Variables";
        final char columnDivider = '|';
        int column1Length = column1Heading.length();

        for (int i = 1; i <= dfs.numOfVertices(); ++i)
        {
            int subprogramID = dfs.getPostVertexID(i);
            Subprogram subprogram = program.getSubprogram(subprogramID);
            String subprogramName = subprogram.getSubprogramName();

            if (subprogramName.length() > column1Length)
            {
                column1Length = subprogramName.length();
            }
        }

        StringBuffer rowDivider = new StringBuffer();
        rowDivider.append(columnDivider);
        for (int i = 0; i < column1Length + column2Heading.length()
                + column3Heading.length() + column4Heading.length()
                + numOfColumns - 1; ++i)
        {
            rowDivider.append("=");
        }
        rowDivider.append(columnDivider);

        StringBuffer tableHeading = new StringBuffer();
        tableHeading.append(columnDivider + column1Heading
                + pad(column1Heading, column1Length) + columnDivider
                + column2Heading + columnDivider + column3Heading
                + columnDivider + column4Heading + columnDivider);
        System.out.println(tableHeading.toString());
        System.out.println(rowDivider.toString());

        for (int i = 1; i <= dfs.numOfVertices(); ++i)
        {
            int subprogramID = dfs.getPostVertexID(i);
            Subprogram subprogram = program.getSubprogram(subprogramID);
            String subprogramName = subprogram.getSubprogramName();

            IPETModelCFGInFile ilp = ILPsInFile.get(subprogramName);
            IPETModelCFGInFileWithSuperBlocks ilp2 = ILPsInFileSuperBlocks
                    .get(subprogramName);

            StringBuffer row1 = new StringBuffer();
            String row1WCET = Long.toString(ilp.wcet);
            String row1Constraints = Long.toString(ilp.flowConstraints
                    + ilp.loopConstraints);
            String row1Variables = Long.toString(ilp.numberOfVariables);

            row1.append(columnDivider + subprogramName
                    + pad(subprogramName, column1Length) + columnDivider
                    + row1WCET + pad(row1WCET, column2Heading.length())
                    + columnDivider + row1Constraints
                    + pad(row1Constraints, column3Heading.length())
                    + columnDivider + row1Variables
                    + pad(row1Variables, column4Heading.length())
                    + columnDivider);

            StringBuffer row2 = new StringBuffer();
            String row2WCET = Long.toString(ilp2.wcet);
            String row2Constraints = Long.toString(ilp2.flowConstraints
                    + ilp2.loopConstraints);
            String row2Variables = Long.toString(ilp2.numberOfVariables);

            row2.append(columnDivider + subprogramName
                    + pad(subprogramName, column1Length) + columnDivider
                    + row2WCET + pad(row2WCET, column2Heading.length())
                    + columnDivider + row2Constraints
                    + pad(row2Constraints, column3Heading.length())
                    + columnDivider + row2Variables
                    + pad(row2Variables, column4Heading.length())
                    + columnDivider);

            System.out.println(row1.toString());
            System.out.println(row2.toString());
            System.out.println(rowDivider.toString());
        }
    }

    public final long getWCET (int subprogramID)
    {
        for (String subprogramName : ILPsInFile.keySet())
        {
            if (program.getSubprogram(subprogramName).getSubprogramID() == subprogramID)
            {
                return ILPsInFile.get(subprogramName).wcet;
            }
        }
        return 0;
    }

    private class IPETModelCFG extends IPETModel
    {

        protected final ControlFlowGraph cfg;
        protected final LoopNests lnt;
        protected final int subprogramID;
        protected final String subprogramName;

        protected int flowConstraints = 0;
        protected int loopConstraints = 0;
        protected int numberOfVariables = 0;

        public IPETModelCFG (ControlFlowGraph cfg, LoopNests lnt,
                int subprogramID, String subprogramName)
        {
            this.cfg = cfg;
            this.lnt = lnt;
            this.subprogramID = subprogramID;
            this.subprogramName = subprogramName;

            numOfColumns = cfg.numOfEdges() + cfg.numOfVertices();
            colArray = new int[numOfColumns];
            rowArray = new double[numOfColumns];
        }

        protected void solve () throws LpSolveException, SolutionException
        {
            Debug.debugMessage(getClass(), "Solving ILP", 3);

            int solution = lp.solve();
            switch (solution)
            {
                case LpSolve.OPTIMAL:
                    Debug.debugMessage(getClass(), "Optimal solution found "
                            + lp.getObjective(), 3);
                    wcet = Math.round(lp.getObjective());

                    resetArray(rowArray);
                    lp.getVariables(rowArray);

                    if (columnToUnit.size() > 0)
                    {
                        for (int i = 0; i < rowArray.length; ++i)
                        {
                            Debug.debugMessage(getClass(), "rowArray[" + i
                                    + "] = " + rowArray[i], 4);
                        }
                    }

                    break;
                default:
                    Debug.debugMessage(getClass(),
                            "Problem with the LP model: " + solution, 2);
                    throw new SolutionException(solution);
            }
        }

        protected void resetArray (int arr[])
        {
            for (int i = 0; i < arr.length; ++i)
            {
                arr[i] = 0;
            }
        }

        protected void resetArray (double arr[])
        {
            for (int i = 0; i < arr.length; ++i)
            {
                arr[i] = 0.0d;
            }
        }

        protected void dumpLPToFile ()
        {
            final String fileName = subprogramName + ".cfg.lp.memory";
            Debug.debugMessage(getClass(), "Writing LP model to " + fileName, 1);

            try
            {
                lp.writeLp(fileName);
            }
            catch (LpSolveException lpe)
            {
                lpe.printStackTrace();
            }
        }
    }

    private class IPETModelCFGInFile extends IPETModelCFG
    {

        public IPETModelCFGInFile (ControlFlowGraph cfg, LoopNests lnt,
                int subprogramID, String subprogramName)
        {
            super(cfg, lnt, subprogramID, subprogramName);

            final String fileName = subprogramName + ".cfg.lp";
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
                     * The Linear Program Will Always Have at Least |V|
                     * Structural Constraints (Equivalent To Rows) and exactly
                     * |V| Variables (equivalent to Columns).
                     */
                    numOfColumns = cfg.numOfVertices();
                    lp = LpSolve.makeLp(numOfColumns, numOfColumns);

                    lp = LpSolve.readLp(file.getAbsolutePath(),
                            IPETModel.getLpSolveVerbosity(), null);
                    try
                    {
                        lp = LpSolve.readLp(file.getAbsolutePath(),
                                MainTraceParser.getLpSolveVerbosity(), null);
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
                System.err.println("Problem with file " + fileName);
                System.exit(1);
            }
        }

        private void writeObjectiveFunction (BufferedWriter out)
                throws IOException
        {
            Debug.debugMessage(getClass(), "Writing objective function", 3);

            out.write("// Objective function\n");
            out.write("max: ");

            int num = 1;
            int numOfVertices = cfg.numOfVertices();
            for (Vertex v : cfg)
            {
                int vertexID = v.getVertexID();
                long wcet = 0;

                /*
                 * Check whether this basic block is a call site
                 */
                int calleeID = callg.isCallSite(subprogramID, vertexID);

                if (calleeID == Vertex.DUMMY_VERTEX_ID)
                {
                    wcet = database.getUnitWCET(subprogramID, vertexID);
                }
                else
                {
                    wcet = getWCET(calleeID);
                }

                out.write(Long.toString(wcet) + " " + vertexPrefix
                        + Integer.toString(vertexID));

                if (num < numOfVertices)
                {
                    out.write(" + ");
                }
                if (num % 10 == 0)
                {
                    out.newLine();
                }
                num++;
            }

            out.write(";\n\n");
        }

        private void writeFlowConstraints (BufferedWriter out)
                throws IOException
        {
            Debug.debugMessage(getClass(), "Writing flow constraints", 3);
            for (Vertex v : cfg)
            {
                if (v.hasPredecessors() && v.hasSuccessors())
                {
                    flowConstraints++;

                    out.write("// Vertex " + Integer.toString(v.getVertexID())
                            + "\n");

                    out.write(vertexPrefix + Long.toString(v.getVertexID())
                            + " = ");

                    int num = 1;
                    Iterator <Edge> predIt = v.predecessorIterator();
                    while (predIt.hasNext())
                    {
                        FlowEdge e = (FlowEdge) predIt.next();
                        int edgeID = e.getEdgeID();
                        out.write(edgePrefix + Integer.toString(edgeID));

                        if (num++ < v.numOfPredecessors())
                        {
                            out.write(" + ");
                        }
                    }
                    out.write(";\n");

                    writeEdgeConstraints(v, out);
                }
            }
        }

        private void writeEdgeConstraints (Vertex v, BufferedWriter out)
                throws IOException
        {
            flowConstraints++;

            int num = 1;
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
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
                FlowEdge e = (FlowEdge) predIt.next();
                int edgeID = e.getEdgeID();
                out.write(edgePrefix + Integer.toString(edgeID));

                if (num++ < v.numOfPredecessors())
                {
                    out.write(" + ");
                }
            }

            out.write(";\n\n");
        }

        private void writeLoopConstraints (BufferedWriter out)
                throws IOException
        {
            Debug.debugMessage(getClass(), "Writing loop constraints", 3);

            for (int level = lnt.getHeight() - 1; level >= 0; --level)
            {
                Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
                while (levelIt.hasNext())
                {
                    TreeVertex v = levelIt.next();

                    if (v instanceof HeaderVertex)
                    {
                        HeaderVertex headerv = (HeaderVertex) v;

                        out.write("// Header "
                                + Integer.toString(headerv.getHeaderID())
                                + "\n");
                        if (headerv.getHeaderID() == cfg.getEntryID())
                        {
                            loopConstraints++;
                            
                            out.write(vertexPrefix
                                    + Long.toString(headerv.getHeaderID())
                                    + " = 1;\n");
                        }
                        else
                        {
                            writeInnerLoopConstraints(headerv, out);
                        }
                        out.newLine();
                    }
                }
            }
        }

        private void writeInnerLoopConstraints (HeaderVertex headerv,
                BufferedWriter out) throws IOException
        {
            for (int ancestorID : lnt.getProperAncestors(headerv.getVertexID()))
            {
                HeaderVertex ancestorv = (HeaderVertex) lnt
                        .getVertex(ancestorID);

                out.write("//...with respect to "
                        + Integer.toString(ancestorv.getHeaderID()) + "\n");

                int bound = database.getLoopBound(subprogramID,
                        headerv.getVertexID(), ancestorID);
                Debug.debugMessage(getClass(), "Adding constraint on loop "
                        + headerv.getVertexID() + " relative to loop "
                        + ancestorv.getVertexID() + ". Bound = " + bound, 4);

                writeSuccessorEdges(out, headerv);
                out.write(" <= ");

                if (ancestorID == headerv.getParentID())
                {
                    StringBuffer buffer = new StringBuffer();

                    Iterator <Edge> predIt = cfg.getVertex(
                            headerv.getHeaderID()).predecessorIterator();
                    while (predIt.hasNext())
                    {
                        FlowEdge e = (FlowEdge) predIt.next();
                        int predID = e.getVertexID();

                        if (lnt.inLoopBody(headerv.getHeaderID(), predID) == false)
                        {
                            int edgeID = e.getEdgeID();
                            buffer.append(Integer.toString(bound) + edgePrefix
                                    + Integer.toString(edgeID) + " + ");
                        }
                    }

                    buffer.delete(buffer.length() - 3, buffer.length() - 1);
                    out.write(buffer.toString() + ";\n");
                }
                else
                {
                    out.write(Integer.toString(bound) + " " + vertexPrefix
                            + Integer.toString(ancestorv.getHeaderID()) + ";\n");
                }

                loopConstraints++;
            }
        }

        private void writeSuccessorEdges (BufferedWriter out,
                HeaderVertex headerv) throws IOException
        {
            StringBuffer buffer = new StringBuffer();

            Vertex v = cfg.getVertex(headerv.getHeaderID());
            Iterator <Edge> succIt = v.successorIterator();
            while (succIt.hasNext())
            {
                FlowEdge e = (FlowEdge) succIt.next();
                int succID = e.getVertexID();

                if (lnt.inLoopBody(headerv.getHeaderID(), succID))
                {
                    int edgeID = e.getEdgeID();
                    buffer.append(edgePrefix + Integer.toString(edgeID) + " + ");
                }
            }

            buffer.delete(buffer.length() - 3, buffer.length() - 1);
            out.write(buffer.toString());
        }

        private void writeIntegerConstraints (BufferedWriter out)
                throws IOException
        {
            Debug.debugMessage(getClass(), "Writing integer constraints", 3);

            StringBuffer buffer = new StringBuffer();

            out.write("// Integer constraints\n");
            out.write("int ");
            for (Vertex v : cfg)
            {
                numberOfVariables++;
                buffer.append(vertexPrefix + Integer.toString(v.getVertexID())
                        + ", ");

                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    numberOfVariables++;
                    FlowEdge e = (FlowEdge) succIt.next();
                    buffer.append(edgePrefix + Integer.toString(e.getEdgeID())
                            + ", ");
                }
            }

            buffer.delete(buffer.length() - 3, buffer.length() - 1);
            buffer.append(";\n");

            out.write(buffer.toString());
        }
    }

    private class IPETModelCFGInFileWithSuperBlocks extends IPETModelCFG
    {

        protected Set <Integer> variables = new HashSet <Integer>();

        public IPETModelCFGInFileWithSuperBlocks (ControlFlowGraph cfg,
                LoopNests lnt, int subprogramID, String subprogramName)
        {
            super(cfg, lnt, subprogramID, subprogramName);

            final String fileName = subprogramName + ".cfg.super.lp";
            try
            {
                final File file = new File(ILPdirectory, fileName);

                BufferedWriter out = new BufferedWriter(new FileWriter(
                        file.getAbsolutePath()));

                writeObjectiveFunction(out);
                writeConstraints(out);
                writeIntegerConstraints(out);

                out.close();

                try
                {

                    lp = LpSolve.makeLp(cfg.numOfVertices(),
                            cfg.numOfVertices());

                    lp = LpSolve.readLp(file.getAbsolutePath(),
                            IPETModel.getLpSolveVerbosity(), null);
                    try
                    {
                        lp = LpSolve.readLp(file.getAbsolutePath(),
                                MainTraceParser.getLpSolveVerbosity(), null);
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
                System.err.println("Problem with file " + fileName);
                System.exit(1);
            }
        }

        private void writeObjectiveFunction (BufferedWriter out)
                throws IOException
        {
            Debug.debugMessage(getClass(), "Writing objective function", 3);

            out.write("// Objective function\n");
            out.write("max: ");

            int num = 1;
            int numOfVertices = cfg.numOfVertices();
            for (Vertex v : cfg)
            {
                int vertexID = v.getVertexID();
                long wcet = 0;

                /*
                 * Check whether this basic block is a call site
                 */
                int calleeID = callg.isCallSite(subprogramID, vertexID);

                if (calleeID == Vertex.DUMMY_VERTEX_ID)
                {
                    wcet = database.getUnitWCET(subprogramID, vertexID);
                }
                else
                {
                    wcet = getWCET(calleeID);
                }

                out.write(Long.toString(wcet) + " " + vertexPrefix
                        + Integer.toString(vertexID));

                if (num < numOfVertices)
                {
                    out.write(" + ");
                }
                if (num % 10 == 0)
                {
                    out.newLine();
                }
                num++;
            }

            out.write(";\n\n");
        }

        private void writeConstraints (BufferedWriter out) throws IOException
        {
            Debug.debugMessage(getClass(), "Writing flow constraints", 3);

            for (int level = lnt.getHeight() - 1; level >= 0; --level)
            {
                Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
                while (levelIt.hasNext())
                {
                    TreeVertex v = levelIt.next();

                    if (v instanceof HeaderVertex)
                    {
                        HeaderVertex headerv = (HeaderVertex) v;

                        writeFlowConstraints(out, headerv);

                        if (headerv.getHeaderID() == cfg.getEntryID())
                        {
                            loopConstraints++;
                            
                            out.write(createComment("Header "
                                    + Integer.toString(headerv.getHeaderID())));

                            out.write(createVertexVariable(headerv
                                    .getHeaderID())
                                    + equals
                                    + "1"
                                    + statementTerminator + newLine + newLine);
                        }
                        else
                        {

                        }
                    }
                }
            }
        }

        private void writeFlowConstraints (BufferedWriter out,
                HeaderVertex headerv) throws IOException
        {
            FlowGraph flowg = lnt.induceSubraph(headerv);

            SuperBlockCFGStructureGraph superg = new SuperBlockCFGStructureGraph(
                    flowg);

            if (Globals.uDrawDirectorySet())
            {
                UDrawGraph.makeUDrawFile(superg,
                        subprogramName + headerv.getHeaderID());
            }

            DepthFirstTree dfs = new DepthFirstTree(superg, superg.getRootID());

            for (int postOrderID = superg.numOfVertices(); postOrderID > 0; --postOrderID)
            {
                int vertexID = dfs.getPostVertexID(postOrderID);
                SuperBlockVertex v = superg.getVertex(vertexID);

                Debug.debugMessage(getClass(), "Analysing vertex " + vertexID,
                        4);

                if (v.numberOfBasicBlocks() > 1)
                {
                    // Choose a representative basic block for this super block
                    // vertex
                    int repID = v.basicBlockIDs().get(0);

                    for (int bbID : v.basicBlockIDs())
                    {
                        if (bbID != repID)
                        {
                            flowConstraints++;

                            out.write(createComment("Super block vertex. Basic block = "
                                    + Integer.toString(bbID)));

                            out.write(createVertexVariable(bbID) + equals
                                    + createVertexVariable(repID)
                                    + statementTerminator + newLine + newLine);
                        }
                    }
                }

                if (v.numOfSuccessors() > 1)
                {
                    writeBranchVertexConstraints(out, superg, v, headerv);
                }

                if (v.numOfPredecessors() > 1)
                {
                    writeMergeVertexConstraints(out, superg, v, headerv);
                }
            }
        }

        private void writeBranchVertexConstraints (BufferedWriter out,
                SuperBlockCFGStructureGraph superg, SuperBlockVertex superv,
                HeaderVertex headerv) throws IOException
        {
            int lhsID = superv.pickRandomBasicBlockID();

            HashMap <Integer, Set <SuperBlockCFGStructureEdge>> branchToSuccMap = superg
                    .partitionEdges(superv);
            for (int partitionID : branchToSuccMap.keySet())
            {
                flowConstraints++;

                Set <SuperBlockCFGStructureEdge> partitionedEdges = branchToSuccMap
                        .get(partitionID);

                out.write(createComment("Branch vertex. Random basic block = "
                        + Integer.toString(lhsID)));
                out.write(createVertexVariable(lhsID) + equals);

                int num = 1;
                for (SuperBlockCFGStructureEdge supere : partitionedEdges)
                {
                    if (supere.getEdgeType() == SuperBlockCFGStructureEdgeType.ACYCLIC_IRREDUCIBLE)
                    {
                        int newVariableID = supere.getEdgeID()
                                + headerv.getHeaderID();

                        variables.add(newVariableID);

                        out.write(createEdgeVariable(newVariableID));
                    }
                    else
                    {
                        SuperBlockVertex succv = superg.getVertex(supere
                                .getVertexID());

                        out.write(createVertexVariable(succv
                                .pickRandomBasicBlockID()));
                    }

                    if (num++ < partitionedEdges.size())
                    {
                        out.write(plus);
                    }
                }

                out.write(statementTerminator + newLine + newLine);
            }
        }

        private void writeMergeVertexConstraints (BufferedWriter out,
                SuperBlockCFGStructureGraph superg, SuperBlockVertex superv,
                HeaderVertex headerv) throws IOException
        {
            flowConstraints++;
            
            int lhsID = superv.pickRandomBasicBlockID();

            out.write(createComment("Merge vertex. Random basic block = "
                    + Integer.toString(lhsID)));
            out.write(createVertexVariable(lhsID) + equals);

            int num = 1;
            Iterator <Edge> predIt = superv.predecessorIterator();
            while (predIt.hasNext())
            {
                Edge prede = predIt.next();
                SuperBlockCFGStructureEdge supere = (SuperBlockCFGStructureEdge) prede;
                SuperBlockVertex predv = superg.getVertex(supere.getVertexID());

                if (supere.getEdgeType() == SuperBlockCFGStructureEdgeType.ACYCLIC_IRREDUCIBLE)
                {
                    int newVariableID = supere.getEdgeID()
                            + headerv.getHeaderID();

                    variables.add(newVariableID);

                    out.write(createEdgeVariable(newVariableID));
                }
                else
                {
                    out.write(createVertexVariable(predv
                            .pickRandomBasicBlockID()));
                }

                if (num++ < superv.numOfPredecessors())
                {
                    out.write(plus);
                }
            }

            out.write(statementTerminator + newLine + newLine);
        }

        private void writeIntegerConstraints (BufferedWriter out)
                throws IOException
        {
            Debug.debugMessage(getClass(), "Writing integer constraints", 3);

            StringBuffer buffer = new StringBuffer();

            out.write(createComment("Integer constraints"));
            out.write("int ");

            int vertexNum = 1;
            for (Vertex v : cfg)
            {
                numberOfVariables++;
                buffer.append(vertexPrefix + Integer.toString(v.getVertexID()));

                if (vertexNum++ < cfg.numOfVertices())
                {
                    buffer.append(", ");
                }
            }

            if (variables.size() > 0)
            {
                buffer.append(", ");
            }

            int edgeNum = 1;
            for (int edgeID : variables)
            {
                numberOfVariables++;
                buffer.append(edgePrefix + Integer.toString(edgeID));

                if (edgeNum++ < variables.size())
                {
                    buffer.append(", ");
                }
            }

            buffer.append(statementTerminator + newLine);
            out.write(buffer.toString());
        }
    }

    private class IPETModelCFGInMemory extends IPETModelCFG
    {

        public IPETModelCFGInMemory (ControlFlowGraph cfg, LoopNests lnt,
                int subprogramID, String subprogramName)
        {
            super(cfg, lnt, subprogramID, subprogramName);

            try
            {
                lp = LpSolve.makeLp(cfg.numOfVertices(), numOfColumns);

                addColumns();
                lp.setAddRowmode(true);

                addVertexConstraints();
                addEdgeContraints();
                addLoopConstraints();

                lp.setAddRowmode(false);
                addObjectiveFunction();

                lp.setVerbose(getLpSolveVerbosity());
                lp.setMaxim();
                solve();

                if (Debug.getDebugLevel() == Debug.HIGHEST_LEVEL)
                {
                    dumpLPToFile();
                }
            }
            catch (SolutionException e)
            {
                dumpLPToFile();
                System.exit(1);
            }
            catch (LpSolveException e)
            {
                e.printStackTrace();
                System.exit(1);
            }
        }

        private void addColumns () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding columns", 3);

            int columnNum = 1;
            for (Vertex v : cfg)
            {
                int vertexID = v.getVertexID();
                lp.setColName(columnNum, vertexPrefix + vertexID);
                lp.setInt(columnNum, true);
                unitToColumn.put(vertexID, columnNum);
                columnToUnit.put(columnNum, vertexID);
                columnNum++;

                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    FlowEdge e = (FlowEdge) succIt.next();
                    int edgeID = e.getEdgeID();
                    lp.setColName(columnNum, edgePrefix + edgeID);
                    lp.setInt(columnNum, true);
                    unitToColumn.put(edgeID, columnNum);
                    columnToUnit.put(columnNum, edgeID);
                    columnNum++;
                }
            }

            numberOfVariables = columnNum - 1;
        }

        private void addVertexConstraints () throws LpSolveException
        {
            Debug.debugMessage(getClass(),
                    "Adding flow constraints between vertices", 3);

            for (Vertex v : cfg)
            {
                /*
                 * Only add the flow constraint for a vertex if it has both
                 * successors and predecessors
                 */
                if (v.hasPredecessors() && v.hasSuccessors())
                {
                    resetArray(rowArray);
                    resetArray(colArray);

                    int index = 0;

                    colArray[index] = unitToColumn.get(v.getVertexID());
                    rowArray[index] = -1;
                    index++;

                    Iterator <Edge> succIt = v.successorIterator();
                    while (succIt.hasNext())
                    {
                        FlowEdge e = (FlowEdge) succIt.next();
                        int edgeID = e.getEdgeID();
                        colArray[index] = unitToColumn.get(edgeID);
                        rowArray[index] = 1;
                        index++;
                    }

                    lp.addConstraintex(1 + v.numOfSuccessors(), rowArray,
                            colArray, LpSolve.EQ, 0);
                    flowConstraints++;
                }
            }
        }

        private void addEdgeContraints () throws LpSolveException
        {
            Debug.debugMessage(getClass(),
                    "Adding flow constraints between edges", 3);

            for (Vertex v : cfg)
            {
                /*
                 * Only add the flow constraint for a vertex if it has both
                 * successors and predecessors
                 */
                if (v.hasPredecessors() && v.hasSuccessors())
                {
                    resetArray(rowArray);
                    resetArray(colArray);

                    int index = 0;

                    Iterator <Edge> succIt = v.successorIterator();
                    while (succIt.hasNext())
                    {
                        FlowEdge e = (FlowEdge) succIt.next();
                        int edgeID = e.getEdgeID();
                        colArray[index] = unitToColumn.get(edgeID);
                        rowArray[index] = 1;
                        index++;
                    }

                    Iterator <Edge> predIt = v.predecessorIterator();
                    while (predIt.hasNext())
                    {
                        FlowEdge e = (FlowEdge) predIt.next();
                        int edgeID = e.getEdgeID();
                        colArray[index] = unitToColumn.get(edgeID);
                        rowArray[index] = -1;
                        index++;
                    }

                    /*
                     * Add the constraint to the model. Note that the last 2
                     * parameters states that flow in = flow out
                     */
                    lp.addConstraintex(
                            v.numOfPredecessors() + v.numOfSuccessors(),
                            rowArray, colArray, LpSolve.EQ, 0);
                    flowConstraints++;
                }
            }
        }

        private void addLoopConstraints () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding loop constraints", 3);

            for (int level = lnt.getHeight() - 1; level >= 0; --level)
            {
                Iterator <TreeVertex> levelIt = lnt.levelIterator(level);
                while (levelIt.hasNext())
                {
                    TreeVertex v = levelIt.next();

                    if (v instanceof HeaderVertex)
                    {
                        HeaderVertex headerv = (HeaderVertex) v;

                        if (headerv.getHeaderID() == cfg.getEntryID())
                        {
                            resetArray(rowArray);
                            resetArray(colArray);

                            int entryID = cfg.getEntryID();
                            colArray[0] = unitToColumn.get(entryID);
                            rowArray[0] = 1;

                            lp.addConstraintex(1, rowArray, colArray,
                                    LpSolve.EQ, 1);
                            loopConstraints++;
                        }
                        else
                        {
                            writeInnerLoopConstraints(headerv);
                        }
                    }
                }
            }
        }

        private void writeInnerLoopConstraints (HeaderVertex headerv)
                throws LpSolveException
        {
            for (int ancestorID : lnt.getProperAncestors(headerv.getVertexID()))
            {
                HeaderVertex ancestorv = (HeaderVertex) lnt
                        .getVertex(ancestorID);

                int index = 0;
                resetArray(rowArray);
                resetArray(colArray);

                int bound = database.getLoopBound(subprogramID,
                        headerv.getVertexID(), ancestorID);
                Debug.debugMessage(getClass(), "Adding constraint on loop "
                        + headerv.getVertexID() + " relative to loop "
                        + ancestorv.getVertexID() + ". Bound = " + bound, 4);

                Vertex v = cfg.getVertex(headerv.getHeaderID());
                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    FlowEdge e = (FlowEdge) succIt.next();
                    int succID = e.getVertexID();

                    if (lnt.inLoopBody(headerv.getHeaderID(), succID))
                    {
                        int edgeID = e.getEdgeID();
                        colArray[index] = unitToColumn.get(edgeID);
                        rowArray[index] = 1;
                        index++;
                    }
                }

                if (ancestorID == headerv.getParentID())
                {
                    Iterator <Edge> predIt = cfg.getVertex(
                            headerv.getHeaderID()).predecessorIterator();
                    while (predIt.hasNext())
                    {
                        FlowEdge e = (FlowEdge) predIt.next();
                        int predID = e.getVertexID();

                        if (lnt.inLoopBody(headerv.getHeaderID(), predID) == false)
                        {
                            int edgeID = e.getEdgeID();
                            colArray[index] = unitToColumn.get(edgeID);
                            rowArray[index] = -bound;
                            index++;
                        }
                    }

                    index--;
                }
                else
                {
                    colArray[index] = unitToColumn.get(ancestorv.getHeaderID());
                    rowArray[index] = -bound;
                }

                lp.addConstraintex(index + 1, rowArray, colArray, LpSolve.LE, 0);
                loopConstraints++;
            }

        }

        private void addObjectiveFunction () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding objective function", 3);

            resetArray(rowArray);
            resetArray(colArray);

            int index = 0;
            for (Vertex v : cfg)
            {
                int vertexID = v.getVertexID();
                long wcet = 0;

                int calleeID = callg.isCallSite(subprogramID, vertexID);
                if (calleeID == Vertex.DUMMY_VERTEX_ID)
                {
                    wcet = database.getUnitWCET(subprogramID, vertexID);
                }
                else
                {
                    wcet = getWCET(calleeID);
                }

                colArray[index] = unitToColumn.get(vertexID);
                rowArray[index] = wcet;
                index++;
            }

            lp.setObjFnex(cfg.numOfVertices(), rowArray, colArray);
        }
    }

    private class IPETModelCFGInMemoryWithSuperBlocks extends IPETModelCFG
    {

        public IPETModelCFGInMemoryWithSuperBlocks (ControlFlowGraph cfg,
                LoopNests lnt, int subprogramID, String subprogramName)
        {
            super(cfg, lnt, subprogramID, subprogramName);

            try
            {
                lp = LpSolve.makeLp(cfg.numOfVertices(), numOfColumns);

                addColumns();
                lp.setAddRowmode(true);

                addVertexConstraints();
                addEdgeContraints();
                addLoopConstraints();

                lp.setAddRowmode(false);
                addObjectiveFunction();

                lp.setVerbose(getLpSolveVerbosity());
                lp.setMaxim();
                solve();

                if (Debug.getDebugLevel() == Debug.HIGHEST_LEVEL)
                {
                    dumpLPToFile();
                }
            }
            catch (SolutionException e)
            {
                dumpLPToFile();
                System.exit(1);
            }
            catch (LpSolveException e)
            {
                e.printStackTrace();
                System.exit(1);
            }
        }

        private void addColumns () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding columns", 3);

        }

        private void addVertexConstraints () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding vertex constraints", 3);

        }

        private void addEdgeContraints () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding edge constraints", 3);

        }

        private void addLoopConstraints () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding loop constraints", 3);

        }

        private void addObjectiveFunction () throws LpSolveException
        {
            Debug.debugMessage(getClass(), "Adding objective function", 3);

            resetArray(rowArray);
            resetArray(colArray);

            int index = 0;
            for (Vertex v : cfg)
            {
                int vertexID = v.getVertexID();
                long wcet = 0;

                int calleeID = callg.isCallSite(subprogramID, vertexID);
                if (calleeID == Vertex.DUMMY_VERTEX_ID)
                {
                    wcet = database.getUnitWCET(subprogramID, vertexID);
                }
                else
                {
                    wcet = getWCET(calleeID);
                }

                colArray[index] = unitToColumn.get(vertexID);
                rowArray[index] = wcet;
                index++;
            }

            lp.setObjFnex(cfg.numOfVertices(), rowArray, colArray);
        }
    }
}
