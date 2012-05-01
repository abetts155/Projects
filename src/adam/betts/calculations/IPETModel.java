package adam.betts.calculations;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;

import lpsolve.LpSolve;
import adam.betts.utilities.Enums;
import adam.betts.utilities.Enums.LpSolveVerbosity;

public class IPETModel
{

    protected static String ILPdirectory = ".";
    protected static int loopConstraintLevel = 1;
    protected static Enums.LpSolveVerbosity lpVerbosity = LpSolveVerbosity.CRITICAL;

    protected final static String vertexPrefix = "v_";
    protected final static String edgePrefix = "e_";
    protected final static String newLine = "\n";
    protected final static String statementTerminator = ";";
    protected final static String equals = " = ";
    protected final static String lessThanOrEquals = " <= ";
    protected final static String plus = " + ";
    protected final static String underscore = "_";
    protected final static String space = " ";
    protected final static String variableSeparator = ", ";
    protected final static String intString = "int ";
    protected final static String maxString = "max: ";

    protected LpSolve lp;
    protected int numOfColumns;
    protected long wcet;
    protected int[] colArray;
    protected double[] rowArray;
    protected HashMap <Integer, Integer> executionCounts = new HashMap <Integer, Integer>();
    protected HashMap <Integer, Integer> unitToColumn = new LinkedHashMap <Integer, Integer>();
    protected HashMap <Integer, Integer> columnToUnit = new LinkedHashMap <Integer, Integer>();

    /*
     * Needed for analysis of loops in flow graph
     */
    protected HashMap <Integer, ArrayList <Integer>> ancestors = new HashMap <Integer, ArrayList <Integer>>();
    protected HashMap <Integer, ArrayList <Integer>> backEdges = new HashMap <Integer, ArrayList <Integer>>();
    protected HashMap <Integer, ArrayList <Integer>> entryEdges = new HashMap <Integer, ArrayList <Integer>>();
    protected HashMap <Integer, ArrayList <Integer>> exitEdges = new HashMap <Integer, ArrayList <Integer>>();

    public final static void setILPDirectory (String directory)
    {
        ILPdirectory = directory;
        File dir = new File(ILPdirectory);
        if (!dir.exists())
        {
            dir.mkdir();
        }
    }

    public final static String getILPDirectory ()
    {
        return ILPdirectory;
    }

    public final static boolean lpSolveDirectorySet ()
    {
        return ILPdirectory != null;
    }

    public final static void setLoopConstraintLevel (int level)
    {
        loopConstraintLevel = level;
    }

    public final static void setLpSolveVerbosity (String verbosity)
    {
        lpVerbosity = LpSolveVerbosity.valueOf(verbosity);
    }

    public final static int getLpSolveVerbosity ()
    {
        switch (lpVerbosity)
        {
            case CRITICAL:
                return 1;
            case SEVERE:
                return 2;
            case IMPORTANT:
                return 3;
            case NORMAL:
                return 4;
            case DETAILED:
                return 5;
            case FULL:
                return 6;
        }
        return 0;
    }

    protected final static String createComment (String val)
    {
        return "// " + val + newLine;
    }

    protected final static String createVertexVariable (int vID)
    {
        return vertexPrefix + Integer.toString(vID);
    }

    protected final static String createVertexVariable (int vID, int index)
    {
        return vertexPrefix + Integer.toString(vID) + underscore
                + Integer.toString(index);
    }

    protected final static String createEdgeVariable (int eID)
    {
        return edgePrefix + Integer.toString(eID);
    }

    protected final static String createEdgeVariable (int eID, int index)
    {
        return edgePrefix + Integer.toString(eID) + underscore
                + Integer.toString(index);
    }
}
