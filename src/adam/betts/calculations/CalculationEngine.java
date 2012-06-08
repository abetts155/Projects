package adam.betts.calculations;

import java.io.IOException;
import java.util.HashMap;

import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.DepthFirstTree;
import adam.betts.outputs.WCETOutput;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;

public class CalculationEngine
{

    protected Database database;
    protected Program program;
    protected DepthFirstTree dfs;
    protected HashMap <String, IPETModel> ipetModels = new HashMap <String, IPETModel>();

    public CalculationEngine (IPGDatabase database, Program program)
    {
        this.program = program;
        this.database = database;
        dfs = new DepthFirstTree(program.getCallGraph(), program.getRootID());
    }

    public void calculateWCETWithIPGs ()
    {
        try
        {
            for (int i = 1; i <= dfs.numOfVertices(); ++i)
            {
                int subprogramID = dfs.getPostVertexID(i);
                Subprogram subprogram = program.getSubprogram(subprogramID);
                String subprogramName = subprogram.getSubprogramName();

                IpointGraph ipg = subprogram.getIPG(Globals
                        .getInstrumentationProfile());
            }

            WCETOutput.writeRunDelimiter();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }

    public final long getWCET (String subprogramName)
    {
        return ipetModels.get(subprogramName).wcet;
    }

    public final long getWCET (int subprogramID)
    {
        for (String subprogramName : ipetModels.keySet())
        {
            if (program.getSubprogram(subprogramName).getSubprogramID() == subprogramID)
            {

                return ipetModels.get(subprogramName).wcet;
            }
        }
        return 0;
    }
}
