package adam.betts.vertices;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class SuperBlockVertex extends Vertex
{
    protected ArrayList <Integer> basicBlocks = new ArrayList <Integer>();

    public SuperBlockVertex (int vertexID)
    {
        super(vertexID);
    }

    public final void addBasicBlock (int vertexID)
    {
        basicBlocks.add(vertexID);
    }

    public final boolean containsBasicBlock (int vertexID)
    {
        return basicBlocks.contains(vertexID);
    }

    public final int pickRandomBasicBlockID ()
    {
        Random random = new Random();
        return basicBlocks.get(random.nextInt(basicBlocks.size()));
    }

    public final int numberOfBasicBlocks ()
    {
        return basicBlocks.size();
    }

    public final List <Integer> basicBlockIDs ()
    {
        return Collections.unmodifiableList(basicBlocks);
    }

    public static SuperBlockVertex copy (SuperBlockVertex superv)
    {
        SuperBlockVertex newv = new SuperBlockVertex(superv.vertexID);
        for (int bbID : superv.basicBlocks)
        {
            newv.basicBlocks.add(bbID);
        }
        return newv;
    }
}
