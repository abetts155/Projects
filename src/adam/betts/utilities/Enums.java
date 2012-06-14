package adam.betts.utilities;

public final class Enums
{

    public enum DFSEdgeType
    {
        TREE_EDGE, BACK_EDGE, CROSS_FORWARD_EDGE
    };

    public enum IPGEdgeType
    {
        TRACE_EDGE, GHOST_EDGE, INLINED_EDGE
    };

    public enum DJEdgeType
    {
        DOMINATOR_EDGE, CROSS_JOIN_EDGE, BACK_JOIN_EDGE
    }

    public enum IProfile
    {
        BASIC_BLOCK, BRANCH, FUNCTION, PRE_DOMINATOR, SUPER_BLOCK, RANDOM, NONE
    };

    public enum BranchType
    {
        TAKEN, NOTTAKEN, CALL, RETURN, CASE, UNKNOWN
    };

    public enum LpSolveVerbosity
    {
        CRITICAL, SEVERE, IMPORTANT, NORMAL, DETAILED, FULL
    };

    public enum ISA
    {
        PISA, ARM, ALPHA, SPARC, X86
    };

    public enum PipelineStage
    {
        IF, RET
    };

    public enum DataType
    {
        INTEGER, FLOAT
    };

    public enum DominatorTreeType
    {
        PRE_DOMINATOR, POST_DOMINATOR
    }

    public enum LoopType
    {
        SINGLE_ENTRY, MULTIPLE_ENTRY
    }

    public enum SuperBlockCFGStructureEdgeType
    {
        NORMAL, LOOP
    }
}
