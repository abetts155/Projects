package adam.betts.instructions;

import java.io.RandomAccessFile;
import java.util.regex.Pattern;

import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;

public class PTXInstructionSet
{

    // PTX directives
    public final static String directiveAddressSize = ".address_size";
    public final static String directiveAlign = ".align";
    public final static String directiveBranchTargets = ".branchtargets";
    public final static String directiveCallPrototype = ".callprototype";
    public final static String directiveCallTargets = ".calltargets";
    public final static String directiveConst = ".const";
    public final static String directiveEntry = ".entry";
    public final static String directiveExtern = ".extern";
    public final static String directiveFile = ".file";
    public final static String directiveFunc = ".func";
    public final static String directiveGlobal = ".global";
    public final static String directiveLoc = ".loc";
    public final static String directiveLocal = ".local";
    public final static String directiveMaxNumberOfCTAPerSM = ".maxnctapersm";
    public final static String directiveMaxNumberOfRegisters = ".maxnreg";
    public final static String directiveMaxNumberOfTreads = ".maxntid";
    public final static String directiveMinNumberOfCTAPerSM = ".minnctapersm";
    public final static String directiveParam = ".param";
    public final static String directivePragma = ".pragma";
    public final static String directiveReg = ".reg";
    public final static String directiveRegNumberThreadID = ".reqntid";
    public final static String directiveSection = ".section";
    public final static String directiveShared = ".shared";
    public final static String directiveSReg = ".sreg";
    public final static String directiveTarget = ".target";
    public final static String directiveTex = ".tex";
    public final static String directiveVersion = ".version";
    public final static String directiveVisible = ".visible";

    // PTX instruction mnemonics
    public final static String absInstr = "abs";
    public final static String addInstr = "add";
    public final static String addcInstr = "addc";
    public final static String andInstr = "and";
    public final static String atomInstr = "atom";
    public final static String barInstr = "bar";
    public final static String bfeInstr = "bfe";
    public final static String bfiInstr = "bfi";
    public final static String bfindInstr = "bfind";
    public final static String braInstr = "bra";
    public final static String brevInstr = "brev";
    public final static String brkptInstr = "brkpt";
    public final static String callInstr = "call";
    public final static String clzInstr = "clz";
    public final static String cnotInstr = "cnot";
    public final static String copyassignInstr = "copyassign";
    public final static String cosInstr = "cos";
    public final static String cvtInstr = "cvt";
    public final static String cvtaInstr = "cvta";
    public final static String divInstr = "div";
    public final static String ex2Instr = "ex2";
    public final static String exitInstr = "exit";
    public final static String fmaInstr = "fma";
    public final static String isspacepInstr = "isspacep";
    public final static String ldInstr = "ld";
    public final static String lduInstr = "ldu";
    public final static String lg2Instr = "lg2";
    public final static String madInstr = "mad";
    public final static String mad24Instr = "mad24";
    public final static String madcInstr = "madc";
    public final static String maxInstr = "max";
    public final static String membarInstr = "membar";
    public final static String minInstr = "min";
    public final static String movInstr = "mov";
    public final static String mulInstr = "mul";
    public final static String mul24Instr = "mul24";
    public final static String negInstr = "neg";
    public final static String notInstr = "not";
    public final static String orInstr = "or";
    public final static String pmeventInstr = "pmevent";
    public final static String popcInstr = "popc";
    public final static String prefetchInstr = "prefetch";
    public final static String prefetchuInstr = "prefetchu";
    public final static String prmtInstr = "prmt";
    public final static String rcpInstr = "rcp";
    public final static String redInstr = "red";
    public final static String remInstr = "rem";
    public final static String retInstr = "ret";
    public final static String rsqrtInstr = "rsqrt";
    public final static String sadInstr = "sad";
    public final static String selpInstr = "selp";
    public final static String setInstr = "set";
    public final static String setpInstr = "setp";
    public final static String shflInstr = "shfl";
    public final static String shlInstr = "shl";
    public final static String shrInstr = "shr";
    public final static String sinInstr = "sin";
    public final static String slctInstr = "slct";
    public final static String sqrtInstr = "sqrt";
    public final static String stInstr = "st";
    public final static String subInstr = "sub";
    public final static String subcInstr = "subc";
    public final static String suldInstr = "suld";
    public final static String suqInstr = "suq";
    public final static String suredInstr = "sured";
    public final static String sustInstr = "sust";
    public final static String testpInstr = "testp";
    public final static String texInstr = "tex";
    public final static String tld4Instr = "tld4";
    public final static String trapInstr = "trap";
    public final static String txqInstr = "txq";
    public final static String vabsdiffInstr = "vabsdiff";
    public final static String vabsdiff2Instr = "vabsdiff2";
    public final static String vabsdiff4Instr = "vabsdiff4";
    public final static String vaddInstr = "vadd";
    public final static String vadd2Instr = "vadd2";
    public final static String vadd4Instr = "vadd4";
    public final static String vavrg2Instr = "vavrg2";
    public final static String vavrg4Instr = "vavrg4";
    public final static String vmadInstr = "vmad";
    public final static String vmaxInstr = "vmax";
    public final static String vmax2Instr = "vmax2";
    public final static String vmax4Instr = "vmax4";
    public final static String vminInstr = "vmin";
    public final static String vmin2Instr = "vmin2";
    public final static String vmin4Instr = "vmin4";
    public final static String voteInstr = "vote";
    public final static String vsetInstr = "vset";
    public final static String vset2Instr = "vset2";
    public final static String vset4Instr = "vset4";
    public final static String vshlInstr = "vshl";
    public final static String vshrInstr = "vshr";
    public final static String vsubInstr = "vsub";
    public final static String vsub2Instr = "vsub2";
    public final static String vsub4Instr = "vsub4";
    public final static String xorInstr = "xor";

    // PTX pre-defined identifiers
    public final static String clock = "clock";
    public final static String clock64 = "clock64";
    public final static String ctaid = "ctaid";
    public final static String envreg = "envreg";
    public final static String gridid = "gridid";
    public final static String landid = "landid";
    public final static String lanemask_eq = "lanemask_eq";
    public final static String lanemask_le = "lanemask_le";
    public final static String lanemask_lt = "lanemask_lt";
    public final static String lanemask_ge = "lanemask_ge";
    public final static String lanemask_gt = "lanemask_gt";
    public final static String nctaid = "nctaid";
    public final static String ntid = "ntid";
    public final static String nsmid = "nsmid";
    public final static String nwarpid = "nwarpid";
    public final static String pm0 = "pm0";
    public final static String pm1 = "pm1";
    public final static String pm2 = "pm2";
    public final static String pm3 = "pm3";
    public final static String smid = "smid";
    public final static String tid = "tid";
    public final static String warpid = "warpid";
    public final static String WARP_SZ = "WARP_SZ";

    private int majorVersion = -1;
    private int minorVersion = -1;

    public PTXInstructionSet (String ptxFileName)
    {
        try
        {
            RandomAccessFile raf = new RandomAccessFile(ptxFileName, "r");
            String line = raf.readLine().replaceAll("^\\s+", "");
            raf.close();

            int index = line.lastIndexOf(directiveVersion);
            if (index == -1)
            {
                Debug.errorMessage(getClass(),
                        "Unable to find PTX version in module '" + ptxFileName
                                + "'");
            }
            else
            {
                String[] lexemes = line.split("\\s+");
                assert lexemes.length == 2;

                String[] versionLexemes = lexemes[1].split(Pattern.quote("."));
                assert versionLexemes.length == 2;
                majorVersion = Integer.parseInt(versionLexemes[0]);
                minorVersion = Integer.parseInt(versionLexemes[1]);

                Debug.debugMessage(getClass(), "Version of PTX is "
                        + getISAVersionString(), 1);
            }
        }
        catch (Exception e)
        {
            Debug.errorMessage(getClass(), e.getMessage());
        }
    }

    public final int getMinorVersion ()
    {
        assert minorVersion > 0 : "Unable to ascertain minor version of ISA";
        return minorVersion;
    }

    public final int getMajorVersion ()
    {
        assert majorVersion > 0 : "Unable to ascertain major version of ISA";
        return majorVersion;
    }

    public final String getISAVersionString ()
    {
        return getMajorVersion() + "." + getMinorVersion();
    }
}
