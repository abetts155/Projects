package adam.betts.instructions;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import adam.betts.utilities.Debug;
import adam.betts.utilities.Globals;
import adam.betts.utilities.Enums.ISA;

public class InstructionSet
{
	/*
	 * Different types of jump instructions sorted by length of instruction
	 * mnemonic and then alphabetically
	 */
	private final static String bInstruction = "b";
	private final static String jInstruction = "j";

	private final static String baInstruction = "ba";
	private final static String beInstruction = "be";
	private final static String bgInstruction = "bg";
	private final static String blInstruction = "bl";
	private final static String brInstruction = "br";
	private final static String jaInstruction = "ja";
	private final static String jbInstruction = "jb";
	private final static String jcInstruction = "jc";
	private final static String jeInstruction = "je";
	private final static String jgInstruction = "jg";
	private final static String jlInstruction = "jl";
	private final static String joInstruction = "jo";
	private final static String jpInstruction = "jp";
	private final static String jrInstruction = "jr";
	private final static String jsInstruction = "js";
	private final static String jzInstruction = "jz";

	private final static String bccInstruction = "bcc";
	private final static String bcsInstruction = "bcs";
	private final static String beqInstruction = "beq";
	private final static String bgeInstruction = "bge";
	private final static String bgtInstruction = "bgt";
	private final static String bguInstruction = "bgu";
	private final static String bhiInstruction = "bhi";
	private final static String bleInstruction = "ble";
	private final static String blsInstruction = "bls";
	private final static String bltInstruction = "blt";
	private final static String bmiInstruction = "bmi";
	private final static String bneInstruction = "bne";
	private final static String bplInstruction = "bpl";
	private final static String bsrInstruction = "bsr";
	private final static String bvcInstruction = "bvc";
	private final static String bvsInstruction = "bvs";
	private final static String jalInstruction = "jal";
	private final static String jaeInstruction = "jae";
	private final static String jbeInstruction = "jbe";
	private final static String jgeInstruction = "jge";
	private final static String jleInstruction = "jle";
	private final static String jnaInstruction = "jna";
	private final static String jnbInstruction = "jnb";
	private final static String jncInstruction = "jnc";
	private final static String jneInstruction = "jne";
	private final static String jngInstruction = "jng";
	private final static String jnlInstruction = "jnl";
	private final static String jnoInstruction = "jno";
	private final static String jnpInstruction = "jnp";
	private final static String jnsInstruction = "jns";
	private final static String jnzInstruction = "jnz";
	private final static String jpeInstruction = "jpe";
	private final static String jpoInstruction = "jpo";
	private final static String jmpInstruction = "jmp";
	private final static String jsrInstruction = "jsr";
	private final static String retInstruction = "ret";

	private final static String bDotnInstruction = "b.n";
	private final static String beqDotnInstruction = "beq.n";
	private final static String bgeDotnInstruction = "bge.n";
	private final static String bgtDotnInstruction = "bgt.n";
	private final static String bhiDotnInstruction = "bhi.n";
	private final static String bleDotnInstruction = "ble.n";
	private final static String blsDotnInstruction = "bls.n";
	private final static String bltDotnInstruction = "blt.n";
	private final static String bneDotnInstruction = "bne.n";

	private final static String bc1fInstruction = "bc1f";
	private final static String bc1tInstruction = "bc1t";
	private final static String bgezInstruction = "bgez";
	private final static String bgtzInstruction = "bgtz";
	private final static String blbcInstruction = "blbc";
	private final static String blbsInstruction = "blbs";
	private final static String bleuInstruction = "bleu";
	private final static String blezInstruction = "blez";
	private final static String bltzInstruction = "bltz";
	private final static String callInstruction = "call";
	private final static String jalrInstruction = "jalr";
	private final static String jcxzInstruction = "jcxz";
	private final static String jnaeInstruction = "jnae";
	private final static String jnbeInstruction = "jnbe";
	private final static String jngeInstruction = "jnge";
	private final static String jnleInstruction = "jnle";
	private final static String retlInstruction = "retl";

	private final static String jsr_coroutineInstruction = "jsr_coroutine";

	/*
	 * Branches of the instruction set
	 */
	private HashSet <String> branches;

	/*
	 * Unconditional branch instructions
	 */
	private HashSet <String> unconditionalBranch;

	/*
	 * The subprogram call instruction
	 */
	private String subprogramCall;

	/*
	 * The subprogram return instruction
	 */
	private String subprogramReturn;

	/*
	 * The offset to add an address to find the next instruction. Normally 4 but
	 * sometimes can be 8
	 */
	private int addressOffset = 4;

	/*
	 * The particular instruction set, which is determined in the constructor
	 */
	private ISA isa = null;

	public InstructionSet ()
	{
		Debug.verboseMessage ("DETERMINING ISA");
		setupISA ();
		Debug.debugMessage (getClass (), "ISA is " + isa, 1);
	}

	private void setupISA ()
	{
		try
		{
			/*
			 * Check whether the file passed in is a valid disassembly file
			 */
			String programFileName = Globals.getProgramFileName ();
			RandomAccessFile raf = new RandomAccessFile (programFileName, "r");
			/*
			 * Skip the first byte in the disassembly file as it is normally
			 * carriage return
			 */
			raf.seek (1);

			String line = raf.readLine ();
			if (line.endsWith ("ss-coff-little"))
			{
				/*
				 * The different types of jump instructions that the
				 * disassembler reader recognises for the MIPS instruction set.
				 * Note that neither "jr" nor "jalr" are fully supported since
				 * these are indirect calls and we cannot statically determine
				 * their destinations
				 */
				branches = new HashSet <String> (Arrays.asList (new String[] { jInstruction,
						jalInstruction, beqInstruction, bneInstruction, blezInstruction,
						bgtzInstruction, bltzInstruction, bgezInstruction, bc1fInstruction,
						bc1tInstruction }));
				unconditionalBranch = new HashSet <String> (Arrays
						.asList (new String[] { jInstruction }));
				subprogramCall = jalInstruction;
				isa = ISA.PISA;
				addressOffset = 8;
			} else if (line.endsWith ("elf32-littlearm"))
			{
				/*
				 * The different types of jump instructions that the
				 * disassembler reader recognises for the ARM instruction set
				 */
				branches = new HashSet <String> (Arrays.asList (new String[] { bInstruction,
						blInstruction, bccInstruction, bcsInstruction, beqInstruction,
						bgeInstruction, bgtInstruction, bhiInstruction, bleInstruction,
						blsInstruction, bltInstruction, bmiInstruction, bneInstruction,
						bplInstruction, bvsInstruction, bvcInstruction, bDotnInstruction,
						beqDotnInstruction, bgeDotnInstruction, bgtDotnInstruction,
						bhiDotnInstruction, bleDotnInstruction, blsDotnInstruction,
						bltDotnInstruction, bneDotnInstruction }));

				unconditionalBranch = new HashSet <String> (Arrays.asList (new String[] {
						bInstruction, bDotnInstruction }));

				subprogramCall = blInstruction;
				
				isa = ISA.ARM;
			} else if (line.endsWith ("elf64-alpha"))
			{
				/*
				 * The different types of jump instructions that the
				 * disassembler reader recognises for the ALPHA instruction set.
				 * Note "ret" is included as it is merely a return from
				 * subprogram instruction that has no edge in the CFG. Also note
				 * that the "jsr_coroutine" and "jmp" are not supported
				 */
				branches = new HashSet <String> (Arrays.asList (new String[] { brInstruction,
						beqInstruction, bneInstruction, bltInstruction, bleInstruction,
						bgtInstruction, bgeInstruction, blbcInstruction, blbsInstruction,
						bsrInstruction, jsrInstruction }));
				unconditionalBranch = new HashSet <String> (Arrays
						.asList (new String[] { brInstruction }));
				subprogramCall = bsrInstruction;
				isa = ISA.ALPHA;
			} else if (line.endsWith ("elf32-sparc"))
			{
				branches = new HashSet <String> (Arrays.asList (new String[] { bInstruction,
						baInstruction, bneInstruction, beInstruction, bgInstruction,
						bleInstruction, bgeInstruction, blInstruction, bguInstruction,
						bleuInstruction, callInstruction, retInstruction, retlInstruction }));
				unconditionalBranch = new HashSet <String> (Arrays.asList (new String[] {
						bInstruction, retInstruction, retlInstruction }));
				subprogramCall = callInstruction;
				isa = ISA.SPARC;
			} else if (line.endsWith ("elf32-i386"))
			{
				branches = new HashSet <String> (Arrays.asList (new String[] { callInstruction,
						jaInstruction, jaeInstruction, jbInstruction, jbeInstruction,
						jcInstruction, jcxzInstruction, jeInstruction, jgInstruction,
						jgeInstruction, jlInstruction, jleInstruction, jmpInstruction,
						jnaInstruction, jnaeInstruction, jnbInstruction, jnbeInstruction,
						jncInstruction, jneInstruction, jngInstruction, jngeInstruction,
						jnlInstruction, jnleInstruction, jnoInstruction, jnpInstruction,
						jnsInstruction, jnzInstruction, joInstruction, jpInstruction,
						jpeInstruction, jpoInstruction, jsInstruction, jzInstruction }));
				unconditionalBranch = new HashSet <String> (Arrays
						.asList (new String[] { jmpInstruction }));
				subprogramCall = callInstruction;
				isa = ISA.X86;
			} else
			{
				Debug.debugMessage (getClass (), "Disassembly first line " + line, 4);
				throw new IOException (programFileName + " is not a valid disassembly file");
			}
		} catch (Exception e)
		{
			e.printStackTrace ();
			System.exit (1);
		}
	}

	public final ISA getISA ()
	{
		return isa;
	}

	public final Set <String> getBranches ()
	{
		return Collections.unmodifiableSet (branches);
	}

	public final Set <String> getUnconditionalBranches ()
	{
		return Collections.unmodifiableSet (unconditionalBranch);
	}

	public final String getSubprogramCallInstruction ()
	{
		return subprogramCall;
	}

	public final int getAddressOffset ()
	{
		return addressOffset;
	}
}
