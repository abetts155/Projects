package adam.betts.instructions;

import java.util.HashSet;

public class Instruction
{

    protected long address;
    protected String[] instruction;
    protected HashSet <String> labels = new HashSet <String>();

    public Instruction (long address, String instructionStr)
    {
        this.address = address;
        instruction = instructionStr.split("\\s+");
    }

    public Instruction (long address, String instructionStr,
            HashSet <String> labels)
    {
        this.address = address;
        instruction = instructionStr.split("\\s+");
        this.labels.addAll(labels);
    }

    public final long getAddress ()
    {
        return address;
    }

    public final boolean hasLabels ()
    {
        return labels.isEmpty() == false;
    }

    public final String getLabelString ()
    {
        return "\\tLabels = " + labels;
    }

    public final String getInstruction ()
    {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < instruction.length; ++i)
        {
            buffer.append(instruction[i].replace(">", "").replace("<", "")
                    .replace("\"", "").replace("&", ""));
            if (i < instruction.length - 1)
            {
                buffer.append(" ");
            }
        }
        return buffer.toString();
    }

    public final String getOperation ()
    {
        return instruction[0].trim();
    }

    public final String getOperand (int i)
    {
        return instruction[i];
    }

    public String toString ()
    {
        return Long.toHexString(address) + " " + getInstruction();
    }
}
