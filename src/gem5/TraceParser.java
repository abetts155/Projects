package gem5;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import tvgen.util.SystemOutput;

public class TraceParser
{

    private Set <Long> bbAddrs = new HashSet <Long>();
    private Set <Long> branchInsts = new HashSet <Long>();
    // Maps basic block start addresses to basic block IDs
    private Map <Long, Integer> bbAddrId = new HashMap <Long, Integer>();

    public TraceParser (String programXMLFile)
    {
        File xmlFile = new File(programXMLFile);
        if (!xmlFile.exists())
        {
            SystemOutput.exitWithError("Error: XML file " + programXMLFile
                    + " does not exist");
        }

        try
        {
            SystemOutput.debugMessage("Extracting program info from "
                    + programXMLFile);

            DocumentBuilder builder = DocumentBuilderFactory.newInstance()
                    .newDocumentBuilder();
            Document document = builder.parse(xmlFile);
            Element rootElement = document.getDocumentElement();

            NodeList cfgNodes = rootElement.getElementsByTagName("cfg");
            for (int i = 0; i < cfgNodes.getLength(); i++)
            {
                Node cfgNode = cfgNodes.item(i);
                if (cfgNode instanceof Element)
                {
                    NodeList bbNodes = ((Element) cfgNode)
                            .getElementsByTagName("bb");
                    for (int j = 0; j < bbNodes.getLength(); j++)
                    {
                        Node bbNode = bbNodes.item(j);
                        if (bbNode instanceof Element)
                        {
                            parseBasicBlock((Element) bbNode);
                        }
                    }
                }
            }
        }
        catch (Exception e)
        {
            SystemOutput.printMessage(e.getStackTrace().toString());
            SystemOutput.exitWithError("Error parsing programXMLFile: "
                    + e.getMessage());
        }
    }

    private void parseBasicBlock (Element bbElement)
    {
        try
        {
            int bbId = Integer.parseInt(bbElement.getAttribute("id"));
            NodeList instNodes = bbElement.getElementsByTagName("inst");
            if (instNodes.getLength() > 0)
            {
                Node firstInst = instNodes.item(0);
                if (firstInst instanceof Element)
                {
                    String firstInstAddr = ((Element) firstInst)
                            .getAttribute("addr");
                    long firstInstId = parseAddr(firstInstAddr);
                    bbAddrs.add(firstInstId);
                    bbAddrId.put(firstInstId, bbId);
                }

                Node endInst = instNodes.item(instNodes.getLength() - 1);
                if (endInst instanceof Element)
                {
                    String endInstAddr = ((Element) endInst)
                            .getAttribute("addr");
                    long endInstId = parseAddr(endInstAddr);

                    if (branchBlock(bbElement))
                    {
                        branchInsts.add(endInstId);
                    }
                }
            }
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing basic block");
        }
    }

    private boolean branchBlock (Element bbElement)
    {
        try
        {
            int bbId = Integer.parseInt(bbElement.getAttribute("id"));

            NodeList succNodes = bbElement.getElementsByTagName("succ");
            if (succNodes.getLength() != 1)
            {
                SystemOutput.exitWithError("Error: basic block with id " + bbId
                        + " does not have one succ child node");
            }

            Node succNode = succNodes.item(0);
            if (succNode instanceof Element)
            {
                NodeList links = ((Element) succNode)
                        .getElementsByTagName("link");
                if (links.getLength() == 2)
                {
                    Node firstBranch = links.item(0);
                    Node sndBranch = links.item(1);
                    if (firstBranch instanceof Element
                            && sndBranch instanceof Element)
                    {
                        String firstType = ((Element) firstBranch)
                                .getAttribute("type");
                        String sndType = ((Element) sndBranch)
                                .getAttribute("type");
                        if ((firstType.equals("taken") && sndType
                                .equals("nottaken"))
                                || (firstType.equals("nottaken") && sndType
                                        .equals("taken")))
                        {
                            return true;
                        }
                    }
                }
            }
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing basic block successors");
        }
        return false;
    }

    // Returns a set of basic blocks covered in trace
    public Set <Integer> basicBlocksInTrace (String traceName)
    {
        Set <Integer> bbs = new HashSet <Integer>();
        try
        {
            InputStream traceFile = new FileInputStream(traceName);
            BufferedReader trace = new BufferedReader(new InputStreamReader(
                    traceFile));

            List <InstructionTime> instTimes = getInstructionTimes(trace);
            trace.close();

            for (InstructionTime instTime : instTimes)
            {
                Integer bbId = bbAddrId.get(instTime.instruction);
                if (bbId != null)
                {
                    bbs.add(bbId);
                }
            }
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing trace file" + traceName);
        }
        return bbs;
    }

    // Returns a map of basic blocks to the number of times they were executed
    public Map <Integer, Integer> basicBlocksCount (String traceName)
    {
        Map <Integer, Integer> bbcount = new HashMap <Integer, Integer>();
        try
        {
            InputStream traceFile = new FileInputStream(traceName);
            BufferedReader trace = new BufferedReader(new InputStreamReader(
                    traceFile));

            List <InstructionTime> instTimes = getInstructionTimes(trace);
            trace.close();

            for (InstructionTime instTime : instTimes)
            {
                Integer bbId = bbAddrId.get(instTime.instruction);
                if (bbId != null)
                {
                    Integer blockCount = bbcount.get(bbId);
                    if (blockCount == null)
                        bbcount.put(bbId, 1);
                    else
                        bbcount.put(bbId, blockCount + 1);
                }
            }
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing trace file" + traceName);
        }
        return bbcount;
    }

    public long getInstructionTimeDiff (Set<Long> entryBlockInsts, String traceName)
    {
        long time1 = -1;
        long time2 = -1;
        try
        {
            InputStream traceFile = new FileInputStream(traceName);
            BufferedReader trace = new BufferedReader(new InputStreamReader(
                    traceFile));

            List <InstructionTime> instTimes = getInstructionTimes(trace);
            trace.close();

            for (InstructionTime instTime : instTimes)
            {
            	if (entryBlockInsts.contains(instTime.instruction));
            	{
            		if(time1 == -1)
            			time1 = instTime.time;
            		time2 = instTime.time;
            	}
            }
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing trace file" + traceName);
        }
        
        long timeDiff = time2 - time1;
        if(timeDiff < 0)
        {
        	SystemOutput.debugMessage("Time between first and last instruction < 0");
        	timeDiff = 0;
        }
        return timeDiff;
    }

    public String parseTrace (String traceName, String instrumentation,
            boolean hexAddrs)
    {
        try
        {
            InputStream traceFile = new FileInputStream(traceName);
            BufferedReader trace = new BufferedReader(new InputStreamReader(
                    traceFile));

            List <InstructionTime> instTimes = getInstructionTimes(trace);
            trace.close();

            String parsedTrace = null;
            if (instrumentation.equals("BASIC_BLOCK"))
            {
                parsedTrace = parseBasicBlockTrace(instTimes, hexAddrs);
            }
            else if (instrumentation.equals("BRANCH"))
            {
                parsedTrace = parseBranchTrace(instTimes, hexAddrs);
            }

            return parsedTrace;
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing trace file" + traceName);
        }
        return "";
    }

    private List <InstructionTime> getInstructionTimes (BufferedReader trace)
    {
        int timeIndex = 0;
        int cpuAddrIndex = 1;
        int addrIndex = 1;

        List <InstructionTime> times = new ArrayList <InstructionTime>();

        try
        {
            for (String line = trace.readLine(); line != null; line = trace
                    .readLine())
            {
                String[] splitLine = line.split(":");
                Long time = Long.parseLong(splitLine[timeIndex].trim());
                String[] cpuAddr = splitLine[cpuAddrIndex].trim().split(" ");
                String addr = cpuAddr[addrIndex];

                String[] microInst = addr.split("\\.");
                if (microInst.length == 1)
                    times.add(new InstructionTime(parseAddr(addr.trim()), time));
                else
                {
                    if (microInst[1].equals("0"))
                        times.add(new InstructionTime(parseAddr(microInst[0]
                                .trim()), time));
                }
            }
        }
        catch (Exception e)
        {
            handleException(e, "Error parsing trace");
        }

        return times;
    }

    private String parseBasicBlockTrace (List <InstructionTime> instTimes,
            boolean hexAddrs)
    {
        String bbString = "";

        for (InstructionTime instTime : instTimes)
        {
            if (bbAddrs.contains(instTime.instruction))
            {
                bbString += instTime.toString(hexAddrs) + "\n";
            }
        }

        return bbString;
    }

    private String parseBranchTrace (List <InstructionTime> instTimes,
            boolean hexAddrs)
    {
        String branchString = "";

        InstructionTime prev = null;
        SystemOutput.printMessage(Integer.toString(branchInsts.size()));
        for (InstructionTime instTime : instTimes)
        {
            if (prev != null && branchInsts.contains(prev.instruction))
            {
                branchString += instTime.toString(hexAddrs) + "\n";
            }

            prev = instTime;
        }

        return branchString;
    }

    private class InstructionTime
    {

        public long instruction;
        public long time;

        public InstructionTime (long inst, long time)
        {
            this.instruction = inst;
            this.time = time;
        }

        public String toString (boolean hexAddrs)
        {
            String addr;
            if (hexAddrs)
            {
                addr = Long.toHexString(instruction);
            }
            else
            {
                addr = Long.toString(instruction);
            }
            return addr + " " + time;
        }
    }

    private long parseAddr (String addr)
    {
        String hex = addr.substring(2);
        return Long.parseLong(hex, 16);
    }

    private void handleException (Exception e, String message)
    {
        SystemOutput.printMessage(e.getMessage());
        e.printStackTrace();
        SystemOutput.exitWithError(message);
    }
}
