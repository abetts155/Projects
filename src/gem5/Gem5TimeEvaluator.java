package gem5;

import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import tvgen.util.SystemOutput;
import tvgen.util.TestVector;

public class Gem5TimeEvaluator extends Gem5Evaluator {

	private int firstInst;
	private int lastInst;
	String traceFile;
	
	public Gem5TimeEvaluator(int threadID, String programName, Gem5Tools g5tools,
			String entryPoint) {
		super(threadID, programName, g5tools);
		traceFile = "m5out/thread" + threadID + "/trace.out";
		findBorderInstructions(programName + ".xml", entryPoint);
	}
	
	public void evaluate(TestVector vector) {
		double time = g5Tools.runGem5(vector.toString(), "m5out/thread" + getThreadID(),
				"trace.out");

		long entryTime = g5Tools.getInstructionTimeDiff(firstInst, lastInst, traceFile);
		
		vector.setScore(entryTime);
		vector.setTime(time);
		String traceOutput = g5Tools.sanitiseGem5Trace(traceFile, "BASIC_BLOCK");
		
		//Append trace to compressed file
		addToTraceOutput(traceOutput);
	}
	
	public void findBorderInstructions(String programXMLFile, String entryPoint)
	{
		boolean entryFound = false;
		int lastInstruction = 0;
		
		File xmlFile = new File(programXMLFile);
		if(!xmlFile.exists())
		{
			SystemOutput.exitWithError("Error: XML file " + programXMLFile +
					" does not exist");
		}
		
		try
		{
			DocumentBuilder builder = DocumentBuilderFactory.newInstance ()
					.newDocumentBuilder ();
			Document document = builder.parse (xmlFile);
			Element rootElement = document.getDocumentElement ();
			
			NodeList cfgNodes = rootElement.getElementsByTagName("cfg");
			// Build all cfg Nodes
			for(int i = 0; i < cfgNodes.getLength(); i++)
			{
				Node cfgNode = cfgNodes.item(i);
				if(cfgNode instanceof Element)
				{
					if(((Element)cfgNode).getAttribute("name").equals(entryPoint))
					{
						entryFound = true;
						NodeList bbNodes = ((Element)cfgNode).getElementsByTagName("bb");
						
						Node bbNode = bbNodes.item(0);
						if(bbNode instanceof Element)
						{
							NodeList bbInsts = ((Element)bbNode).getElementsByTagName("inst");
							Node firstInstNode = bbInsts.item(0);
							if(firstInstNode instanceof Element)
							{
								String firstAddr = ((Element)firstInstNode).getAttribute("addr");
								firstInst = Integer.parseInt(firstAddr.substring(2), 16);
							}
						}
						
						bbNode = bbNodes.item(bbNodes.getLength()-1);
						if(bbNode instanceof Element)
						{
							NodeList bbInsts = ((Element)bbNode).getElementsByTagName("inst");
							int index = bbInsts.getLength() - 1;
							Node lastInstNode = bbInsts.item(index);
							if(lastInstNode instanceof Element)
							{
								while(((Element)lastInstNode).getAttribute("instr").equals("nop"))
								{
									index--;
									lastInstNode = bbInsts.item(index);
								}
								String lastAddr = ((Element)lastInstNode).getAttribute("addr");
								lastInst = Integer.parseInt(lastAddr.substring(2), 16);
							}
						}
					}
				}
			}
		}
		catch (Exception e)
		{
			SystemOutput.printMessage(e.getStackTrace().toString());
			SystemOutput.exitWithError("Error parsing programXMLFile: " + e.getMessage());
		}
			
		if(!entryFound)
		{
			SystemOutput.exitWithError("Error could not find entry point " + entryPoint);
		}
	}

}
