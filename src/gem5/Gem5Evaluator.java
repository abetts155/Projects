package gem5;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.zip.GZIPOutputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import tvgen.ga.TVEvaluator;
import tvgen.util.*;

public abstract class Gem5Evaluator extends TVEvaluator {

	private String programName;
	
	protected Gem5Tools g5Tools;
	
	protected int firstInst;
	protected int lastInst;
	
	private static PrintStream traceOutput = null;
	private static PrintStream timesOutput = null;
	private static String traceFileName = "trace.BASIC_BLOCK.GA.gz";
	private static String timesFileName = "vector-times.txt";
	
	public Gem5Evaluator(int threadID, String programName, Gem5Tools g5tools,
			String entryPoint) {
		super(threadID);
		
		this.programName = programName;
		
		this.g5Tools = g5tools;
		
		checkFiles();
		findBorderInstructions(programName + ".xml", entryPoint);
	}
	
	public abstract void evaluate(TestVector vector);
	
	private void checkFiles() {
		File programBin = new File(programName);
		if(!programBin.exists()) {
			SystemOutput.exitWithError("Error: file " + programName + " does not exist");
		}
	}
	
	protected synchronized static void addToTraceOutput(TestVector vector, String content) {
		if(traceOutput == null) {
			try {
				GZIPOutputStream outStream = new GZIPOutputStream(
						new FileOutputStream(traceFileName));
				traceOutput = new PrintStream(outStream);
				
				// Ensure trace output stream is closed at end of execution
				Runtime.getRuntime().addShutdownHook(new Thread() {
					public void run() { closeTraceOutput(); }
				});
			} catch(Exception e) {
				SystemOutput.exitWithError("Error creating trace output file");
			}
		}
		
		if(timesOutput == null) {
			try {
				FileOutputStream timeOutStream = 
						new FileOutputStream(timesFileName);
				timesOutput = new PrintStream(timeOutStream);
				
				// Ensure trace output stream is closed at end of execution
				Runtime.getRuntime().addShutdownHook(new Thread() {
					public void run() { closeTimesOutput(); }
				});
			} catch(Exception e) {
				SystemOutput.exitWithError("Error creating times output file");
			}
		}
		
		try {
			traceOutput.print(content);
			timesOutput.println(vector.getTime());
		} catch (Exception e) {
			SystemOutput.exitWithError("Error wrting to trace output");
		}
	}
	
	private void findBorderInstructions(String programXMLFile, String entryPoint)
	{
		boolean entryFound = false;
		
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
	
	private static void closeTraceOutput() {
		if(traceOutput != null) {
			traceOutput.close();
		}
	}
	
	private static void closeTimesOutput() {
		if(timesOutput != null) {
			timesOutput.close();
		}
	}

}
