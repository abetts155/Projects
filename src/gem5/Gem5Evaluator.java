package gem5;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;
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
	
	protected Set<Long> entryBlockInsts;
	
	private static PrintStream traceOutput = null;
	private static PrintStream timesOutput = null;
	private static String traceFileName = "trace.BASIC_BLOCK.GA.gz";
	private static String timesFileName = "vector-times.txt";
	private static String outputFolder = ".";
	
	public Gem5Evaluator(int threadID, String programName, Gem5Tools g5tools,
			String entryPoint) {
		super(threadID);
		
		this.programName = programName;
		
		this.g5Tools = g5tools;
		
		checkFiles();
		
		entryBlockInsts = new HashSet<Long>();
		findEntryInstructions(programName + ".xml", entryPoint);
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
						new FileOutputStream(outputFolder + "/" + traceFileName));
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
						new FileOutputStream(outputFolder + "/" + timesFileName);
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
	
	private void findEntryInstructions(String programXMLFile, String entryPoint)
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
					// Find correct cfg block
					if(((Element)cfgNode).getAttribute("name").equals(entryPoint))
					{
						entryFound = true;
						NodeList bbNodes = ((Element)cfgNode).getElementsByTagName("bb");
						
						// Find all bb instructions
						for(int j = 0; j < bbNodes.getLength(); j++)
						{
							Node bbNode = bbNodes.item(j);
							if(bbNode instanceof Element)
							{
								NodeList bbInsts = ((Element)bbNode).getElementsByTagName(
										"inst");
								for(int k = 0; k < bbNodes.getLength(); k++)
								{
									Node InstNode = bbInsts.item(k);
									if(InstNode instanceof Element)
									{
										String addr = ((Element)InstNode).getAttribute("addr");
										entryBlockInsts.add(Long.parseLong(
												addr.substring(2), 16));
									}
								}
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
	
	public static void setOutputFolder(String folderName)
	{
		outputFolder = folderName;
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
