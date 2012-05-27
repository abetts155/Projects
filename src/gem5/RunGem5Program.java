package gem5;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import tvgen.util.SystemOutput;

public class RunGem5Program {

	private static Options options;
	private static Option helpOption;
	private static Option debugOption;
	private static Option programOption;
	private static Option entryOption;
	private static Option argsOption;
	private static Option configOption;
	
	private Set<Long> entryBlockInsts;
	
	private Set<Integer> basicBlocks;
	
	public RunGem5Program(String programName, String entryPoint,
			String progArgs, String configFile)
	{
		Gem5Tools g5Tools = new Gem5Tools(programName, configFile);
		
		entryBlockInsts = new HashSet<Long>();
		findBorderInstructions(programName + ".xml", entryPoint);
		basicBlocks = new HashSet<Integer>();
		CFGNode root = buildCFGNodes(programName + ".xml", entryPoint);
		addBasicBlocks(root);
		
		String traceFile = "m5out/trace.out";
		double totaltime = g5Tools.runGem5(progArgs, "m5out", "trace.out");
		double entrytime = g5Tools.getInstructionTimeDiff(
				entryBlockInsts, traceFile);
		
		Map<Integer,Integer> blockCounts = g5Tools.getBlockCoverageCount(traceFile);
		
		Set<Integer> blocksCovered = blockCounts.keySet();
		blocksCovered.retainAll(basicBlocks);
		
		int bbCount = 0;
		for (Integer bbId : blocksCovered)
		{
			bbCount += blockCounts.get(bbId);
		}
		
		double bbCov = (double)blocksCovered.size() / (double)basicBlocks.size();
		
		SystemOutput.printMessage("Time from entry point = " + entrytime);
		SystemOutput.printMessage("Basic block coverage = " + bbCov);
		SystemOutput.printMessage("Total basic block execution count = " + bbCount);
		SystemOutput.printMessage("Total execution time = " + totaltime);
	}
	
	private static void addOptions ()
	{
		options = new Options ();
		
		helpOption = new Option ("h", "help", false, "Display this message.");
		options.addOption (helpOption);
		
		debugOption = new Option ("d", "debug", false, "Debug mode.");
		options.addOption (debugOption);
		
		programOption = new Option ("p", "program", true,
				"The name of the program binary to be tested");
		programOption.setRequired (true);
		options.addOption (programOption);
		
		entryOption = new Option ("r", "entry-point", true,
				"The name of the function to use as the entry point of the program");
		entryOption.setRequired (true);
		options.addOption (entryOption);
		
		argsOption = new Option ("a", "arguments", true,
				"The arguments to pass to the program");
		argsOption.setRequired (true);
		options.addOption (argsOption);
		
		configOption = new Option ("c", "config-file", true,
				"The config file specifying the architecture options for gem5");
		configOption.setRequired (true);
		options.addOption (configOption);
	}
	
	private static CommandLine parseCommandLine (String[] args)
	{
		final String toolName = "run-gem5.jar";
		CommandLineParser parser = new GnuParser ();
		HelpFormatter formatter = new HelpFormatter ();
		formatter.setWidth (80);
		CommandLine line = null;
		try
		{
			line = parser.parse (options, args);

			if (line.hasOption (helpOption.getOpt ()))
			{
				formatter.printHelp (toolName, options);
				System.exit (1);
			} 
			else
			{
				if (line.hasOption (debugOption.getOpt ()))
					SystemOutput.debugMode = true;
			}
		}
		catch (ParseException e)
		{
			System.out.println (e.getMessage ());
			formatter.printHelp (toolName, options);
			System.exit (1);
		}
		
		return line;
	}
	
	/**
	 * Run a supplied gem5 program and extract information about the execution
	 * @param args
	 */
	public static void main(String[] args) {
		addOptions();
		CommandLine line = parseCommandLine(args);
		
		String programName = line.getOptionValue(programOption.getOpt());
		String entryPoint = line.getOptionValue(entryOption.getOpt());
		String configFile = line.getOptionValue(configOption.getOpt());
		String arguments = line.getOptionValue(argsOption.getOpt());
		
		@SuppressWarnings("unused")
		RunGem5Program runGem5 = new RunGem5Program(programName, entryPoint,
				arguments, configFile);
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
	
	private void addBasicBlocks(CFGNode rootNode)
	{
		basicBlocks.addAll(rootNode.basicBlocks);
		for(CFGNode node : rootNode.successors)
		{
			addBasicBlocks(node);
		}
	}
	
	private CFGNode buildCFGNodes(String programXMLFile, String entryPoint)
	{
		File xmlFile = new File(programXMLFile);
		if(!xmlFile.exists())
		{
			SystemOutput.exitWithError("Error: XML file " + programXMLFile +
					" does not exist");
		}
		
		Map<Integer,CFGNode> idToCFGNode = new HashMap<Integer,CFGNode>();
		CFGNode root = null;
		
		try
		{
			SystemOutput.debugMessage("Extracting program info from " + programXMLFile);
			
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
					CFGNode newNode = buildCFGNode((Element)cfgNode);
					idToCFGNode.put(newNode.Id, newNode);
					if(newNode.name.equals(entryPoint))
					{
						root = newNode;
					}
				}
			}
			
			// Find successors for cfg Nodes
			for(int i = 0; i < cfgNodes.getLength(); i++)
			{
				Node cfgNode = cfgNodes.item(i);
				if(cfgNode instanceof Element)
				{
					findSuccessors((Element)cfgNode, idToCFGNode);
				}
			}
		} 
		catch (Exception e)
		{
			SystemOutput.printMessage(e.getStackTrace().toString());
			SystemOutput.exitWithError("Error parsing programXMLFile: " + e.getMessage());
		}
		
		if(root == null)
		{
			SystemOutput.exitWithError("Error could not find entry point " + entryPoint);
		}
		return root;
	}
	
	private CFGNode buildCFGNode(Element cfgElement)
	{
		String name = cfgElement.getAttribute("name");
		int Id = Integer.parseInt(cfgElement.getAttribute("id"));
		
		CFGNode newNode = new CFGNode(name, Id);
		
		NodeList bbNodes = cfgElement.getElementsByTagName("bb");
		for(int j = 0; j < bbNodes.getLength(); j++)
		{
			Node bbNode = bbNodes.item(j);
			if(bbNode instanceof Element)
			{
				String bbId = ((Element)bbNode).getAttribute("id");
				newNode.addBasicBlock(Integer.parseInt(bbId));
			}
		}
		
		return newNode;
	}
	
	private void findSuccessors(Element cfgElement, Map<Integer,CFGNode> idToCFGNode)
	{
		int Id = Integer.parseInt(cfgElement.getAttribute("id"));
		CFGNode node = idToCFGNode.get(Id);
		
		NodeList bbNodes = cfgElement.getElementsByTagName("bb");
		for(int j = 0; j < bbNodes.getLength(); j++)
		{
			Node bbNode = bbNodes.item(j);
			if(bbNode instanceof Element)
			{
				NodeList succNodes = ((Element)bbNode).getElementsByTagName("succ");
				for(int i = 0; i < succNodes.getLength(); i++)
				{
					Node succNode = succNodes.item(i);
					if(succNode instanceof Element)
					{
						NodeList linkNodes = ((Element)succNode).getElementsByTagName("link");
						for(int k = 0; k < linkNodes.getLength(); k++)
						{
							Node linkNode = linkNodes.item(k);
							if(linkNode instanceof Element)
							{
								Element linkElement = (Element)linkNode;
								if(linkElement.getAttribute("type").equals("call"))
								{
									String calledId = linkElement.getAttribute("cfg");
									int calledInt = Integer.parseInt(calledId);
									node.addSuccessor(idToCFGNode.get(calledInt));
								}
							}
						}
					}
				}
			}
		}
	}
	
	private class CFGNode
	{
		private String name;
		private int Id;
		private List<Integer> basicBlocks;
		private List<CFGNode> successors;
		
		public CFGNode(String name, int id)
		{
			this.name = name;
			this.Id = id;
			basicBlocks = new LinkedList<Integer>();
			successors = new LinkedList<CFGNode>();
		}
		
		public void addBasicBlock(int bbId)
		{
			basicBlocks.add(bbId);
		}
		
		public void addSuccessor(CFGNode succ)
		{
			successors.add(succ);
		}
	}

}
