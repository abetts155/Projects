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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import tvgen.util.SystemOutput;
import tvgen.util.TestVector;

public abstract class Gem5CoverageEvaluator extends Gem5Evaluator {

	protected String traceFile;
	protected Set<Integer> basicBlocks;
	
	public Gem5CoverageEvaluator(int threadID, String programName, Gem5Tools g5tools,
			String entryPoint) {
		super(threadID, programName, g5tools, entryPoint);
		traceFile = "m5out/thread" + threadID + "/trace.out";
		
		basicBlocks = new HashSet<Integer>();
		CFGNode root = buildCFGNodes(programName + ".xml", entryPoint);
		addBasicBlocks(root);
	}

	@Override
	public abstract void evaluate(TestVector vector);
	
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
