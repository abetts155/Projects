package adam.betts.outputs;

import java.io.File;
import java.util.Iterator;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import adam.betts.edges.Edge;
import adam.betts.edges.FlowEdge;
import adam.betts.graphs.CallGraph;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.instructions.Instruction;
import adam.betts.programs.Program;
import adam.betts.programs.Subprogram;
import adam.betts.utilities.Debug;
import adam.betts.utilities.Enums.BranchType;
import adam.betts.vertices.Vertex;

public class WriteProgram
{

    private Program program;
    protected final String fileName;

    public WriteProgram (Program program, String fileName)
    {
        this.program = program;
        this.fileName = fileName;

        try
        {
            writeXML();
        }
        catch (ParserConfigurationException e)
        {
            Debug.errorMessage(getClass(), e.getMessage());
        }
        catch (TransformerException e)
        {
            Debug.errorMessage(getClass(), e.getMessage());
        }
    }

    private void writeXML () throws ParserConfigurationException,
            TransformerException
    {
        DocumentBuilderFactory docFactory = DocumentBuilderFactory
                .newInstance();
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

        Document doc = docBuilder.newDocument();
        Element rootElement = doc.createElement("program");
        doc.appendChild(rootElement);

        for (Subprogram subprg : program)
        {
            Element cfgElem = doc.createElement("cfg");
            rootElement.appendChild(cfgElem);

            Attr cfgAttr1 = doc.createAttribute("id");
            cfgAttr1.setValue(Integer.toString(subprg.getSubprogramID()));
            cfgElem.setAttributeNode(cfgAttr1);

            Attr cfgAttr2 = doc.createAttribute("name");
            cfgAttr2.setValue(subprg.getSubprogramName());
            cfgElem.setAttributeNode(cfgAttr2);

            ControlFlowGraph cfg = subprg.getCFG();
            for (Vertex v : cfg)
            {
                Element bbElem = doc.createElement("bb");
                cfgElem.appendChild(bbElem);

                Attr bbAttr1 = doc.createAttribute("id");
                bbAttr1.setValue(Integer.toString(v.getVertexID()));
                bbElem.setAttributeNode(bbAttr1);

                Iterator <Instruction> instrIt = cfg.getBasicBlock(
                        v.getVertexID()).instructionIterator();
                while (instrIt.hasNext())
                {
                    Instruction instr = instrIt.next();
                    Element instElem = doc.createElement("inst");
                    bbElem.appendChild(instElem);

                    Attr instrAttr1 = doc.createAttribute("addr");
                    instrAttr1.setValue("0x"
                            + Long.toHexString(instr.getAddress()));
                    instElem.setAttributeNode(instrAttr1);

                    Attr instrAttr2 = doc.createAttribute("instr");
                    instrAttr2.setValue(instr.getInstruction());
                    instElem.setAttributeNode(instrAttr2);
                }

                Element succElem = doc.createElement("succ");
                bbElem.appendChild(succElem);
                Iterator <Edge> succIt = v.successorIterator();
                while (succIt.hasNext())
                {
                    FlowEdge succEdge = (FlowEdge) succIt.next();
                    if (succEdge.getVertexID() != cfg.getEntryID())
                    {
                        Element linkElem = doc.createElement("link");
                        succElem.appendChild(linkElem);

                        Attr linkAttr1 = doc.createAttribute("type");
                        linkAttr1.setValue(succEdge.getBranchType().toString()
                                .toLowerCase());
                        linkElem.setAttributeNode(linkAttr1);

                        Attr linkAttr2 = doc.createAttribute("cfg");
                        linkAttr2.setValue(Integer.toString(subprg
                                .getSubprogramID()));
                        linkElem.setAttributeNode(linkAttr2);

                        Attr linkAttr3 = doc.createAttribute("bb");
                        linkAttr3.setValue(Integer.toString(succEdge
                                .getVertexID()));
                        linkElem.setAttributeNode(linkAttr3);
                    }
                }

                int calleeID = program.getCallGraph().isCallSite(
                        subprg.getSubprogramID(), v.getVertexID());
                if (calleeID != Vertex.DUMMY_VERTEX_ID)
                {
                    Element linkElem = doc.createElement("link");
                    succElem.appendChild(linkElem);

                    Attr linkAttr1 = doc.createAttribute("type");
                    linkAttr1
                            .setValue(BranchType.CALL.toString().toLowerCase());
                    linkElem.setAttributeNode(linkAttr1);

                    Attr linkAttr2 = doc.createAttribute("cfg");
                    linkAttr2.setValue(Integer.toString(calleeID));
                    linkElem.setAttributeNode(linkAttr2);

                    Attr linkAttr3 = doc.createAttribute("bb");
                    linkAttr3.setValue(Integer.toString(program
                            .getSubprogram(calleeID).getCFG().getEntryID()));
                    linkElem.setAttributeNode(linkAttr3);
                }
            }
        }

        TransformerFactory transformerFactory = TransformerFactory
                .newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "2");
        DOMSource source = new DOMSource(doc);
        StreamResult result = new StreamResult(new File(fileName));
        transformer.transform(source, result);
    }

}
