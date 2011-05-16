package adam.betts.programs;

import java.util.HashMap;

import adam.betts.graphs.CFGStar;
import adam.betts.graphs.ControlFlowGraph;
import adam.betts.graphs.IpointGraph;
import adam.betts.graphs.trees.SyntaxTree;
import adam.betts.utilities.Enums.IProfile;

public class Subprogram
{
	private int subprogramID;
	private String subprogramName;
	private ControlFlowGraph cfg;
	private SyntaxTree stree;
	private HashMap<IProfile, CFGStar> cfgStars = new HashMap<IProfile, CFGStar> ();
	private HashMap<IProfile, IpointGraph> ipgs = new HashMap<IProfile, IpointGraph> ();

	public Subprogram (int subprogramID, String subprogramName)
	{
		this.subprogramID = subprogramID;
		this.subprogramName = subprogramName;
	}

	public final int getSubprogramID ()
	{
		return subprogramID;
	}

	public final String getSubprogramName ()
	{
		return subprogramName;
	}

	public final void setCFG (ControlFlowGraph cfg)
	{
		this.cfg = cfg;
	}

	public final ControlFlowGraph getCFG ()
	{
		if (cfg == null)
		{
			cfg = new ControlFlowGraph ();
		}
		return cfg;
	}

	public final void setSyntaxTree (SyntaxTree stree)
	{
		this.stree = stree;
	}

	public final SyntaxTree getSyntaxTree ()
	{
		return stree;
	}

	public final void buildCFGStar (IProfile iprofile)
	{
		cfgStars.put (iprofile, new CFGStar (cfg, iprofile, subprogramName));
	}

	public final CFGStar getCFGStar (IProfile iprofile)
	{
		return cfgStars.get (iprofile);
	}

	public final void setIPG (IProfile iprofile, IpointGraph ipg)
	{
		ipgs.put (iprofile, ipg);
	}

	public final IpointGraph getIPG (IProfile iprofile)
	{
		return ipgs.get (iprofile);
	}
}
