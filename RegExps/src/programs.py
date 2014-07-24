import trees
import udraw
import regular_expressions
    
class Program():
    def __init__(self):
        self.cfgs = {}
        self.lnts = {}
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg
        udraw.make_file(cfg, "%s.cfg" % (cfg.name))
        
    def create_LNTs(self):
        for name, cfg in self.cfgs.iteritems():
            self.lnts[name] = trees.LoopNests(cfg, cfg.get_entryID())
            udraw.make_file(self.lnts[name], "%s.lnt" % (name)) 
            
    def create_path_expressions(self):
        for name, cfg in self.cfgs.iteritems():
            regular_expressions.CFGPathExpression(cfg, self.lnts[name])        