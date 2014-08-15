import super_block_graphs
import udraw
import debug
import instrumentation
    
class Program():
    def __init__(self):
        self.cfgs             = {}
        self.super_block_cfgs = {}
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg
        udraw.make_file(cfg, "%s.cfg" % (cfg.name))
        
    def create_LNTs(self):
        for name, cfg in self.cfgs.iteritems():
            udraw.make_file(cfg.get_loop_nesting_tree(), "%s.lnt" % (name))
            
    def create_super_block_CFGs(self):
        for name, cfg in self.cfgs.iteritems():
            debug.debug_message("Analysing CFG %s" % name, __name__, 5)
            self.super_block_cfgs[name] = super_block_graphs.SuperBlockCFG(cfg)
            udraw.make_file(self.super_block_cfgs[name], "%s.superg" % (name))
    
    def instrument(self):      
        for function_name in self.cfgs.keys():
            instrumentation.Instrumenter(self.cfgs[function_name],
                                         self.cfgs[function_name].get_loop_nesting_tree(),
                                         self.super_block_cfgs[function_name])
                