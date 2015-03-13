import directed_graphs

class Program:
    def __init__(self):
        self.cfgs = {}
        self.callg = directed_graphs.CallGraph()
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg 