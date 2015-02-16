class Program:
    def __init__(self):
        self.cfgs = {}
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg 