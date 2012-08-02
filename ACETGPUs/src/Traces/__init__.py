
class Warp ():
    def __init__(self, multiprocessorID, warpID):
        self.multiprocessorID = multiprocessorID
        self.warpID = warpID
        self.trace = []
                
    def getWarpID (self):
        return self.warpID
    
    def getMultiprocessorID (self):
        return self.multiprocessorID
    
    def appendToTrace (self, traceTuple):
        self.trace.append(traceTuple)
    
    def getTrace (self):
        return self.trace
    
class TraceParser ():
    def __init__(self, warp):
        self.warp = warp
    