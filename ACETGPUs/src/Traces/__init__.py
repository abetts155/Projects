
class Warp ():
    def __init__(self, multiprocessorID, warpID):
        self.__multiprocessorID = multiprocessorID
        self.__warpID = warpID
        self.__trace = []
                
    def getWarpID (self):
        return self.__warpID
    
    def getMultiprocessorID (self):
        return self.__multiprocessorID
    
    def appendToTrace (self, traceTuple):
        self.__trace.append(traceTuple)
    
    def getTrace (self):
        return self.__trace
    
class TraceParser ():
    def __init__(self, warp):
        self.__warp = warp
    