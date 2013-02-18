verbose = False
debug   = 0

def debugMessage (string, debugLevel=1):
    if debug > 0 and debugLevel <= debug:
        print(string)

def verboseMessage (string):
    if verbose:
        print(string)
        
def warningMessage(string):
    print("*** WARNING: %s ***" % string)

def exitMessage(string):
    print(string)
    exit(1)
