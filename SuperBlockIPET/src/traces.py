import config
import debug
import os
import random

trace_delimiter = "%s%s%s" % ('<', "-" * 30, '>')

class GenerateExecutionTraces:    
    def __init__ (self, program):
        self.program = program
        self.covered = set()
        self.do_it()
                
    def do_it(self):
        filename = config.Arguments.basepath + os.sep + config.Arguments.basename + ".traces"
        with open(filename, 'w') as self.the_file:
            for trace in xrange(1, config.Arguments.generate_traces+1):
                debug.debug_message("Generating trace #%d" % trace, __name__, 10)
                self.the_file.write("%s\n" % trace_delimiter)
                self.generate_a_trace() 
                self.the_file.write("\n")
    
    def generate_a_trace(self):
        root_callv    = self.program.callg.getVertex(self.program.callg.rootID)
        current_callv = root_callv 
        current_CFG   = self.program.cfgs[root_callv.name]
        currentv      = current_CFG.getVertex(current_CFG.get_entryID())
        call_stack    = []
        while True:
            self.the_file.write("%d " % currentv.vertexID)
            if currentv.vertexID == current_CFG.get_exitID():
                if current_callv == root_callv:
                    # End of the program reached
                    break
                else:
                    # End of function call
                    debug.debug_message("Returning from %s" % current_callv.name, __name__, 10)
                    current_callv, current_CFG, currentv = call_stack.pop()
                    # Go past the call site
                    currentv = self.choose_intra_procedural_successor(current_CFG, currentv)
            elif current_CFG.is_call_site(currentv.vertexID):
                call_stack.append((current_callv, current_CFG, currentv))
                succID        = current_callv.get_successor_with_call_site(currentv.vertexID)
                current_callv = self.program.callg.getVertex(succID)
                debug.debug_message("Calling %s" % current_callv.name, __name__, 10)
                current_CFG   = self.program.cfgs[current_callv.name]
                currentv      = current_CFG.getVertex(current_CFG.get_entryID())
            else:
                currentv = self.choose_intra_procedural_successor(current_CFG, currentv)
    
    def choose_intra_procedural_successor(self, current_CFG, currentv):
        # Try to iterate if it is possible
        lnt = current_CFG.get_LNT()
        if lnt.is_loop_tail(currentv.vertexID):
            headerv = lnt.getVertex(lnt.getVertex(currentv.vertexID).parentID)
            if random.uniform(0.0, 1.0) < 0.75:
                self.covered.add((currentv.vertexID, headerv.headerID))
                return current_CFG.getVertex(headerv.headerID)
        # Try to explore as many edges as possible
        for succID in currentv.successors.keys():
            if (currentv.vertexID, succID) not in self.covered:
                self.covered.add((currentv.vertexID, succID))
                return current_CFG.getVertex(succID)
        # If all else fails, return an arbitrary successor
        idx    = random.randint(0, currentv.number_of_successors() - 1)
        succID = currentv.successors.keys()[idx]
        self.covered.add((currentv.vertexID, succID))
        return current_CFG.getVertex(succID)
        