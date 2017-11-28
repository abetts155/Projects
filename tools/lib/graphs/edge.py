class Edge:
    def __init__(self, predecessor, successor):
        self.predecessor = predecessor
        self.successor = successor

    def __str__(self):
        return "({},{})".format(self.predecessor.id, self.successor.id)


class PathEdge(Edge):
    def __init__(self, predecessor, successor):
        Edge.__init__(self, predecessor, successor)
        self.path = []

    def extend(self, path):
        self.path.extend(path)


class CallGraphEdge(Edge):
    def __init__(self, predecessor, successor, call_sites):
        Edge.__init__(self, predecessor, successor)
        self.call_sites = call_sites