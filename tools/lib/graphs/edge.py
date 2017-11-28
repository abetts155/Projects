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
        self._call_sites = call_sites

    @property
    def call_sites(self):
        return self._call_sites