import unittest

from programs import vertices
from programs import directed_graphs



class SanityChecks(unittest.TestCase):
    
    def setUp(self):
        self.the_graph = directed_graphs.DirectedGraph()


    def test_addition_and_removal_of_vertex(self):
        vertex_id_one   = 1
        vertex_id_two   = 2
        vertex_id_three = 3
        self.the_graph.add_vertex(vertices.Vertex(vertex_id_one))
        self.the_graph.add_vertex(vertices.Vertex(vertex_id_two))
        self.the_graph.add_vertex(vertices.Vertex(vertex_id_three))
        self.the_graph.add_edge(vertex_id_one, vertex_id_two, 1)
        self.the_graph.add_edge(vertex_id_two, vertex_id_three, 1)
        self.the_graph.add_edge(vertex_id_one, vertex_id_three, 1)
        self.the_graph.remove_vertex(vertex_id_two)
        self.assertEqual(self.the_graph.number_of_vertices(), 2)
        self.assertEqual(self.the_graph.number_of_edges(), 1)


if __name__ == "__main__":
    unittest.main()