import unittest

from tools.lib import vertices



class SanityChecks(unittest.TestCase):
    
    def setUp(self):
        self.the_vertex = vertices.Vertex(1)
    
    
    def test_addition_and_removal_of_predecessor(self):
        pred_id = 2
        self.the_vertex.add_predecessor(pred_id, edge_id=0)
        self.the_vertex.remove_predecessor(pred_id)
        self.assertEqual(self.the_vertex.number_of_predecessors(), 0)
    
        
    def test_addition_of_predecessors(self):
        predecessor_ids = [2, 3, 4, 5]
        for pred_id in predecessor_ids:
            self.the_vertex.add_predecessor(pred_id, edge_id=0)
        for pred_id, _ in self.the_vertex.predecessors_iterator():
            self.assertTrue(pred_id in predecessor_ids) 
                
        
    def test_addition_and_removal_of_successor(self):
        succ_id = 2
        self.the_vertex.add_successor(succ_id, edge_id=0)
        self.the_vertex.remove_successor(succ_id)
        self.assertEqual(self.the_vertex.number_of_successors(), 0)
    
        
    def test_addition_of_succcessors(self):
        successor_ids = [2, 3, 4, 5]
        for succ_id in successor_ids:
            self.the_vertex.add_successor(succ_id, edge_id=0)
        for succ_id, _ in self.the_vertex.successors_iterator():
            self.assertTrue(succ_id in successor_ids)
            

class BadInput(unittest.TestCase):
    
    def setUp(self):
        self.the_vertex = vertices.Vertex(1)
        
    
    def test_double_addition_of_predecessor(self):
        pred_id = 2
        self.the_vertex.add_predecessor(pred_id, edge_id=0)
        self.assertRaises(vertices.DuplicateEdgeError, 
                          self.the_vertex.add_predecessor, 
                          pred_id, 
                          edge_id=0)
        
    def test_removal_and_retrieval_of_missing_predecessor(self):
        pred_id = 2
        self.assertRaises(KeyError, 
                          self.the_vertex.remove_predecessor, 
                          pred_id)
        self.assertRaises(KeyError, 
                          self.the_vertex.get_predecessor_edge, 
                          pred_id)
        
        
    def test_double_addition_of_successor(self):
        succ_id = 2
        self.the_vertex.add_successor(succ_id, edge_id=0)
        self.assertRaises(vertices.DuplicateEdgeError, 
                          self.the_vertex.add_successor, 
                          succ_id, 
                          edge_id=0)
    
    
    def test_removal_and_retrieval_of_missing_successor(self):
        succ_id = 2
        self.assertRaises(KeyError, 
                          self.the_vertex.remove_successor, 
                          succ_id)
        self.assertRaises(KeyError, 
                          self.the_vertex.get_successor_edge, 
                          succ_id)
        

if __name__ == "__main__":
    unittest.main()
    