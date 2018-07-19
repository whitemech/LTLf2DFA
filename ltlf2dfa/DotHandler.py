import pydot
import os

class DotHandler:

    def __init__(self, path):
        self.dot_path = path
        self.new_digraph = None

    def modify_dot(self):
        if os.path.isfile(self.dot_path):
            dot_graph = pydot.graph_from_dot_file(self.dot_path)
            graph = dot_graph[0]
            graph.del_node("0")
            graph.del_edge("init")
            graph.del_edge(["init", "0"])
            graph.del_edge(["0", "1"])
            graph.add_edge(pydot.Edge("init","1"))
            self.new_digraph = graph
        else:
            print('[ERROR] - No file DOT exists')
            exit()

    def delete_intermediate_automaton(self):
        if os.path.isfile("./inter-automa.dot"):
            os.remove("./inter-automa.dot")
            return True
        else:
            return False

    def output_dot(self):
        try:
            if self.delete_intermediate_automaton():
                with open("./automa.dot", 'w+') as f:
                    f.write(self.new_digraph.to_string())
                    f.close()
            else:
                raise IOError('[ERROR] - Something wrong occurred in the elimination of inter-automa.dot file.')
        except IOError:
            print('[ERROR] - Problem with the opening of the file automa.dot!')