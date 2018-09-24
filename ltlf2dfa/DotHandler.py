from dotpy.parser.parser import MyParser
import os

class DotHandler:

    def __init__(self, path='./inter-automa.dot'):
        self.dot_path = path
        self.new_digraph = None

    def modify_dot(self):
        if os.path.isfile(self.dot_path):
            parser = MyParser()
            with open(self.dot_path, 'r') as f:
                dot = f.read()
                f.close()

            graph = parser(dot)
            if not graph.is_singleton():
                graph.delete_node('0')
                graph.delete_edge('init', '0')
                graph.delete_edge('0', '1')
                graph.add_edge('init', '1')
            else:
                graph.delete_edge('init', '0')
                graph.add_edge('init', '0')
            self.new_digraph = graph
        else:
            print('[ERROR] - No file DOT exists')
            exit()

    def delete_intermediate_automaton(self):
        if os.path.isfile(self.dot_path):
            os.remove(self.dot_path)
            return True
        else:
            return False

    def output_dot(self, result_path='./automa.dot'):
        try:
            if self.delete_intermediate_automaton():
                with open(result_path, 'w+') as f:
                    f.write(str(self.new_digraph))
                    f.close()
            else:
                raise IOError('[ERROR] - Something wrong occurred in the elimination of intermediate automaton.')
        except IOError:
            print('[ERROR] - Problem with the opening of the file %s!' %result_path)