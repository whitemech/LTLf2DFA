from Translator import Translator
import argparse

args_parser = argparse.ArgumentParser(description = 'LTLf2DFA is a tool that processes an LTLf formula (future or past)'+
                                                    ' and generates the corresponding DFA using MONA.')
args_parser.add_argument('path_file_formula', help = 'Path to the file containing the LTLf formula -- MANDATORY')

params = vars(args_parser.parse_args())
if not params['path_file_formula']:
	print('File argument required!')
	exit()
else:
    with open(params['path_file_formula'], 'r') as input:
        formula = input.readline()   

translator = Translator(formula)
translator.formula_parser()
translator.translate()
translator.createMonafile("automa")

