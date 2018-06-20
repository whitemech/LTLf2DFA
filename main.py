from Translator import Translator
import argparse
import subprocess
import os.path

## ARG PARSER ##
args_parser = argparse.ArgumentParser(description = 'LTLf2DFA is a tool that processes an LTLf formula (future or past)'+
                                                    ' and generates the corresponding DFA using MONA.')
args_parser.add_argument('path_file_formula', help = 'Path to the file containing the LTLf formula -- MANDATORY')
################

## READ FORMULA ##
params = vars(args_parser.parse_args())
if not params['path_file_formula']:
	print('File argument required!')
	exit()
else:
    with open(params['path_file_formula'], 'r') as input:
        formula = input.readline()
        input.close()
#################

## TRANSLATOR ##
translator = Translator(formula)
translator.formula_parser()
translator.translate()
translator.createMonafile()
################

## CALL MONA TOOL ##
if os.path.isfile("/mona") and os.access("/mona", os.X_OK): #check if mona exists and if it's executable
    subprocess.call('./mona -u -gw automa.mona > automa.dot', shell=True)
else:
    print('MONA tool does not exists or it is not executable...')
    exit()
####################