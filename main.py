from Translator import Translator
from DotHandler import DotHandler
import argparse
import subprocess
import os.path

## ARG PARSER ##
args_parser = argparse.ArgumentParser(description='LTLf2DFA is a tool that processes an LTLf formula (future or past)' +
                                                  ' and generates the corresponding DFA using MONA.')
args_parser.add_argument('formula', type=str, help='String containing the LTLf formula -- MANDATORY')
args_parser.add_argument('-d', '--declare', action='store_true', help='Compute DECLARE assumption for the formula. -- OPTIONAL')
################

## READ FORMULA ##
params = vars(args_parser.parse_args())
if not params['formula']:
    print('Formula argument required!')
    exit()
else:
    formula = params['formula']
#################

## TRANSLATOR ##
translator = Translator(formula)
translator.formula_parser()
translator.translate()
translator.createMonafile(params['declare'])
################

## CALL MONA TOOL ##
if os.path.isfile("mona") and os.access("mona", os.X_OK):  # check if mona exists and if it's executable
    subprocess.call('mona -u -gw automa.mona > automa.dot', shell=True)
else:
    print('[ERROR] - MONA tool does not exist or it is not executable...')
    exit()
####################

## POST-PROCESS MONA AUTOMATON ##
dot_handler = DotHandler('automa.dot')
dot_handler.modify_dot()
dot_handler.output_dot()
#################################
