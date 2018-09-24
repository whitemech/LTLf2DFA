from ltlf2dfa.Parser import MyParser
import itertools as it
import subprocess
import os, sys, re
import pkg_resources

class Translator:

    def __init__(self, formula):
        self.headerMona = "m2l-str;\n"
        self.alphabet = []
        self.formula_to_be_parsed = formula
        self.formulaType = self.search_mixed_formula()
        self.parsed_formula = None
        self.translated_formula = None

    def formula_parser(self):
        if self.formulaType in {1,2,3}:
            self.compute_alphabet()
            parser = MyParser()
            self.parsed_formula = parser(self.formula_to_be_parsed)
        else: raise ValueError('Ooops! You typed a formula with mixed past/future operators')

    def tuple_to_string(self):
        return '_'.join(str(self.formula_to_be_parsed))

    def search_mixed_formula(self):
        '''
        search_mixed_formula() possible outputs:
        0: formula is mixed
        1: formula is only future
        2: formula is only past
        3: formula is only present
        '''
        formula_to_check_str = self.tuple_to_string()
        separated_formula = formula_to_check_str.split('_')

        past_operators = []
        future_operators = []
        for character in separated_formula:
            if character.isupper():
                if character in {'X','F','G','U', 'W', 'R'}: future_operators.append(character)
                elif character in {'Y','O','H','S'}: past_operators.append(character)
                else: continue
            else: continue

        if not past_operators and future_operators:
            return 1
        elif past_operators and not future_operators:
            return 2
        elif not past_operators and not future_operators:
            return 3
        else:
            return 0

    def rem_duplicates_order(self, seq):
        seen = set()
        seen_add = seen.add
        return [x for x in seq if not (x in seen or seen_add(x))]

    def compute_alphabet(self):

        symbols = re.findall('(?<![a-z])(?!true|false)[_a-z0-9]+', str(self.formula_to_be_parsed))
        _symbols = self.rem_duplicates_order(symbols)
        self.alphabet = [character.upper() for character in _symbols]

    def compute_declare_assumption(self):
        pairs = list(it.combinations(self.alphabet, 2))

        if pairs:
            first_assumption = "~(ex1 y: 0<=y & y<=max($) & ~("
            for symbol in self.alphabet:
                if symbol == self.alphabet[-1]: first_assumption += 'y in '+ symbol +'))'
                else : first_assumption += 'y in '+ symbol +' | '

            second_assumption = "~(ex1 y: 0<=y & y<=max($) & ~("
            for pair in pairs:
                if pair == pairs[-1]: second_assumption += '(y notin '+ pair[0]+' | y notin '+pair[1]+ ')));'
                else: second_assumption += '(y notin '+ pair[0]+' | y notin '+pair[1]+ ') & '

            return first_assumption +' & '+ second_assumption
        else:
            return None

    def translate(self):
        self.translated_formula = translate_bis(self.parsed_formula, var='v_0')+";\n"

    def buildMonaProgram(self, flag_for_declare):
        if not self.alphabet and not self.translated_formula:
            raise ValueError
        else:
            if flag_for_declare:
                if self.compute_declare_assumption() is None:
                    if self.alphabet:
                        return self.headerMona + 'var2 ' + ", ".join(self.alphabet) + ';\n' + self.translated_formula
                    else:
                        return self.headerMona + self.translated_formula
                else: return self.headerMona + 'var2 ' + ", ".join(self.alphabet) + ';\n' + self.translated_formula + self.compute_declare_assumption()
            else:
                if self.alphabet:
                    return self.headerMona + 'var2 ' + ", ".join(self.alphabet) + ';\n' + self.translated_formula
                else:
                    return self.headerMona + self.translated_formula

    def createMonafile(self, flag):
        program = self.buildMonaProgram(flag)
        try:
            with open('./automa.mona', 'w+') as file:
                file.write(program)
                file.close()
        except IOError:
            print('Problem with the opening of the file!')

    def invoke_mona(self, path='./inter-automa'):
        if sys.platform == 'linux':
            package_dir = os.path.dirname(os.path.abspath(__file__))
            mona_path = pkg_resources.resource_filename('ltlf2dfa','mona')
            if os.access(mona_path, os.X_OK):  # check if mona is executable
                try:
                    subprocess.call(package_dir+'/./mona -u -gw ./automa.mona > ' + path + '.dot', shell=True)
                except subprocess.CalledProcessError as e:
                    print(e)
                    exit()
                except OSError as e:
                    print(e)
                    exit()
            else:
                print('[ERROR]: MONA tool is not executable...')
                exit()
        else:
            try:
                subprocess.call('mona -u -gw ./automa.mona > ' + path + '.dot', shell=True)
            except subprocess.CalledProcessError as e:
                print(e)
                exit()
            except OSError as e:
                print(e)
                exit()

def translate_bis(formula_tree, var):
    if type(formula_tree) == tuple:
        #enable this print to see the tree pruning
        # print(self.parsed_formula)
        # print(var)
        if formula_tree[0] == '&':
            # print('computed tree: '+ str(self.parsed_formula))
            if var == 'v_0':
                a = translate_bis(formula_tree[1], '0')
                # a = translate_bis(self.parsed_formula[1], '0')
                b = translate_bis(formula_tree[2], '0')
            else:
                a = translate_bis(formula_tree[1], var)
                b = translate_bis(formula_tree[2], var)
            if a == 'false' or b == 'false':
                return 'false'
            elif a == 'true':
                if b == 'true': return 'true'
                else: return b
            elif b == 'true': return a
            else: return '('+a+' & '+b+')'
        elif formula_tree[0] == '|':
            # print('computed tree: '+ str(self.parsed_formula))
            if var == 'v_0':
                a = translate_bis(formula_tree[1], '0')
                b = translate_bis(formula_tree[2], '0')
            else:
                a = translate_bis(formula_tree[1], var)
                b = translate_bis(formula_tree[2], var)
            if a == 'true' or b == 'true':
                return 'true'
            elif a == 'false':
                if b == 'true': return 'true'
                elif b == 'false': return 'false'
                else: return b
            elif b == 'false': return a
            else: return '('+a+' | '+b+')'
        elif formula_tree[0] == '~':
            # print('computed tree: '+ str(self.parsed_formula))
            if var == 'v_0': a = translate_bis(formula_tree[1], '0')
            else: a = translate_bis(formula_tree[1], var)
            if a == 'true': return 'false'
            elif a == 'false': return 'true'
            else: return '~('+ a +')'
        elif formula_tree[0] == 'X':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            a = translate_bis(formula_tree[1],new_var)
            if var == 'v_0':
                return '('+ 'ex1 '+new_var+': '+ new_var +' = 1 '+ '& '+ a +')'
            else:
                return '('+ 'ex1 '+new_var+': '+ new_var +' = '+ var + ' + 1 '+ '& '+ a +')'
        elif formula_tree[0] == 'U':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            new_new_var = _next(new_var)
            a = translate_bis(formula_tree[2],new_var)
            b = translate_bis(formula_tree[1],new_new_var)

            if var == 'v_0':
                if b == 'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
                elif a ==  'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
                elif a == 'false': return 'false'
                else: return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
            else:
                if b == 'true': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
                elif a ==  'true': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
                elif a == 'false': return 'false'
                else: return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'

        elif formula_tree[0] == 'W':
            new_var = _next(var)
            a = translate_bis(formula_tree[1], new_var)
            if var == 'v_0':
                return '(0 = max($)) | ('+ 'ex1 '+new_var+': '+ new_var +' = 1 '+ '& '+ a +')'
            else:
                return '('+ var +' = max($)) | ('+ 'ex1 '+new_var+': '+ new_var +' = '+ var + ' + 1 '+ '& '+ a +')'

        elif formula_tree[0] == 'R':
            new_var = _next(var)
            new_new_var = _next(new_var)
            a = translate_bis(formula_tree[2],new_new_var)
            b = translate_bis(formula_tree[1],new_var)

            if var == 'v_0':
                if b == 'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' <= '+new_var+' => '+a+' ) |'\
                                        '(all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' <= max($) => '+a+' )'
                elif a ==  'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+b+')'
                elif b == 'false': return '(all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' <= max($) => '+a+' )'
                else: return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ b +' & all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' <= '+new_var+' => '+a+' ) |'\
                            '(all1 '+new_new_var+': 0 <= '+new_new_var+' & '+new_new_var+' <= max($) => '+a+' )'
            else:
                if b == 'true': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' <= '+new_var+' => '+a+' ) |'\
                                        '(all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' <= max($) => '+a+' )'
                elif a ==  'true': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+b+')'
                elif b == 'false': return '(all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' <= max($) => '+a+' )'
                else: return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ b +' & all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' <= '+new_var+' => '+a+' ) |'\
                            '(all1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' <= max($) => '+a+' )'
                                            
        elif formula_tree[0] == 'Y':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            a = translate_bis(formula_tree[1],new_var)
            if var == 'v_0':
                return '('+ 'ex1 '+new_var+': '+ new_var +' = max($) - 1 '+ '& max($) > 0 & '+ a +')'
            else:
                return '('+ 'ex1 '+new_var+': '+ new_var +' = '+ var + ' - 1 '+ '& '+new_var+' > 0 & '+ a +')'
        elif formula_tree[0] == 'S':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            new_new_var = _next(new_var)
            a = translate_bis(formula_tree[2],new_var)
            b = translate_bis(formula_tree[1],new_new_var)

            if var == 'v_0':
                if b == 'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
                elif a ==  'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & all1 '+new_new_var+': '+new_var+' < '+new_new_var+' & '+new_new_var+' <= max($) => '+b+' )'
                elif a == 'false': return 'false'
                else: return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & all1 '+new_new_var+': '+new_var+' < '+new_new_var+' & '+new_new_var+' <= max($) => '+b+' )'
            else:
                if b == 'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
                elif a ==  'true': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= '+var+' & all1 '+new_new_var+': '+new_var+' < '+new_new_var+' & '+new_new_var+' <= '+var+' => '+b+' )'
                elif a == 'false': return 'false'
                else: return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= '+var+' & '+ a +' & all1 '+new_new_var+': '+new_var+' < '+new_new_var+' & '+new_new_var+' <= '+var+' => '+b+' )'
    else:
        # handling non-tuple cases
        if formula_tree == 'true': return 'true'
        elif formula_tree == 'false': return 'false'

        # enable if you want to see recursion
        # print('computed tree: '+ str(self.parsed_formula))

        # BASE CASE OF RECURSION
        else:
            if formula_tree.isalpha():
                if var == 'v_0':
                    return '0 in '+ formula_tree.upper()
                else:
                    return var + ' in ' + formula_tree.upper()
            else:
                return var + ' in ' + formula_tree.upper()

def _next(var):
    if var == '0': return 'v_1'
    else:
        s = var.split('_')
        s[1] = str(int(s[1])+1)
        return '_'.join(s)
