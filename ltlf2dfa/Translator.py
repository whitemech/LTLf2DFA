from ltlf2dfa.Parser import MyParser
import itertools as it
from subprocess import PIPE, Popen, TimeoutExpired
import os
import re
import signal
from sympy import symbols, And, Not, Or, simplify

package_dir = os.path.dirname(os.path.abspath(__file__))

UNSAT_DOT = '''digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle];
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="true"];
}'''


def get_value(text, regex, value_type=float):
    """dump a value from a file based on a regex passed in."""
    # Get the text of the time
    pattern = re.compile(regex, re.MULTILINE)
    results = pattern.search(text)
    if results:
        return value_type(results.group(1))
    else:
        print("Could not find the value, {}, in the text provided".format(regex))
        return value_type(0.0)


def ter2symb(ap, ternary):
    """translate ternary output to symbolic."""
    expr = And()
    i = 0
    for value in ternary:
        if value == 'X':
            continue
        elif value == '1':
            expr = And(expr, ap[i] if isinstance(ap, tuple) else ap)
        else:
            assert(value == '0')
            expr = And(expr, Not(ap[i] if isinstance(ap, tuple) else ap))
        i += 1
    return expr


def simp_guard(guards):
    """make a big OR among guards and simplify them."""
    final = Or()
    for g in guards:
        final = Or(final, g)
    return simplify(final)


def parse_mona(mona_output):
    """parse mona output and construct a dot."""
    free_variables = get_value(mona_output, '.*DFA for formula with free variables:[\s]*(.*?)\n.*', str)
    free_variables = symbols(' '.join(x.strip().lower() for x in free_variables.split() if len(x.strip()) > 0))

    # initial_state = get_value(mona_output, '.*Initial state:[\s]*(\d+)\n.*', int)
    accepting_states = get_value(mona_output, '.*Accepting states:[\s]*(.*?)\n.*', str)
    accepting_states = [str(x.strip()) for x in accepting_states.split() if len(x.strip()) > 0]
    # num_states = get_value(mona_output, '.*Automaton has[\s]*(\d+)[\s]states.*', int) - 1

    dot = '''digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];\n'''
    dot += " node [shape = doublecircle]; {};\n".format('; '.join(accepting_states))
    dot += ''' node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;'''

    dot_trans = dict()  # maps each couple (src, dst) to a list of guards
    for line in mona_output.splitlines():
        if line.startswith("State "):
            orig_state = get_value(line, '.*State[\s]*(\d+):\s.*', int)
            guard = get_value(line, '.*:[\s](.*?)[\s]->.*', str)
            guard = ter2symb(free_variables, guard)
            dest_state = get_value(line, '.*state[\s]*(\d+)[\s]*.*', int)
            if orig_state:
                if (orig_state, dest_state) in dot_trans.keys():
                    dot_trans[(orig_state, dest_state)].append(guard)
                else:
                    dot_trans[(orig_state, dest_state)] = [guard]

    for c, guards in dot_trans.items():
        simplified_guard = simp_guard(guards)
        dot += " {} -> {} [label=\"{}\"];\n".format(c[0], c[1], str(simplified_guard).lower())

    dot += "}"
    return dot


class Translator:
    """The Translator class translates the LTLf/PLTLf formula into its corresponding DFA."""

    def __init__(self, formula):
        self.headerMona = "m2l-str;\n"
        self.alphabet = []
        self.formula_to_be_parsed = formula
        self.formulaType = self.search_mixed_formula()
        self.parsed_formula = None
        self.translated_formula = None

    def formula_parser(self):
        if self.formulaType in {1, 2, 3}:
            self.compute_alphabet()
            parser = MyParser()
            self.parsed_formula = parser(self.formula_to_be_parsed)
        else:
            raise ValueError('Ooops! You typed a formula with mixed past/future operators')

    def tuple_to_string(self):
        return '_'.join(str(self.formula_to_be_parsed))

    def search_mixed_formula(self):
        """
        search_mixed_formula() possible outputs:
        0: formula is mixed
        1: formula is only future
        2: formula is only past
        3: formula is only present
        """
        formula_to_check_str = self.tuple_to_string()
        separated_formula = formula_to_check_str.split('_')

        past_operators = []
        future_operators = []
        for character in separated_formula:
            if character.isupper():
                if character in {'X', 'F', 'G', 'U', 'W', 'R'}:
                    future_operators.append(character)
                elif character in {'Y', 'O', 'H', 'S'}:
                    past_operators.append(character)
                else:
                    continue
            else:
                continue

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
                if symbol == self.alphabet[-1]:
                    first_assumption += 'y in ' + symbol + '))'
                else:
                    first_assumption += 'y in ' + symbol + ' | '

            second_assumption = "~(ex1 y: 0<=y & y<=max($) & ~("
            for pair in pairs:
                if pair == pairs[-1]:
                    second_assumption += '(y notin ' + pair[0] + ' | y notin ' + pair[1] + ')));'
                else:
                    second_assumption += '(y notin ' + pair[0] + ' | y notin ' + pair[1] + ') & '

            return first_assumption + ' & ' + second_assumption
        else:
            return None

    def translate(self):
        self.translated_formula = translate_bis(self.parsed_formula, self.formulaType, var='v_0') + ";\n"

    def buildMonaProgram(self, flag_for_declare):
        if not self.alphabet and not self.translated_formula:
            raise ValueError('Formula not parsed or translated...')
        else:
            if flag_for_declare:
                if self.compute_declare_assumption() is None:
                    if self.alphabet:
                        return self.headerMona + 'var2 ' + ", ".join(self.alphabet) + ';\n' + self.translated_formula
                    else:
                        return self.headerMona + self.translated_formula
                else:
                    return self.headerMona + 'var2 ' + ", ".join(
                        self.alphabet) + ';\n' + self.translated_formula + self.compute_declare_assumption()
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
            print('[ERROR]: Problem with the opening of the file!')

    def invoke_mona(self):
        # if sys.platform == 'linux':
        #     package_dir = os.path.dirname(os.path.abspath(__file__))
        #     mona_path = pkg_resources.resource_filename('ltlf2dfa', 'mona')
        #     if os.access(mona_path, os.X_OK):  # check if mona is executable
        #         command = package_dir + '/./mona -q -w ./automa.mona'
        #         process = Popen(args=command, stdout=PIPE, stderr=PIPE, preexec_fn=os.setsid, shell=True,
        #                         encoding="utf-8")
        #         try:
        #             output, error = process.communicate(timeout=30)
        #             return str(output).strip()
        #             # output, error = subprocess.call('mona -q -w ./automa.mona', shell=True)
        #         except TimeoutExpired:
        #             os.killpg(os.getpgid(process.pid), signal.SIGTERM)
        #             return False
        #             # subprocess.call(package_dir+'/./mona -u -gw ./automa.mona > ' + path + '.dot', shell=True)
        #             # output, error = subprocess.call(package_dir + '/./mona -q -w ./automa.mona', shell=True)
        #         # except subprocess.CalledProcessError as e:
        #         #     print(e)
        #         #     exit()
        #         # except OSError as e:
        #         #     print(e)
        #         #     exit()
        #     else:
        #         print('[ERROR]: MONA tool is not executable...')
        #         exit()
        # else:
        #     command = 'mona -q -w ./automa.mona'
        #     process = Popen(args=command, stdout=PIPE, stderr=PIPE, preexec_fn=os.setsid, shell=True,
        #                     encoding="utf-8")
        #     try:
        #         output, error = process.communicate(timeout=30)
        #         return str(output).strip()
        #         # output, error = subprocess.call('mona -q -w ./automa.mona', shell=True)
        #     except TimeoutExpired:
        #         os.killpg(os.getpgid(process.pid), signal.SIGTERM)
        #         return False
        #     # except subprocess.CalledProcessError as e:
        #     #     print(e)
        #     #     exit()
        #     # except OSError as e:
        #     #     print(e)
        #     #     exit()
        command = 'mona -q -w {}/automa.mona'.format(package_dir)
        process = Popen(args=command, stdout=PIPE, stderr=PIPE, preexec_fn=os.setsid, shell=True,
                        encoding="utf-8")
        try:
            output, error = process.communicate(timeout=30)
            return str(output).strip()
            # output, error = subprocess.call('mona -q -w ./automa.mona', shell=True)
        except TimeoutExpired:
            os.killpg(os.getpgid(process.pid), signal.SIGTERM)
            return False

    def output2dot(self, mona_output):
        """Parse the mona output or return the unsatisfiable dot."""
        if "Formula is unsatisfiable" in mona_output:
            return UNSAT_DOT
        else:
            return parse_mona(mona_output)


def translate_bis(formula_tree, _type, var):
    if type(formula_tree) == tuple:
        # enable this print to see the tree pruning
        # print(self.parsed_formula)
        # print(var)
        if formula_tree[0] == '&':
            # print('computed tree: '+ str(self.parsed_formula))
            if var == 'v_0':
                if _type == 2:
                    a = translate_bis(formula_tree[1], _type, 'max($)')
                    b = translate_bis(formula_tree[2], _type, 'max($)')
                else:
                    a = translate_bis(formula_tree[1], _type, '0')
                    b = translate_bis(formula_tree[2], _type, '0')
            else:
                a = translate_bis(formula_tree[1], _type, var)
                b = translate_bis(formula_tree[2], _type, var)
            if a == 'false' or b == 'false':
                return 'false'
            elif a == 'true':
                if b == 'true':
                    return 'true'
                else:
                    return b
            elif b == 'true':
                return a
            else:
                return '(' + a + ' & ' + b + ')'
        elif formula_tree[0] == '|':
            # print('computed tree: '+ str(self.parsed_formula))
            if var == 'v_0':
                if _type == 2:
                    a = translate_bis(formula_tree[1], _type, 'max($)')
                    b = translate_bis(formula_tree[2], _type, 'max($)')
                else:
                    a = translate_bis(formula_tree[1], _type, '0')
                    b = translate_bis(formula_tree[2], _type, '0')
            else:
                a = translate_bis(formula_tree[1], _type, var)
                b = translate_bis(formula_tree[2], _type, var)
            if a == 'true' or b == 'true':
                return 'true'
            elif a == 'false':
                if b == 'true':
                    return 'true'
                elif b == 'false':
                    return 'false'
                else:
                    return b
            elif b == 'false':
                return a
            else:
                return '(' + a + ' | ' + b + ')'
        elif formula_tree[0] == '~':
            # print('computed tree: '+ str(self.parsed_formula))
            if var == 'v_0':
                if _type == 2:
                    a = translate_bis(formula_tree[1], _type, 'max($)')
                else:
                    a = translate_bis(formula_tree[1], _type, '0')
            else:
                a = translate_bis(formula_tree[1], _type, var)
            if a == 'true':
                return 'false'
            elif a == 'false':
                return 'true'
            else:
                return '~(' + a + ')'
        elif formula_tree[0] == 'X':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            a = translate_bis(formula_tree[1], _type, new_var)
            if var == 'v_0':
                return '(' + 'ex1 ' + new_var + ': ' + new_var + ' = 1 ' + '& ' + a + ')'
            else:
                return '(' + 'ex1 ' + new_var + ': ' + new_var + ' = ' + var + ' + 1 ' + '& ' + a + ')'
        elif formula_tree[0] == 'U':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            new_new_var = _next(new_var)
            a = translate_bis(formula_tree[2], _type, new_var)
            b = translate_bis(formula_tree[1], _type, new_new_var)

            if var == 'v_0':
                if b == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' )'
                elif a == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' < ' + new_var + ' => ' + b + ' )'
                elif a == 'false':
                    return 'false'
                else:
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' & all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' < ' + new_var + ' => ' + b + ' )'
            else:
                if b == 'true':
                    return '( ' + 'ex1 ' + new_var + ': ' + var + ' <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' )'
                elif a == 'true':
                    return '( ' + 'ex1 ' + new_var + ': ' + var + ' <= ' + new_var + ' & ' + new_var + ' <= max($) & all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' < ' + new_var + ' => ' + b + ' )'
                elif a == 'false':
                    return 'false'
                else:
                    return '( ' + 'ex1 ' + new_var + ': ' + var + ' <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' & all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' < ' + new_var + ' => ' + b + ' )'

        elif formula_tree[0] == 'W':
            new_var = _next(var)
            a = translate_bis(formula_tree[1], _type, new_var)
            if var == 'v_0':
                return '(0 = max($)) | (' + 'ex1 ' + new_var + ': ' + new_var + ' = 1 ' + '& ' + a + ')'
            else:
                return '(' + var + ' = max($)) | (' + 'ex1 ' + new_var + ': ' + new_var + ' = ' + var + ' + 1 ' + '& ' + a + ')'

        elif formula_tree[0] == 'R':
            new_var = _next(var)
            new_new_var = _next(new_var)
            a = translate_bis(formula_tree[2], _type, new_new_var)
            b = translate_bis(formula_tree[1], _type, new_var)

            if var == 'v_0':
                if b == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' <= ' + new_var + ' => ' + a + ' ) |' \
                                                                                                                                                                                                                  '(all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + a + ' )'
                elif a == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + b + ')'
                elif b == 'false':
                    return '(all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + a + ' )'
                else:
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + b + ' & all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' <= ' + new_var + ' => ' + a + ' ) |' \
                                                                                                                                                                                                                              '(all1 ' + new_new_var + ': 0 <= ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + a + ' )'
            else:
                if b == 'true':
                    return '( ' + 'ex1 ' + new_var + ': ' + var + ' <= ' + new_var + ' & ' + new_var + ' <= max($) & all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' <= ' + new_var + ' => ' + a + ' ) |' \
                                                                                                                                                                                                                                      '(all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + a + ' )'
                elif a == 'true':
                    return '( ' + 'ex1 ' + new_var + ': ' + var + ' <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + b + ')'
                elif b == 'false':
                    return '(all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + a + ' )'
                else:
                    return '( ' + 'ex1 ' + new_var + ': ' + var + ' <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + b + ' & all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' <= ' + new_var + ' => ' + a + ' ) |' \
                                                                                                                                                                                                                                                  '(all1 ' + new_new_var + ': ' + var + ' <= ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + a + ' )'

        elif formula_tree[0] == 'Y':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            a = translate_bis(formula_tree[1], _type, new_var)
            if var == 'v_0':
                return '(' + 'ex1 ' + new_var + ': ' + new_var + ' = max($) - 1 ' + '& max($) > 0 & ' + a + ')'
            else:
                return '(' + 'ex1 ' + new_var + ': ' + new_var + ' = ' + var + ' - 1 ' + '& ' + new_var + ' > 0 & ' + a + ')'
        elif formula_tree[0] == 'S':
            # print('computed tree: '+ str(self.parsed_formula))
            new_var = _next(var)
            new_new_var = _next(new_var)
            a = translate_bis(formula_tree[2], _type, new_var)
            b = translate_bis(formula_tree[1], _type, new_new_var)

            if var == 'v_0':
                if b == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' )'
                elif a == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & all1 ' + new_new_var + ': ' + new_var + ' < ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + b + ' )'
                elif a == 'false':
                    return 'false'
                else:
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' & all1 ' + new_new_var + ': ' + new_var + ' < ' + new_new_var + ' & ' + new_new_var + ' <= max($) => ' + b + ' )'
            else:
                if b == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= max($) & ' + a + ' )'
                elif a == 'true':
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= ' + var + ' & all1 ' + new_new_var + ': ' + new_var + ' < ' + new_new_var + ' & ' + new_new_var + ' <= ' + var + ' => ' + b + ' )'
                elif a == 'false':
                    return 'false'
                else:
                    return '( ' + 'ex1 ' + new_var + ': 0 <= ' + new_var + ' & ' + new_var + ' <= ' + var + ' & ' + a + ' & all1 ' + new_new_var + ': ' + new_var + ' < ' + new_new_var + ' & ' + new_new_var + ' <= ' + var + ' => ' + b + ' )'
    else:
        # handling non-tuple cases
        if formula_tree == 'true':
            return 'true'
        elif formula_tree == 'false':
            return 'false'

        # enable if you want to see recursion
        # print('computed tree: '+ str(self.parsed_formula))

        # BASE CASE OF RECURSION
        else:
            if var == 'v_0':
                if _type == 2:
                    return 'max($) in ' + formula_tree.upper()
                else:
                    return '0 in ' + formula_tree.upper()
            else:
                return var + ' in ' + formula_tree.upper()
            # if formula_tree.isalpha():
            #     if var == 'v_0':
            #         return '0 in '+ formula_tree.upper()
            #     else:
            #         return var + ' in ' + formula_tree.upper()
            # else:
            #     return var + ' in ' + formula_tree.upper()


def _next(var):
    if var == '0' or var == 'max($)':
        return 'v_1'
    else:
        s = var.split('_')
        s[1] = str(int(s[1]) + 1)
        return '_'.join(s)


if __name__ == '__main__':
    formula = 'Fa'
    # formula = '(((G (false  | (~(X(grant0))) | (request0)))) & ((G (false  | (~(X(grant1))) | (request1)))) & ((G (false  | (~(X(grant2))) | (request2)))) & ((G (false  | (~(X(grant3))) | (request3)))) & ((G (false  | (~(X(grant4))) | (request4)))) & ((G (false  | (~(X(grant5))) | (request5)))) & ((G (true  & (false  | (~(X(grant0))) | (~(X(grant1)))) & (false  | (~(X(grant0))) | (~(X(grant2)))) & (false  | (~(X(grant0))) | (~(X(grant3)))) & (false  | (~(X(grant0))) | (~(X(grant4)))) & (false  | (~(X(grant0))) | (~(X(grant5)))) & (false  | (~(X(grant1))) | (~(X(grant0)))) & (false  | (~(X(grant1))) | (~(X(grant2)))) & (false  | (~(X(grant1))) | (~(X(grant3)))) & (false  | (~(X(grant1))) | (~(X(grant4)))) & (false  | (~(X(grant1))) | (~(X(grant5)))) & (false  | (~(X(grant2))) | (~(X(grant0)))) & (false  | (~(X(grant2))) | (~(X(grant1)))) & (false  | (~(X(grant2))) | (~(X(grant3)))) & (false  | (~(X(grant2))) | (~(X(grant4)))) & (false  | (~(X(grant2))) | (~(X(grant5)))) & (false  | (~(X(grant3))) | (~(X(grant0)))) & (false  | (~(X(grant3))) | (~(X(grant1)))) & (false  | (~(X(grant3))) | (~(X(grant2)))) & (false  | (~(X(grant3))) | (~(X(grant4)))) & (false  | (~(X(grant3))) | (~(X(grant5)))) & (false  | (~(X(grant4))) | (~(X(grant0)))) & (false  | (~(X(grant4))) | (~(X(grant1)))) & (false  | (~(X(grant4))) | (~(X(grant2)))) & (false  | (~(X(grant4))) | (~(X(grant3)))) & (false  | (~(X(grant4))) | (~(X(grant5)))) & (false  | (~(X(grant5))) | (~(X(grant0)))) & (false  | (~(X(grant5))) | (~(X(grant1)))) & (false  | (~(X(grant5))) | (~(X(grant2)))) & (false  | (~(X(grant5))) | (~(X(grant3)))) & (false  | (~(X(grant5))) | (~(X(grant4))))))) & ((false  | (~(G (F (request0)))) | (G (F (X(grant0)))))) & ((false  | (~(G (F (request1)))) | (G (F (X(grant1)))))) & ((false  | (~(G (F (request2)))) | (G (F (X(grant2)))))) & ((false  | (~(G (F (request3)))) | (G (F (X(grant3)))))) & ((false  | (~(G (F (request4)))) | (G (F (X(grant4)))))) & ((false  | (~(G (F (request5)))) | (G (F (X(grant5)))))))'
    declare_flag = False

    t = Translator(formula)
    t.formula_parser()
    t.translate()
    t.createMonafile(declare_flag)  # it creates automa.mona file
    result = t.invoke_mona()  # it returns an intermediate automa.dot file
    print(t.output2dot(result))
