import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [

    'TRUE',
    'FALSE',
    'TERM',
    'NOT',
    'AND',
    'OR',
    'IMPLIES',
    'DIMPLIES',
    'NEXT',
    'UNTIL',
    'EVENTUALLY',
    'GLOBALLY',
    'LPAR',
    'RPAR'
]

t_TRUE = r'T'
t_FALSE = r'F'
t_TERM = r'[a-z]+'
t_AND = r'\&'
t_OR = r'\|'
t_IMPLIES = r'\->'
t_DIMPLIES = r'\<->'
t_NEXT = r'X'
t_UNTIL = r'U'
t_EVENTUALLY = r'E'
t_GLOBALLY = r'G'
t_NOT = r'\~'
t_LPAR = r'\('
t_RPAR = r'\)'

t_ignore = r' '

def t_error(t):
    print("Illegal character")
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (

    # ('left', 'NOT', 'TERM'),
    ('left', 'AND', 'OR', 'IMPLIES', 'DIMPLIES'),
    # ('left', 'OR', 'NOT'),
    # ('left', 'IMPLIES', 'NOT'),
    # ('left', 'DIMPLIES', 'NOT'),
    # ('left', 'UNTIL', 'AND'),
    # ('left', 'OR', 'IMPLIES'),
    # ('left', 'LPAR', 'RPAR')
    ('right', 'NOT')
)

def p_ltl(p):
    '''
    ltl : expression
        | empty
    '''
    print(run(p[1]))

# def p_expression_term(p):
#     '''
#     expression : TERM
#     '''
#     p[0] = p[1]

# | LPAR expression UNTIL expression RPAR
# | LPAR expression AND expression RPAR
# | LPAR expression OR expression RPAR
# | LPAR expression IMPLIES expression RPAR
# | LPAR expression DIMPLIES expression RPAR

def p_expression(p):
    '''
    expression : TRUE
               | FALSE
               | TERM
               | expression UNTIL expression
               | expression AND expression
               | expression OR expression
               | expression IMPLIES expression
               | expression DIMPLIES expression
               | LPAR expression RPAR
    '''
    # qua va modificato per la nuova espressione ridotta LPAR expression RPAR
    # if p[1] == 'T' or p[1] == 'F': p[0] = (p[1],)
    # elif len(p)<3: p[0] = p[1]
    # elif p[1] != '(':
    #     p[0] = (p[2], p[1], p[3])
    # else:
    #     p[0] = (p[3], p[2], p[4])

    if p[1] == 'T' or p[1] == 'F': p[0] = (p[1],)
    elif len(p)<3: p[0] = p[1]
    elif p[1] != '(':
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[2]

def p_expression_not(p):
    '''
    expression : NOT expression
               | NOT LPAR expression RPAR
    '''
    if p[2] == '(':
        p[0] = (p[1], p[3])
        print("questa è p0 in "+str(p[0]))
    else:
        p[0] = (p[1], p[2])

def p_expression_next(p):
    '''
    expression : NEXT expression
               | LPAR NEXT expression RPAR
    '''
    if p[1] != '(':
        p[0] = (p[1], p[2])
    else: p[0] = (p[2], p[3])
    # print('questo è p0 in X '+str(p[0]))

def p_expression_eventually(p):
    '''
    expression : EVENTUALLY expression
               | LPAR EVENTUALLY expression RPAR
    '''
    if p[1] != '(':
        p[0] = (p[1], p[2])
    else: p[0] = (p[2], p[3])
    # print(p[0])

def p_expression_globally(p):
    '''
    expression : GLOBALLY expression
               | LPAR GLOBALLY expression RPAR
    '''
    if p[1] != '(':
        p[0] = (p[1], p[2])
    else: p[0] = (p[2], p[3])
    # print(p[0])

def p_error(p):
    print("Syntax error")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

parser = yacc.yacc()

def run(p):
    if type(p) == tuple:
        if p[0] == 'T': return 'True'
        elif p[0] == 'F': return 'False'
        elif p[0] == '&':
            print('computed tree: '+ str(p))
            a = run(p[1])
            b = run(p[2])
            return '('+a+' & '+b+')'
        elif p[0] == '|':
            print('computed tree: '+ str(p))
            a = run(p[1])
            b = run(p[2])
            return '('+a+' | '+b+')'
        elif p[0] == '~':
            print('computed tree: '+ str(p))
            a = run(p[1])
            return '~('+str(a)+')'
        elif p[0] == 'X':
            print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == 'E':
            print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == 'G':
            print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == 'U':
            print('computed tree: '+ str(p))
            return 'to be implemented'
    else:
        # enable if you want to see recursion
        # print('computed tree: '+ str(p))
        return p+'(x)'

# while True:
#     try:
#         s = input('>')
#     except EOFError:
#         break
if __name__=="__main__":
    try:
        print('++++++++++++++++++ TERM a becomes ++++++++++++')
        parser.parse('a')
        print('++++++++++++++++++ Xa becomes ++++++++++++++++')
        parser.parse('Xa')
        print('++++++++++++++++++ Ea becomes ++++++++++++++++')
        parser.parse('Ea')
        print('++++++++++++++++++ Ga becomes ++++++++++++++++')
        parser.parse('Ga')
        print('++++++++++++++++++ aUb becomes +++++++++++++++')
        parser.parse('aUb')
        print('++++++++++++++++++ a&b becomes +++++++++++++++')
        parser.parse('a&b')
        print('++++++++++++++++++ a|b becomes +++++++++++++++')
        parser.parse('a|b')
        print('++++++++++++++++++ (a) becomes +++++++++++++++')
        parser.parse('(a)')
        print('++++++++++++++++++ (Xa) becomes ++++++++++++++')
        parser.parse('(Xa)')
        print('++++++++++++++++++ (Ea) becomes ++++++++++++++')
        parser.parse('(Ea)')
        print('++++++++++++++++++ (Ga) becomes ++++++++++++++')
        parser.parse('(Ga)')
        print('++++++++++++++++++ (aUb) becomes +++++++++++++')
        parser.parse('(aUb)')
        print('++++++++++++++++++ (a&b) becomes +++++++++++++')
        parser.parse('(a&b)')
        print('++++++++++++++++++ (a|b) becomes +++++++++++++')
        parser.parse('(a|b)')

    except Exception as e:
        print(e)





# lexer.input("a <-> b")
#
# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)



# NOT = '~'
# AND = '&'
# OR = '|'
# IMPLIES = '=>'
# EQUIV = '<=>'
# NEXT = 'X'
# UNTIL = 'U'
# LPAR = '('
# RPAR = ')'
#
#
# def main():
#     with open('ltl_formula.txt','r') as file:
#         formula = file.read()
#
#         parse(formula.split())
#
# def parse(f):
#
#
# if __name__ == "__main__":
#     main()


# from ast import *
#
# class Parser:
#     def __init__(self):
#         self.string = ""
#         self.operators = ["and", "or", "not", "(", ")", "->", "<->", "X", "U"]
#         self.constants = ["true", "false"]
#
#     def setString(self, string):
#         self.string = ""
#         for c in string:
#             if c == "(" or c == ")":
#                 self.string = self.string + " " + c + " "
#             else:
#                 self.string += c
#
#     def parse(self):
#         operand_stack  = []
#         operator_stack = []
#
#         e = self.string.split()
#         e.insert(0, "(")
#         e.append(")")
#
#         for el in e:
#             if el in self.operators:
#                 if el != ")":
#                     operator_stack.append(el)
#                 else:
#                     op = operator_stack.pop()
#                     while op != "(":
#                         if op == "not":
#                             v = operand_stack.pop()
#                             exp = Not(v)
#                             operand_stack.append(exp)
#                         elif op == "and":
#                             v2 = operand_stack.pop()
#                             v1 = operand_stack.pop()
#                             exp = And(v1, v2)
#                             operand_stack.append(exp)
#                         elif op == "or":
#                             v2 = operand_stack.pop()
#                             v1 = operand_stack.pop()
#                             exp = Or(v1, v2)
#                             operand_stack.append(exp)
#                         elif op == "->":
#                             v2 = operand_stack.pop()
#                             v1 = operand_stack.pop()
#                             exp = Imply(v1, v2)
#                             operand_stack.append(exp)
#                         elif op == "<->":
#                             v2 = operand_stack.pop()
#                             v1 = operand_stack.pop()
#                             exp = DImply(v1, v2)
#                             operand_stack.append(exp)
#                         else:
#                             raise Exception("Wrong operand")
#                         op = operator_stack.pop()
#             elif el in self.constants:
#                 if el == "true":
#                     operand_stack.append(TTrue())
#                 else:
#                     operand_stack.append(FFalse())
#             else:
#                 operand_stack.append(Var(el))
#         return operand_stack.pop()
#
# if __name__=="__main__":
#     p = Parser()
#     p.setString("not((a or b) -> (a and b))")
#     print p.parse()
#     p.setString("(not a) or b")
#     print p.parse()
#     p.setString("((p -> (q and r)) and ((not q) or (not r)) and (not (not p))")
#     print p.parse()
#     p.setString("true -> false")
#     print p.parse()
#     p.setString("((not true) and a) or b")
#     print p.parse()
