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
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (

    ('nonassoc', 'LPAR', 'RPAR'),
    ('left', 'AND', 'OR', 'IMPLIES', 'DIMPLIES', 'UNTIL'),
    ('right', 'NEXT', 'EVENTUALLY', 'GLOBALLY'),
    ('right', 'NOT')
)

def p_ltl(p):
    '''
    ltl : expression
        | empty
    '''
    print(run(p[1]))

def p_expr_binary(p):
    '''
    expression : expression AND expression
               | expression OR expression
               | expression IMPLIES expression
               | expression DIMPLIES expression
               | expression UNTIL expression
    '''

    p[0] = (p[2],p[1],p[3])

def p_expr_unary(p):
    '''
    expression : NOT expression
               | NEXT expression
               | EVENTUALLY expression
               | GLOBALLY expression
    '''

    p[0] = (p[1], p[2])

def p_expr_group(p):
    '''
    expression : LPAR expression RPAR
    '''
    p[0] = p[2]

def p_expr_term(p):
    '''
    expression : TERM
               | TRUE
               | FALSE
    '''
    p[0] = p[1]

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

def p_error(p):
    print("Syntax error")

parser = yacc.yacc()

def run(p):
    if type(p) == tuple:
        print(p)
        if p[0] == 'T': return 'True'
        elif p[0] == 'F': return 'False'
        elif p[0] == '&':
            # print('computed tree: '+ str(p))
            a = run(p[1])
            b = run(p[2])
            return '('+a+' & '+b+')'
        elif p[0] == '|':
            # print('computed tree: '+ str(p))
            a = run(p[1])
            b = run(p[2])
            return '('+a+' | '+b+')'
        elif p[0] == '~':
            # print('computed tree: '+ str(p))
            a = run(p[1])
            return '~('+str(a)+')'
        elif p[0] == 'X':
            # print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == 'E':
            # print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == 'G':
            # print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == 'U':
            # print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == '->':
            # print('computed tree: '+ str(p))
            return 'to be implemented'
        elif p[0] == '<->':
            # print('computed tree: '+ str(p))
            return 'to be implemented'
    else:
        # enable if you want to see recursion
        # print('computed tree: '+ str(p))
        return p+'(x)'

# ----------- TESTING LEXER ----------------
# lexer.input("a <-> b")
#
# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)

# ----------- TESTING PARSER ----------------
# while True:
#     try:
#         s = input('>')
#     except Exception as e:
#         print(e)
#         break


if __name__=="__main__":
    try:
        print('++++++++++++++++++ ~(a&b)|c becomes +++++++++++')
        parser.parse('~(a&b)|c')
        print('++++++++++++++++++ Ea -> Eb becomes +++++++++++')
        parser.parse('Ea -> Eb')
        print('++++++++++++++++++ E(a -> Eb) becomes +++++++++++')
        parser.parse('E(a -> Eb)')
        print('++++++++++++++++++ Ea <-> Eb becomes +++++++++++')
        parser.parse('Ea <-> Eb')
        print('++++++++++++++++++ G(a -> Eb) becomes +++++++++++')
        parser.parse('G(a -> Eb)')
        print('++++++++++++++++++ (~bUa)|G(~b) becomes +++++++++++')
        parser.parse('(~bUa)|G(~b)')
        print('++++++++++++++++++ G(a -> X(~aUb)) becomes +++++++++++')
        parser.parse('G(a -> X(~aUb))')
        print('++++++++++++++++++ G(a -> Xb) becomes +++++++++++')
        parser.parse('G(a -> Xb)')
        print('++++++++++++++++++ G(Xb -> a) becomes +++++++++++')
        parser.parse('G(Xb -> a)')
        print('++++++++++++++++++ G(a <-> Xb) becomes +++++++++++')
        parser.parse('G(a <-> Xb)')
        print('++++++++++++++++++ ~(Ea & Eb) becomes +++++++++++')
        parser.parse('~(Ea & Eb)')
        print('++++++++++++++++++ G(a -> ~(Eb)) becomes +++++++++++')
        parser.parse('G(a -> ~(Eb))')
        print('++++++++++++++++++ G(a -> X(~b)) becomes +++++++++++')
        parser.parse('G(a -> X(~b))')
    except Exception as e:
        print(e)
