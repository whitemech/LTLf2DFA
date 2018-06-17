import ply.yacc as yacc
import Lexer

class Parser(object):

    def __init__(self):
        lexer = Lexer()

        precedence = (

            ('nonassoc', 'LPAR', 'RPAR'),
            ('left', 'AND', 'OR', 'IMPLIES', 'DIMPLIES', 'UNTIL', 'PASTUNTIL'),
            ('right', 'NEXT', 'EVENTUALLY', 'GLOBALLY', 'PASTNEXT', 'PASTEVENTUALLY', 'PASTGLOBALLY'),
            ('right', 'NOT')
        )


    def p_ltl(p):
        '''
        ltl : expression
            | empty
        '''

    def p_expr_binary(p):
        '''
        expression : expression AND expression
                   | expression OR expression
                   | expression IMPLIES expression
                   | expression DIMPLIES expression
                   | expression UNTIL expression
                   | expression PASTUNTIL expression
        '''
        if p[2] == '->':
            p[0] = ('|', ('~', p[1]), p[3])
        elif p[2] == '<->':
            p[0] = ('&', ('|', ('~', p[1]), p[3]), ('|', ('~', p[3]), p[1]))
        else:
            p[0] = (p[2],p[1],p[3])

    def p_expr_unary(p):
        '''
        expression : NOT expression
                   | NEXT expression
                   | EVENTUALLY expression
                   | GLOBALLY expression
                   | PASTNEXT expression
                   | PASTEVENTUALLY expression
                   | PASTGLOBALLY expression
        '''
        if p[1] == 'E': # eventually A == true UNITL A
            p[0] = ('U','T', p[2])
        elif p[1] == 'G': # globally A == not( eventually (not A) )
            p[0] = ('~',('U', 'T', ('~',p[2])))
        elif p[1] == 'O': # pasteventually A = true SINCE A
            p[0] = ('S','T', p[2])
        elif p[1] == 'H': # pastglobally A == not( pasteventually (not A) )
            p[0] = ('~',('S', 'T', ('~',p[2])))
        else:
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