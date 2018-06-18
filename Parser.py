import ply.yacc as yacc
from Lexer import MyLexer

class MyParser(object):

    def __init__(self):
        self.lexer = MyLexer()
        self.lexer.build()
        self.tokens = self.lexer.tokens
        self.parser = yacc.yacc(module=self)
        self.precedence = (

            ('nonassoc', 'LPAR', 'RPAR'),
            ('left', 'AND', 'OR', 'IMPLIES', 'DIMPLIES', 'UNTIL', 'PASTUNTIL'),
            ('right', 'NEXT', 'EVENTUALLY', 'GLOBALLY', 'PASTNEXT', 'PASTEVENTUALLY', 'PASTGLOBALLY'),
            ('right', 'NOT')
        )

    def __call__(self, s, **kwargs):
        return self.parser.parse(s, lexer=self.lexer.lexer)

    def p_formula(self, p):
        '''
        formula : formula AND formula
                | formula OR formula
                | formula IMPLIES formula
                | formula DIMPLIES formula
                | formula UNTIL formula
                | formula PASTUNTIL formula
                | NEXT formula
                | EVENTUALLY formula
                | GLOBALLY formula
                | PASTNEXT formula
                | PASTEVENTUALLY formula
                | PASTGLOBALLY formula
                | NOT formula
                | TRUE
                | FALSE
                | TERM
        '''

        if len(p) == 2: p[0] = p[1]
        elif len(p) == 3:
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
        elif len(p) == 4:
            if p[2] == '->':
                p[0] = ('|', ('~', p[1]), p[3])
            elif p[2] == '<->':
                p[0] = ('&', ('|', ('~', p[1]), p[3]), ('|', ('~', p[3]), p[1]))
            else:
                p[0] = (p[2],p[1],p[3])
        else: raise ValueError


    def p_expr_group(self, p):
        '''
        formula : LPAR formula RPAR
        '''
        p[0] = p[2]

    def p_error(self, p):
        raise ValueError("Syntax error in input! %s" %str(p))


#########################
    #
    # def run(self, p, var):
    #     if type(p) == tuple:
    #         #enable this print to see the tree pruning
    #         # print(p)
    #         # print(var)
    #         if p[0] == '&':
    #             # print('computed tree: '+ str(p))
    #             if var == 'v_0':
    #                 a = self.run(p[1], '0')
    #                 b = self.run(p[2], '0')
    #             else:
    #                 a = self.run(p[1], var)
    #                 b = self.run(p[2], var)
    #             if a == 'False' or b == 'False':
    #                 return 'False'
    #             elif a == 'True':
    #                 if b == 'True': return 'True'
    #             elif b == 'True': return a
    #             else: return '('+a+' & '+b+')'
    #         elif self.parsed_formula == '|':
    #             # print('computed tree: '+ str(p))
    #             if var == 'v_0':
    #                 a = self.run(p[1], '0')
    #                 b = self.run(p[2], '0')
    #             else:
    #                 a = self.run(p[1], var)
    #                 b = self.run(p[2], var)
    #             if a == 'True' or b == 'True':
    #                 return 'True'
    #             elif a == 'False':
    #                 if b == 'True': return 'True'
    #                 elif b == 'False': return 'False'
    #                 else: return b
    #             elif b == 'False': return a
    #             else: return '('+a+' | '+b+')'
    #         elif self.parsed_formula == '~':
    #             # print('computed tree: '+ str(p))
    #             if var == 'v_0': a = self.run(p[1], '0')
    #             else: a = self.run(p[1], var)
    #             if a == 'True': return 'False'
    #             elif a == 'False': return 'True'
    #             else: return '~('+ a +')'
    #         elif self.parsed_formula == 'X':
    #             # print('computed tree: '+ str(p))
    #             new_var = self.next(var)
    #             a = self.run(p[1],new_var)
    #             if var == 'v_0':
    #                 return '('+ 'ex1 '+new_var+': '+ new_var +' = 1 '+ '& '+ a +')'
    #             else:
    #                 return '('+ 'ex1 '+new_var+': '+ new_var +' = '+ var + ' + 1 '+ '& '+ a +')'
    #         elif self.parsed_formula == 'U':
    #             # print('computed tree: '+ str(p))
    #             new_var = self.next(var)
    #             new_new_var = self.next(new_var)
    #             a = self.run(p[2],new_var)
    #             b = self.run(p[1],new_new_var)
    #
    #             if var == 'v_0':
    #                 if b == 'True': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
    #                 elif a ==  'True': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
    #                 elif a == 'False': return 'False'
    #                 else: return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
    #             else:
    #                 if b == 'True': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
    #                 elif a ==  'True': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
    #                 elif a == 'False': return 'False'
    #                 else: return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
    #     else:
    #         # handling non-tuple cases
    #         if self.parsed_formula == 'T': return 'True'
    #         elif self.parsed_formula == 'F': return 'False'
    #
    #         # enable if you want to see recursion
    #         # print('computed tree: '+ str(p))
    #
    #         # BASE CASE OF RECURSION
    #         else: return var + ' in ' + p
    #
    # def next(self, var):
    #     if var == '0': return 'v_1'
    #     else:
    #         s = var.split('_')
    #         s[1] = str(int(s[1])+1)
    #         return '_'.join(s)

if __name__ == '__main__':
    par = MyParser()
    while True:
       try:
           s = input('calc > ')
       except EOFError:
           break
       if not s: continue
       result = par(s)
       print(result)