import ply.lex as lex

class Lexer(object):

    reserved = {
        'true':     'TRUE',
        'false':    'FALSE',
        'X':        'NEXT',
        'U':        'UNTIL',
        'E':        'EVENTUALLY',
        'G':        'GLOBALLY'
    }
    # List of token names.   This is always required
    tokens = (

        'TERM',
        'NOT',
        'AND',
        'OR',
        'IMPLIES',
        'DIMPLIES',
        'LPAR',
        'RPAR'
    ) + tuple(reserved.values())

    # Regular expression rules for simple tokens
    t_TRUE = r'T'
    t_FALSE = r'F'
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

    t_ignore = r' '+'\n'

    def t_TERM(self, t):
        r'[a-z]+'
        t.type = Lexer.reserved.get(t.value, 'TERM')
        return t  # Check for reserved words

    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Build the lexer
    def build(self,**kwargs):
        self.lexer = lex.lex(module=self, **kwargs)
#
#     # Test it output
#     def test(self,data):
#         self.lexer.input(data)
#         while True:
#             tok = self.lexer.token()
#             if not tok:
#                 break
#             print(tok)
#
# # Build the lexer and try it out
# m = Lexer()
# m.build()           # Build the lexer
# m.test("G(a -> Xb)")     # Test it