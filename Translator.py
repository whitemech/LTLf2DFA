class Translator:

    def __init__(self):
        self.header = "m2l-str;\n"

    def __call__(self, tree_formula):
        init_variable = 'v_0'
        return self.translate(tree_formula, init_variable)

    def translate(self, p, var):
        if type(p) == tuple:
            #enable this print to see the tree pruning
            # print(p)
            # print(var)
            if p[0] == '&':
                # print('computed tree: '+ str(p))
                if var == 'v_0':
                    a = self.translate(p[1], '0')
                    b = self.translate(p[2], '0')
                else:
                    a = self.translate(p[1], var)
                    b = self.translate(p[2], var)
                if a == 'False' or b == 'False':
                    return 'False'
                elif a == 'True':
                    if b == 'True': return 'True'
                elif b == 'True': return a
                else: return '('+a+' & '+b+')'
            elif p[0] == '|':
                # print('computed tree: '+ str(p))
                if var == 'v_0':
                    a = self.translate(p[1], '0')
                    b = self.translate(p[2], '0')
                else:
                    a = self.translate(p[1], var)
                    b = self.translate(p[2], var)
                if a == 'True' or b == 'True':
                    return 'True'
                elif a == 'False':
                    if b == 'True': return 'True'
                    elif b == 'False': return 'False'
                    else: return b
                elif b == 'False': return a
                else: return '('+a+' | '+b+')'
            elif p[0] == '~':
                # print('computed tree: '+ str(p))
                if var == 'v_0': a = self.translate(p[1], '0')
                else: a = self.translate(p[1], var)
                if a == 'True': return 'False'
                elif a == 'False': return 'True'
                else: return '~('+ a +')'
            elif p[0] == 'X':
                # print('computed tree: '+ str(p))
                new_var = self.next(var)
                a = self.translate(p[1],new_var)
                if var == 'v_0':
                    return '('+ 'ex1 '+new_var+': '+ new_var +' = 1 '+ '& '+ a +')'
                else:
                    return '('+ 'ex1 '+new_var+': '+ new_var +' = '+ var + ' + 1 '+ '& '+ a +')'
            elif p[0] == 'U':
                # print('computed tree: '+ str(p))
                new_var = self.next(var)
                new_new_var = self.next(new_var)
                a = self.translate(p[2],new_var)
                b = self.translate(p[1],new_new_var)

                if var == 'v_0':
                    if b == 'True': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
                    elif a ==  'True': return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
                    elif a == 'False': return 'False'
                    else: return '( '+ 'ex1 '+new_var+': 0 <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
                else:
                    if b == 'True': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ a +' )'
                    elif a ==  'True': return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
                    elif a == 'False': return 'False'
                    else: return '( '+ 'ex1 '+new_var+': '+var+' <= '+new_var+' & '+new_var+' <= max($) & '+ a +' & forall1 '+new_new_var+': '+var+' <= '+new_new_var+' & '+new_new_var+' < '+new_var+' => '+b+' )'
        else:
            # handling non-tuple cases
            if p[0] == 'T': return 'True'
            elif p[0] == 'F': return 'False'

            # enable if you want to see recursion
            # print('computed tree: '+ str(p))

            # BASE CASE OF RECURSION
            else: return var + ' in ' + p

    def next(self, var):
        if var == '0': return 'v_1'
        else:
            s = var.split('_')
            s[1] = str(int(s[1])+1)
            return '_'.join(s)