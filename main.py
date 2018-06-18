from Translator import Translator

#
# par = MyParser()
# formula = 'a -> b'
# parsed = par(formula)
# print(parsed)



translator = Translator()
translator.formula_to_be_parsed = "G(a -> Xb)"
translator.formula_parser()
# translator.translate()
# translator.buildMonaProgram()