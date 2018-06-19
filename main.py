from Translator import Translator

translator = Translator("G(a&b)")
translator.compute_alphabet()
translator.formula_parser()
translator.translate()
program = translator.buildMonaProgram()
print(program)
