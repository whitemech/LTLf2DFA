from Translator import Translator

translator = Translator("G(a&b)")
translator.formula_parser()
translator.translate()
translator.createMonafile("automa")

