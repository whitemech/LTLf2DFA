from Translator import Translator

translator = Translator()
translator.formula_to_be_parsed = "G(a -> Xb)"
translator.formula_parser()
parsed = translator.get_parsed_formula()
# print(type(parsed))
# print(parsed)
# print(type(translator.tuple_to_string()))
# print(translator.tuple_to_string())
# translator.search_for_mixed_operators()
translator.translate()
result = translator.get_translated_formula()
print(result)
# translator.buildMonaProgram()