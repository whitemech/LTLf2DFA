AND = '&'
NOT = '~'
OR = '|'
IMPLIES = '=>'
EQUIV = '<=>'
GLOBALLY = 'G'
NEXT = 'X'
UNTIL = 'U'


def main():
    with open('ltl_formula.txt','r') as file:
        formula = file.read()
        parse(formula)

def parse(f):
    words = f.split()








if __name__ == "__main__":
    main()
