def main():
    with open('ltl_formula.txt','r') as file:
        formula = file.read()
        parse(formula)

def parse(f):
    print f




    


if __name__ == "__main__":
    main()
