# LTL<sub>f</sub>2DFA
[![ciaio](https://img.shields.io/badge/python-3.6-blue.svg)]()

LTL<sub>f</sub>2DFA is a simple tool that processes an LTL<sub>f</sub> formula (with past or future operators) and generates the corresponding minimized DFA (Deterministic Finite state Automaton) using [MONA](http://www.brics.dk/mona/).
This tool is written in Python 3.6.

It is tested on Linux Ubuntu 16.04 and on macOS 10.13.6.

## Getting Started

### Requirements

This tool uses MONA for the generation of the DFA. Hence, you should first install MONA with all its dependencies on your OS following the instructions [here](http://www.brics.dk/mona/download.html).

This tool is also based on the following libraries:

- [ply 3.11](https://pypi.org/project/ply/)
- [graphviz 0.8.3](http://graphviz.org)
- [dotpy 0.0.1](https://pypi.org/project/dotpy/)

They are automatically added while installing LTL<sub>f</sub>2DFA.

## How To Install It

- From PyPI:
```
pip install ltlf2dfa
```
- From this repository:
```
pip install git+https://github.com/Francesco17/LTLf2DFA@distribution#egg=ltlf2dfa
```

## How To Use It

- Simply parse an LTL<sub>f</sub> formula with past or future operators:
```python
from ltlf2dfa.Parser import MyParser

formula = "G(a->Xb)"
parser = MyParser()
parsed_formula = parser(formula)

print(parsed_formula)
```
- Translate an LTL<sub>f</sub> formula to the corresponding DFA automaton:
```python
from ltlf2dfa.Translator import Translator
from ltlf2dfa.DotHandler import DotHandler

formula = "G(a->Xb)"
declare_flag = False #True if you want to compute DECLARE assumption for the formula

translator = Translator(formula)
translator.formula_parser()
translator.translate()
translator.createMonafile(declare_flag) #it creates automa.mona file
translator.invoke_mona() #it returns an intermediate automa.dot file

dotHandler = DotHandler()
dotHandler.modify_dot()
dotHandler.output_dot() #it returns the final automa.dot file
```
## Syntax

The syntax accepted by LTL<sub>f</sub>2DFA is the following:

|    OPERATOR   | SYMBOL |
|:-------------:|:------:|
|      TRUE     |    T   |
|     FALSE     |    F   |
|      AND      |    &   |
|       OR      |    \|  |
|      NOT      |    ~   |
|  IMPLICATION  |   ->   |
| D-IMPLICATION |   <->  |
|      NEXT     |    X   |
|     UNTIL     |    U   |
|   EVENTUALLY  |    E   |
|    GLOBALLY   |    G   |
| YESTERDAY (*) |    Y   |
|    SINCE (*)  |    S   |
|    ONCE (*)   |    O   |
|  GLOBALLY (*) |    H   |

(*) are PAST operators.

Also parentheses `(` and `)` can be used.

**NOTE**: LTL<sub>f</sub>2DFA accepts ONLY separated formulas, i.e. formulas that have only past, only future or none operators.

## Author

[Francesco Fuggitti](https://www.linkedin.com/in/francesco-fuggitti-b78336131/)

## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/Francesco17/LTLf2FOL/blob/master/LICENSE) file for details

## Contacts

If, for any reason, you are interested in feel free to contact me by email.
