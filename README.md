# LTL<sub>f</sub>2DFA
[![](https://img.shields.io/pypi/v/ltlf2dfa.svg)](https://pypi.python.org/pypi/ltlf2dfa)
[![](https://img.shields.io/travis/francescofuggitti/ltlf2dfa.svg)](https://travis-ci.org/francescofuggitti/ltlf2dfa)
[![](https://img.shields.io/pypi/pyversions/ltlf2dfa.svg)](https://pypi.python.org/pypi/ltlf2dfa)
[![](https://img.shields.io/badge/docs-mkdocs-9cf)](https://www.mkdocs.org/)
[![](https://img.shields.io/badge/status-development-orange.svg)](https://img.shields.io/badge/status-development-orange.svg)
[![](https://coveralls.io/repos/github/francescofuggitti/ltlf2dfa/badge.svg?branch=develop)](https://coveralls.io/github/francescofuggitti/ltlf2dfa?branch=develop)
[![](https://img.shields.io/badge/flake8-checked-blueviolet)](https://img.shields.io/badge/flake8-checked-blueviolet)
[![](https://img.shields.io/badge/mypy-checked-blue)](https://img.shields.io/badge/mypy-checked-blue)
[![](https://img.shields.io/badge/license-MIT-lightgrey)](https://img.shields.io/badge/license-MIT-lightgrey)

LTL<sub>f</sub>2DFA is a simple tool that processes an LTL<sub>f</sub> or a PLTLf formula and generates the 
corresponding minimized DFA (Deterministic Finite state Automaton) using [MONA](http://www.brics.dk/mona/).
This tool is written in Python 3.7.

It is tested on Linux Ubuntu 18.04 and on macOS 10.15.

It is also available online at [ltlf2dfa.diag.uniroma1.it](http://ltlf2dfa.diag.uniroma1.it).

## Getting Started

### Requirements

This tool uses MONA for the generation of the DFA. Hence, you should first install MONA with all its dependencies on 
your OS following the instructions [here](http://www.brics.dk/mona/download.html).

This tool is also based on the following libraries:

- [ply 3.11](https://pypi.org/project/ply/)
- [sympy 1.5.1](https://pypi.org/project/sympy/)

They are automatically added while installing LTL<sub>f</sub>2DFA.

## How To Install It

- From PyPI:
```
pip install ltlf2dfa
```
- From this repository:
```
pip install git+https://github.com/Francesco17/LTLf2DFA@develop#egg=ltlf2dfa
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

formula = "G(a->Xb)"
declare_flag = False #True if you want to compute DECLARE assumption for the formula

translator = Translator(formula)
translator.formula_parser()
translator.translate()
translator.createMonafile(declare_flag) #it creates automa.mona file
result = translator.invoke_mona() #it returns an intermediate automa.dot file
print(translator.output2dot(result))

```
## Syntax

The syntax accepted by LTL<sub>f</sub>2DFA is the following:

|    OPERATOR   | SYMBOL |
|:-------------:|:------:|
|      TRUE     |  true  |
|     FALSE     |  false |
|      AND      |    &   |
|       OR      |    \|  |
|      NOT      |    !   |
|  IMPLICATION  |   ->   |
| D-IMPLICATION |   <->  |
|      NEXT     |    X   |
|     UNTIL     |    U   |
|   EVENTUALLY  |    F   |
|    GLOBALLY   |    G   |
|   WEAK NEXT   |    W   |
|    RELEASE    |    R   |
| YESTERDAY (*) |    Y   |
|    SINCE (*)  |    S   |
|    ONCE (*)   |    O   |
|  GLOBALLY (*) |    H   |

(*) are PAST operators.

Also parentheses `(` and `)` can be used.

**NOTE**: LTL<sub>f</sub>2DFA accepts either LTL<sub>f</sub> formulas or PLTL<sub>f</sub> formulas, i.e. formulas that 
have only past, only future or none operators.

## Author

[Francesco Fuggitti](https://francescofuggitti.github.io/)

## License

This project is licensed under the MIT License - see the [LICENSE](AUTHORS.md) file for details

## Contacts

If, for any reason, you are interested in feel free to contact me by email.
