# LTL<sub>f</sub>2DFA
[![](https://img.shields.io/pypi/v/ltlf2dfa.svg)](https://pypi.python.org/pypi/ltlf2dfa)
[![](https://img.shields.io/pypi/pyversions/ltlf2dfa.svg)](https://pypi.python.org/pypi/ltlf2dfa)
[![LTLf2DFA CI pipeline](
  https://github.com/whitemech/LTLf2DFA/workflows/LTLf2DFA%20CI%20pipeline./badge.svg)](
  https://github.com/whitemech/LTLf2DFA/actions?query=workflow%3A%22LTLf2DFA+CI+pipeline.%22)
[![](https://img.shields.io/badge/docs-mkdocs-9cf)](https://www.mkdocs.org/)
[![](https://img.shields.io/badge/status-development-orange.svg)](https://img.shields.io/badge/status-development-orange.svg)
[![codecov](https://codecov.io/gh/whitemech/LTLf2DFA/branch/master/graph/badge.svg)](https://codecov.io/gh/whitemech/LTLf2DFA)
[![](https://img.shields.io/badge/flake8-checked-blueviolet)](https://img.shields.io/badge/flake8-checked-blueviolet)
[![](https://img.shields.io/badge/mypy-checked-blue)](https://img.shields.io/badge/mypy-checked-blue)
[![](https://img.shields.io/badge/license-LGPLv3%2B-blue)](https://img.shields.io/badge/license-LGPLv3%2B-blue)

LTL<sub>f</sub>2DFA is a tool that transforms an LTL<sub>f</sub> or a PLTL<sub>f</sub> formula into a minimal 
Deterministic Finite state Automaton (DFA) using [MONA](http://www.brics.dk/mona/).

It is also available online at [ltlf2dfa.diag.uniroma1.it](http://ltlf2dfa.diag.uniroma1.it).

## Prerequisites

This tool uses MONA for the generation of the DFA. Hence, you should first install MONA with all its dependencies on 
your system following the instructions [here](http://www.brics.dk/mona/download.html).

This tool is also based on the following libraries:

- [lark-parser 0.8.5](https://pypi.org/project/lark-parser/)
- [sympy 1.5.1](https://pypi.org/project/sympy/)

They are automatically added while installing LTL<sub>f</sub>2DFA.

## Install

- from [PyPI](https://pypi.org/project/ltlf2dfa/):
```
pip install ltlf2dfa
```
- or, from source (`master` branch):
```
pip install git+https://github.com/whitemech/LTLf2DFA.git
```

- or, clone the repository and install:
```
git clone htts://github.com/whitemech/LTLf2DFA.git
cd ltlf2dfa
pip install .
```
## How To Use

- Parse an LTL<sub>f</sub> formula:
```python
from ltlf2dfa.parser.ltlf import LTLfParser

parser = LTLfParser()
formula_str = "G(a -> X b)"
formula = parser(formula_str)       # returns an LTLfFormula

print(formula)                      # prints "G(a -> X (b))"
```
- Or, parse a PLTL<sub>f</sub> formula:
```python
from ltlf2dfa.parser.pltlf import PLTLfParser

parser = PLTLfParser()
formula_str = "H(a -> Y b)"
formula = parser(formula_str)       # returns a PLTLfFormula

print(formula)                      # prints "H(a -> Y (b))"
```
- Translate a formula to the corresponding DFA automaton:
```python
dfa = formula.to_dfa()
print(dfa)                          # prints the DFA in DOT format
```
## Features

* Syntax and parsing support for the following formal languages:
    * Propositional Logic;
    * Linear Temporal Logic on Finite Traces;
    * Pure-Past Linear Temporal Logic on Finite Traces.

* Conversion from LTL<sub>f</sub>/PLTL<sub>f</sub> formula to MONA (First-order Logic)

**NOTE**: LTL<sub>f</sub>2DFA accepts either LTL<sub>f</sub> formulas or PLTL<sub>f</sub> formulas, i.e., formulas that 
have only past, only future or none operators.

## Tests

To run tests: `tox`

To run only the code tests: `tox -e py37`

To run only the code style checks: `tox -e flake8`

## Docs

To build the docs: `mkdocs build`

To view documentation in a browser: `mkdocs serve`
and then go to [http://localhost:8000](http://localhost:8000)

## License

LTL<sub>f</sub>2DFA is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2018-2020 WhiteMech

## Author

[Francesco Fuggitti](https://francescofuggitti.github.io/)
