<h1 align="center">
  <img src="http://ltlf2dfa.diag.uniroma1.it/static/images/logo-ltlf2dfa.svg">
</h1>

<p align="center">
  <a href="https://pypi.org/project/ltlf2dfa">
    <img alt="PyPI" src="https://img.shields.io/pypi/v/ltlf2dfa">
  </a>
  <a href="https://pypi.org/project/ltlf2dfa">
    <img alt="PyPI - Python Version" src="https://img.shields.io/pypi/pyversions/ltlf2dfa" />
  </a>
  <a href="">
    <img alt="PyPI - Implementation" src="https://img.shields.io/pypi/implementation/ltlf2dfa">
  </a>
  <a href="https://github.com/whitemech/ltlf2dfa/blob/master/LICENSE">
    <img alt="GitHub" src="https://img.shields.io/badge/license-LGPLv3%2B-blue">
  </a>  
  <a href="">
    <img alt="PyPI - Wheel" src="https://img.shields.io/pypi/wheel/ltlf2dfa">
  </a>
</p>
<p align="center">
  <a href="">
    <img alt="test" src="https://github.com/whitemech/ltlf2dfa/workflows/test/badge.svg">
  </a>
  <a href="">
    <img alt="lint" src="https://github.com/whitemech/ltlf2dfa/workflows/lint/badge.svg">
  </a>
  <a href="">
    <img alt="docs" src="https://github.com/whitemech/ltlf2dfa/workflows/docs/badge.svg">
  </a>
  <a href="https://codecov.io/gh/whitemech/pddl">
    <img alt="codecov" src="https://codecov.io/gh/whitemech/ltlf2dfa/branch/master/graph/badge.svg">
  </a>
  <a href="">
    <img alt="PyPI - Status" src="https://img.shields.io/pypi/status/ltlf2dfa" />
  </a>
</p>
<p align="center">
  <a href="https://img.shields.io/badge/flake8-checked-blueviolet">
    <img alt="" src="https://img.shields.io/badge/flake8-checked-blueviolet">
  </a>
  <a href="https://img.shields.io/badge/mypy-checked-blue">
    <img alt="" src="https://img.shields.io/badge/mypy-checked-blue">
  </a>
  <a href="https://img.shields.io/badge/isort-checked-yellow">
    <img alt="isort" src="https://img.shields.io/badge/isort-checked-yellow" />
  </a>
  <a href="https://img.shields.io/badge/code%20style-black-black">
    <img alt="black" src="https://img.shields.io/badge/code%20style-black-black" />
  </a>
  <a href="https://www.mkdocs.org/">
    <img alt="" src="https://img.shields.io/badge/docs-mkdocs-9cf">
</p>
<p align="center">
  </a>
    <a href="https://zenodo.org/badge/DOI/10.5281/zenodo.3888410.svg">
    <img alt="" src="https://zenodo.org/badge/DOI/10.5281/zenodo.3888410.svg">
  </a>
</p>

---

LTL<sub>f</sub>2DFA is a tool that transforms an LTL<sub>f</sub> or a PLTL<sub>f</sub> formula into a minimal 
Deterministic Finite state Automaton (DFA) using [MONA](http://www.brics.dk/mona/).

It is also available online at [http://ltlf2dfa.diag.uniroma1.it](http://ltlf2dfa.diag.uniroma1.it).

## Prerequisites

This tool uses MONA for the generation of the DFA. Hence, you should first install MONA with all its dependencies on 
your system following the instructions [here](http://www.brics.dk/mona/download.html).

This tool is also based on the following libraries:

- [lark-parser 0.9.0](https://pypi.org/project/lark-parser/)
- [sympy 1.6.1](https://pypi.org/project/sympy/)

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
git clone https://github.com/whitemech/LTLf2DFA.git
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

To run only the code tests: `tox -e py3.7`

To run only the code style checks: `tox -e flake8`

## Docs

To build the docs: `mkdocs build`

To view documentation in a browser: `mkdocs serve`
and then go to [http://localhost:8000](http://localhost:8000)

## License

LTL<sub>f</sub>2DFA is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2018-2022 WhiteMech @ Sapienza University

## Citing
If you are interested in this tool, and you use it in your own work, please consider citing it.

## Author

[Francesco Fuggitti](https://francescofuggitti.github.io/)
