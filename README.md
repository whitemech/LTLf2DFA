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
  <a href="https://github.com/whitemech/ltlf2dfa/blob/master/LICENSE">
    <img alt="GitHub" src="https://img.shields.io/badge/license-LGPLv3%2B-blue">
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
</p>
<p align="center">
  </a>
    <a href="https://zenodo.org/badge/DOI/10.5281/zenodo.3888410.svg">
    <img alt="" src="https://zenodo.org/badge/DOI/10.5281/zenodo.3888410.svg">
  </a>
</p>

---

LTL<sub>f</sub>2DFA is a tool that transforms an LTL<sub>f</sub> or a PPLTL formula into a minimal 
Deterministic Finite state Automaton (DFA) using [MONA](http://www.brics.dk/mona/).

It is also available online at [http://ltlf2dfa.diag.uniroma1.it](http://ltlf2dfa.diag.uniroma1.it).

## Prerequisites

### MONA Installation

LTL<sub>f</sub>2DFA relies on the MONA tool for the generation of the DFA. 
Please, make sure you have the MONA tool installed on your system before running LTL<sub>f</sub>2DFA. 
You can follow the instructions [here](http://www.brics.dk/mona/download.html) to get MONA.

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

## Quickstart

You can use the LTL<sub>f</sub>2DFA package in two ways: as a library, and as a CLI tool.


### As a Library

- Parse an LTL<sub>f</sub> formula:
```python
from ltlf2dfa.parser.ltlf import LTLfParser

parser = LTLfParser()
formula_str = "G(a -> X b)"
formula = parser(formula_str)       # returns an LTLfFormula

print(formula)                      # prints "G(a -> X (b))"
```
- Or, parse a PPLTL formula:
```python
from ltlf2dfa.parser.ppltl import PPLTLParser

parser = PPLTLParser()
formula_str = "H(a -> Y b)"
formula = parser(formula_str)       # returns a PPLTLFormula

print(formula)                      # prints "H(a -> Y (b))"
```
- Translate a formula to the corresponding DFA automaton:
```python
dfa = formula.to_dfa()
print(dfa)                          # prints the DFA in DOT format
```

### As a CLI Interface
```python
ltlf2dfa -l {ltlf | ppltl} -f <path/to/formula>
```

## Features

* Syntax and parsing support for the following formal languages:
    * Propositional Logic;
    * Linear Temporal Logic on Finite Traces;
    * Pure-Past Linear Temporal Logic on Finite Traces.

* Conversion from LTL<sub>f</sub>/PPLTL formula to MONA (First-order Logic)

**NOTE**: LTL<sub>f</sub>2DFA accepts either LTL<sub>f</sub> formulas or PPLTL formulas, i.e., formulas that 
have only past, only future or none operators.

## Development

If you want to contribute, set up your development environment as follows:

- Intall [Poetry](https://python-poetry.org)
- Clone the repository: `git clone https://github.com/whitemech/LTLf2DFA.git && cd LTLf2DFA`
- Install the dependencies: `poetry shell && poetry install`

## Tests

To run tests: `tox`

To run only the code tests: `tox -e py38`

To run only the code style checks: `tox -e flake8`

## Docs

To build the docs: `mkdocs build`

To view documentation in a browser: `mkdocs serve`
and then go to [http://localhost:8000](http://localhost:8000)

## License

LTL<sub>f</sub>2DFA is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2018-2023 WhiteMech

## Citing
If you use LTL<sub>f</sub>2DFA in your research, please consider citing it with the following bibtex:
```
@software{fuggitti-ltlf2dfa,
  author       = {Francesco Fuggitti},
  title        = {LTLf2DFA},
  month        = {March},
  year         = {2019},
  publisher    = {Zenodo},
  version      = {1.0.3},
  doi          = {10.5281/zenodo.3888410},
  url_code    = {https://github.com/whitemech/LTLf2DFA},
  url_website = {http://ltlf2dfa.diag.uniroma1.it},
}
```

## Author

[Francesco Fuggitti](https://francescofuggitti.github.io/)
