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
    <img alt="test" src="https://github.com/whitemech/LTLf2DFA/actions/workflows/ci.yml/badge.svg">
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

## Installation

Install the latest release from PyPI:

```bash
pip install ltlf2dfa
```

When using uv in another Python project:

```bash
uv add ltlf2dfa
```

Install the latest development version from GitHub:

```bash
pip install git+https://github.com/whitemech/LTLf2DFA.git
```

Alternatively, clone the repository:

```bash
git clone https://github.com/whitemech/LTLf2DFA.git
cd LTLf2DFA
uv sync
```

## Quickstart

You can use the LTL<sub>f</sub>2DFA package in two ways: as a library, and as a CLI tool.


### As a Library

- Parse an LTL<sub>f</sub> formula:
```python
from ltlf2dfa.parser.ltlf import LTLfParser

parser = LTLfParser()
formula_str = "G(a -> X b)"
formula = parser(formula_str)  # returns an LTLfFormula

print(formula)  # prints "G(a -> X (b))"
```
- Or, parse a PPLTL formula:
```python
from ltlf2dfa.parser.ppltl import PPLTLParser

parser = PPLTLParser()
formula_str = "H(a -> Y b)"
formula = parser(formula_str)  # returns a PPLTLFormula

print(formula)  # prints "H(a -> Y (b))"
```
- Translate a formula to the corresponding DFA automaton:
```python
dfa = formula.to_dfa()
print(dfa)  # prints the DFA in DOT format
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

### Requirements

To work on LTLf2DFA, you need:

- Python 3.11 or later
- [uv](https://docs.astral.sh/uv/)
- [MONA](https://www.brics.dk/mona/)

Clone the repository and synchronize the development environment:

```bash
git clone https://github.com/whitemech/LTLf2DFA.git
cd LTLf2DFA
uv sync
```

There is no need to activate the virtual environment manually. Project commands can be run with `uv run`.

### Tests

Run the complete test suite:

```bash
uv run pytest
```

Run tests with coverage:

```bash
uv run pytest \
  --cov=ltlf2dfa \
  --cov-report=term-missing \
  --cov-report=xml
```

### Code quality

Check the code with Ruff:

```bash
uv run ruff check .
uv run ruff format --check .
```

Apply automatic linting and formatting fixes:

```bash
uv run ruff check . --fix
uv run ruff format .
```

### Pre-commit hooks

Install the Git pre-commit hooks:

```bash
uv run pre-commit install
```

Run all hooks manually:

```bash
uv run pre-commit run --all-files
```

### Documentation

Build the documentation:

```bash
uv run mkdocs build --clean --strict
```

Serve the documentation locally:

```bash
uv run mkdocs serve
```

Then open <http://localhost:8000> in your browser.

### Build

Build the source distribution and wheel:

```bash
uv build
```

The generated artifacts are written to the `dist/` directory.

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
