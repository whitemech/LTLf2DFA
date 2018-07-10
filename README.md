# LTL<sub>f</sub>2DFA
[![ciaio](https://img.shields.io/badge/python-3.6-blue.svg)]()

LTL<sub>f</sub>2DFA is a simple tool that processes an LTL<sub>f</sub> formula (with past or future operators) and generates the corresponding minimized DFA (Deterministic Finite state Automaton) using [MONA](http://www.brics.dk/mona/).
This tool is written in Python 3.6.

It is tested on Linux Ubuntu 16.04.

## Getting Started

### Requirements

This tool uses MONA for the generation of the DFA. Hence, you should first install all MONA dependencies following the instructions [here](http://www.brics.dk/mona/download.html).

This tool is also based on the following libraries:

- [ply 3.11](https://pypi.org/project/ply/)
- [graphviz 0.8.3](http://graphviz.org)
- [pydot 1.2.4](https://pypi.org/project/pydot/)
- [pyparsing 2.2.0](https://pypi.org/project/pyparsing/)

You should install them apart from `pyparsing`, which is automatically installed by `pydot`

## How To Use It

- Download the LTL<sub>f</sub>2DFA.zip repository or clone it locally:
```
git clone https://github.com/Francesco17/LTLf2DFA.git
```
- Unzip it
- Enter the folder with the terminal and run the program
```
python3 main.py [-h] [-d] "formula"
```
**Flags**:
```
-h, --help     Show help message and exit
-d, --declare  Compute DECLARE assumption for the formula. -- OPTIONAL
```
- You will get the DFA automaton in .dot format within the current folder.

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
<!---
## Examples
Type the following, where `path/to/file/containing/formula.txt` is a text file containing the LTL<sub>f</sub> formula.
```
python3 main.py [-h] path/to/file/containing/formula.txt
```
--->
## Author

[Francesco Fuggitti](https://www.linkedin.com/in/francesco-fuggitti-b78336131/)

## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/Francesco17/LTLf2FOL/blob/master/LICENSE) file for details

## Contacts

If, for any reason, you are interested in feel free to contact me by email.
