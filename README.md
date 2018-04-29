# LTLf2FOL
[![ciaio](https://img.shields.io/badge/python-3.6-blue.svg)]()

LTLf2FOL is a simple python script that translates an LTLf formula to a FOL formula.

## Getting Started

### Prerequisites

This script is based on `ply 3.11` library which is an implementation of Lex and Yacc for Python.

Install it by typing the following (I strongly suggest you to use a virtual environment):

```
pip install ply
```

## How To Use It

- Download the LTLf2FOL.zip repository or clone it locally:
```
git clone https://github.com/Francesco17/LTLf2FOL.git
```
- Unzip it
- Type an LTLf formula in the `ltl_in.txt` file
- Enter the folder with the terminal and run the program
```
python3 ltl_parser.py
```
- The translation will be in the `fol_out.txt` file.

## Syntax

The syntax employed by this parser is the following:

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

Also parentheses `(` and `)` can be used.

## Examples
If you would like to try it, you can find some examples in `examples.txt` used as a test.

## Built With

- [ply 3.11](https://pypi.org/project/ply/)

## Author

- [Francesco Fuggitti](https://www.linkedin.com/in/francesco-fuggitti-b78336131/)

## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/Francesco17/LTLf2FOL/blob/master/LICENSE) file for details

## Contacts

If, for any reason, you are interested in feel free to contact me by email.
