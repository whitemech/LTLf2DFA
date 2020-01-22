#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""This is the command line tool for translating LTLf and PLTLf formulas to DFA."""
import click

from ltlf2dfa.core import HOA
from ltlf2dfa.parsers import HOAParser


@click.command()
@click.argument('infile', type=click.Path(exists=True, file_okay=True, dir_okay=False, readable=True))
@click.option('-o', '--output', type=click.Path(file_okay=True, dir_okay=False, writable=True),
              help='Path to the output file.')
def main(infile, output):
    """From HOA to DOT format."""
    input_string = open(infile).read()
    parser = HOAParser()
    hoa = parser(input_string)  # type: HOA
    print(hoa.dumps(), file=open(output, "w") if output is not None else None)


if __name__ == '__main__':
    main()  # pragma: no cover