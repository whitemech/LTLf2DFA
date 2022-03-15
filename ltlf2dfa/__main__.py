#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This file is part of ltlf2dfa.
#
# ltlf2dfa is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ltlf2dfa is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ltlf2dfa.  If not, see <https://www.gnu.org/licenses/>.
#

"""This is the command line tool for the LTLf2DFA tool."""

import click  # type: ignore

from ltlf2dfa.base import Logic
from ltlf2dfa.parser.ltlf import LTLfParser
from ltlf2dfa.parser.pltlf import PLTLfParser


def execute(logic, formula):
    if logic == Logic.LTLf:
        f_parser = LTLfParser()
        try:
            parsed_formula = f_parser(formula)
        except Exception as e:
            raise ValueError(e)
    elif logic == Logic.PLTLf:
        p_parser = PLTLfParser()
        try:
            parsed_formula = p_parser(formula)
        except Exception as e:
            raise ValueError(e)
    else:
        raise ValueError("Formula has mixed future/past operators.")
    dfa = parsed_formula.to_dfa(mona_dfa_out=True)
    print(dfa)


@click.command()
@click.option("-l", "--logic", type=click.Choice(["ltlf", "pltlf"], case_sensitive=False))
@click.option(
    "-f",
    "--formula",
    required=True,
    help="Path to the LTLf/PLTLf formula file.",
    type=click.Path(exists=True, readable=True),
)
def main(logic, formula):
    """From LTLf/PLTLf formulas to DFA."""
    execute(Logic(logic.lower), formula)


if __name__ == "__main__":
    main()  # pragma: no cover
