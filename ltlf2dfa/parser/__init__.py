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
"""This module contains the implementation of the parsers for the supported logic formalisms."""

from ltlf2dfa import _ROOT_PATH

PARSERS_DIRECTORY = _ROOT_PATH / "ltlf2dfa" / "parser"
PL_GRAMMAR_FILE = PARSERS_DIRECTORY / "pl.lark"
LTLF_GRAMMAR_FILE = PARSERS_DIRECTORY / "ltlf.lark"
PPLTL_GRAMMAR_FILE = PARSERS_DIRECTORY / "ppltl.lark"
