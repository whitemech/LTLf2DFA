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
"""Test LTLf."""
import os

import lark
import pytest

from ltlf2dfa.ltlf import (
    LTLfAlways,
    LTLfAnd,
    LTLfAtomic,
    LTLfEquivalence,
    LTLfEventually,
    LTLfFalse,
    LTLfImplies,
    LTLfLast,
    LTLfNext,
    LTLfNot,
    LTLfOr,
    LTLfRelease,
    LTLfTrue,
    LTLfUntil,
    LTLfWeakNext,
)
from ltlf2dfa.parser.ltlf import LTLfParser
from ltlf2dfa.qltlf import QLTLfExist, QLTLfForAll

# from .conftest import LTLfFixtures
from .parsing import ParsingCheck

# from ltlf2dfa.pl import PLAtomic, PLTrue, PLFalse, PLAnd, PLOr


def test_parser():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]

    assert parser("EX(a)") == QLTLfExist(a)
    assert parser("EX a") == QLTLfExist(a)
    # assert parser("EX(EX a)") == QLTLfExist(QLTLfExist(a))
    assert parser("EX(a & b)") == QLTLfExist(LTLfAnd([a, b]))
    assert parser("EX(a | b)") == QLTLfExist(LTLfOr([a, b]))
    assert parser("EX(a & !b)") == QLTLfExist(LTLfAnd([a, LTLfNot(b)]))
    assert parser("EX(a -> !b)") == QLTLfExist(LTLfImplies([a, LTLfNot(b)]))
    assert parser("EX(a <-> !b & c)") == QLTLfExist(
        LTLfEquivalence([a, LTLfAnd([LTLfNot(b), c])])
    )

    assert parser("ALL(a)") == QLTLfForAll(a)
    assert parser("ALL a") == QLTLfForAll(a)
    assert parser("ALL(a & b)") == QLTLfForAll(LTLfAnd([a, b]))
    assert parser("ALL(a | b)") == QLTLfForAll(LTLfOr([a, b]))
    assert parser("ALL(a & !b)") == QLTLfForAll(LTLfAnd([a, LTLfNot(b)]))
    assert parser("ALL(a -> !b)") == QLTLfForAll(LTLfImplies([a, LTLfNot(b)]))
    assert parser("ALL(a <-> !b & c)") == QLTLfForAll(
        LTLfEquivalence([a, LTLfAnd([LTLfNot(b), c])])
    )

    assert parser("F(EX(a))") == LTLfEventually(QLTLfExist(a))
    assert parser("G(EX a)") == LTLfAlways(QLTLfExist(a))
    assert parser("EX(a & b) U F(EX(a))") == LTLfUntil(
        [QLTLfExist(LTLfAnd([a, b])), LTLfEventually(QLTLfExist(a))]
    )
    assert parser("X EX(a | b)") == LTLfNext(QLTLfExist(LTLfOr([a, b])))

    assert parser("F(ALL(a))") == LTLfEventually(QLTLfForAll(a))
    assert parser("G(ALL a)") == LTLfAlways(QLTLfForAll(a))
    assert parser("ALL(a & b) U F(ALL(a))") == LTLfUntil(
        [QLTLfForAll(LTLfAnd([a, b])), LTLfEventually(QLTLfForAll(a))]
    )
    assert parser("X ALL(a | b)") == LTLfNext(QLTLfForAll(LTLfOr([a, b])))


def test_nnf():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]

    # Exists
    f = parser("!(EX a)")
    assert f.to_nnf() == QLTLfForAll(LTLfNot(a))

    f = parser("!(EX !(a | b))")
    assert f.to_nnf() == QLTLfForAll(LTLfOr([a, b]))

    f = parser("!(EX !F(a | b))")
    assert f.to_nnf() == QLTLfForAll(LTLfEventually(LTLfOr([a, b])))

    # ForAll
    f = parser("!(ALL a)")
    assert f.to_nnf() == QLTLfExist(LTLfNot(a))


def test_mona():
    parser = LTLfParser()

    f = parser("EX a")
    assert f.to_mona(v="0", w="j") == "(ex1 w_1, j: 0<=w_1 & w_1<=j & (0 in A))"

    f = parser("EX(X a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(ex1 w_1, j: 0<=w_1 & w_1<=j & (ex1 v_1: v_1=0+1 & (v_1 in A)))"
    )

    f = parser("EX(F a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(ex1 w_1, j: 0<=w_1 & w_1<=j & (ex1 v_1: 0<=v_1&v_1<=w_1 & (v_1 in A) & (all1 v_2: 0<=v_2&v_2<v_1 => true)))"
    )

    f = parser("EX(G a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(ex1 w_1, j: 0<=w_1 & w_1<=j & ((ex1 v_1: 0<=v_1&v_1<=w_1 & false & (all1 v_2: 0<=v_2&v_2<=v_1 => (v_2 in A))) | "
        "(all1 v_2: 0<=v_2&v_2<=w_1 => (v_2 in A))))"
    )

    f = parser("EX(EX a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(ex1 w_1, j: 0<=w_1 & w_1<=j & (ex1 w_2: 0<=w_2 & w_2<=w_1 & (0 in A)))"
    )

    f = parser("F(EX(a))")
    assert (
        f.to_mona(v="0", w="j")
        == "(ex1 v_1, j: 0<=v_1&v_1<=j & (ex1 w_1: v_1<=w_1 & w_1<=j & (v_1 in A)) & (all1 v_2: 0<=v_2&v_2<v_1 => true))"
    )

    f = parser("ALL a")
    assert f.to_mona(v="0", w="j") == "(all1 w_1, j: 0<=w_1 & w_1<=j => (0 in A))"

    f = parser("ALL(X a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(all1 w_1, j: 0<=w_1 & w_1<=j => (ex1 v_1: v_1=0+1 & (v_1 in A)))"
    )

    f = parser("ALL(F a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(all1 w_1, j: 0<=w_1 & w_1<=j => (ex1 v_1: 0<=v_1&v_1<=w_1 & (v_1 in A) & (all1 v_2: 0<=v_2&v_2<v_1 => true)))"
    )

    f = parser("ALL(ALL a)")
    assert (
        f.to_mona(v="0", w="j")
        == "(all1 w_1, j: 0<=w_1 & w_1<=j => (all1 w_2: 0<=w_2 & w_2<=w_1 => (0 in A)))"
    )

    f = parser("F(ALL(a))")
    assert (
        f.to_mona(v="0", w="j")
        == "(ex1 v_1, j: 0<=v_1&v_1<=j & (all1 w_1: v_1<=w_1 & w_1<=j => (v_1 in A)) & (all1 v_2: 0<=v_2&v_2<v_1 => true))"
    )

    f = parser("G(a -> EX(b))")
    assert (
        f.to_mona(v="0", w="j")
        == "((ex1 v_1, j: 0<=v_1&v_1<=j & false & (all1 v_2: 0<=v_2&v_2<=v_1 => (~((v_2 in A)) | (ex1 w_1: v_2<=w_1 & "
        "w_1<=j & (v_2 in B))))) | (all1 v_2, j: 0<=v_2&v_2<=j => (~((v_2 in A)) | (ex1 w_1: v_2<=w_1 & w_1<=j & "
        "(v_2 in B)))))"
    )


# @pytest.fixture(scope="session", params=LTLfFixtures.ltlf_formulas)
# def ltlf_formula_automa_pair(request):
#     formula_obj = parser(request.param)
#     automaton = formula_obj.to_automaton()
#     return formula_obj, automaton
#
#
# @pytest.fixture(scope="session", params=LTLfFixtures.ltlf_formulas)
# def ltlf_formula_nnf_pair(request):
#     formula_obj = parser(request.param)
#     nnf = formula_obj.to_nnf()
#     return formula_obj, nnf
