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
"""Test the ltlf2dfa tool."""

from sympy import And, Not, Or, Symbol, simplify, symbols

from ltlf2dfa.ltlf2dfa import simplify_guard, ter2symb
from ltlf2dfa.parser.ltlf import LTLfParser
from ltlf2dfa.parser.pltlf import PLTLfParser


def test_ltlf_dfa():
    parser = LTLfParser()

    f = parser("a")
    dfa = f.to_dfa(mona_dfa_out=False)
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 3 [label="a"];
 2 -> 2 [label="true"];
 3 -> 3 [label="true"];
}"""
    expected_mona = """DFA for formula with free variables: A 
Initial state: 0
Accepting states: 3 
Rejecting states: 0 1 2 

Automaton has 4 states and 4 BDD-nodes
Transitions:
State 0: X -> state 1
State 1: 0 -> state 2
State 1: 1 -> state 3
State 2: X -> state 2
State 3: X -> state 3
A counter-example of least length (0) is:
A               X 

A = {}

A satisfying example of least length (1) is:
A               X 1

A = {0}"""

    assert dfa == expected
    assert mona_dfa == expected_mona

    f = parser("true")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 1;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="true"];
}"""
    assert dfa == expected

    f = parser("false")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle];
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="true"];
}"""
    assert dfa == expected

    f = parser("G a")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 1;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 1 [label="a"];
 2 -> 2 [label="true"];
}"""
    assert dfa == expected

    f = parser("F(a & b)")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 2;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="~a | ~b"];
 1 -> 2 [label="a & b"];
 2 -> 2 [label="true"];
}"""
    assert dfa == expected

    f = parser("X(a)")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 4;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="true"];
 2 -> 3 [label="~a"];
 2 -> 4 [label="a"];
 3 -> 3 [label="true"];
 4 -> 4 [label="true"];
}"""
    assert dfa == expected

    f = parser("a U b")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected1 = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a & ~b"];
 1 -> 3 [label="b"];
 1 -> 4 [label="a & ~b"];
 2 -> 2 [label="true"];
 3 -> 3 [label="true"];
 4 -> 2 [label="~a & ~b"];
 4 -> 3 [label="b"];
 4 -> 4 [label="a & ~b"];
}"""
    expected2 = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a & ~b"];
 1 -> 3 [label="a & ~b"];
 1 -> 4 [label="b"];
 2 -> 2 [label="true"];
 3 -> 2 [label="~a & ~b"];
 3 -> 3 [label="a & ~b"];
 3 -> 4 [label="b"];
 4 -> 4 [label="true"];
}"""
    assert dfa == expected1 or expected2

    f = parser("G(a) & F(b)")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 1 [label="a & ~b"];
 1 -> 3 [label="a & b"];
 2 -> 2 [label="true"];
 3 -> 2 [label="~a"];
 3 -> 3 [label="a"];
}"""
    assert dfa == expected


def test_ltlf_mona_dfa():
    parser = LTLfParser()

    f = parser("a")
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected_mona = """DFA for formula with free variables: A 
Initial state: 0
Accepting states: 3 
Rejecting states: 0 1 2 

Automaton has 4 states and 4 BDD-nodes
Transitions:
State 0: X -> state 1
State 1: 0 -> state 2
State 1: 1 -> state 3
State 2: X -> state 2
State 3: X -> state 3
A counter-example of least length (0) is:
A               X 

A = {}

A satisfying example of least length (1) is:
A               X 1

A = {0}"""

    assert mona_dfa == expected_mona

    f = parser("true")
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected_mona = """DFA for formula with free variables: 
Initial state: 0
Accepting states: 1 
Rejecting states: 0 

Automaton has 2 states and 1 BDD-node
Transitions:
State 0:  -> state 1
State 1:  -> state 1
Formula is valid
A satisfying example of least length (0) is:"""
    assert mona_dfa == expected_mona

    f = parser("false")
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected_mona = """DFA for formula with free variables: 
Initial state: 0
Accepting states: 
Rejecting states: 0 

Automaton has 1 state and 1 BDD-node
Transitions:
State 0:  -> state 0
Formula is unsatisfiable

A counter-example of least length (0) is:"""
    assert mona_dfa == expected_mona

    f = parser("G a")
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected_mona = """DFA for formula with free variables: A 
Initial state: 0
Accepting states: 1 
Rejecting states: 0 2 

Automaton has 3 states and 3 BDD-nodes
Transitions:
State 0: X -> state 1
State 1: 0 -> state 2
State 1: 1 -> state 1
State 2: X -> state 2
A counter-example of least length (1) is:
A               X 0

A = {}

A satisfying example of least length (0) is:
A               X 

A = {}"""
    assert mona_dfa == expected_mona

    f1 = parser("F(WX(false))")
    f2 = parser("F(!(X(!(false))))")
    mona_dfa_1 = f1.to_dfa(mona_dfa_out=True)
    mona_dfa_2 = f2.to_dfa(mona_dfa_out=True)
    assert mona_dfa_1 == mona_dfa_2

    f1 = parser("F(b & WX false) -> F(a & (WX false | X(WX false)))")
    f2 = parser(
        "((! (F (! ((! b) || (X (! false)))))) || (F (! ((! a) || (! ((! (X (! false))) || (X (! (X (! false))))))))))"
    )
    mona_dfa_1 = f1.to_dfa(mona_dfa_out=True)
    mona_dfa_2 = f2.to_dfa(mona_dfa_out=True)
    assert mona_dfa_1 == mona_dfa_2


def test_pltlf_dfa():
    parser = PLTLfParser()

    f = parser("a")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 2;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="~a"];
 1 -> 2 [label="a"];
 2 -> 1 [label="~a"];
 2 -> 2 [label="a"];
}"""
    assert dfa == expected

    f = parser("true")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 1;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="true"];
}"""
    assert dfa == expected

    f = parser("O(a) -> O(b)")

    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 1; 2;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="~a & ~b"];
 1 -> 2 [label="b"];
 1 -> 3 [label="a & ~b"];
 2 -> 2 [label="true"];
 3 -> 3 [label="~b"];
 3 -> 2 [label="b"];
}"""
    assert dfa == expected


def test_ter2symb():
    ap = symbols("a b c")

    tern_0 = "X"
    actual = ter2symb(None, tern_0)
    expected = And()
    assert expected == actual

    tern_1 = "0X"
    actual = ter2symb(ap, tern_1)
    expected = And(Not(Symbol("a")))
    assert expected == actual

    tern_2 = "10"
    actual = ter2symb(ap, tern_2)
    expected = And(Symbol("a"), Not(Symbol("b")))
    assert expected == actual

    tern_3 = "0X1"
    actual = ter2symb(ap, tern_3)
    expected = And(Not(Symbol("a")), Symbol("c"))
    assert expected == actual

    ap = symbols("a b c d e f g h i")
    tern_4 = "0X110XX01"
    actual = ter2symb(ap, tern_4)
    expected = And(
        Not(Symbol("a")),
        Symbol("c"),
        Symbol("d"),
        Not(Symbol("e")),
        Not(Symbol("h")),
        Symbol("i"),
    )
    assert expected == actual


def test_simplify_guard():
    ap = symbols("a b c")

    tern_1 = "0XX"
    tern_2 = "10X"
    tern_3 = "110"
    sym_1 = ter2symb(ap, tern_1)
    sym_2 = ter2symb(ap, tern_2)
    sym_3 = ter2symb(ap, tern_3)

    actual = simplify_guard([sym_1, sym_2, sym_3])
    expected = Or(Not(Symbol("a")), Not(Symbol("b")), Not(Symbol("c")))
    assert expected == actual

    ap = symbols("a b c d e f g h")

    tern_1 = "0XXXXXXX"
    tern_2 = "10XXXXXX"
    tern_3 = "110XXXXX"
    tern_4 = "1110XXXX"
    tern_5 = "11110XXX"
    tern_6 = "111110XX"
    tern_7 = "1111110X"
    tern_8 = "11111110"
    sym_1 = ter2symb(ap, tern_1)
    sym_2 = ter2symb(ap, tern_2)
    sym_3 = ter2symb(ap, tern_3)
    sym_4 = ter2symb(ap, tern_4)
    sym_5 = ter2symb(ap, tern_5)
    sym_6 = ter2symb(ap, tern_6)
    sym_7 = ter2symb(ap, tern_7)
    sym_8 = ter2symb(ap, tern_8)

    actual = simplify_guard([sym_1, sym_2, sym_3, sym_4, sym_5, sym_6, sym_7, sym_8])
    expected = Or(
        Not(Symbol("a")),
        Not(Symbol("b")),
        Not(Symbol("c")),
        Not(Symbol("d")),
        Not(Symbol("e")),
        Not(Symbol("f")),
        Not(Symbol("g")),
        Not(Symbol("h")),
    )
    assert expected == actual

    sym_1 = Not(Symbol("a"))
    sym_2 = And(Symbol("a"), Not(Symbol("b")))
    sym_3 = And(Symbol("a"), Symbol("b"), Not(Symbol("c")))
    sym_4 = And(Symbol("a"), Symbol("b"), Symbol("c"), Not(Symbol("d")))
    sym_5 = And(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"), Not(Symbol("e")))
    sym_6 = And(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Not(Symbol("f")),
    )
    sym_7 = And(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Not(Symbol("g")),
    )
    sym_8 = And(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Not(Symbol("h")),
    )

    actual = simplify_guard([sym_1, sym_2, sym_3, sym_4, sym_5, sym_6, sym_7, sym_8])
    expected = Or(
        Not(Symbol("a")),
        Not(Symbol("b")),
        Not(Symbol("c")),
        Not(Symbol("d")),
        Not(Symbol("e")),
        Not(Symbol("f")),
        Not(Symbol("g")),
        Not(Symbol("h")),
    )
    assert expected == actual
