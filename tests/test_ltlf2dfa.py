# -*- coding: utf-8 -*-
"""Test the ltlf2dfa tool."""

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
Rejecting states: 1 2 
Don't-care states: 0 

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
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 3 [label="a"];
 2 -> 2 [label="true"];
 3 -> 2 [label="~a"];
 3 -> 3 [label="a"];
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
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a | ~b"];
 1 -> 3 [label="a & b"];
 2 -> 2 [label="~a | ~b"];
 2 -> 3 [label="a & b"];
 3 -> 3 [label="true"];
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
 node [shape = doublecircle]; 4;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 3 [label="a & ~b"];
 1 -> 4 [label="a & b"];
 2 -> 2 [label="true"];
 3 -> 2 [label="~a"];
 3 -> 3 [label="a & ~b"];
 3 -> 4 [label="a & b"];
 4 -> 2 [label="~a"];
 4 -> 4 [label="a"];
}"""
    assert dfa == expected


def test_ltlf_mona_dfa():
    parser = LTLfParser()

    f = parser("a")
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected_mona = """DFA for formula with free variables: A 
Initial state: 0
Accepting states: 3 
Rejecting states: 1 2 
Don't-care states: 0 

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
Rejecting states: 
Don't-care states: 0 

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
Rejecting states: 1 
Don't-care states: 0 

Automaton has 2 states and 1 BDD-node
Transitions:
State 0:  -> state 1
State 1:  -> state 1
Formula is unsatisfiable

A counter-example of least length (0) is:"""
    assert mona_dfa == expected_mona

    f = parser("G a")
    mona_dfa = f.to_dfa(mona_dfa_out=True)
    expected_mona = """DFA for formula with free variables: A 
Initial state: 0
Accepting states: 3 
Rejecting states: 2 
Don't-care states: 0 1 

Automaton has 4 states and 4 BDD-nodes
Transitions:
State 0: X -> state 1
State 1: 0 -> state 2
State 1: 1 -> state 3
State 2: X -> state 2
State 3: 0 -> state 2
State 3: 1 -> state 3
A counter-example of least length (1) is:
A               X 0

A = {}

A satisfying example of least length (1) is:
A               X 1

A = {0}"""
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
 node [shape = doublecircle]; 3;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 3 [label="a"];
 2 -> 2 [label="true"];
 3 -> 3 [label="true"];
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

    f = parser("H a")
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
 1 -> 3 [label="a"];
 2 -> 2 [label="true"];
 3 -> 2 [label="~a"];
 3 -> 3 [label="a"];
}"""
    assert dfa == expected

    f = parser("O(a & b)")
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
 1 -> 2 [label="~a | ~b"];
 1 -> 3 [label="a & b"];
 2 -> 2 [label="~a | ~b"];
 2 -> 3 [label="a & b"];
 3 -> 3 [label="true"];
}"""
    assert dfa == expected

    f = parser("Y(a)")
    dfa = f.to_dfa(mona_dfa_out=False)
    expected = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle]; 4; 5;
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 2 [label="~a"];
 1 -> 3 [label="a"];
 2 -> 2 [label="~a"];
 2 -> 3 [label="a"];
 3 -> 4 [label="~a"];
 3 -> 5 [label="a"];
 4 -> 2 [label="~a"];
 4 -> 3 [label="a"];
 5 -> 4 [label="~a"];
 5 -> 5 [label="a"];
}"""
    assert dfa == expected

    f = parser("a S b")
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
 1 -> 2 [label="~b"];
 1 -> 3 [label="b"];
 2 -> 2 [label="~b"];
 2 -> 3 [label="b"];
 3 -> 2 [label="~a & ~b"];
 3 -> 3 [label="a | b"];
}"""
    assert dfa == expected

    f = parser("H(a) & O(b)")
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
 1 -> 2 [label="~a"];
 1 -> 3 [label="a & ~b"];
 1 -> 4 [label="a & b"];
 2 -> 2 [label="true"];
 3 -> 2 [label="~a"];
 3 -> 3 [label="a & ~b"];
 3 -> 4 [label="a & b"];
 4 -> 2 [label="~a"];
 4 -> 4 [label="a"];
}"""
    assert dfa == expected
