# -*- coding: utf-8 -*-
"""Test the ltlf2dfa tool."""

from ltlf2dfa.parser.ltlf import LTLfParser
from ltlf2dfa.parser.pltlf import PLTLfParser


def test_ltlf_dfa():
    parser = LTLfParser()

    f = parser("a")
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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


def test_pltlf_dfa():
    parser = PLTLfParser()

    f = parser("a")
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
    dfa = f.to_dfa()
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
