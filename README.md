# Bogu 
**A fully symbolic music composition language.**

Bogu is a high-level symbolic language designed for algorithmic composition
and generative sound design. It treats music as a tree of symbols that
can be transformed, inverted, and evolved before being rendered by a
high-fidelity Csound audio engine.

## Installation
1.  Install [SBCL](http://www.sbcl.org/) and [Csound](https://csound.com/).
2.  Clone this repository: `git clone git@github.com:your-username/bogu.git`

## Quick Start
Load the environment and enter the REPL:
```lisp
(load "bogu/boot.lisp")
(bogu)

Try a simultaneous volume sweep and aleatoric cloud:
bogu> sweep vol 20 80 [ fluid 4 11 [ c4 eb4 g4 bb4 ] ]
bogu> play