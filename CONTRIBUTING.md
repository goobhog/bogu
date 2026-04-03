# Contributing to Bogu

Thank you for checking out Bogu; any help on the project is much appreciated, regardless of experience!

## The Philosophy
Bogu was conceived to be a tool for thought and experimentation within a combinatoric and symbolic framework. However, please let your imagination run wild, my goal would be for this to be a highly collaborative project where all ideas are encouraged.

## Getting Set Up for Dev
1. Fork the repo and clone it locally.
2. Make sure you have SBCL and Csound installed.
3. Make your changes on a new branch.

## Running the Tests
Bogu has a diagnostic suite to ensure the AST parser and Type Checker remain mathematically sound. Before you submit a Pull Request, try to ensure the tests pass.

Open your REPL, load Bogu, and run:
```lisp
(in-package :bogu)
(load "tests.lisp")
(run-bogu-tests)
```
If you see `SUCCESS: All tests passed!`, you're in the money!

## Where to start?
Look for issues tagged with `good first issue`. If you want to add a new synthesizer, look at `audio/synth-defs.lisp`. If you want to add a new musical tree-transformer, look at `stdlib/commands.lisp`. I'm learning as I go and am by no means an expert, but I'm happy to help anyone through their first commit!