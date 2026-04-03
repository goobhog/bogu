# Bogu 🎛️ (λ)

> A symbolic, Lisp-based composition language powered by Csound.

[![Build Status](https://github.com/goobhog/bogu/actions/workflows/tests.yml/badge.svg)](https://github.com/goobhog/bogu/actions)


## What is Bogu?
Bogu is a micro-scale language designed for algorithmic and structural music composition.

As conceived, Bogu is primarily a tool for composition. Its conception was inspired by a desire to think about music in different ways, and Lisp provided a fascinating structure by which to do so. While its motivation was primarily theoretical and cognitive, I hope it can be a useful tool for making interesting music as well! It has some live-coding capability, and I'm personally interested in the unexpected behaviors its symbolic and combinatoric nature might lead to as it grows.

### A Quick Example
```lisp
;; Define a raw data pool of pitch symbols
def my-arp [ eb2 eb3 bb3 eb4 f4 g4 c5 d5 a5 ]

;; Generate a structure (sarp - sustained arpeggio) with rhythmic values (st - sixteenth triplet; w - whole)
def my-sarp [ sarp st w my-arp ]

;; Test the structure in real-time with automated envelopes
i 1
live-loop my-loop [
  sweep pan 0 100 [ sweep flt 75 30 [ my-sarp ] ]
  sweep pan 100 0 [ sweep flt 30 75 [ transpose -2 [ my-sarp ] ] ] 
]
```

## Getting Started
Bogu is lightweight and runs entirely locally.
1. **Prerequisites:** Install [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp) and [Csound](https://csound.com/).
2. **Clone:** `git clone https://github.com/yourusername/bogu.git`
3. **Boot:** Open your terminal, navigate to the `bogu` folder, and run your Lisp compiler (`sbcl`).
4. **Load:** Type `(load "boot.lisp")` to spin up the Csound server and drop into the Bogu prompt.

## Contributing
Bogu is still in an early stage. If you love Lisp, Csound, or generative music, check out the `good first issue` tags in the tracker. See `CONTRIBUTING.md` for details on how to run the test harness.