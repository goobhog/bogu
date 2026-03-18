<CsoundSynthesizer>
<CsOptions>
-odac -L stdin
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 4
giwave ftgen 2, 0, 4096, 10, 1
instr 1
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 2
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 3
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 4
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 5
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 6
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 7
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 8
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 9
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 10
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 11
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 12
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 13
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 14
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 15
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

instr 16
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin

</CsInstruments>
<CsScore>
f0 36000
e
</CsScore>
</CsoundSynthesizer>