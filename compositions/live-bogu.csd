<CsoundSynthesizer>
<CsOptions>
-odac -m0d -L stdin
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 4
giwave ftgen 2, 0, 4096, 10, 0.216, 0.130, 0.043, 0.026, 0.016, 0.011, 0.008, 0.007, 0.004, 0.001, 0.002, 0.003, 0.001, 0.001
instr 1
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 2
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 3
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 4
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 5
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 6
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 7
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 8
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 9
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 10
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 11
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 12
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 13
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 14
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 15
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
instr 16
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin
</CsInstruments>
<CsScore>
f 0 36000
</CsScore>
</CsoundSynthesizer>
