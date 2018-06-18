<CsoundSynthesizer>
<CsOptions>

-odac

</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 4

instr 1

ares linen .5, .03, p3, .02
asig oscil ares, cpspch(p4)
     outs asig,asig

endin

</CsInstruments>
<CsScore>

t 0 60

i 1.01 0 4.0 7.01
i 1.02 0.25 3.75 7.08
i 1.03 0.5 3.5 8.01
i 1.04 0.75 3.25 8.05
i 1.05 1.0 3.0 8.07
i 1.06 1.25 2.75 8.04

e
</CsScore>
</CsoundSynthesizer>