<CsoundSynthesizer>
<CsOptions>

-odac

</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 4

giwave	ftgen	2, 0, 4096, 10, 0.216, 0.130, 0.043, 0.026, 0.016, 0.011, 0.008, 0.007, 0.004, 0.001, 0.002, 0.003, 0.001, 0.001

instr 1
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin


</CsInstruments>
<CsScore>

t 0 60

i 1 0 1.0 8.03
i 1 1.0 1.0 8.07
i 1 2.0 1.0 7.1
i 1 3.0 1.0 8.0
i 1 4.0 1.0 8.04
i 1 5.0 1.0 8.07
i 1 6.0 1.0 7.09
i 1 7.0 1.0 8.01
i 1 8.0 1.0 8.04
i 1 9.0 0.5 8.0
i 1 9.5 0.5 8.04
i 1 10.0 0.5 8.07
i 1 10.5 0.33333334 8.0
i 1 10.833333 0.33333334 8.04
i 1 11.166666 0.33333334 8.07

e
</CsScore>
</CsoundSynthesizer>