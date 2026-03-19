<CsoundSynthesizer>
<CsOptions>
-odac -L stdin -m0
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 4
ga_master init 0
ga_rvb init 0
gk_reverb init 0
giwave ftgen 2, 0, 4096, 10, 1
instr 98
gk_reverb = p4
endin

instr 99
aWetL, aWetR reverbsc ga_rvb, ga_rvb, 0.85, 12000
aWetL butterhp aWetL, 150
aWetR butterhp aWetR, 150
aMixL = ga_master + (aWetL * gk_reverb)
aMixR = ga_master + (aWetR * gk_reverb)
aLimitL = 3.9 * tanh(aMixL / 3.9)
aLimitR = 3.9 * tanh(aMixR / 3.9)
outs aLimitL, aLimitR
clear ga_master, ga_rvb
endin

instr 1
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 2
icps = cpspch(p4)
iamp = p5 * 0.15
asig vco2 iamp, icps, 0
ares linen asig, .03, p3, .05
vincr ga_master, ares
vincr ga_rvb, ares
endin

instr 3
icps = cpspch(p4)
iamp = p5 * 0.15
asig pluck iamp, icps, icps, 2, 1
ares linen asig, .01, p3, .1
vincr ga_master, ares
vincr ga_rvb, ares
endin

instr 4
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 5
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 6
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 7
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 8
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 9
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 10
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 11
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 12
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 13
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 14
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 15
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

instr 16
iamp = p5 * 0.15
ares linen iamp, .03, p3, .05
asig poscil ares, cpspch(p4), 2
vincr ga_master, asig
vincr ga_rvb, asig
endin

</CsInstruments>
<CsScore>
f 0 36000
i 99 0 36000
</CsScore>
</CsoundSynthesizer>