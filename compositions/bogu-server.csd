<CsoundSynthesizer>

<CsOptions>
-odac -m0 -d -L /tmp/bogu_pipe
</CsOptions>

<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0

ga_master init 0
ga_rvb init 0
gk_reverb init 0.85

; --- HARDWARE RACK ---
instr 1 ; SINE
  icps = cpspch(p4)
  iamp = p5 * 0.15
  ares linen iamp, .03, p3, .05
  asig poscil ares, icps, 2
  vincr ga_master, asig
  vincr ga_rvb, asig
endin

instr 2 ; SAW
  icps = cpspch(p4)
  iamp = p5 * 0.15
  asig vco2 iamp, icps, 0
  ares linen asig, .03, p3, .05
  vincr ga_master, ares
  vincr ga_rvb, ares
endin

instr 3 ; PLUCK
  icps = cpspch(p4)
  iamp = p5 * 0.15
  asig pluck iamp, icps, icps, 2, 1
  ares linen asig, .01, p3, .1
  vincr ga_master, ares
  vincr ga_rvb, ares
endin

; --- THE NETWORK BRAIN & MASTER BUS ---
instr 99
  k_instr init 0
  k_dur init 0
  k_pitch init 0
  k_vol init 0
  

  ; Master Reverb and Limiter
  aWetL, aWetR reverbsc ga_rvb, ga_rvb, 0.85, 12000
  aMixL = ga_master + (aWetL * gk_reverb)
  aMixR = ga_master + (aWetR * gk_reverb)
  outs 3.9 * tanh(aMixL/3.9), 3.9 * tanh(aMixR/3.9)
  clear ga_master, ga_rvb
endin

</CsInstruments>
<CsScore>
f 2 0 4096 10 1
; Keep the server open and listening for 24 hours
i 99 0 86400 
</CsScore>
</CsoundSynthesizer>