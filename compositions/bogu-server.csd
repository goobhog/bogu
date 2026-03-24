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

; --- INSTRUMENT 4: WARM PAD / SUB BASS ---
instr 4
  idur = p3
  ifreq = cpspch(p4)   ; Convert Bogu's pitch decimal (e.g., 8.00) to Hz
  iamp = p5 * 0.15     ; Read Bogu's velocity (e.g., 0.8)

  ; 1. The Oscillator: Generate the raw saw wave first!
  asig vco2 iamp, ifreq

  ; 2. The Envelope: Shape the audio with a slow attack and release
  aenv linen asig, 0.5, idur, 0.8 

  ; 3. The Filter: Roll off the harsh highs to make it sound dark and rolling
  afilt moogladder aenv, ifreq * 3, 0.2

  ; ROUTING: Send the audio to the Master Bus and the Reverb Bus!
  vincr ga_master, afilt
  vincr ga_rvb, afilt
endin

; --- THE FX CONTROLLERS ---
instr 98
  gk_reverb = p5
endin

; --- THE NETWORK BRAIN & MASTER BUS ---
instr 99
  ; Master Reverb and Limiter
  aWetL, aWetR reverbsc ga_rvb, ga_rvb, 0.85, 12000
  aMixL = ga_master + (aWetL * gk_reverb)
  aMixR = ga_master + (aWetR * gk_reverb)
  ; The Analog Soft-Clipper
  ; The output will gracefully saturate but will NEVER exceed 0.95 (leaving 5% safety headroom)
  aOutL = 0.95 * tanh(aMixL)
  aOutR = 0.95 * tanh(aMixR)
  outs aOutL, aOutR
  clear ga_master, ga_rvb
endin

</CsInstruments>
<CsScore>
f 2 0 4096 10 1
; Keep the server open and listening for 24 hours
i 99 0 86400 
</CsScore>
</CsoundSynthesizer>