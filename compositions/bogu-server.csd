<CsoundSynthesizer>
<CsOptions>
-odac -m0 -d -L /tmp/bogu_pipe
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0
;===========================================
; 1. The True Stereo Busses
;===========================================
ga_master_L init 0
ga_master_R init 0
ga_rvb_L init 0
ga_rvb_R init 0

; ==========================================
; 2. THE SYNTHS
; ==========================================
instr 1 ; SINE
  icps = cpspch(p4)
  iamp = p5 * 0.15
  
  Svol sprintf "vol_%d", int(p1)
  Span sprintf "pan_%d", int(p1)
  Srvb sprintf "rvb_%d", int(p1)
  
  ; 1. INIT-RATE: Fetch exactly once when the note is born
  ivol chnget Svol
  ipan chnget Span
  irvb chnget Srvb
  
  ; 2. CONTROL-RATE: Fetch continuously while the note plays
  kvol chnget Svol
  kpan chnget Span
  krvb chnget Srvb

  kvol_sm portk kvol, 0.05, ivol
  kpan_sm portk kpan, 0.05, ipan
  krvb_sm portk krvb, 0.05, irvb

  asig poscil iamp, icps, 2
  ares linen asig, .03, p3, .05

  aL, aR pan2 (ares * kvol_sm), kpan_sm
  vincr ga_master_L, aL
  vincr ga_master_R, aR
  vincr ga_rvb_L, aL * krvb_sm
  vincr ga_rvb_R, aR * krvb_sm
endin

instr 2 ; SAW
  icps = cpspch(p4)
  iamp = p5 * 0.15
  Svol sprintf "vol_%d", int(p1)
  Span sprintf "pan_%d", int(p1)
  Srvb sprintf "rvb_%d", int(p1)
  ivol chnget Svol
  ipan chnget Span
  irvb chnget Srvb
  kvol chnget Svol
  kpan chnget Span
  krvb chnget Srvb
  kvol_sm portk kvol, 0.05, ivol
  kpan_sm portk kpan, 0.05, ipan
  krvb_sm portk krvb, 0.05, irvb

  asig vco2 iamp, icps, 0
  ares linen asig, .03, p3, .05

  aL, aR pan2 (ares * kvol_sm), kpan_sm
  vincr ga_master_L, aL
  vincr ga_master_R, aR
  vincr ga_rvb_L, aL * krvb_sm
  vincr ga_rvb_R, aR * krvb_sm
endin

instr 3 ; PLUCK
  icps = cpspch(p4)
  iamp = p5 * 0.15
  Svol sprintf "vol_%d", int(p1)
  Span sprintf "pan_%d", int(p1)
  Srvb sprintf "rvb_%d", int(p1)
  ivol chnget Svol
  ipan chnget Span
  irvb chnget Srvb
  kvol chnget Svol
  kpan chnget Span
  krvb chnget Srvb
  kvol_sm portk kvol, 0.05, ivol
  kpan_sm portk kpan, 0.05, ipan
  krvb_sm portk krvb, 0.05, irvb

  asig pluck iamp, icps, icps, 2, 1
  ares linen asig, .01, p3, .1

  aL, aR pan2 (ares * kvol_sm), kpan_sm
  vincr ga_master_L, aL
  vincr ga_master_R, aR
  vincr ga_rvb_L, aL * krvb_sm
  vincr ga_rvb_R, aR * krvb_sm
endin

; ==========================================
; 3. THE FX AND MASTER BUS
; ==========================================

instr 98 ; STEREO REVERB
  ; Uses Csound's high-quality stereo reverb opcode
  aL, aR reverbsc ga_rvb_L, ga_rvb_R, 0.85, 7000
  vincr ga_master_L, aL
  vincr ga_master_R, aR
endin

instr 99 ; MASTER BUS
  aOutL = 0.95 * tanh(ga_master_L)
  aOutR = 0.95 * tanh(ga_master_R)
  outs aOutL, aOutR
  
  ; Clear the bus for the next cycle
  ga_master_L = 0
  ga_master_R = 0
  ga_rvb_L = 0
  ga_rvb_R = 0
endin

instr 100 ; CONTROL ROUTER
  itrack = p4
  iparam = p5
  istart = p6
  iend = p7
  
  kval linseg istart, p3, iend
  
  if iparam == 1 then
    chnset kval, sprintf("vol_%d", itrack)
  elseif iparam == 2 then
    chnset kval, sprintf("pan_%d", itrack)
  elseif iparam == 3 then
    chnset kval, sprintf("rvb_%d", itrack)
  endif
endin

</CsInstruments>
<CsScore>
f 2 0 4096 10 1
; Keep the server open and listening for 24 hours
i 99 0 86400
i 98 0 86400
</CsScore>
</CsoundSynthesizer>