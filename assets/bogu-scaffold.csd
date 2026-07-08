<CsoundSynthesizer>
<CsOptions>
-odac -m0 -d -L /tmp/bogu_pipe
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 128
nchnls = 2
0dbfs = 1.0

ga_master_L init 0
ga_master_R init 0
ga_rvb_L init 0
ga_rvb_R init 0

gieng fluidEngine
gisf fluidLoad "orchestra.sf2", gieng, 1

;;;BOGU_SYNTH_RACK;;;

instr 97 ; SOUNDFONT BUS
aL, aR fluidOut gieng
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * 0.20
vincr ga_rvb_R, aR * 0.20
endin

instr 98 ; STEREO REVERB
aSafeL = tanh(ga_rvb_L)
aSafeR = tanh(ga_rvb_R)
aL, aR reverbsc aSafeL, aSafeR, 0.85, 7000
vincr ga_master_L, aL
vincr ga_master_R, aR
endin

instr 99 ; MASTER BUS
aOutL = 0.95 * tanh(ga_master_L)
aOutR = 0.95 * tanh(ga_master_R)
outs aOutL, aOutR
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
elseif iparam == 4 then
  chnset kval, sprintf("flt_%d", itrack)
endif
endin

</CsInstruments>
<CsScore>
f 2 0 4096 10 1
i 99 0 86400
i 98 0 86400
i 97 0 86400
</CsScore>
</CsoundSynthesizer>