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

instr 1

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 2

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 3

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw pluck iamp, icps, icps, 2, 1  

kcutoff = cpsoct((kflt_sm * 10) + 4)
asig moogladder asig_raw, kcutoff, 0.25  

ares linen asig, 0.01, p3, 0.1

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 4

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 5

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 6

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 7

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 8

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 9

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 10

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 11

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 12

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 13

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 14

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 15

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 16

icps = cpspch(p4)
iamp = p5 * 0.15

Svol sprintf "vol_%d", int(p1)
Span sprintf "pan_%d", int(p1)
Srvb sprintf "rvb_%d", int(p1)
Sflt sprintf "flt_%d", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

asig_raw poscil iamp, icps, 2  

asig = asig_raw  

ares linen asig, 0.03, p3, 0.05

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm
endin

instr 97 ; SOUNDFONT BUS
aL, aR fluidOut gieng
vincr ga_master_L, aL
vincr ga_master_R, aR
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
